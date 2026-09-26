#!/usr/bin/env python3
"""MCP server exposing the org PostgreSQL database to the Hermes agent.

This script lives in the org-jw repository at ``org-db-mcp/org-db-mcp.py``
and is packaged as the flake output ``packages.<system>.org-db-mcp``, a
wrapper that runs it under its own python312 with ``mcp`` and ``psycopg2``.
It runs as a stdio MCP child. On vulcan it is the ``org-db`` entry of the
Hermes microVM's ``mcpServers``: the ``orgDbMcpServer`` wrapper in
/etc/nixos/modules/services/hermes-vm.nix supplies the PG*, ORG_CONFIG and
OPENROUTER_API_KEY environment and puts ``org`` (org-jw) on PATH before
exec'ing this package.
Provides two READ-ONLY views into the ``org`` database:

  * ``org_sql``    — direct, sanitized SELECT queries via psycopg2.
  * ``org_search`` — semantic search by shelling out to the ``org`` CLI
    (``org db search``).

Both tools are strictly read-only. ``org_sql`` rejects anything that is
not a single bare SELECT, and the database connection itself never prints
or returns the PGPASSWORD value.

Environment variables (PostgreSQL — psycopg2 reads these via libpq):
  PGHOST       default: 127.0.0.1
  PGPORT       default: 5432
  PGDATABASE   default: org
  PGUSER       default: openclaw
  PGPASSWORD   no default — supplied by the caller's environment

Environment variables (semantic search — org CLI):
  ORG_CONFIG          default: ${HOME}/.config/org/config.yaml
  ORG_DB_BASE_URL     default: http://127.0.0.1:4000 (host LLM gateway)
  ORG_DB_MODEL        default: bge-m3-mlx-fp16 (embedding model)
  OPENROUTER_API_KEY  no default — passed to org as --api-key

The ``org`` binary (org-jw) is expected on PATH; the caller's wrapper puts
it there.
"""

import json
import os
import re
import shutil
import subprocess
from typing import Any

import psycopg2
from mcp.server.fastmcp import FastMCP

# -- PostgreSQL connection parameters --------------------------------------
# psycopg2/libpq already honors PG* env vars, but we read them explicitly so
# the defaults are pinned here (PGUSER=openclaw is the org database's actual
# role name, etc.) and so we can pass them explicitly to psycopg2.connect().
PGHOST = os.getenv("PGHOST", "127.0.0.1")
PGPORT = os.getenv("PGPORT", "5432")
PGDATABASE = os.getenv("PGDATABASE", "org")
PGUSER = os.getenv("PGUSER", "openclaw")
# PGPASSWORD is intentionally NOT echoed anywhere. We pull it from the env at
# connect time only and never include it in tool output or error messages.

# -- Semantic-search (org CLI) parameters ----------------------------------
ORG_CONFIG = os.getenv(
    "ORG_CONFIG", os.path.expanduser("~/.config/org/config.yaml")
)
ORG_DB_BASE_URL = os.getenv("ORG_DB_BASE_URL", "http://127.0.0.1:4000")
# bge-m3-mlx-fp16, NOT the old "hera/bge-m3". The latter was a LiteLLM alias and
# LiteLLM was retired 2026-08-01; the oMLX relay on :4000 serves exactly one
# embedding model under its bare name (verified against /v1/models). Nothing in
# the tree sets ORG_DB_MODEL, so this default is what actually gets used -- it had
# been naming a model that no longer exists, which 400s every semantic search.
ORG_DB_MODEL = os.getenv("ORG_DB_MODEL", "bge-m3-mlx-fp16")
SEARCH_TIMEOUT_S = float(os.getenv("ORG_DB_TIMEOUT_S", "60"))

# Statement-level keywords that mutate state or escape the single-SELECT
# contract. A query containing any of these as a standalone word is rejected
# outright — this is a coarse but deliberately conservative guard, layered on
# top of the "must start with SELECT" and "no semicolon" checks below.
FORBIDDEN_KEYWORDS = frozenset(
    {
        "insert",
        "update",
        "delete",
        "drop",
        "alter",
        "create",
        "truncate",
        "grant",
        "revoke",
        "copy",
        "call",
        "do",
        "merge",
        "replace",
        "vacuum",
        "analyze",
        "reindex",
        "cluster",
        "comment",
        "set",
        "reset",
        "begin",
        "commit",
        "rollback",
        "savepoint",
        "lock",
        "execute",
        "prepare",
        "deallocate",
        "listen",
        "notify",
        "refresh",
        "import",
        "load",
        "security",  # guards against "SECURITY LABEL"
    }
)


def _err(msg: str) -> str:
    return json.dumps({"error": msg})


def _strip_sql_comments(sql: str) -> str:
    """Remove ``--`` line comments and ``/* */`` block comments.

    Comments are stripped before validation so that a payload cannot hide a
    forbidden keyword (or a smuggled second statement) behind a comment.
    """
    # Block comments (non-greedy, across newlines).
    sql = re.sub(r"/\*.*?\*/", " ", sql, flags=re.DOTALL)
    # Line comments to end-of-line.
    sql = re.sub(r"--[^\n]*", " ", sql)
    return sql


def _validate_select(query: str) -> str | None:
    """Return None if ``query`` is a single read-only SELECT, else an error.

    The contract is deliberately strict:
      * exactly one statement (no semicolon-chaining; a single trailing
        semicolon is tolerated and trimmed),
      * the statement must begin with SELECT (or a leading WITH ... that
        feeds a SELECT — common-table-expression read queries),
      * no statement-mutating keyword appears as a standalone token.
    """
    cleaned = _strip_sql_comments(query).strip()
    if not cleaned:
        return "query is empty"

    # Tolerate a single trailing semicolon, but reject any semicolon that
    # would chain a second statement.
    if cleaned.endswith(";"):
        cleaned = cleaned[:-1].rstrip()
    if ";" in cleaned:
        return "only a single statement is allowed (no ';'-chained statements)"

    # Must be a read query: a bare SELECT, or a CTE (WITH ...) that resolves
    # to a SELECT. Anything else (including a WITH that wraps an INSERT) is
    # caught by the keyword scan below.
    lowered = cleaned.lower()
    if not (lowered.startswith("select") or lowered.startswith("with")):
        return "query must be a single read-only SELECT (or WITH ... SELECT)"

    # Scan for any forbidden keyword as a standalone token. Using word
    # boundaries avoids flagging substrings like "selected_at" as "select".
    tokens = set(re.findall(r"[a-z_]+", lowered))
    hit = tokens & FORBIDDEN_KEYWORDS
    if hit:
        return f"query contains forbidden keyword(s): {sorted(hit)}"

    return None


def _rows_to_markdown(columns: list[str], rows: list[tuple[Any, ...]]) -> str:
    """Render a result set as a GitHub-flavored Markdown table.

    Cell values are stringified and pipe characters escaped so the table
    stays well-formed. An empty result set returns just the header row.
    """

    def cell(value: Any) -> str:
        if value is None:
            return ""
        return str(value).replace("|", "\\|").replace("\n", " ")

    header = "| " + " | ".join(columns) + " |"
    divider = "| " + " | ".join("---" for _ in columns) + " |"
    body = [
        "| " + " | ".join(cell(v) for v in row) + " |" for row in rows
    ]
    return "\n".join([header, divider, *body])


mcp = FastMCP("org-db")


@mcp.tool()
def org_sql(query: str, limit: int = 100) -> str:
    """Run a READ-ONLY SQL SELECT against the org PostgreSQL database.

    Use this when you know the schema and want exact rows — e.g. to list
    headings, tags, or properties from the org store. For fuzzy/semantic
    lookup ("find notes about X"), prefer ``org_search``.

    SECURITY: only a single bare ``SELECT`` (or ``WITH ... SELECT``) is
    permitted. Semicolon-chained statements and any mutating keyword
    (INSERT/UPDATE/DELETE/DROP/ALTER/CREATE/GRANT/COPY/CALL/etc.) are
    rejected before the query ever reaches the database.

    Args:
      query: a single read-only SELECT statement. A lone trailing semicolon
        is tolerated; multiple statements are not.
      limit: maximum rows to return (1–1000). Default 100. Applied as an
        outer ``LIMIT`` so it is enforced even if the query omits one.

    Returns a Markdown table of the result rows, or a JSON ``{"error": ...}``
    object on validation/connection failure. The PGPASSWORD value is never
    included in any output.
    """
    if limit < 1 or limit > 1000:
        return _err("limit must be between 1 and 1000")

    bad = _validate_select(query)
    if bad is not None:
        return _err(bad)

    # Strip a trailing semicolon (validated as safe above) before wrapping.
    inner = _strip_sql_comments(query).strip().rstrip(";").rstrip()
    # Wrap the validated SELECT in an outer LIMIT so the cap is enforced
    # regardless of any LIMIT inside the user's query.
    wrapped = f"SELECT * FROM ({inner}) AS org_sql_sub LIMIT %s"

    conn = None
    try:
        # psycopg2 reads PGPASSWORD from the environment via libpq; we never
        # name the password in code or output.
        conn = psycopg2.connect(
            host=PGHOST,
            port=PGPORT,
            dbname=PGDATABASE,
            user=PGUSER,
        )
        # Defense in depth: mark the whole transaction read-only so the
        # backend itself rejects any write that slipped past our parser.
        conn.set_session(readonly=True, autocommit=False)
        with conn.cursor() as cur:
            cur.execute(wrapped, (limit,))
            columns = (
                [desc[0] for desc in cur.description] if cur.description else []
            )
            rows = cur.fetchall()
        conn.rollback()
    except psycopg2.Error as exc:
        # psycopg2 errors can carry the failing statement but not the
        # connection password; still, return only the diagnostic message.
        return _err(f"database error: {exc.pgerror or exc}")
    except Exception as exc:  # noqa: BLE001 — surface anything else cleanly
        return _err(f"query failed: {exc}")
    finally:
        if conn is not None:
            conn.close()

    if not columns:
        return _err("query returned no result set (not a SELECT?)")

    return _rows_to_markdown(columns, rows)


@mcp.tool()
def org_search(query: str, n: int = 10) -> str:
    """Semantic search over the org database via embeddings.

    Shells out to the ``org`` CLI (``org db search``): it embeds the query
    through the LLM gateway (the embedding model on the local gateway) and
    returns the nearest org entries. Use this for natural-language lookup
    ("notes about the pool heater") where you don't know the exact schema
    or wording.

    Args:
      query: free-text search terms.
      n: number of nearest results to return (1–50). Default 10.

    Returns the raw ``org db search`` output, or a JSON ``{"error": ...}``
    object on failure. The OPENROUTER_API_KEY is passed to ``org`` only as
    a process argument and is never echoed back.
    """
    if not query.strip():
        return _err("query is empty")
    if n < 1 or n > 50:
        return _err("n must be between 1 and 50")

    if shutil.which("org") is None:
        return _err("the 'org' CLI is not on PATH")

    api_key = os.getenv("OPENROUTER_API_KEY", "")

    # Equivalent to (with the default base URL and model):
    #   org -c <config> db search --base-url http://127.0.0.1:4000 \
    #       -m bge-m3-mlx-fp16 --api-key "$KEY" "<query>" -n <n>
    cmd = [
        "org",
        "-c",
        ORG_CONFIG,
        "db",
        "search",
        "--base-url",
        ORG_DB_BASE_URL,
        "-m",
        ORG_DB_MODEL,
        "--api-key",
        api_key or "unused",
        query,
        "-n",
        str(n),
    ]

    try:
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=SEARCH_TIMEOUT_S,
            check=False,
        )
    except subprocess.TimeoutExpired:
        return _err(f"org db search timed out after {SEARCH_TIMEOUT_S}s")
    except OSError as exc:
        return _err(f"failed to invoke org: {exc}")

    if proc.returncode != 0:
        # stderr may name the config path but not the API key (which we pass
        # as an arg, not echoed by org); still trim to keep output bounded.
        return _err(
            f"org db search exited {proc.returncode}: {proc.stderr.strip()[:500]}"
        )

    return proc.stdout


if __name__ == "__main__":
    mcp.run()
