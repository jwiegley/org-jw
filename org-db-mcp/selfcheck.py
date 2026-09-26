"""Flake check: MCP handshake over stdio, then org_sql must reject writes.

Usage: selfcheck.py <path-to-org-db-mcp-binary>
Stdlib only; waits for each response id (no sleeps). The caller points
PGHOST at a nonexistent socket dir so nothing can reach a real database.
"""

import json
import signal
import subprocess
import sys

signal.alarm(120)  # fail rather than hang the build
srv = subprocess.Popen([sys.argv[1]], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)


def send(msg):
    srv.stdin.write(json.dumps({"jsonrpc": "2.0", **msg}) + "\n")
    srv.stdin.flush()


def call(i, method, params):
    send({"id": i, "method": method, "params": params})
    for line in srv.stdout:
        resp = json.loads(line)
        if resp.get("id") == i:
            assert "result" in resp, resp
            return resp["result"]
    sys.exit(f"server closed stdout before answering id {i}")


call(1, "initialize", {"protocolVersion": "2025-06-18", "capabilities": {},
                       "clientInfo": {"name": "selfcheck", "version": "0"}})
send({"method": "notifications/initialized"})
tools = {t["name"] for t in call(2, "tools/list", {})["tools"]}
assert {"org_sql", "org_search"} <= tools, tools

expected = {
    "DELETE FROM entries":
        "query must be a single read-only SELECT (or WITH ... SELECT)",
    "SELECT 1; DELETE FROM entries":
        "only a single statement is allowed (no ';'-chained statements)",
    "WITH d AS (DELETE FROM entries RETURNING *) SELECT * FROM d":
        "query contains forbidden keyword(s): ['delete']",
}
for i, (query, err) in enumerate(expected.items(), start=3):
    res = call(i, "tools/call", {"name": "org_sql", "arguments": {"query": query}})
    got = json.loads(res["content"][0]["text"])
    assert got == {"error": err}, (query, got)

srv.stdin.close()
assert srv.wait() == 0, srv.returncode
print("org-db-mcp selfcheck: ok")
