# The org-db MCP server (stdio), run under its own Python with mcp + psycopg2.
# The caller supplies the PG* / ORG_CONFIG / OPENROUTER_API_KEY environment
# (ORG_DB_* have defaults) and puts `org` on PATH; see org-db-mcp.py's docstring.
{ writeShellScriptBin, python312 }:

let
  python = python312.withPackages (ps: [ ps.mcp ps.psycopg2 ]);
in
writeShellScriptBin "org-db-mcp" ''
  exec ${python}/bin/python3 ${./org-db-mcp.py} "$@"
''
