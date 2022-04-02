# Steps for publishing new version

1. Update version in `src/telemetry.app.src`
2. Run `rebar3 hex publish` (requires https://hexdocs.pm/rebar3_hex)
3. Run `rebar3 hex publish docs` (requires https://hexdocs.pm/rebar3_ex_doc)
