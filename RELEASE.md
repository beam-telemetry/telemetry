# Steps for publishing new version

1. Update `CHANGELOG.md` and version in `src/telemetry.app.src`
2. Run `rebar3 hex publish` (requires https://hexdocs.pm/rebar3_hex)
3. Run `rebar3 hex publish docs` (requires https://hexdocs.pm/rebar3_ex_doc)

## Documentation

Run `rebar3 docs` to generate HTML, Markdown, and EPUB files in `doc/`.
The Markdown output includes `llms.txt`.

Run `rebar3 hex build docs` to build a local documentation archive before publishing.
The publishing commands use the same documentation formats.
