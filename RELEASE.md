# Steps for publishing new version

1. Update version in `src/telemetry.app.src`
2. Update version in `docs.sh`
3. Run `./docs.sh`
4. Run `rebar3 hex publish`
5. Run `rebar3 hex docs`
