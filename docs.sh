#!/bin/bash
set -e

# Setup:
#
#     mix escript.install github elixir-lang/ex_doc
#     asdf install erlang 24.0.2
#     asdf local erlang 24.0.2

rebar3 compile
rebar3 as docs edoc
version=0.4.3
ex_doc "telemetry" $version "_build/default/lib/telemetry/ebin" \
  --source-ref v${version} \
  --config docs.exs $@
