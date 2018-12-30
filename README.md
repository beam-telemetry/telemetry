# Telemetry

[Documentation](https://hexdocs.pm/telemetry/)

Telemetry is a dynamic dispatching library for metrics and instrumentations. It is lightweight,
small and can be used in any Erlang or Elixir project.

In a nutshell, you register a custom module and function to be invoked for certain events,
which are executed whenever there is such event. Event name is a list of atoms. Each event is
composed of a numeric value and can have metadata attached to it. Let's see an example.

Imagine that you have a web application and you'd like to log latency and response status for each
incoming request. With Telemetry, you can build a module which does exactly that whenever a response
is sent. The first step is to execute a measurement.

In Elixir:

```elixir
:telemetry.execute([:web, :request, :done], latency, %{request_path: path, status_code: status})
```

In Erlang:

```erlang
telemetry:execute([web, request, done], Latency, #{request_path => Path, status_code => Status})
```

Then you can create a module to be invoked whenever the event happens.

In Elixir:

```elixir
defmodule LogResponseHandler do
  require Logger

  def handle_event([:web, :request, :done], latency, metadata, _config) do
    Logger.info("[#{metadata.request_path}] #{metadata.status_code} sent in #{latency}")
  end
end
```

In Erlang:

```erlang
-module(log_response_handler).

-include_lib("kernel/include/logger.hrl")

handle_event([web, request, done], Latency, #{path := Path,
                                              status_code := Status}, _Config) ->
  ?LOG_INFO("[~s] ~p sent in ~p", [Path, Status, Latency]).
  
```

Finally, all you need to do is to attach the module to the executed event.

In Elixir:

```elixir
:telemetry.attach("log-response-handler", [:web, :request, :done], &LogResponseHandler.handle_event/4, nil)
```

In Erlang:

```erlang
telemetry:attach(<<"log-response-handler">>, [web, request, done], fun log_response_handler:handle_event/4, [])
```

You might think that it isn't very useful, because you could just as well write a log statement
instead of `Telemetry.execute/2` call - and you would be right! But now imagine that each Elixir library
would publish its own set of events with information useful for introspection. Currently each library
rolls their own instrumentation layer - Telemetry aims to provide a single interface for these use
cases across whole ecosystem.

## Installation

Telemetry is available on [Hex](https://hex.pm/packages/telemetry). To install, just add it to
your dependencies in `mix.exs`:

```elixir
defp deps() do
  [
    {:telemetry, "~> 0.3.0"}
  ]
end
```

or `rebar.config`:

``` erlang
{deps, [{telemetry, "~> 0.3.0"}]}.
```

## Copyright and License

Copyright (c) 2018, Chris McCord and Erlang Solutions.

Telemetry source code is licensed under the [Apache License, Version 2.0](LICENSE).
