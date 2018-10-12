# Telemetry

[Documentation](https://hexdocs.pm/telemetry/0.2.0)

Telemetry is a dynamic dispatching library for metrics and instrumentations. It is lightweight,
small and can be used in any Elixir project.

In a nutshell, you register a custom module and function to be invoked for certain events,
which are executed whenever there is such event. Event name is a list of atoms. Each event is
composed of a numeric value and can have metadata attached to it. Let's see an example.

Imagine that you have a web application and you'd like to log latency and response status for each
incoming request. With Telemetry, you can build a module which does exactly that whenever a response
is sent:

```elixir
defmodule LogResponseHandler do
  require Logger

  def handle_event([:web, :request, :done], latency, metadata, _config) do
    Logger.info("[#{metadata.request_path}] #{metadata.status_code} sent in #{latency}")
  end
end
```

and attach this module to the `[:web, :request, :done]` event:

```elixir
Telemetry.attach("log-response-handler", [:web, :request, :done], LogResponseHandler, :handle_event, nil)
```

Finally, in your application code you would run:

```elixir
Telemetry.execute([:web, :request, :done], latency, %{request_path: path, status_code: status})
```

You might think that it isn't very useful, because you could just as well write a log statement
instead of `Telemetry.execute/2` call - and you would be right! But now imagine that each Elixir library
would publish its own set of events with information useful for introspection. Currently each library
rolls their own instrumentation layer - Telemetry aims to provide a single interface for these use
cases across whole ecosystem.

## Installation

Telemetry is available on [Hex](https://hex.pm/packages/telemetry). To install, just add it to
your dependencies:

```elixir
defp deps() do
  [
    {:telemetry, "~> 0.2.0"}
  ]
end
```

## Copyright and License

Copyright (c) 2018, Chris McCord and Erlang Solutions.

Telemetry source code is licensed under the [Apache License, Version 2.0](LICENSE).
