# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [UNRELEASED]

The event value has been removed. Now event payload is an arbitrary map. There are two reasons for
this change:

* It invalidates the notion of some specific property of the event being special;
* It is up to the consumer of the event to decide what part of the payload is important - the event
  only indicates that *a thing*  happened.

The `execute/2` API should be now used instead of `execute/3` (previous `execute/2` which set
metadata as empty map has been removed, this is the only backward-incompatible change). This change
also means that a handler function now has only three argument (a four-argument version is still
valid but is deprecated).

### Changed

* `execute/2` now accepts an event name and a map of event data;
* Handler functions have only 3 arguments now.

### Deprecated

* `:telemetry.execute/3` in favour of `:telemetry.execute/2`. If `execute/3` is used, the event
  value is merged into metadata map under `:value` key and provided as event data to `execute/2`.
* 4-argument handler functions. If a 4-argument handler function is invoked, the event value is
  extracted from event data map from `:value` key, defaulting to `0`.

## [0.3.0](https://github.com/elixir-telemetry/telemetry/tree/v0.3.0)

This release marks the conversion from Elixir to Erlang. This is a breaking change, but the benefits
largely surpass the drawbacks - Telemetry core can now be used by all projects running on the BEAM,
regardless of the language they're written in.

### Added

* Added `:telemetry.handler/0`, `:telemetry.handler_function/0` and `:telemetry.handler_config/0`
  types.

### Changed

* The library has been rewritten to Erlang. In Elixir, `:telemetry` module has to be used in place
  of `Telemetry`. In Erlang, `telemetry` module has to be used in place of `'Elixir.Telemetry'`;
* `:telemetry.list_handlers/1` returns a list of maps (of type `:telemetry.handler/0`) instead of
  a list of tuples;
* `:telemetry.attach/4` and `:telemetry.attach_many/4` replaced the 5-arity versions and now accept
  an anonymous function for the handler function instead of a module and function name.

### Removed

* Removed `:telemetry.attach/5` and `:telemetry.attach_many/5` - 4-arity versions need to be used
  now instead.

## [0.2.0](https://github.com/elixir-telemetry/telemetry/tree/v0.2.0)

The main point of this release is to mark base Telemetry API as stable, so that other libraries can
rely on it without worrying about backwards compatibility.

### Removed

* Removed `Telemetry.attach/4` and `Telemetry.attach_many/4` - the handler config is now required.

### Fixed

* Fixed type specs which were producing Dialyzer errors.

## [0.1.0](https://github.com/elixir-telemetry/telemetry/tree/v0.1.0)

First release of Telemetry library.
