# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0](https://github.com/elixir-telemetry/telemetry/tree/v0.2.0)

The main point of this release is to mark base Telemetry API as stable, so that other libraries can
rely on it without worrying about backwards compatibility.

### Removed

* Removed `Telemetry.attach/4` and `Telemetry.attach_many/4` - the handler config is now required.

### Fixed

* Fixed type specs which were producing Dialyzer errors.

## [0.1.0](https://github.com/elixir-telemetry/telemetry/tree/v0.1.0)

First release of Telemetry library.
