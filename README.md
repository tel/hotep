
# Hotep

*"Fail Fast" process management for Haskell; inspired by Erlang*

[![Build Status](https://travis-ci.org/tel/hotep.svg?branch=master)](https://travis-ci.org/tel/hotep)

## Overview

Hotep implements some Erlang process concepts as a shallow DSL in Haskell. Hotep
can help you to write applications which use concepts like process linking,
supervision trees, and fail-fast-and-restart. Unlike Cloud Haskell, Hotep does
not attempt to solve process distribution. Generally, using Hotep can help you
write robust single-node applications.

**Hotep is currently non-functional software.** No warranties are made about its
accuracy or completeness.

See more detail in the [documentation](https://github.com/tel/hotep/blob/master/docs/Intro.md).

## Contributing

Hotep code governance is not yet well-defined but should implement some
variation of the ["Collective Code Construction Contract (C4)"](https://rfc.zeromq.org/spec:42/C4)
in time.

## Authors

- Joseph Abrahamson <me@jspha.com>
