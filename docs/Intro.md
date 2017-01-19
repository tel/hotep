
# Hotep, an introduction

Hotep bring single node Erlang-like process management to Haskell as a shallow
DSL. This can help in architecting `IO` operations in the large in your
applications while ensuring robustness.

Hotep only brings Erlang's process model and does not handle process
distribution or hot code swapping.

## Why should I care?

Erlang-style process management is an architectural design built from the ground
up by Ericsson to help craft telephone routers with extreme uptime requirements.
The Erlang language and this architectural design were both born out of this
special environment and thus embody powerful ideas of robust operation.

### Key problem: "What do I do about thrown exceptions?"

There are a number of different ways to handle exceptional situations in
Haskell. Generally, they break up into two camps:

- data-driven error reporting which demands complete analysis (`Maybe`, `Either err`) and
- exception-driven failure modes which unwind stacks unless caught (`Control.Exception`).

Pure functional methods handle the first case adroitly and have spread into many
new languages on this merit. They tend to dominate in simple code where complete
reasoning is feasible and pure functional methods have proven to scale very far
in struturing a program.

On the other hand, Haskell applications often end up having a higher level
structure where large interactions with `IO` are unavoidable. At this point,
vastly more failure modes are available and case analysis can become burdensome
and partial. Haskell's exception system is very well-designed but applications
often aren't architected to handle exceptions gracefully. This situation is
particularly multiplied in code that takes advantage of Haskell's wonderful
concurrency story.

The key problem is that it's not feasible to handle all failure modes locally
and it's difficult to structure "large scale" error handling code. Some kinds of
recoverable errors may be detected and restarts attempted, but when this grows
too complex applications tend to just fall over.

### Erlang's solution: "Just let it (gracefully) crash!"

Erlang achieves robustness by noting that `IO`-like failure states are usually
casued by either 

- transient external situations or 
- irrecoverable flaws in the code. 

Both of these situations can be gracefully handled by a policy of fast partial
system failures, broadly applied restart-and-retry policies, and good logging.
Transient situations will be naturally caught by a later invocation of a
subsystem under the various restart policies and irrecoverable flaws will
eventually take down the system entirely but produce a trace of the failures
they caused.

When these policies are in place, exception handling is massively simplified:
**just let it crash!**. Restart policies will largely automatically heal bad
states and anything more difficult is best handled by engineering analysis
instead of custom automatic exception-handling policies.

To achieve this solution, Erlang is designed around frequent idiomatic use of
lightweight concurrent processes ("actors") that share no state and are
organized into "supervision hierarchies". Supervising processes boot subsystems
(which may themselves be supervisors) composed from many sub-processes. Failures
casecade up to the supervisor where retry policies are attempted. If the failure
persists the supervisor itself dies and a higher-level retry policy is
attempted. Eventually this crashes the whole application.

This architecture provides a simple and effective policy for handling `IO`-like
failures throughout the system. It also obviates the need for almost all custom
exception handling code: just let the exceptions float and crash the current
process.

## Applying "Let it crash!" in Haskell

Haskell (on GHC) is extremely well-suited to incorporate Erlang-like actors:

- idiomatic Haskell very aggressively avoids shared state
- Haskell supports "asynchronous exceptions" which are necessary to properly
  manage supervision hierarchies
- GHC has a magnificent lightweight threading runtime
- Haskell supports very natural DSLs via `do`-syntax 

On the other hand, "Let it crash!" is almost completely opposed to standard
Haskell dogma. Functional programming increases the scope at which complete case
analysis can be performed and encourages modes of programming which seek to
avoid the need for "Let it crash!" mentality from the ground up.

It's the author's belief that this will ultimately turn out to be an advantage.
"Let it crash!" works best at larger application scales where complex
interactions with external state becomes a principle source of failure. By using
pure functional error handling in the small the boundary between these two
error-handling mechanisms becomes sharp and the tradeoffs between the two
designs explicit.
