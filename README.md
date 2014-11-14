#Join-calculus within Haskell
This module is an attempt at encoding the
<a href="http://en.wikipedia.org/wiki/Join-calculus">join-calculus</a>
within the Haskell programming language as an Embedded-DSL.

Note: This code is at a pre-alpha stage.

##Directory Structure

| Filepath                                                                  | Contains                                                            |
| ------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| ['Join.Language'](/Join/Language.hs)                                      | The core DSL                                                        |
| ['Join.Interpretation.Basic'](/Join/Interpretation/Basic.hs)              | A basic interpreter for the DSL                                     |
| ['Join.Examples'](/Join/Examples.hs)                                      | Simple example programs                                             |
| ['Join.Examples.DiningPhilosophers'](Join/Examples/DiningPhilosophers.hs) | Example simulation of the dining philosophers problem               |
| Join.Data.*                                                               | Several simple concurrency primitives. Barriers,Buffers,Locks, etc. |

## Overview of language components
This section briefly discusses the language components exported by 'Join.Language'.

Examples of short programs written in the language can be found:
- ['Join.Examples'](/Join/Examples.hs)
- ['Join.Examples.DiningPhilosophers'](Join/Examples/DiningPhilosophers.hs)
- Join.Data.*

### Processes are the units of computation.

A 'Process' is the core type of the DSL and represents an sequence of join 'Instruction's
and 'IO' actions to be executed according to the given semantics.

Communication between Processes is achieved by message passing over 'Channel's.

'Process' is a Monadic type and so supports do-notation in which it is recommended that
programs are written.

Each 'Instruction' that comprises the language has a corresponding function which enters it
as a 'Process' context. These are the atomic functions in which join programs are built.

#### Composing Processes
| Function            | Type                                       | Meaning                                                  |
| ------------------- | ------------------------------------------ | -------------------------------------------------------- |
| >>= / do-desugaring | Process a -> (a -> Process b) -> Process b | Sequence two processes                                   |
| spawn               | Process a -> Process ()                    | Spawn a process, not waiting for a result.               |
| with                | Process () -> Process () -> Process ()     | Concurrently execute two processes without result        |
| withAll             | [Process ()] -> Process ()                 | Concurrently execute a list of processes without result  |

#### Channels and messages
Channels are the communication medium of the join-calculus.
The core calculus defines Channel's as being asynchronous, unidirectional and parameterised
over the type of messages that they carry.

##### Declaring
In a 'Process' first a 'Channel' is declared by a call to 'newChannel':

```haskell
chan <- newChannel
```

The type of message the Channel carries can normally be inferred from its usage
, but otherwise can be annotated:

```haskell
chan <- newChannel :: Process (Channel A Int)
```

Notice the 'Channel' type specifies a type parameter 'A'.
This is because the language has opted to define Channel's as being of two varieties:
the traditional asynchronous variety or a synchronous variety.
The type parameter is either 'A' or 'S' denoting 'A'synchronous or
'S'ynchronous respectively.

Aynchronous Channel over messages of type t:

```haskell
:: Channel A t
```

Synchronous Channel over messages of type t, returning message of
type r:

```haskell
:: Channel (S r) t @
```

##### Using
After a Channel has been defined, it may be sent messages is
a number of distinct ways:

| Function    | Meaning                                                                                                                                                             |
| --------    | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| send        | Send a value on an asynchronous Channel, returning immediately with no return value                                                                                 |
| signal      | A convenience for 'send c ()' "signaling" the unit value to a 'Signal' channel ('Chan A ()').                                                                       |
| sync        | Send a value to a synchronous Channel, returning immediately with a 'Response'. A reference to a reply value which can be 'wait'ed upon when the value is required. |
| sync’       | A variant of 'sync' which immediately blocks on a reply value.                                                                                                      |
| reply       | Send a message in reply to a synchronous Channel.                                                                                                                   |
| syncSignal  | A convenience for 'sync s ()' "signaling" the unit value to a 'SyncSignal' channel ('SyncChan () r').                                                               |
| syncSignal' | A variant of 'syncSignal' which immediately blocks on a reply value.                                                                                                |
| acknowledge | A convenience for 'reply s ()' "acknowledging" a message sent on a synchronous channel by replying with the unit value.                                             |

Each of these functions also provide an 'all'-suffixed variant
which runs the corresponding action on a list of arguments, in
parallel via 'with' when possible.

It is noted that the addition of synchronous channels does not add to
the expressive power of the join-calculs by virtue of the fact that they
could otherwise by implemented by a continuation-passing-style on the
primitive asynchronous Channels.

#### Join definitions
Join definitions are the key construct provided by the join-calculus
and allow a declarative style of defining reactions to messages sent
to collections of channels.

On the left-hand-side (LHS) of a Join definition is a pattern to match
upon.

Given the channels:
```haskell
intChan :: Channel (S Integer) Int
boolChan :: Channel A Bool
```
Types of pattern are:

| Form of pattern             | Example                                   | Meaning                                        |
| ---------------             | ----------------------------------------- | -------                                        |
| A single Channel            | intChan                                   | Match all messages sent to the channel         |
| A Channel &= value          | intChan &= 1                              | Match any messages which equal the given value |
| A & conjunction of patterns | intChan & boolChan&=True                  | Match only when both patterns match            |

On the right-hand-side of the Join definition is a trigger function, typed to accept
each message defined on the LHS in order and result in a function in
'Process'.

The operator '|>' may be used to build definitions in infix
style.

E.G. Given the previous example patterns,
some arbitrary valid definitions are:

```haskell
def $ intChan                  |> (\i -> reply intChan (i+1) :: Int -> Process ())
def $ intChan &=1              |> (reply intChan 0           :: Process ())
def $ intChan & boolChan&=True |> (\i -> reply intChan (i-1) :: Int -> Process ())
```

The semantics of a Join 'Def' are that when the LHS
pattern matches, the corresponding messages are passed to the RHS
trigger function which is executed asynchronously in the background.

