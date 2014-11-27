#Join-calculus within Haskell
This module is an attempt at encoding the
<a href="http://en.wikipedia.org/wiki/Join-calculus">join-calculus</a>
within the Haskell programming language as an Embedded-DSL.

Note: This code is at a pre-alpha stage.

##Directory Structure

| Filepath                                                                  | Contains                                                            |
| ------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| ['Join.Language'](/Join/Language.hs)                                      | The core DSL                                                        |
| [‘Join.Interpreter.Interface’](/Join/Interpreter/Interface.hs)            | Interface(s) for building interpreters for the DSL.                 |
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

##### Declaring channels
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

##### Messaging channels
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

‘def’ is used to define join definitions and can be thought to have the type:
```haskell
def :: JoinDefinitions j => j -> Process ()
```

I.E. The type of join definitions is polymorphic to aid in simplifying user syntax.

##### Single Clauses
A single join definition clause is defined by the infix operator, typed like:

```haskell
‘|>’ :: JoinPattern pat trigger => pat -> trigger
```

E.G.

```haskell
pattern |> trigger
```

declares:
- ‘pattern’ to be a type which represents a join pattern.
- ‘trigger’ to be a trigger function, correctly typed to accept messages
 from the matching pattern in a ‘Process ()’.

###### Matching a Channel
The simplest pattern is a single ‘Channel s a’ type.
This declares a pattern that matches all messages sent on the Channel.

E.G.
```haskell
(intChan  :: Channel s Int)  |> (trigger :: Int -> Process ())
(charChan :: Channel s Char) |> (trigger :: Char -> Process ())
```

In general a ‘Channel s a’ will require a trigger typed ‘a -> Process ()’.

There is one exception, which is when the message type of the Channel is ‘()’.
Because the unit type ‘()’ only has one value (also named ‘()’) explicitly passing
the value to the trigger is unnecessary. The required trigger type therefore does not
accept a value.

I.E.
```haskell
(sigChan :: Channel s ()) |> (trigger :: Process ())
```
**NOT**:
```haskell
(sigChan :: Channel s ()) |> (trigger :: () -> Process ())

```

###### Matching equality on a Channel
A channel equality pattern, declared infix via ‘&=’ has a type like:

```haskell
(&=) :: Channel s a -> a -> pat
```

This declares a pattern that matches messages sent on the Channel **ONLY** when they are equal to
some given value.

Like the special case of a ‘Channel s ()’ pattern, a corresponding trigger is **NOT**
passed the matching value.
This is because when matching for message equality, by definition we know what the message value is
- it’s whatever was equality matched upon - and so there’s no need to pass it.

I.E.
```haskell
boolChan&=False |> (trigger :: Process ())
```

**NOT**:
```haskell
boolChan&=False |> (trigger :: Bool -> Process ())
```

###### Matching arbitrary predicates on a Channel
A Channel predicate pattern, declared infix via ‘&~’ has a type like:

```haskell
(&~) :: Channel s a -> (a -> Bool) -> pat
```

This declares a pattern that matches message sent on the channel **ONLY** when they satisfy the
given predicate.

E.G.
```haskell
intChan&~(<10) |> (trigger :: Int -> Process ())
```
is a definition which passes messages on intChan to a trigger **ONLY** when they are less than 10.

###### Matching multiple patterns simultaneously
A Channel composition pattern, declared infix via ‘&‘ has a type like:

```haskell
(&) :: patl -> patr -> pat
```
This declares a pattern that matches only when both component patterns match.

E.G.

```haskell
intChan & charChan                 |> (trigger :: Int -> Char -> Process ())
intChan &=1 & charChan             |> (trigger :: Char -> Process ())
intChan & charChan&=’a’ & boolChan |> (trigger :: Int -> Bool -> Process ())
```

Note the type of trigger required by an ‘&‘ pattern is the composition of both sub-patterns.

##### Multiple clauses
The power of the join calculus comes from the ability to declare multiple overlapping definitions
in a single place.

A definition composition, declared infix via ‘|$’ has a type like:

```haskell
(|$) :: defl -> defr -> def
```

E.G.
```haskell
   intChan & charChan         |> (trigger :: Int -> Char -> Process ())
|$ intChan&=1                 |> (trigger :: Process ())
|$ intChan&=1 & charChan&=’a’ |> (trigger :: Process ())
```

##### Summary of pattern behavior

| Pattern type        | Used                    | Passes?          | Match when:                   |
| ------------------- | ----------------------- | ---------------- | ----------------------------- |
| Signal Channel      | sigChan :: Channel s () | NO               | ‘()’ sent to channel          |
| Any Channel         | chan :: Channel s a     | YES              | Any message sent to channel   |
| Channel equality    | chan &= value           | NO               | Sent message == value         |
| Channel predicate   | chan &~ predicate       | YES              | pred (sent-message) == True   |
| Channel composition | chan1 & chan2           | ‘->’ composition | Each composed pattern matches |

