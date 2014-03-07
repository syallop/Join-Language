{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Join.Types.Channel
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module defines a 'Channel' type, a representation of typed Join-Calculus Channels.

Channels themselves do not provide a means of queuing messages, what they provide is a means
for a Join-Calculus program to:

 (1) Track where it should route messages sent on the channel.

  2. Enforce the type of messages allowed.

  3. Determine whether reply-values are allowed/ expected.

The first point is facilitated by the id each Channel is constructed with.
The second and third, by the type variables.

'inferSync' allows an implementation to infer whether a newly created Channel
should be Asynchronous or Synchronous based upon it's usage.
For example, a reply function could be defined to operate exclusively on Synchronous Channels,
then, if reply is called on a channel, it can be inferred that it is intended to be Synchronous
and the user can omit type signatures. By constraining the synchronicity type variable in an
implementation sufficiently, type annotations may be able to be avoided entirely.
-}

module Join.Types.Channel
    (
     Channel()
    ,Synchronicity(..)
    ,getId
    ,InferSync()
    ,inferSync
    ) where

-- | Synchronicity tag Type & Kind.
data Synchronicity
    = A -- ^ Asynchronous.
    | S -- ^ Synchronous.

-- | A Channel uniquely identifies a port of communication.
--
-- The type parameter 'a' denotes the type of values accepted.
--
-- The type parameter 's' is of kind 'Synchronicity' and denotes whether
-- values should be sent 'A'synchronously (a regular channel as defined by
-- the core calculus) or 'S'ynchronously, where a return value is expected.
--
-- Channels are constructed with an 'Int' parameter which serves as it's
-- unique ID. Interpreters should ensure these are unique.
data Channel s a where
    Channel  :: Int -> Channel A a
    SChannel :: Int -> Channel S a

instance Show (Channel s a) where
    show (Channel  i) = "Channel-"  ++ show i
    show (SChannel i) = "SChannel-" ++ show i

-- | Extract the Id of a Channel.
getId :: Channel s a -> Int
getId (Channel i) = i
getId (SChannel i) = i

-- | Closed Class of Synchronicity-kinded types which may be used to infer
-- how to construct a corresponding Channel.
--
-- I.E. When:
--
-- @
--  i :: Int
--  c = inferSync i :: Channel s a
-- @
-- 
-- and in a context where 's' is constrained to 'A' or 'S', then inferSync will
-- create the corresponding type of Channel.
class    InferSync s where inferSync :: Int -> Channel s a
instance InferSync A where inferSync = Channel
instance InferSync S where inferSync = SChannel
