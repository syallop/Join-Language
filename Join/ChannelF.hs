{-# LANGUAGE ConstraintKinds
            ,DataKinds
            ,FlexibleContexts
            ,FlexibleInstances
            ,FunctionalDependencies
            ,GADTs
            ,KindSignatures
            ,MultiParamTypeClasses
            ,RankNTypes
            ,UndecidableInstances
            ,InstanceSigs
            ,TypeFamilies
            ,ScopedTypeVariables
  #-}

module Join.ChannelF
  ( ChannelF(C)
  , ChanF
  , SyncChanF
  , toChannelF
  , fromChannelF

  , newChannelF
  , newChanF
  , newSyncChanF

  , sendF
  , syncF
  , syncF'
  , replyF

  , ChannelPred()
  ) where

import Join.Channel
import Join.Language
import Join.Message
import Join.Pattern.Channel
import Join.Pattern.Rep
import Join.Response

import Data.Typeable


-- | A ChannelF is a 'Channel' represented as a function type which
-- can be applied to different types of values to produce different results.
--
-- A 'ChannelF' is defined for several contexts:
--
-- - Applied to a message value ~> Send/sync/sync' a message on the channel.
--
-- - Applied to a predicate     ~> A pattern matching messages on the channel satisfying the predicate.
--
-- - Not applied                ~> A pattern matching all messages on the channel.
--
-- E.G. if:
--
-- @ C chanF :: ChanF Int @
--
-- Then:
--
-- @ chanF 1                                          -- Send/sync 1 to the channel @
-- @ def chanF(<10) |> (trigger :: Int -> Process ()) -- Define predicate pattern @
-- @ def chanF      |> (trigger :: Int -> Process)    -- define channel pattern @
data ChannelF s a = C (ApplyChannelF i o s a => i -> o)

-- | Synonym for asynchronous 'ChannelF's.
type ChanF     a   = ChannelF A     a

-- | Synonym for synchronous 'ChannelF's.
type SyncChanF a r = ChannelF (S r) a

-- | 'Upgrade' a regular 'Channel' to a channel function.
toChannelF :: (MessageType a,InferSync s) => Channel s a -> ChannelF s a
toChannelF chan = C (applyChannelF chan)

-- | Recover a 'ChannelF s a's 'Channel s a'
fromChannelF :: forall s a. (MessageType a,Typeable s) => ChannelF s a -> Channel s a
fromChannelF (C f) = case f (undefined :: a -> Bool) of
  (ChannelPred chan _) -> chan
  -- Apply a dummy predicate as a hack to retrieve the Channel constructed with.

-- | Request a new ChannelF be created. Whether the ChannelF is synchronous or asynchonous is
-- determined by the calling context.
newChannelF :: forall s a. (MessageType a,InferSync s) => Process (ChannelF s a)
newChannelF = do
  chan <- newChannel
  return (toChannelF chan)

-- | Request a new ChanF be created.
newChanF :: forall a. MessageType a => Process (ChanF a)
newChanF = newChannelF

-- | Request a new SyncChanF be created.
newSyncChanF :: forall a r. MessageType a => Process (SyncChanF a r)
newSyncChanF = newChannelF

-- An input type 'i', channel synchronicity 's'
-- and 'b' determine the expected output type.
--
-- 'b' is a boolean specifying whether a stricter
-- output is requested.
type family InOut i s b where
  InOut (a -> Bool) s    b     = ChannelPred s a
  InOut a           A    b     = Process ()
  InOut a          (S r) False = Process (Response r)
  InOut a          (S r) True  = Process r

-- An output type 'o', channel synchronicity 's' and
-- channel message type 'a' determine the expected input type.
type family OutIn o s a where
  OutIn (ChannelPred s a')     s     a = (a -> Bool)
  OutIn (Process ())           A     a = a
  OutIn (Process (Response r)) (S r) a = a
  OutIn (Process r)            (S r) a = a

type family IsStrict o where
  IsStrict (Process (Response r)) = False
  IsStrict a                      = True

-- 'i' and 'o' determine each other with InOut/OutIn.
type InOutBijective i o s a = (i ~ OutIn o s a
                              ,o ~ InOut i s (IsStrict o)
                              )

-- | Apply a 'Channel s a' to an input type 'i' to produce an output type 'o'.
class (InOutBijective i o s a) => ApplyChannelF i o s a where
  applyChannelF :: Channel s a -> i -> o

-- send
instance (InOutBijective a (Process ()) A a
         ,MessageType a
         ) => ApplyChannelF a (Process ()) A a where
  applyChannelF chan msg = send chan msg

-- sync
instance (InOutBijective a (Process (Response r)) (S r) a
         ,MessageType a
         ,MessageType r
         )
         => ApplyChannelF a (Process (Response r)) (S r) a where
  applyChannelF chan msg = sync chan msg

-- sync'
instance (InOutBijective a (Process r) (S r) a
         ,MessageType a
         ,MessageType r
         )
         => ApplyChannelF a (Process r) (S r) a where
  applyChannelF chan msg = sync' chan msg

-- pred
instance (InOutBijective (a -> Bool) (ChannelPred s a) s a
         ,MessageType a
         ,Typeable s
         )
         => ApplyChannelF (a -> Bool) (ChannelPred s a) s a where
  applyChannelF chan pred = ChannelPred chan pred


-- | Encapsulate the parameters to a 'Channel' predicate pattern.
data ChannelPred (s :: Synchronicity *) a = ChannelPred (Channel s a) (a -> Bool)

-- A ChannelPred can be used as a Pattern.
instance (MessageType a
         ,Typeable s
         )
         => ToPattern (ChannelPred s a) s a Pass where
  toPattern (ChannelPred c pred) = Pattern c (MatchWhen pred) DoPass
-- A ChannelPred can be used as Patterns.
instance (MessageType a
         ,Typeable s
         )
         => ToPatterns (ChannelPred s a) '[Pattern s a Pass] where
  toPatterns (ChannelPred c pred) = OnePattern $ Pattern c (MatchWhen pred) DoPass


-- | Extract the corresponding 'Channel s a' from a 'ChannelF's internal function.
extractChannel :: ((a -> Bool) -> ChannelPred s a) -> Channel s a
extractChannel f = case f (undefined :: a -> Bool) of
  ChannelPred chan _ -> chan

-- | Explicitly call 'send' on a ChannelF function.
sendF :: MessageType a => ((a -> Bool) -> ChannelPred A a) -> a -> Process ()
sendF f msg = send (extractChannel f) msg

-- | Explicitly call 'sync' on a ChannelF function.
syncF :: (MessageType a,MessageType r) => ((a -> Bool) -> ChannelPred (S r) a) -> a -> Process (Response r)
syncF f msg = sync (extractChannel f) msg

-- | Explicitly call 'sync\'' on a ChannelF function.
syncF' :: (MessageType a,MessageType r) => ((a -> Bool) -> ChannelPred (S r) a) -> a -> Process r
syncF' f msg = sync' (extractChannel f) msg

-- | Explicitly call 'reply' on a ChannelF function.
replyF :: (MessageType r,MessageType a) => ((a -> Bool) -> ChannelPred (S r) a) -> r -> Process ()
replyF f msg = reply (extractChannel f) msg


-- An unapplied ChannelF is a pattern matching all messages on the Channel.
instance (MessageType a
         ,Typeable s

         ,p~DecideChannelShouldPass a
         ,ShouldPassValue p
         )
         => ToPattern (ChannelF s a) s a p where
  toPattern f = Pattern (fromChannelF f) MatchAll (shouldPassValue (undefined :: p))

instance (MessageType a
         ,Typeable s

         ,p~DecideChannelShouldPass a
         ,ShouldPassValue p
         )
         =>ToPatterns (ChannelF s a) '[Pattern s a p] where
  toPatterns f = OnePattern $ Pattern (fromChannelF f) MatchAll (shouldPassValue (undefined :: p))


-- An unapplied, unwrapped ChannelF is a pattern matching all messages on the Channel.
instance (MessageType a
         ,Typeable s

         ,p~DecideChannelShouldPass a
         , ShouldPassValue p

         ,InOutBijective (a -> Bool) (ChannelPred s a) s a
         ,i~(a -> Bool)
         ,o~(ChannelPred s a)
         )
         =>ToPattern (i -> o) s a p where
  toPattern f = Pattern (extractChannel f) MatchAll (shouldPassValue (undefined :: p))
instance (MessageType a
         ,Typeable s

         ,p~DecideChannelShouldPass a
         ,ShouldPassValue p

         ,InOutBijective (a -> Bool) (ChannelPred s a) s a
         ,i~(a -> Bool)
         ,o~(ChannelPred s a)
         )
         =>ToPatterns (i -> o) '[Pattern s a p] where
  toPatterns f = OnePattern $
                Pattern (extractChannel f) MatchAll (shouldPassValue (undefined :: p))

