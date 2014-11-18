{-# LANGUAGE ConstraintKinds
            ,DataKinds
            ,FlexibleContexts
            ,FlexibleInstances
            ,FunctionalDependencies
            ,GADTs
            ,KindSignatures
            ,MultiParamTypeClasses
            ,RankNTypes
            ,ScopedTypeVariables
            ,UndecidableInstances
            ,InstanceSigs
            ,TypeFamilies
  #-}
module Join.ChannelF
  ( ChannelF(C)
  , ChanF
  , SyncChanF
  , toChannelF
  , fromChannelF

  , newChanF

  , ChannelPred
  ) where

import Join.Channel
import Join.Language
import Join.Message
import Join.Pattern.Channel
import Join.Pattern.Rep

import Data.Typeable

-- | A ChannelF is a 'Channel' represented as a function type which
-- can be applied to different types of values to produce different results.
--
-- A 'ChannelF' is defined for three contexts:
--
-- - Applied to a message value ~> Send a message on the channel.
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
-- @ chanF 1                                          -- Send 1 to the channel @
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
  chan <- newChannel :: Process (Channel s a)
  return (toChannelF chan)

-- | Request a new ChanF be created.
newChanF :: forall a. MessageType a => Process (ChanF a)
newChanF = newChannelF

-- | Request a new SyncChanF be created.
newSyncChanF :: forall a r. MessageType a => Process (SyncChanF a r)
newSyncChanF = newChannelF




-- Kind of type a ChannelF may be applied to.
data In
  = MsgIn  -- ^ Applied to a message
  | PredIn -- ^ Applied to a predicate

-- Kind of output type a ChannelF may produce.
data Out
  = SendOut -- ^ Output is a message send type
  | PredOut -- ^ Output is a predicate pattern type

-- | Bijectively map of In <-> Out
class (input  ~ (OutIn output)
      ,output ~ (InOut input)
      ) => InOutBijection input output where
  type OutIn output :: In
  type InOut input  :: Out

instance InOutBijection MsgIn SendOut where
  type OutIn SendOut = MsgIn
  type InOut MsgIn   = SendOut

instance InOutBijection PredIn PredOut where
  type OutIn PredOut = PredIn
  type InOut PredIn  = PredOut

-- Decide the 'In' type.
type family ToIn i where
  ToIn (a -> Bool) = PredIn
  ToIn a           = MsgIn

-- Construct from an 'In' type.
type family FromIn i a where
  FromIn MsgIn  a = a
  FromIn PredIn a = (a -> Bool)

-- Decide the 'Out' type.
type family ToOut a where
  ToOut (ChannelPred s a) = PredOut
  ToOut (Process ())      = SendOut

-- Construct from an 'Out' type.
type family FromOut o s a where
  FromOut SendOut s a = Process ()
  FromOut PredOut s a = ChannelPred s a



type ValidInput  i     a = (i~FromIn  (ToIn  i)   a)
type ValidOutput   o s a = (o~FromOut (ToOut o) s a)
type ValidApply  i o s a = (ValidInput i a,ValidOutput o s a)

-- | Apply a 'Channel s a' to an input type 'i' to produce an output type 'o'.
-- 'i' determines 'o', under 's a'
-- 'o' derminines 'i', under 's a'
class (ValidApply i o s a
      ,InOutBijection (ToIn i) (ToOut o)
      ,MessageType a
      ,Typeable s
      )
      => ApplyChannelF i o s a where
    applyChannelF :: Channel s a -> i -> o

-- Message send instance
instance (MsgIn~ToIn a
         ,MessageType a
         ,Typeable A
         )
         => ApplyChannelF a (Process ()) A a where
  applyChannelF chan msg = send chan msg

-- channel predicate instance
instance (MessageType a
         ,Typeable s
         )
         => ApplyChannelF (a -> Bool) (ChannelPred s a) s a where
  applyChannelF chan pred = ChannelPred chan pred

-- | Extract the corresponding 'Channel s a' from a 'ChannelF's internal function.
extractChannel :: ((a -> Bool) -> ChannelPred s a) -> Channel s a
extractChannel f = case f (undefined :: a -> Bool) of
  ChannelPred chan _ -> chan


-- | Encapsulate the parameters to a 'Channel' predicate pattern.
data ChannelPred (s :: Synchronicity *) a = ChannelPred (Channel s a) (a -> Bool)

-- A ChannelPred can be used as a Pattern.
instance (MessageType a,Typeable s) => ToPattern (ChannelPred s a) s a Pass where
  toPattern (ChannelPred c pred) = Pattern c (MatchWhen pred) DoPass

instance (MessageType a,Typeable s) => ToPatterns (ChannelPred s a) '[Pattern s a Pass] where
  toPatterns (ChannelPred c pred) = OnePattern $ Pattern c (MatchWhen pred) DoPass


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
         ,ShouldPassValue p

         ,PredIn ~ToIn  i
         ,PredOut~ToOut o
         ,i~(a -> Bool)
         ,o~(ChannelPred s a)
         )
         =>ToPattern (i -> o) s a p where
  toPattern f = Pattern (extractChannel f) MatchAll (shouldPassValue (undefined :: p))
instance (MessageType a
         ,Typeable s
         ,p~DecideChannelShouldPass a
         ,ShouldPassValue p

         ,PredIn ~ToIn  i
         ,PredOut~ToOut o
         ,i~(a -> Bool)
         ,o~(ChannelPred s a)
         )
         =>ToPatterns (i -> o) '[Pattern s a p] where
  toPatterns f = OnePattern $
                Pattern (extractChannel f) MatchAll (shouldPassValue (undefined :: p))

