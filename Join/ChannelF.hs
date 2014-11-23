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
  , newSyncChanF

  , replyF

  , ChannelPred
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
  | SyncMsgIn Bool
  | PredIn -- ^ Applied to a predicate

-- Kind of output type a ChannelF may produce.
data Out
  = SendOut -- ^ Output is a message send type
  | SyncOut Bool
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

instance InOutBijection (SyncMsgIn False) (SyncOut False) where
  type OutIn (SyncOut False)   = SyncMsgIn False
  type InOut (SyncMsgIn False) = SyncOut False

instance InOutBijection (SyncMsgIn True) (SyncOut True) where
  type OutIn (SyncOut True)   = SyncMsgIn True
  type InOut (SyncMsgIn True) = SyncOut True

-- Decide the 'In' type.
type family ToIn i s b where
  ToIn (a -> Bool) s     b = PredIn
  ToIn a           A     b = MsgIn
  ToIn a           (S r) b = SyncMsgIn b

-- Construct from an 'In' type.
type family FromIn i a where
  FromIn MsgIn         a = a
  FromIn (SyncMsgIn b) a = a
  FromIn PredIn        a = (a -> Bool)

-- Decide the 'Out' type.
type family ToOut o s where
  ToOut (ChannelPred s a)      s     = PredOut
  ToOut (Process ())           A     = SendOut
  ToOut (Process (Response r)) (S r) = SyncOut False
  ToOut (Process r)            (S r) = SyncOut True

-- Construct from an 'Out' type.
type family FromOut o s a where
  FromOut SendOut          A    a = Process ()
  FromOut (SyncOut False) (S r) a = Process (Response r)
  FromOut (SyncOut True)  (S r) a = Process r
  FromOut PredOut          s    a = ChannelPred s a



{-type ValidInput  i   s a b = (i~FromIn  (ToIn  i s b)  a)-}
type ValidInput  i   s a b = (i~FromIn  (ToIn  i s b)  a)
type ValidOutput   o s a   = (o~FromOut (ToOut o s)  s a)
type ValidApply  i o s a b = (ValidInput i s a b,ValidOutput o s a)

type family IsStrict o where
  IsStrict (Process (Response r)) = False
  IsStrict (Process ())           = True
  IsStrict (Process r)            = True
  IsStrict (ChannelPred s a)      = True

-- | Apply a 'Channel s a' to an input type 'i' to produce an output type 'o'.
-- 'i' determines 'o', under 's a'
-- 'o' derminines 'i', under 's a'
class (ValidApply i o s a (IsStrict o)
      ,InOutBijection (ToIn i s (IsStrict o)) (ToOut o s)
      )
      => ApplyChannelF i o s a where
    applyChannelF :: Channel s a -> i -> o

-- Message send instance
instance (MsgIn  ~ToIn a A True
         ,SendOut~ToOut (Process ()) A
         ,MessageType a
         ,Typeable A
         )
         => ApplyChannelF a (Process ()) A a where
  applyChannelF chan msg = send chan msg

instance ((SyncMsgIn False) ~ToIn a (S r) False
         ,(SyncOut   False) ~ToOut (Process (Response r)) (S r)
         ,MessageType a
         ,MessageType r
         )
         => ApplyChannelF a (Process (Response r)) (S r) a where
  applyChannelF chan msg = sync chan msg

instance(True ~ IsStrict (Process r)
        ,(SyncMsgIn True) ~ToIn a (S r) True
        ,(SyncOut   True) ~ToOut (Process r) (S r)
        ,MessageType a
        ,MessageType r
        )
        => ApplyChannelF a (Process r) (S r) a where
  applyChannelF chan msg = sync' chan msg


-- channel predicate instance
instance (PredIn ~ToIn (a -> Bool) s True
         ,PredOut~ToOut (ChannelPred s a) s
         ,MessageType a
         ,Typeable s
         )
         => ApplyChannelF (a -> Bool) (ChannelPred s a) s a where
  applyChannelF chan pred = ChannelPred chan pred

sendF :: MessageType a => ((a -> Bool) -> ChannelPred A a) -> a -> Process ()
sendF f msg = send (extractChannel f) msg

syncF :: (MessageType a,MessageType r) => ((a -> Bool) -> ChannelPred (S r) a) -> a -> Process (Response r)
syncF f msg = sync (extractChannel f) msg

syncF' :: (MessageType a,MessageType r) => ((a -> Bool) -> ChannelPred (S r) a) -> a -> Process r
syncF' f msg = sync' (extractChannel f) msg

replyF :: (MessageType r,MessageType a) => ((a -> Bool) -> ChannelPred (S r) a) -> r -> Process ()
replyF f msg = reply (extractChannel f) msg

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

         ,PredIn ~ToIn  i s True
         ,PredOut~ToOut o s
         ,i~(a -> Bool)
         ,o~(ChannelPred s a)
         )
         =>ToPattern (i -> o) s a p where
  toPattern f = Pattern (extractChannel f) MatchAll (shouldPassValue (undefined :: p))
instance (MessageType a
         ,Typeable s
         ,p~DecideChannelShouldPass a
         ,ShouldPassValue p

         ,PredIn ~ToIn  i s True
         ,PredOut~ToOut o s
         ,i~(a -> Bool)
         ,o~(ChannelPred s a)
         )
         =>ToPatterns (i -> o) '[Pattern s a p] where
  toPatterns f = OnePattern $
                Pattern (extractChannel f) MatchAll (shouldPassValue (undefined :: p))

