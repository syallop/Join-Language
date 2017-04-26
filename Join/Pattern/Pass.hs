{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , MultiParamTypeClasses
  , RankNTypes
  , TypeFamilies
  , UndecidableInstances
  #-}
module Join.Pattern.Pass where

import Join.Pattern.Rep

import Join.Channel
import Join.Message

import Data.Typeable

-- | If a type almost comprises a pattern by defining the channel it matches and
-- when it matches BUT NOT whether it passes its result, then this data type can
-- be used to explicitly pin the 'pass'ness down.
data PassPattern sync msg pass where
  -- | Some almost-pattern type 't' 'Pass'es any matching messages into
  -- triggers.
  Pass :: forall t sync msg pass
        . (MatchesChannel t sync msg
          ,MatchesWhen t msg
          )
       => t
       -> PassPattern sync msg Pass

  -- | Some almost-pattern type 't' 'Keep's any matching messages from being
  -- passed into triggers.
  Keep :: forall t sync msg pass
        . (MatchesChannel t sync msg
          ,MatchesWhen t msg
          )
       => t
       -> PassPattern sync msg Keep

-- Adding 'Passness' to an almost-pattern makes it usable as a pattern.
instance (MessageType msg
         ,Typeable sync
         )
      => ToPattern (PassPattern sync msg pass) sync msg pass where
  toPattern ep = case ep of
    Pass t
      -> Pattern (getMatchingChannel t) (getMatchesWhen t) DoPass

    Keep t
      -> Pattern (getMatchingChannel t) (getMatchesWhen t) DontPass

instance (MessageType msg
         ,Typeable sync
         )
      => ToPatterns (PassPattern sync msg pass) '[Pattern sync msg pass] where
  toPatterns ep = OnePattern $ toPattern ep

