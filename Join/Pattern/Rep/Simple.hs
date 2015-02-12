{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , ExistentialQuantification
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , IncoherentInstances
  , MultiParamTypeClasses
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  #-}
{-|
Module      : Join.DefinitionRep.Simple
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental
-}
module Join.Pattern.Rep.Simple where

import           Join.Apply
import           Join.Channel
import qualified Join.Pattern.Rep as PR
import           Join.Pattern.Rep       hiding (MatchWhen,MatchAll)
import           Join.Message

-- | How messages should be matched on a channel.
data MatchType where

  -- Match messages which satisfy a predicate, 'ShouldPass' declaring
  -- whether a matching message is passed into trigger functions or not.
  MatchWhen
    :: MessageType m => (m -> Bool) -> Bool -> MatchType

  -- Match any message with 'ShouldPass' declaring whether a matching message
  -- is passed into trigger functions or not.
  MatchAll
    :: Bool -> MatchType

pass = True
keep = False

-- | A 'PatternDescription'=[(ChanId,MatchType)] states to match when
-- a message is waiting on each listed channel, as identified by the
-- 'ChanId'. Each 'Channel' must in turn be matched according to it's
-- 'MatchType'.
type PatternDescription = [(ChanId,MatchType)]

data TriggerF r = forall f. Apply f r => TriggerF f
instance Show (TriggerF r) where show _ = "TRIGGERF"

-- | Convert a definitions type to a less strongly typed representation.
describe :: ToDefinitions t tss r => t -> [(PatternDescription,TriggerF r)]
describe t = simplifyDefinitions (toDefinitions t)

simplifyDefinitions :: Definitions tss r -> [(PatternDescription,TriggerF r)]
simplifyDefinitions (OneDefinition dr)     = [simplifyDefinition dr]
simplifyDefinitions (AndDefinition dr dsr) = simplifyDefinition dr : simplifyDefinitions dsr

simplifyDefinition :: Definition ts tr r -> (PatternDescription,TriggerF r)
simplifyDefinition (Definition pr tr) = (simplifyPatterns pr,simplifyTrigger tr)

simplifyPatterns :: Patterns tr -> PatternDescription
simplifyPatterns (OnePattern pr)     = [simplifyPattern pr]
simplifyPatterns (AndPattern pr psr) = simplifyPattern pr : simplifyPatterns psr

simplifyPattern :: Pattern s m p -> (ChanId,MatchType)
simplifyPattern (Pattern c mr sp) =
  (getId c
  ,case mr of
     PR.MatchWhen pred -> MatchWhen pred (simplifyShouldPass sp)
     PR.MatchAll       -> MatchAll (simplifyShouldPass sp)
  )

simplifyShouldPass :: ShouldPass p -> Bool
simplifyShouldPass DoPass   = True
simplifyShouldPass DontPass = False

simplifyTrigger :: Trigger tr r -> TriggerF r
simplifyTrigger (Trigger t) = TriggerF t

