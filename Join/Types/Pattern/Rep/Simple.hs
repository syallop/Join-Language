{-# LANGUAGE ConstraintKinds
            ,DataKinds
            ,ExistentialQuantification
            ,FlexibleInstances
            ,FunctionalDependencies
            ,GADTs
            ,IncoherentInstances
            ,MultiParamTypeClasses
            ,TypeFamilies
            ,TypeOperators
            ,UndecidableInstances
 #-}
{-|
Module      : Join.Types.DefinitionRep.Simple
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental
-}
module Join.Types.Pattern.Rep.Simple where

import           Join.Types.Apply
import           Join.Types.Channel
import qualified Join.Types.Pattern.Rep as PR
import           Join.Types.Pattern.Rep       hiding (MatchWhen,MatchAll)
import           Join.Types.Message

-- | How messages should be matched on a channel.
data MatchType where

  -- ^ Match messages which satisfy a predicate, 'ShouldPass' declaring
  -- whether a matching message is passed into trigger functions or not.
  MatchWhen :: MessageType m => (m -> Bool) -> Bool -> MatchType

  -- ^ Match any message with 'ShouldPass' declaring whether a matching message
  -- is passed into trigger functions or not.
  MatchAll  :: Bool -> MatchType

pass = True
keep = False

-- | A 'PatternDescription' describes the runtime semantics of a pattern.
--
-- A 'PatternDescription'=[(ChanId,MatchType)] states to match when
-- a message is waiting on each listed channel, as identified by the
-- 'ChanId'. Each 'Channel' must in turn be matched according to it's
-- 'MatchType'.
type PatternDescription = [(ChanId,MatchType)]

data TriggerF r = forall f. Apply f r => TriggerF f
instance Show (TriggerF r) where show _ = "TRIGGERF"

describe :: Definitions t tss r => t -> [(PatternDescription,TriggerF r)]
describe t = simplifyDefinitionsRep (toDefinitionsRep t)

simplifyDefinitionsRep :: DefinitionsRep tss r -> [(PatternDescription,TriggerF r)]
simplifyDefinitionsRep (OneDefinition dr)     = [simplifyDefinitionRep dr]
simplifyDefinitionsRep (AndDefinition dr dsr) = simplifyDefinitionRep dr : simplifyDefinitionsRep dsr

simplifyDefinitionRep :: DefinitionRep ts tr r -> (PatternDescription,TriggerF r)
simplifyDefinitionRep (Definition pr tr) = (simplifyPatternsRep pr,simplifyTrigger tr)

simplifyPatternsRep :: PatternsRep s m p tr -> PatternDescription
simplifyPatternsRep (OnePattern pr)     = [simplifyPatternRep pr]
simplifyPatternsRep (AndPattern pr psr) = simplifyPatternRep pr : simplifyPatternsRep psr

simplifyPatternRep :: PatternRep s m p -> (ChanId,MatchType)
simplifyPatternRep (Pattern c mr sp) =
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

