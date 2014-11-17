{-|
Module      : Join.Examples.DiningPhilosophers
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module provides two simulations of the dining philosophers concurrency problem.

- 'diningPhilosophersExplicit' : Explicitly defines a simulation for 5 philosophers.
- 'diningPhilosophers' : Uses "Join.Pattern.Builder" to define a simulation for n>1 philosophers using
   pseudo-dependant-type tricks.

For a description of the problem, see "http://en.wikipedia.org/wiki/Dining_philosophers_problem".
-}
module Join.Examples.DiningPhilosophers
    ( diningPhilosophersExplicit
    , diningPhilosophers
    ) where

import Prelude hiding (append,zip,head,tail)

import Join
import Join.Pattern.Rep
import Join.Pattern.Builder
import Join.Interpretation.Basic

import Control.Applicative    ((<$>),(<*>),pure)
import Control.Concurrent     (threadDelay)
import Control.Monad          (forM)
import Control.Monad.IO.Class (liftIO)
import System.Random          (randomRIO)


{- Utilities -}
-- | The named philosopher eats, then waits 0-3 seconds.
eatRandom :: String -> Process ()
eatRandom = doThenDelayRandom "Eating"

-- | The named philosopher thinks, then waits 0-3 seconds.
thinkRandom :: String -> Process ()
thinkRandom = doThenDelayRandom "Thinking"

-- | The action is performed by the philosopher, then waits 0-3 seconds.
doThenDelayRandom :: String -> String -> Process ()
doThenDelayRandom action n = do
    liftIO $ putStrLn $ action ++ ": " ++ n
    liftIO $ randomRIO (0, 300000) >>= threadDelay


-- | Simulate the dining philosophers problem for 5 philosophers.
-- Each channel and pattern is defined explicitly.
--
-- An 'unrolled' version of 'diningPhilosophers 5'.
--
-- @ run $ diningPhilosophersExplicit 5 @
--
-- > Thinking: 1
-- > Thinking: 2
-- > Thinking: 3
-- > Thinking: 4
-- > Thinking: 5
-- > Eating: 1
-- > Eating: 3
-- > Thinking: 3
-- > Eating: 5
-- > Thinking: 1
-- > Eating: 2
-- > Eating: 4
-- > ...
diningPhilosophersExplicit :: Process ()
diningPhilosophersExplicit = do

    -- Declare a 15 Signals:
    [tA,tB,tC,tD,tE          -- A..E are thinking
       ,hA,hB,hC,hD,hE       -- A..E are hungry
       ,fAB,fBC,fCD,fDE,fEA  -- AB..EA forks are set
       ]                 <- newChannels 15 :: Process [Signal]

    -- For each philosopher:
    -- - When the philosopher is thinking => think for a random amount of
    --   time before becoming hungry.
    def $ tA             |> do thinkRandom "A"; signal hA
       |$ tB             |> do thinkRandom "B"; signal hB
       |$ tC             |> do thinkRandom "C"; signal hC
       |$ tD             |> do thinkRandom "D"; signal hD
       |$ tE             |> do thinkRandom "E"; signal hE

       -- For each seating arrangement (a philosopher between two forks):
       -- - When the philosopher is hungry and both forks are free =>
       --   eat for a random amount of time before replacing the forks and
       --   resuming thinking.
       |$ fEA & hA & fAB |> do eatRandom "A"; signal fEA `with` signal tA `with` signal fAB
       |$ fAB & hB & fBC |> do eatRandom "B"; signal fAB `with` signal tB `with` signal fBC
       |$ fBC & hC & fCD |> do eatRandom "C"; signal fBC `with` signal tC `with` signal fCD
       |$ fCD & hD & fDE |> do eatRandom "D"; signal fCD `with` signal tD `with` signal fDE
       |$ fDE & hE & fEA |> do eatRandom "E"; signal fDE `with` signal tE `with` signal fEA

    -- All begin thinking and lay all forks.
    signalAll [fAB,fBC,fCD,fDE,fEA
              ,tA,tB,tC,tD,tE
              ]


{- DiningPhilosophers, abstracted over the number of philosophers -}

type Name      = String      -- ^ Philosophers name,used in printing

type Fork      = Signal      -- ^ A signal on a Fork declares the fork available.
type ForkPair  = (Fork,Fork) -- ^ Pair of left and right hand Fork's

-- | A single Philosopher encapsulates several signals.
data Philosopher = Philosopher
  { name      :: String
  , leftFork  :: Signal -- ^ Left fork availability.
  , rightFork :: Signal -- ^ Right fork availability.
  , thinking  :: Signal -- ^ Should begin/resume thinking?
  , hungry    :: Signal -- ^ Should try to begin/resume eating?
  }

-- | Create a new philosopher given a name and left,right fork pair.
mkPhilosopher :: (Name,ForkPair) -> Process Philosopher
mkPhilosopher (n,(lFork,rFork)) =
  Philosopher <$> pure n
              <*> pure lFork
              <*> pure rFork
              <*> newChannel -- thinking
              <*> newChannel -- hungry

-- | Create 'n' philosophers.
mkPhilosophers :: Vector n (Name,ForkPair) -> Process (Vector n Philosopher)
mkPhilosophers = mapMVector mkPhilosopher

-- Create a cycle of 'n' ForkPairs such that:
-- - The rightFork at 'i' is the leftFork of 'i+1'
-- - / The leftFork at 'i' is the rightFork of 'i-1'
-- - 0-1 = n
-- - n+1 = 0
-- - n > 1
--
-- E.G. (a,b) : (b,c) : (c,d) : (d,a)
mkForkPairs :: Natural (Suc n) -> Process (Vector (Suc n) ForkPair)
mkForkPairs n = do
  leftForks <- replicateM n newChannel
  let t          = tail leftForks
      h          = head leftForks
      rightForks = t `snoc` h
  return $ zip leftForks rightForks

-- | Simulate the dining philosophers problem for i > 1 philosophers.
--
-- @ run $ diningPhilosophers $(toNatural 5) @
--
-- > Thinking: 1
-- > Thinking: 2
-- > Thinking: 3
-- > Thinking: 4
-- > Thinking: 5
-- > Eating: 1
-- > Eating: 3
-- > Thinking: 3
-- > Eating: 5
-- > Thinking: 1
-- > Eating: 2
-- > Eating: 4
-- > ...
diningPhilosophers :: Natural (Suc n) -> Process ()
diningPhilosophers n = do

    let names = fromNatural show n -- Name philosophers by an index
    forkPairs <- mkForkPairs n     -- create 'n' pairs of forks.

    -- Create a number of philosophers with a seating arrangement.
    let arrangement = zip names forkPairs
    philosophers <- mkPhilosophers arrangement

    -- For each philosopher:
    -- - When the philosopher is thinking => think for a random amount of
    --   time before becoming hungry.
    -- - When the philosopher is hungry and both forks are free =>
    --   eat for a random amount of time before replacing the forks and
    --   resuming thinking.
    def $ buildWith
            (\(Philosopher name leftFork rightFork thinking hungry) -> toDefinitions
              $ thinking                      |> do thinkRandom name; signal hungry
             |$ leftFork & hungry & rightFork |> do eatRandom name; signalAll [leftFork,thinking,rightFork]
            )
            philosophers

    signalAll . map fst . toList $ forkPairs         -- Lay all forks
    signalAll . map thinking . toList $ philosophers -- Begin thinking

