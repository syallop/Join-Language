module Join.Examples.DiningPhilosophers
    ( diningPhilosophersExplicit
    , diningPhilosophers
    ) where

import Join
import Join.Interpretation.Basic

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

-- | Simulate the dining philosophers problem for i > 1 philosophers.
diningPhilosophers :: Int -> Process ()
diningPhilosophers i =
    if i < 2
    then liftIO $ putStrLn "Require at least two philosophers"
    else do
    -- Name philosophers by an index
    -- An arrangement pairs each philosopher with 
    let names = take i $ map show [1..i]

    -- Create 'i' pairs of forks
    forkPairs <- mkForkPairs i

    -- The seating arrangement pairs each philosophers with their left and right
    -- fork.
    let arrangement = zip names forkPairs

    -- For each seating arrangement (a philosopher between two forks):
    -- - When the philosopher is thinking => think for a random amount of
    --   time before becoming hungry.
    -- - When the philosopher is hungry and both forks are free =>
    --   eat for a random amount of time before replacing the forks and
    --   resuming thinking.
    philosophers <- forM arrangement $ \(name,(leftFork,rightFork)) -> do
        [thinking,hungry]                   <- newChannels 2
        def $ thinking                      |> do thinkRandom name; signal hungry
           |$ leftFork & hungry & rightFork |> do eatRandom name; signalAll [leftFork,thinking,rightFork]
        return thinking

    -- All begin thinking and lay all forks.
    signalAll $ philosophers ++ forks forkPairs

type Fork      = Signal      -- ^ A signal on a Fork declares the fork available.
type Forks     = [Fork]      -- ^ List of Fork's
type ForkPair  = (Fork,Fork) -- ^ Pair of left and right hand Fork's
type ForkPairs = [ForkPair]  -- ^ List of left and right hand Fork's

-- | Pair a cycle of forks ofsize 'i'.
mkForkPairs :: Int -> Process ForkPairs
mkForkPairs i = do
    leftForks@(f:fs) <- newChannels i :: Process [Signal]
    let rightForks = fs ++ [f]
    return $ zip leftForks rightForks

-- | List the unique forks from a given fork pairs list.
forks :: ForkPairs -> Forks
forks = map fst

