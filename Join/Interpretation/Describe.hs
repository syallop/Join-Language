{-# LANGUAGE GADTs #-}
{-|
Module      : Join.Interpretation.Describe
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module provides an interpretation of "Join.Language" which describes the computation only.

-}
module Join.Interpretation.Describe where

import Join.Language
import Join.Types

import Control.Monad.Operational
import Data.Serialize (encode)

-- | Interpret a Process by describing each instruction on stdout.
-- Does not describe triggered Process's on RHS of Def patterns.
describe :: Process a -> IO ()
describe = describe' 0
  where
    describe' :: Int -> Process a -> IO ()
    describe' i p = do
        instr <- viewT p
        case instr of
            Return a  -> putStrLn "Terminate Process."

            Def dp p
                :>>= k -> do putStrLn $ "Def " ++ show dp ++ " |- {PROCESS}"
                             describe' i (k ())
            NewChannel
                :>>= k -> do putStrLn "NewChannel"
                             describe' (i+1) (k (inferSync i))

            Send c m
                :>>= k -> do putStrLn $ "Send " ++ show c ++ show (encode m)
                             describe' i (k ())

            Spawn p
                :>>= k -> do putStrLn "Spawn:{"
                             describe' i p
                             putStrLn "}"
                             describe' i (k ())

            Sync s m
                :>>= k -> do putStrLn $ "Sync " ++ show s ++ show (encode m)
                             describe' i (k undefined)

            Reply s m
                :>>= k -> do putStrLn $ "Reply " ++ show s ++ show (encode m)
                             describe' i (k ())

            With p q
                :>>= k -> do putStrLn "{"
                             describe' i p
                             putStrLn "} && {"
                             describe' i q
                             putStrLn "}"
                             describe' i (k ())

