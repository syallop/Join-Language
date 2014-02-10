module Join.Language.Interpretation.Describe where

import Join.Language
import Join.Language.Types

import Control.Monad (void)

-- | Interpret a ProcessM by describing each instruction on stdout.
describe :: Interpretation IO
describe = Interpretation
    { iDef        = \c _ -> void $ putStrLn $ "Define a pattern for " ++ show c
    , iInert      = putStrLn "End."
    , iNewChannel = putStrLn "Request new Channel" >> return (Channel 0)
    , iSend       = \c _ -> void $ putStrLn $ "Send a value on " ++ show c
    , iSpawn      = \p -> do putStrLn "Asynchronously spawn ("
                             interpret describe p
                             putStrLn ")"
    , iSync       = \s _ -> do putStrLn $ "Synchronously send a value on: " ++ show s
                               return undefined
    , iReply      = \s _ -> putStrLn $ "Asynchronously reply a value to: " ++ show s
    , iWith       = \ p q -> do putStrLn "Simultaneously run: ( "
                                interpret describe p
                                putStr ", "
                                interpret describe q
                                putStrLn ")"
    }

