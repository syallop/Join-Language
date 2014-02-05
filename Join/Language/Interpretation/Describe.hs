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
    , iSend       = \c _ -> void $ putStrLn $ "Spawn a value on " ++ show c
    , iSpawn      = \p -> do putStrLn "Asynchronously spawn ("
                             interpret describe p
                             putStrLn ")"
    , iWith       = \ p q -> do putStrLn "Simultaneously run: ( "
                                interpret describe p
                                putStr ", "
                                interpret describe q
                                putStrLn ")"
    }

