{-# LANGUAGE FlexibleInstances
            ,IncoherentInstances
            ,MultiParamTypeClasses
 #-}
module Join.Types.Apply
    ( Application(..)
    , Apply(..)
    , unsafeApply
    ) where

import Data.ByteString (ByteString)
import Data.Serialize (Serialize, decode)

-- | Encapsulates the result of some type of function application that may
-- either succeed or fail in a limited number of ways.
data Application r
    = Result r -- ^ Successful result value.
    | Excess   -- ^ Too many arguments.
    | Shortage -- ^ Too few arguments.
    | Mistyped -- ^ Mistyped argument.

instance Show (Application r) where
    show (Result _) = "Successful application."
    show Excess = "Too many arguments."
    show Shortage = "Too few arguments."
    show Mistyped = "Mistyped arguments."

-- | Class of types 'f' which may be applied to a sequence of ByteString
-- parameters, resulting in a value of type 'r'.
--
-- 'apply' may be used in interpreters to run Join-Definition trigger
-- function on messages that have been deemed to match the corresponding
-- pattern.
class Apply f r where
    apply :: f -> [ByteString] -> Application r

instance (Serialize a, Apply b r) => Apply (a -> b) r where
    apply f (m:ms) = case decode m of
        Left  _ -> Mistyped
        Right v -> apply (f v) ms
    apply _ [] = Shortage

instance Apply r r where
    apply r [] = Result r
    apply _ _  = Excess

-- | Unsafe version of 'Apply's 'apply'.
--
-- Only guaranteed to be safe when:
--
-- - The number of list items is exactly equal to the number of arguments
-- expected by 'f'.
--
-- - Each argument Serializes to the corresponding expected type.
unsafeApply :: Apply f r => f -> [ByteString] -> r
unsafeApply f ms = case apply f ms of
    Result r -> r
    failure  -> error $ show failure

