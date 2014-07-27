module Join.Data.Loop
    ( loop
    ) where

import Join

loop :: Signal -> Int -> Process ()
loop c i = if i > 0 then (signal c `with` loop c (i-1)) else inert

