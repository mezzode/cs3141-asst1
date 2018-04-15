module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.

andThen :: Instructions -> Instructions -> Instructions
andThen Stop i = i
andThen i Stop = i
andThen (Move d i1) i2 = Move d (i1 `andThen` i2)
andThen (Turn a i1) i2 = Turn a (i1 `andThen` i2)
andThen (SetStyle ls i1) i2 = SetStyle ls (i1 `andThen` i2)
andThen (SetColour c i1) i2 = SetColour c (i1 `andThen` i2)
andThen (PenDown i1) i2 = PenDown (i1 `andThen` i2)
andThen (PenUp i1) i2 = PenUp (i1 `andThen` i2)

loop :: Int -> Instructions -> Instructions
loop n i = error "'loop' unimplemented"

invisibly :: Instructions -> Instructions
invisibly i = error "'invisibly' unimplemented"

retrace :: Instructions -> Instructions
retrace i = error "'retrace' unimplemented"

overlay :: [Instructions] -> Instructions
overlay is = error "'overlay' unimplemented"

