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
loop n i
    | n > 0 = i `andThen` (loop (n-1) i)
    | otherwise = Stop

-- no picture but produces same state, which includes pen state
invisibly :: Instructions -> Instructions
invisibly i = PenUp (penless i True) where
    -- we pass whether penDown in the visible version so final state penDown will match
    penless :: Instructions -> Bool -> Instructions
    penless (Move d i) penDown = Move d (penless i penDown)
    penless (Turn a i ) penDown = Turn a (penless i penDown)
    penless (SetStyle ls i) penDown = SetStyle ls (penless i penDown)
    penless (SetColour c i) penDown = SetColour c (penless i penDown)
    penless (PenDown i) penDown = penless i True
    penless (PenUp i) penDown = penless i False
    -- if penDown then we PenDown so final state correctly has penDown too
    penless Stop penDown
        | penDown = PenDown Stop
        | otherwise = Stop

-- requires start line style and colour
retrace :: Instructions -> Instructions
retrace i = retracer i Stop (Solid 1) white True where
    retracer :: Instructions -> Instructions -> LineStyle -> Colour -> Bool -> Instructions
    retracer (Move d i) acc s c p = retracer i (Move (-d) acc) s c p
    retracer (Turn a i) acc s c p = retracer i (Turn (-a) acc) s c p
    -- line style, colour, and pen actions are off by one due to start state so we pass what they should be next
    retracer (SetStyle nextStyle i) acc currStyle c p = retracer i (SetStyle currStyle acc) nextStyle c p
    retracer (SetColour nextColour i) acc s currColour p = retracer i (SetColour currColour acc) s nextColour p
    retracer (PenUp i) acc s c penDown
        | penDown = retracer i (PenDown acc) s c False
        | otherwise = retracer i (PenUp acc) s c False
    retracer (PenDown i) acc s c penDown
        | penDown = retracer i (PenDown acc) s c True
        | otherwise = retracer i (PenUp acc) s c True
    retracer Stop acc _ _ _ = acc

overlay :: [Instructions] -> Instructions
overlay is = error "'overlay' unimplemented"

