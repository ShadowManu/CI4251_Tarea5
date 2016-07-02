module Solution
( droppingsR
, droppingsD
) where

import Data.Array.IArray

type Balloons = Int
type Floors = Int
type Drops = Int

-- Helper function to create arrays from functions
mkArray :: (Ix i) => (i -> a) -> (i, i) -> Array i a
mkArray f bs = array bs [ (i, f i) | i <- range bs ]

-- Original signature
droppingsR :: Int -> Int -> Int
droppingsR = dropR

-- Aliased signature
dropR :: Balloons -> Floors -> Drops
dropR 1 f = f
dropR _ 0 = 0
dropR _ 1 = 1
dropR b f = max whenAlive whenExplode
  where
    lowerHalf = (f-1) `div` 2
    upperHalf = f `div` 2

    whenAlive = 1 + dropR b lowerHalf
    whenExplode = 1 + dropR (b-1) upperHalf

-- Original signature
droppingsD :: Int -> Int -> Int
droppingsD = dropD

-- Aliased signature
dropD :: Balloons -> Floors -> Drops
dropD ba fl = calc (ba, fl)
  where
    table = mkArray calc ((0, 0), (ba, fl))

    calc (1, f) = f
    calc (_, 0) = 0
    calc (_, 1) = 1
    calc (b, f) = max whenAlive whenExplode
      where
        lowerHalf = (f-1) `div` 2
        upperHalf = f `div` 2

        whenAlive = 1 + table ! (b, lowerHalf)
        whenExplode = 1 + table ! (b-1, upperHalf)
