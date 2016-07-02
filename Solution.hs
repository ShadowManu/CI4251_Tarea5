import Data.Array.IArray
import Data.List (foldl1')
import Control.Monad (forM_)

type Balloons = Int
type Floors = Int
type Drops = Int

type Solver = Balloons -> Floors -> Drops

-- Original signature
droppingsR :: Int -> Int -> Int
droppingsR = dropR

-- Aliased signature
dropR :: Balloons -> Floors -> Drops
dropR 1 f = f
dropR _ 0 = 0
dropR _ 1 = 1
dropR b f = allAttempts + 1
  where
    -- An attempt at a floor is the worst case of either...
    attempt fl = max blowing notBlowing
      where
        blowing = dropR (b-1) (fl-1)
        notBlowing = dropR b (f-fl)

    -- We need to get the best (the minimum) from initially trying from any LT or EQ floor
    allAttempts = foldl1' min $ map attempt [1..f]

-- TESTING ONLY (NOT PART OF THE ACTUAL SOLUTIONS)

-- Matrix of given solutions by the teacher
correctSolution :: [[Int]]
correctSolution =
  [ [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
  , [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6]
  , [1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5]
  , [1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5]
  , [1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5]
  , [1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5]
  , [1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5]
  , [1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5] ]

-- Compares a solver function against the correct solution
verifySolution :: Solver -> Bool
verifySolution s = solution == correctSolution
  where
    solution = [ [ s b f | f <- [1..20] ] | b <- [1..8]]

-- Runs a solver (for given balloons and floors
-- printing the answers as a very basic matrix. Used for debugging
runnerR :: Balloons -> Floors -> Solver -> IO ()
runnerR ba fl s = do
  putStrLn $ "SOLUTION for" ++ show ba ++ " " ++ show fl
  forM_ [1..ba] $ \b -> do
    forM_ [1..fl] $ \f -> do
      putStr . show $ s b f
      putStr " "
    putStrLn ""

main :: IO ()
main = return ()
