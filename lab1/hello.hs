import qualified Data.Set as Set
import System.Random
import Data.List

type Object = [Float]

type ClasterCenters = [Object]

type AssignMatrix = [[Float]]

type CenterCount = Int

data CsvSettingsOption = CsvIgnoreHeader | CsvIgnoreFirstCol | CsvIgnoreLastCol

data DistanceType = Euclidean | Hamming deriving (Enum,Eq)

--parseArgs :: String -> Settings
--parseArgs = (\x -> Set.empty) . words

distanceEuclidean :: Object -> Object -> Float
distanceEuclidean = (sqrt .) . (sum .) . zipWith (\x v -> (x - v)^2)

distanceHamming :: Object -> Object -> Float
distanceHamming = (sum .) . zipWith (\x v -> abs (x - v))

distance :: DistanceType -> Object -> Object -> Float
distance Euclidean = distanceEuclidean
distance Hamming = distanceHamming

normMatrix :: AssignMatrix -> AssignMatrix -> Float
normMatrix kss1 kss = maximum $ zipWith ((abs .) . (-)) (concat kss1) (concat kss)

-- ===========================
col :: [[a]] -> [a]
col = map head

woCol :: [[a]] -> [[a]]
woCol = map tail

-- transpose :: [[a]] -> [[a]]
-- transpose xss
--     | sum (map length xss) == 0 = []
--     | otherwise = (col xss):(transpose (woCol xss))

-- ===========================
genCenters :: Float -> AssignMatrix -> [Object]-> ClasterCenters
genCenters m uss xss = map byL (transpose uss)
    where byL us = map (byJ (pow us)) (transpose xss)
          byJ p_us xs = (sum $ zipWith (*) p_us xs) / (sum p_us)
          pow = map (**m)

randGenCenters :: CenterCount -> [Object] -> AssignMatrix
randGenCenters c = take c . shuffle
    where shuffle = sortBy (\a b -> (toEnum randOrdInt :: Ordering))
          randOrdInt = fst ((randomR (0,2) $ mkStdGen 4) :: (Int,StdGen))

-- ===========================
genAssign :: Float -> DistanceType -> ClasterCenters -> [Object]-> AssignMatrix
genAssign m dt vss xss = map byI xss
    where byI xs = map (byK xs) vss
          byK xs vs_k = 1.0 / ((sum . pow) $ map (\vs_j -> (d xs vs_k) / (d xs vs_j)) vss)
          pow = map (**(2/(m-1)))
          d = distance dt

randGenAssign :: CenterCount -> [Object] -> AssignMatrix
randGenAssign _ [] = []
randGenAssign c (xs:xss) = (normalize randList):(randGenAssign c xss)
    where randList = take c $ randoms (mkStdGen $ floor(sum xs)) :: [Float]
          normalize xs = map (/(sum xs)) xs


-- ===========================

--main = do print $ transpose [[1,2,3],[4,5,6],[7,8,9]]
main = do print $ randGenCenters 4 [[1,2,3],[4,5,6],[7,8,9],[11,12,13],[41,51,61],[17,18,19]]