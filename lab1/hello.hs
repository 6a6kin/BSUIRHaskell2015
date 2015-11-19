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

distance :: DistanceType -> [Float] -> [Float] -> Float
distance Euclidean = distanceEuclidean
distance Hamming = distanceHamming

-- ===========================
col :: [[a]] -> [a]
col = map head

woCol :: [[a]] -> [[a]]
woCol = map tail

--transpose :: [[a]] -> [[a]]
--transpose xss
--    | sum (map length xss) == 0 = []
--    | otherwise = (col xss):(transpose (woCol xss))

-- ===========================
--genCenters :: AssignMatrix -> [Object]-> ClasterCenters
--genCenters mss (xs:xss) = ():(genCenters mss xss)
--    where sumU = map (sum . map (^2)) . transpose
--          sumUX mss xss = zipWith (\x v -> x * v) (map (map (^2)) (transpose mss)) 

randGenCenters :: CenterCount -> [Object] -> AssignMatrix
randGenCenters c = take c . shuffle
    where shuffle = sortBy (\a b -> (toEnum (fst ((randomR (0,2) $ mkStdGen 4)::(Int,StdGen)))::Ordering))

-- ===========================
genAssign :: [Object]-> ClasterCenters -> AssignMatrix
genAssign s ss = [[1.0]]

randGenAssign :: CenterCount -> [Object] -> AssignMatrix
randGenAssign _ [] = []
randGenAssign c (xs:xss) = (normalize randList):(randGenAssign c xss)
    where randList = take c $ randoms (mkStdGen $ floor(sum xs)) :: [Float]
          normalize xs = map (/(sum xs)) xs

-- ===========================

--main = do print $ transpose [[1,2,3],[4,5,6],[7,8,9]]
main = do print $ randGenCenters 4 [[1,2,3],[4,5,6],[7,8,9],[11,12,13],[41,51,61],[17,18,19]]