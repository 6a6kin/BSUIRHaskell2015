import qualified Data.Set as Set
import System.Random
import Data.List
import Data.List.Split

type Object = [Float]

type ClasterCenters = [Object]

type AssignMatrix = [[Float]]

type CenterCount = Int

data CsvSettingsOption = CsvIgnoreHeader | CsvIgnoreFirstCol | CsvIgnoreLastCol

data DistanceType = Euclidean | Hamming deriving (Enum,Eq,Show,Read)

data Settings = Settings { csvColSplitter :: String
                         , csvIgnoreFirstCol :: Bool
                         , csvIgnoreLastCol :: Bool
                         , csvIgnoreHeader :: Bool
                         , filename :: String
                         , clasterCount :: Int
                         , precision :: Float
                         , distanceType :: DistanceType
                         , expCoeff :: Float
                         , isRandMatrix :: Bool
                         } deriving (Show)

defaultSettings = Settings {csvColSplitter = ","
                         , csvIgnoreFirstCol = False
                         , csvIgnoreLastCol = True
                         , csvIgnoreHeader = False
                         , filename = "input.txt"
                         , clasterCount = 2
                         , precision = 0.00001
                         , distanceType = Euclidean
                         , expCoeff = 2
                         , isRandMatrix = True
                         }

--parseArgs :: String -> Settings
--parseArgs = (\x -> Set.empty) . words

distanceEuclidean :: Object -> Object -> Float
distanceEuclidean = (sqrt .) . (sum .) . zipWith (\x v -> (x - v)^2)

distanceHamming :: Object -> Object -> Float
distanceHamming = (sum .) . zipWith (\x v -> abs (x - v))

distance :: DistanceType -> Object -> Object -> Float
distance Euclidean = distanceEuclidean
distance Hamming = distanceHamming

matrixNorma :: AssignMatrix -> AssignMatrix -> Float
matrixNorma kss1 kss = maximum $ zipWith ((abs .) . (-)) (concat kss1) (concat kss)

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
genCenters :: Settings -> AssignMatrix -> [Object]-> ClasterCenters
genCenters st uss xss = map byL (transpose uss)
    where byL us = map (byJ (pow us)) (transpose xss)
          byJ p_us xs = sum (zipWith (*) p_us xs) / sum p_us
          pow = map (**expCoeff st)

randGenCenters :: StdGen -> CenterCount -> [Object] -> ClasterCenters
randGenCenters seed c xss = map ((!!) xss) (randomRs (0, length xss - 1) seed :: [Int])

-- ===========================
genAssign :: Settings -> ClasterCenters -> [Object]-> AssignMatrix
genAssign st vss = map byI
    where byI xs = map (byK xs) vss
          byK xs vs_k = 1.0 / (sum . pow) (map (\vs_j -> d xs vs_k / d xs vs_j) vss)
          pow = map (**(2/(expCoeff st - 1)))
          d = distance $ distanceType st

randGenAssign :: StdGen -> CenterCount -> [Object] -> AssignMatrix
randGenAssign _ _ [] = []
randGenAssign seed c xss = map normalize $ randList xss
    where randList xss = chunksOf c $ take (c * length xss) (randoms seed :: [Float])
          normalize xs = map (/ sum xs) xs

-- ===========================
cmeans :: StdGen -> Settings -> [Object] -> AssignMatrix
cmeans seed st xss = cmeansInternal st xss uss
    where uss = if isRandMatrix st then randGenAssign seed (clasterCount st) xss
                                   else genAssign st vss xss
          vss = randGenCenters seed (clasterCount st) xss

-- gen assign matrix is always first step for internal func
cmeansInternal :: Settings -> [Object] -> AssignMatrix -> AssignMatrix
cmeansInternal st xss uss = let uss_k1 = genAssign st (genCenters st uss xss) xss
                             --in uss
                             in if matrixNorma uss_k1 uss < precision st then uss_k1
                                else cmeansInternal st xss uss_k1

-- ===========================
main = do 
    seed <- getStdGen
    print $ cmeans seed (defaultSettings {clasterCount=2, distanceType=Euclidean, expCoeff=2, isRandMatrix=False}) [[1.0,3],[1,5],[2,4],[3,3],[2,2],[2,1],[1,0],[5,5],[6,5],[7,6],[5,3],[7,3],[6,2],[6,1],[8,1]]