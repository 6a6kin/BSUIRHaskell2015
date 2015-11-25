{-# LANGUAGE DeriveDataTypeable #-}

import System.Random
import Data.List
import Data.List.Split
import System.Console.CmdArgs

import qualified Data.Vector as V
import Data.CSV.Conduit

type Object = [Float]

type ClasterCenters = [Object]

type AssignMatrix = [[Float]]

type CenterCount = Int

data DistanceType = Euclidean | Hamming deriving (Enum, Eq, Show, Read, Data, Typeable)

data Settings = Settings { csvColSplitter :: String
                         , csvIgnoreFirstCol :: Bool
                         , csvIgnoreLastCol :: Bool
                         , csvIgnoreHeader :: Bool
                         , inputFile :: FilePath
                         , outputFile :: FilePath
                         , clasterCount :: Int
                         , precision :: Float
                         , distanceType :: DistanceType
                         , expCoeff :: Float
                         , isRandMatrix :: Bool
                         } deriving (Data, Typeable, Show, Eq)

defaultSettings :: Settings
defaultSettings = Settings { csvColSplitter = ","         &= help "Column delimiter (default=,)"        &= typ "delim"
                           , csvIgnoreFirstCol = False    &= help "Ignore first column"   
                           , csvIgnoreLastCol = True      &= help "Ignore last column"    
                           , csvIgnoreHeader = False      &= help "Ignore header"         
                           , clasterCount = 2             &= help "Claster count"                       &= typ "count"
                           , precision = 0.00001          &= help "Precision"
                           , distanceType = Euclidean     &= help "Distance type (Euclidean, Hamming)"
                           , expCoeff = 2                 &= help "m - exp coefficient"
                           , isRandMatrix = True          &= help "First action in FCM (True - generate matrix, False - generate centers)"   
                           , inputFile = "input.txt"      &= help "Input filename"                      &= typ "infile"
                           , outputFile = ""              &= help "Output filename"                     &= typ "outfile"
                           } &= summary "Lab1 FCM 2015" &= program "lab1"

-- ===========================
distanceEuclidean :: Object -> Object -> Float
distanceEuclidean = (sqrt .) . (sum .) . zipWith (\x v -> (x - v)**2)

distanceHamming :: Object -> Object -> Float
distanceHamming = (sum .) . zipWith (\x v -> abs (x - v))

distance :: DistanceType -> Object -> Object -> Float
distance Euclidean = distanceEuclidean
distance Hamming = distanceHamming

matrixNorma :: AssignMatrix -> AssignMatrix -> Float
matrixNorma kss1 kss = maximum $ zipWith ((abs .) . (-)) (concat kss1) (concat kss)

-- ===========================
genCenters :: Settings -> AssignMatrix -> [Object]-> ClasterCenters
genCenters st uss xss = map byL (transpose uss)
    where byL us = map (byJ (pow us)) (transpose xss)
          byJ p_us xs = sum (zipWith (*) p_us xs) / sum p_us
          pow = map (**expCoeff st)

randGenCenters :: StdGen -> CenterCount -> [Object] -> ClasterCenters
randGenCenters seed c xss = map (xss !!) (take c $ randomRs (0, length xss - 1) seed :: [Int])

-- ===========================
genAssign :: Settings -> ClasterCenters -> [Object]-> AssignMatrix
genAssign st vss = map byI
    where byI xs = map (byK xs) vss
          byK xs vs_k = if xs == vs_k then 1
                        else 1.0 / (sum . pow) (map (\vs_j -> d xs vs_k / d xs vs_j) vss)
          pow = map (**(2/(expCoeff st - 1)))
          d = distance $ distanceType st

randGenAssign :: StdGen -> CenterCount -> [Object] -> AssignMatrix
randGenAssign _ _ [] = []
randGenAssign seed c xss = map normalize randList
    where randList = chunksOf c $ take (c * length xss) (randoms seed :: [Float])
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
                            in if matrixNorma uss_k1 uss < precision st then uss_k1
                               else cmeansInternal st xss uss_k1

-- ===========================
convertFromCsv :: Settings -> V.Vector (Row String) -> [Object]
convertFromCsv st = processCsv . V.toList
    where processCsv = map processRow . ignoreHeader
          ignoreHeader = if csvIgnoreHeader st then tail else id
          processRow = map (read ) . ignoreFirst . ignoreLast
          ignoreFirst = if csvIgnoreFirstCol st then tail else id
          ignoreLast = if csvIgnoreLastCol st then init else id

-- ===========================
main :: IO ()
main = do 
    opts <- cmdArgs defaultSettings
    seed <- getStdGen
    print opts
    inp <- runResourceT $ readCSVFile (defCSVSettings {csvSep = head (csvColSplitter opts), csvQuoteChar = Nothing}) (inputFile opts)
    mapM_ (putStrLn.show) (cmeans seed opts (convertFromCsv opts inp))