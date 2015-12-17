module Lab2
where

import Data.List as L
import Data.List.Split as S
import Options.Applicative as Cmd

import Control.Monad.Trans.Resource

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as BS

import qualified Data.Vector as V
import System.Random
import Data.Function
import Control.Parallel.Strategies
import Control.Monad.Par as P

data CmdArguments =  CmdArguments { 
                                   inputFile :: String,
                                   outputFile :: String,
                                   delimiter :: Char,
                                   skipFirstColumn :: Bool,
                                   skipFirstLane :: Bool,
                                   testPercentage :: Int,
                                   tryCount :: Int
                                   } deriving (Show, Read)

parseCmdArguments :: Cmd.Parser CmdArguments
parseCmdArguments = CmdArguments
    <$> argument str (metavar "FILE" <> help "Input file")
    <*> option str  (long "output"          <> short 'o' <> metavar "FILE"  <> help "Output file"                                       <> value "")
    <*> option auto (long "delimiter"       <> short 'd' <> metavar "CHAR"  <> help "Delimiter (default ,)"                             <> value ',')
    <*> option auto (long "skipFirstColumn" <> short 'f' <> metavar "BOOL"  <> help "Ignore first column of CSV (default False)"        <> value False)
    <*> option auto (long "skipFirstLane"   <> short 'z' <> metavar "BOOL"  <> help "Ignore first (header) lane of CSV (default False)" <> value False)
    <*> option auto (long "testPercentage"  <> short 't' <> metavar "INT"   <> help "testPercentage (default 20 )"                      <> value 20)
    <*> option auto (long "tryCount"        <> short 't' <> metavar "INT"   <> help "Try count (default 20)"                            <> value 20)

type ClassName = String
type ClassProbability = Double

type AttrMathExpectation = Double
type AttrVariance = Double

type Object = [Double]

type InputDictionary = (ClassName, Object)

type ClassDataRaw  = (ClassName, ClassProbability, [Object])
type ClassData     = (ClassName, ClassProbability, [(AttrMathExpectation, AttrVariance)])

-- ================================================

parseRow :: CmdArguments -> Conduit BS.ByteString (ResourceT IO) InputDictionary
parseRow args = awaitForever $ \srow -> do
  let 
    splitElements = splitOn [delimiter args]
    combine r = (last r, map (\x -> read x :: Double) (init r))
    (className, res) = combine . splitElements $ BS.unpack srow
  if className /= "" then yield (className, ignoreFirstCol args res) else return ()

-- ================================================
processCsvFile :: CmdArguments -> IO ([InputDictionary])
processCsvFile args = runResourceT $ stream $$ CB.lines =$ (parseRow args) =$ CL.consume
    where
        stream = CB.sourceFile (inputFile args)

-- ================================================
ignoreFirstCol :: CmdArguments -> Object -> Object
ignoreFirstCol cmdArguments = if skipFirstColumn cmdArguments then L.init else id

-- ================================================
spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
  i <- new
  fork (do x <- p; put i x)
  return i

parMap :: NFData b => (a -> b) -> [a] -> Par [b]
parMap f as = do
  ibs <- mapM (Lab2.spawn . return . f) as
  mapM get ibs

-- ================================================

classToTouple :: [InputDictionary] -> ClassDataRaw
classToTouple = fmap3 transpose . L.foldr (\(cn, vals) (_, count, vectors) -> (cn, count + 1, vals:vectors)) ("", 0.0, [])

matAndDisp :: Object -> (AttrMathExpectation, AttrVariance)
matAndDisp attributes  = (m, disp)
  where m = (sum attributes) / (fromIntegral $ length attributes)
        dispNumerator = L.sum  $ L.map (\x -> (x - m) ** 2)  attributes
        dispDevider = fromIntegral $ (L.length attributes - 1)
        disp = dispNumerator / dispDevider

normalizeClassData :: Int -> ClassDataRaw -> ClassData
normalizeClassData totalCount = fmap2 pcTransform . fmap3 transformAttrsToMat 
  where transformAttrsToMat = L.map matAndDisp
        pcTransform x = x / (fromIntegral totalCount)

groupByClassName :: [InputDictionary] -> [[InputDictionary]]
groupByClassName = L.groupBy (on (==) fst) . L.sortBy (on compare fst)

getTrainedClassData :: [InputDictionary] -> ClassData
getTrainedClassData input = normalizeClassData (length input) (classToTouple input)

computeTraining :: [InputDictionary] -> [ClassData]
computeTraining input = P.runPar $ Lab2.parMap getTrainedClassData (groupByClassName input)

fmap3 :: (c -> d) -> (a, b, c) -> (a, b, d)
fmap3 f (a, b, c) = (a, b, f c)

fmap2 :: (b -> d) -> (a, b, c) -> (a, d, c)
fmap2 f (a, b, c) = (a, f b, c)

-- ================================================
splitToTestAndTrainedSelection :: StdGen -> CmdArguments -> [InputDictionary] -> ([InputDictionary],[InputDictionary])
splitToTestAndTrainedSelection seed arguments input = (trainedObjects, testObjects)
  where
    objectsCount = L.length input
    testpercentD = fromIntegral (testPercentage arguments) / 100.0 :: Double
    testCount = round (fromIntegral objectsCount * testpercentD) :: Int
    trainedCount = objectsCount - testCount;
    trainedObjectsIndex = L.take trainedCount (nub (randomRs (0, objectsCount - 1) seed :: [Int]))
    trainedObjects = V.toList (V.ifilter (\i _ -> i `elem` trainedObjectsIndex) (V.fromList input))
    testObjects = V.toList (V.ifilter (\i _ -> not(i `elem` trainedObjectsIndex)) (V.fromList input))

-- ================================================
--𝑃(𝑥𝑖|𝐶)
proObjBelongToClassFromAttr :: (Double, (AttrMathExpectation, AttrVariance)) -> Double
proObjBelongToClassFromAttr (xi, (m, sqDisp)) = multip * (1.0 / sqrt (2.0 * pi * sqDisp))
  where
    multip = exp ((xi - m) ** 2) / (-2*sqDisp)

-- ================================================
determineClass :: [ClassData] -> Object -> String
determineClass input attributes = fst (L.head $ L.sortOn (\(_, prob) -> prob) getProbForClasses)
  where
    getTupleClassTotalProb (className, pc, attrPcMDisp) = (className, pc * (L.product $ L.map calcProbForAttr (L.zip attributes attrPcMDisp)))
    calcProbForAttr x = (proObjBelongToClassFromAttr x)
    getProbForClasses = L.map getTupleClassTotalProb input

-- ================================================
runBauesInternal :: [InputDictionary] -> Int -> CmdArguments -> StdGen -> [([ClassData], Double)]
runBauesInternal _ 0 _ _ = []
runBauesInternal input _ args gen = [(trained, accuracy)]
  where
    trained = computeTraining trainData
    trainData = fst (splitedObjects)
    testData = snd (splitedObjects)
    splitedObjects = splitToTestAndTrainedSelection gen args input
    accuracy = (getSuucessTestCount) / (fromIntegral $ length trainData)
    isCorrect (vectorClass, vectorData) = (determineClass trained vectorData) == vectorClass
    getSuucessTestCount = L.foldr (\x correctCount -> if isCorrect x then correctCount + 1 else correctCount) 0 testData

-- ================================================
runBaues :: [InputDictionary] -> Int -> CmdArguments -> StdGen -> [ClassData]
runBaues input tryCnt args gen = fst (last $ sortOn (\(_, acc) -> acc) testRes)
  where
    testRes = concat $ P.runPar $ Lab2.parMap (\cnt -> runBauesInternal input cnt args gen) (enumFromTo 0 tryCnt)