module Main
where

import Lab1
import Data.CSV.Conduit
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import System.Random
import System.Console.CmdArgs

main :: IO ()
main = do 
    opts <- cmdArgs defaultSettings
    let csvOpts = defCSVSettings {csvSep = head (csvColSplitter opts), csvQuoteChar = Nothing}

    seed <- getStdGen

    input <- handleAll (\e -> error $ "Cannot read input file: " ++ show e ) $ runResourceT $ readCSVFile csvOpts (inputFile opts)

    let cmeansResult = cmeans seed opts (convertFromCsv opts input)

    runResourceT $ CL.sourceList (convertToCsv opts cmeansResult) $$ CB.sinkIOHandle (buildOutputHandle opts)