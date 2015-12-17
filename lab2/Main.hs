module Main
where

import Lab2

import qualified Data.List as L
import Options.Applicative as Cmd

import System.IO
import Data.Conduit
import qualified Data.Conduit.List as CL

import System.Random
import Control.Monad.State

main :: IO ()
main = do
    seed <- newStdGen 

    let cmdArguments = info (helper <*> parseCmdArguments) fullDesc
    parsedCmdArguments <- execParser cmdArguments
    
    res <- processCsvFile parsedCmdArguments
   
    let baues = runBaues res (tryCount parsedCmdArguments) parsedCmdArguments seed
    let result = CL.sourceList $ [fst $ runState (getState baues) ""]

    h <- if (outputFile parsedCmdArguments) == "" then return stdout else (openFile (outputFile parsedCmdArguments) WriteMode)
    result $$ (CL.mapM_ (hPutStrLn h))

getState :: [ClassData] -> State String String
getState [] = do
    old <- get
    return old
getState xs = do
    put $ concat $ L.map (\x -> show x) xs
    getState []