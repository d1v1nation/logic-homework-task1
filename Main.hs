module Main where
import MLCommon
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.List as List
import System.IO
import System.Environment
import Control.Monad
    
axs = classicAxiomList

annotateC = annotate axs
        
main = do
    args <- getArgs
    let
        al = length args
        in do
        when (al <= 0)
            $ putStrLn "No arguments given (inputFile outputFile) : entering interactive mode"
        hIn <- if (al <= 0) then return stdin else (openFile (args !! 0) ReadMode)
        hOut <- if (al <= 1) then return stdout else (openFile (args !! 1) WriteMode)
        head <- hGetLine hIn
        hPutStrLn hOut head
        inputIter hIn hOut (Map.empty) 1 [] (fst $ parseHeader head)

inputIter hIn hOut sts it listcache ass = do
    end <- hIsEOF hIn
    if (end)
        then return ()
        else do
            line <- hGetLine hIn
            hPutStrLn hOut $ annotateC ass sts listcache it line
            let parsed = parseStatement line in
                inputIter hIn hOut (Map.insert parsed it sts) (it + 1) ((parsed, it):listcache) ass
                
