import Prelude hiding (catch) 
import System.Random as R
import System.Environment
import System.Console.CmdArgs
import System.IO
import System.IO.Error hiding(try, catch, ioError)
import Control.Exception

import FMC
import Help
import Args
import CSVParse

options = InputOptions{
  accuracy = 0.3 &= help "accuracy of the algorithm",
  sourceFile = "source1.txt" &= help "file path for sorce data",
  numberOfClasses = 2 &= help "number of classes",
  resultFile = "" &= help "file path for result data",
  metric = "e" &= help "metric for distance calculation (e - Euclidean, h - Hamming)",
  columnSplitter = "," &= help "column splitter in csv file",
  ignoreTitle = False &= help "ignore title row in csv file",
  ignoreFirstCol = False &= help "ignore first column in csv file",
  ignoreLastCol = True &= help "ignore last column in csv file",
  table = "u" &= help "generation of this table starts the algorithm"   
}

printMatrix m = mapM_ print m 

getData source splitter t fc lc = f (getMaybeData source splitter t fc lc)
   where f (Nothing) = error "Wrong CSV File Format!"
         f (Just a) = a

checkMetrica c = case c of
    "e" ->  euclideanDist
    "h" ->  hammingDist
    _ -> error "Wrong metrica!"

checkTable c = case c of
    "u" -> fmc1
    "v" -> fmc2
    _   -> error "Wrong table!"

main = do
    newGen <- R.getStdGen
    args' <- cmdArgs options
    sourceData <- readFile (sourceFile args') `catch` handlerForFileRoutine (sourceFile args') 
    let x = getData sourceData (columnSplitter args') (ignoreTitle args') (ignoreFirstCol args') (ignoreLastCol args')
        metrica = checkMetrica $ metric args'
        accur = accuracy args'
        n = numberOfClasses args'
        func = checkTable $ table args' 
        u = func newGen metrica x accur n
    let r = resultFile args'
        in if r == "" then printMatrix u else writeFile r (show u) `catch` handlerForFileRoutine r 