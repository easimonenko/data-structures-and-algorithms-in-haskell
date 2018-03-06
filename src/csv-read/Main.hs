module Main where

import GHC.IO.Handle.FD
import System.Environment
import System.IO
import Text.CSV

main = do
  args <- getArgs
  let fileName = if length args > 0
      then args !! 0
      else "stdin"
  input <- if length args > 0
      then openFile (args !! 0) ReadMode
      else return stdin
  content <- hGetContents input
  let csv = parseCSV fileName content
  let res =
        either
          (\err -> "Parse error")
          (\csv -> show csv)
          csv
  (if length args > 1
   then writeFile $ args !! 1
   else putStrLn) $ res
