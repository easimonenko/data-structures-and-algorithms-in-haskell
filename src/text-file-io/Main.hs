module Main where

import GHC.IO.Handle.FD
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- System.Environment.getArgs
  input <-
    if length args > 0
    then openFile (args !! 0) ReadMode
    else return stdin
  content <- hGetContents input
  (if length args > 1
   then writeFile $ args !! 1
   else putStrLn)
    $ show $ map (length . words) (lines $ content)
