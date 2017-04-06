module Main where

comparison :: String -> String -> Bool
comparison [] _ = False
comparison text pattern =
  let
    comparison_next _ [] = True
    comparison_next [] _ = False
    comparison_next (t : txs) (p : pxs) = t == p && comparison_next txs pxs
  in
    (comparison_next text pattern) || (comparison (tail text) pattern)

main :: IO ()
main = do
  text <- getLine
  pattern <- getLine
  putStrLn $ if comparison text pattern then "YES" else "NO"
