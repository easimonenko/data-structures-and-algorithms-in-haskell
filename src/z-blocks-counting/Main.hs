module Main where

comparison _ [] = 0
comparison [] _ = 0
comparison (t : txs) (p : pxs) =
  if t == p then
    1 + comparison txs pxs
  else
    0

zBlocks :: String -> [Int]
zBlocks [] = []
zBlocks s =
  let
    zBlock zBlocksCounted raw k r l =
      if k > r then
        let
          c = comparison s raw
        in
          if c > 0 then
            (c, k + c - 1, k)
          else
            (0, r, l)
      else
        let
          zk = reverse zBlocksCounted !! (k - l)
        in
          if zk < r - k + 1 then
            (zk, r, l)
          else
            let
              c = comparison (drop (k - l + 1) s) (drop (r - k) raw)
            in
              (r - k + c, r + c, k)

    zBlocksImp [] _ _ _ res = res
    zBlocksImp raw k r l res =
      let
        (z, rNext, lNext) = zBlock res raw k r l
      in
        zBlocksImp (tail raw) (k + 1) rNext lNext (z : res)
  in
    reverse $ zBlocksImp (tail s) 2 0 0 []

main :: IO ()
main =
  interact $ show . zBlocks
