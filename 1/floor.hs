module Floor where

whichFloor [] n = n
whichFloor (x:xs) n =
  let increment = case x of
        '(' -> 1
        ')' -> -1
        _ -> 0
  in whichFloor xs (n + increment)

main = do
  instructions <- readFile "input.txt"
  print $ whichFloor instructions 0
