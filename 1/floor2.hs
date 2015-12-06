module Floor where

whichFloor _ (-1) i = i
whichFloor (x:xs) n i =
  let increment = case x of
        '(' -> 1
        ')' -> -1
        _ -> 0
  in whichFloor xs (n + increment) (i + 1)

main = do
  instructions <- readFile "input.txt"
  print $ whichFloor instructions 0 0
