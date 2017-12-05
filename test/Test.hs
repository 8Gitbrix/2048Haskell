module Test where
import Logic

testLeftRow :: Bool
testLeftRow =
  leftRow ([Just 4, Nothing, Just 2, Just 2] == [Just 4, Just 4, Nothing, Nothing])
  && (leftRow [Just 4, Just 4, Just 4, Just 4] == [Just 8, Just 8, Nothing, Nothing])
  && (leftRow [Just 4, Just 2, Nothing, Just 2] == [Just 4, Just 4, Nothing, Nothing])
  && (leftRow [Just 4, Nothing, Nothing, Just 4] == [Just 8, Nothing, Nothing, Nothing])

testMonteCarlo :: Grid -> Bool
testMonteCarlo g = (monteCarloPlayBoard g 0 (5,1,8)) > 1024
