module Test where

testGrid :: Grid
testGrid = [[Just 16, Just 2, Just 4, Nothing],
            [Just 16, Nothing, Just 4, Just 8]
            [Just 16, Just 2, Just 4, Nothing],
            [Nothing, Just 2, Just 4, Just 8]]

testUp :: Grid
testUp = [[Just 32, Just 4, Just 8, Just 16],
          [Just 16, Just 2, Just 8, Nothing]
          [Nothing, Nothing, Nothing, Nothing],
          [Nothing, Nothing, Nothing, Nothing]]

testLeft :: Grid
testLeft = [[Just 16, Just 2, Just 4, Nothing],
            [Just 16, Just 4, Just 8, Nothing]
            [Just 16, Just 2, Just 4, Nothing],
            [Just 2, Just 4, Just 8, Nothing]]

testRight :: Grid
testRight = [[Nothing, Just 16, Just 2, Just 4],
            [Nothing, Just 16, Just 4, Just 8]
            [Nothing, Just 16, Just 2, Just 4],
            [Nothing, Just 2, Just 4, Just 8]]

testDown :: Grid
testDown = [[Nothing, Nothing, Nothing, Nothing],
            [Nothing, Nothing, Nothing, Nothing]
            [Just 16, Just 2, Just 8, Nothing],
            [Just 32, Just 4, Just 8, Just 16]]

testScore :: Bool
testScore = (scoreGrid testGrid 0) == 16
            && (scoreGrid testUp 0) == 32
            && (scoreGrid testLeft 0) == 16
            && (scoreGrid testDown 0) == 32

testLeftRow :: Bool
testLeftRow =
  leftRow ([Just 4, Nothing, Just 2, Just 2] == [Just 4, Just 4, Nothing, Nothing])
  && (leftRow [Just 4, Just 4, Just 4, Just 4] == [Just 8, Just 8, Nothing, Nothing])
  && (leftRow [Just 4, Just 2, Nothing, Just 2] == [Just 4, Just 4, Nothing, Nothing])
  && (leftRow [Just 4, Nothing, Nothing, Just 4] == [Just 8, Nothing, Nothing, Nothing])
  && (leftRow [Just 16, Just 16, Just 2, Just 4] == [Just 32, Just 2, Just 4, Nothing])

testMonteCarlo :: Grid -> Bool
testMonteCarlo g = (monteCarloPlayBoard g 0 (5,1,8)) > 256

testStuck :: Bool
testStuck =
  stuckCheck [[Just 16, Just 2, Just 4, Just 8], [Just 16, Just 2, Just 4, Just 8]
              [Just 16, Just 2, Just 4, Just 8], [Just 16, Just 2, Just 4, Just 8]]
              == True

testDir :: String -> Bool
testDir s = case s of
  "U" -> transpose $ leftGrid $ transpose testGrid == testUp
  "R" -> map reverse $ leftGrid (map reverse testGrid) == testRight
  "L" -> leftGrid testGrid == testLeft
  "D" -> transpose $ map reverse $ leftGrid $ map reverse $ transpose testGrid == testDown

mainTest :: IO ()
mainTest = do
  if testLeftRow
     && testMonteCarlo [[Just 2, Just 2, Nothing, Nothing],[Nothing, Nothing, Nothing, Nothing],
                        [Nothing, Nothing, Nothing, Nothing],[Nothing, Nothing, Nothing, Nothing]]
     && testStuck && testDir "U" && testDir "R" && testDir "D" && testDir "L"
     && testScore
  then
    putStrLn "Passed tests."
  else
    putStrLn "Failed tests."
