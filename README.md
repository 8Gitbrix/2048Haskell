# 2048Haskell

## Run:
```
npm install
stack install --local-bin-path build
./node_modules/.bin/electron electron.js
```

Or just enter the app directory and double click 2048Haskell-exe

## Rules:

Board made up of 16 tiles. Possible moves: Left, Right, Up, Down

1. At each time step a new tile that is a random multiple of 2 will be placed in the board.
2. When you select one of the moves, all tiles will move as far as possible in that direction in the grid.
3. Tiles of the same value will combine into 1 tile with the value of their sum.
4. The objective is to get a highest value tile you can. The highest possible value tile to win the game is 2048.
