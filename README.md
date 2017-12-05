# 2048Haskell
<img width="652" alt="game" src="https://user-images.githubusercontent.com/11791254/33588803-c51f3310-d942-11e7-9d33-38c06214465c.png">


## Run:
```
stack setup
stack build
stack exec 2048Haskell
```

At the start of the game you can select a player (either yourself or a bot). Hit enter, and then enter the character representing the player you want.

## Rules:

Board made up of 16 tiles. Possible moves: Left, Right, Up, Down

1. At each time step a new tile that is a random multiple of 2 will be placed in the board.
2. When you select one of the moves, all tiles will move as far as possible in that direction in the grid.
3. Tiles of the same value will combine into 1 tile with the value of their sum.
4. The objective is to get a highest value tile you can. The highest possible value tile to win the game is 2048.

