{-# LANGUAGE RecordWildCards #-}
module Main where

import Config
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Monoid

data Direction = DUp
               | DLeft
               | DDown
               | DRight
               deriving (Eq)

data Status = Paused
            | Playing
            | GameOver
            deriving (Show)

data Game = Game { snakeCoord   :: [Point]
                 , snakeDir     :: Direction
                 , appleCoord   :: Point
                 , score        :: Int
                 , gameStatus   :: Status }

initGame :: IO Game
initGame = do appleCoord <- mkApple
              return $ Game [(0, 0), (-10, 0), (-20, 0)] DRight appleCoord 0 Playing

mkApple :: IO (Float, Float)
mkApple = do x <- randomRIO (-100, 100) :: IO Float
             y <- randomRIO (-100, 150) :: IO Float
             return (snap x, snap y) 
                where snap = fromInteger . (10*) . (`quot` 10) . round

main :: IO ()
main = do newGame <- initGame
          playIO inWindowSettings black 15 newGame draw handleInput stepGame

changeDir :: Direction -> Game -> IO Game
changeDir dir game = return $ game {snakeDir = dir}

toggleStatus :: Game -> IO Game
toggleStatus game = case gameStatus game of
    Playing -> return $ game {gameStatus = Paused}
    Paused  -> return $ game {gameStatus = Playing}
    _       -> return game

handleInput :: Event -> Game -> IO Game
handleInput (EventKey (SpecialKey KeyUp)    Down _ _) = changeDir DUp
handleInput (EventKey (SpecialKey KeyDown)  Down _ _) = changeDir DDown
handleInput (EventKey (SpecialKey KeyLeft)  Down _ _) = changeDir DLeft
handleInput (EventKey (SpecialKey KeyRight) Down _ _) = changeDir DRight
handleInput (EventKey (SpecialKey KeySpace) Down _ _) = const initGame
handleInput (EventKey (Char 'p')            Down _ _) = toggleStatus
handleInput _ = return

boundaryCollision :: Game -> Bool
boundaryCollision Game{..} = x > halfScreenWidth
                          || x < negate halfScreenWidth
                          || y > halfScreenHeight - 100
                          || y < negate halfScreenHeight
                           where (x, y) = head snakeCoord

bodyCollision :: Game -> Bool
bodyCollision Game{..} = head snakeCoord `elem` tail snakeCoord

checkCollision :: Game -> Game
checkCollision game
    | bodyCollision game || boundaryCollision game = game {gameStatus = GameOver}
    | otherwise = game

moveSnake :: Game -> Float -> Float -> IO Game
moveSnake game@(Game _ _ _ _ Paused) _ _ = return game
moveSnake game@(Game _ _ _ _ GameOver) _ _ = return game
moveSnake game@(Game{..}) ox oy
    | appleCoord /= head snakeCoord =
        return $ checkCollision $ game { snakeCoord = move ox oy snakeCoord : init snakeCoord}
    | otherwise =
        return $ checkCollision $ game { snakeCoord = move ox oy snakeCoord : snakeCoord }
    where
        move :: Float -> Float -> [Point] -> Point
        move offsetx offsety ((x, y):_) = (x + offsetx, y + offsety)
        move _       _       []         = error "Something horrible has happened!"

addHead :: Game -> IO Game -- Based on direction appends new block and pops off end
addHead game = case snakeDir game of
    DDown  -> moveSnake game 0 (-10)
    DUp    -> moveSnake game 0 10
    DRight -> moveSnake game 10 0
    DLeft  -> moveSnake game (-10) 0

stepGame :: Float -> Game -> IO Game
stepGame _ game = do
    game' <- addHead game
    if appleCoord game /= head (snakeCoord game)
        then return game'
        else do newPos <- mkApple
                return $ game' {appleCoord = newPos, score = 1 + score game}

draw :: Game -> IO Picture
draw game = 
  return $ drawApple  game
        <> drawSnake  game
        <> drawStatus game
        <> drawScore  game
        <> drawGrid

drawSnake :: Game -> Picture
drawSnake Game{..} = 
  mconcat [translate x y p | (p, (x, y)) <- zip (buildBody $ length snakeCoord) snakeCoord]
    where buildBody :: Int -> [Picture]
          buildBody 0 = []
          buildBody x = color green (rectangleSolid 10 10) : buildBody (x - 1)

positionText :: Float -> Float -> String -> Picture
positionText dx dy = translate dx dy . scale 0.25 0.25 . color white . text

drawScore :: Game -> Picture
drawScore Game{..} = positionText (-230.0) 200.0 $ "Score: " ++ show score

drawApple :: Game -> Picture
drawApple Game{..} = translate dx dy $ color red $ rectangleSolid 10 10
                        where (dx, dy) = appleCoord

drawStatus :: Game -> Picture
drawStatus Game{..} = case gameStatus of
    Playing -> blank
    _       -> positionText 90 200 $ show gameStatus

drawGrid :: Picture
drawGrid = mconcat $ map (translate 5 5 . color (greyN 0.3) . scale 1 1 . line)
                   $ [[(x,150), (x,-250)] | x <- [-250, -240..250]]
                  ++ [[(-250,y), (250,y)] | y <- [-250, -240..150]]
