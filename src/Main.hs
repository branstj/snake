{-# LANGUAGE RecordWildCards #-}
module Main where

import Config
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Monoid

data Direction = DUp | DDown | DLeft | DRight
data Status    = Paused | Running | GameOver

data Game = Game { snakePos :: [Point]
                 , snakeDir :: Direction
                 , applePos :: Point
                 , score    :: Int
                 }

newGame :: (Float, Float) -> Game
newGame appleCoord = Game [(0, 0), (-10, 0), (-10, 0)] DRight appleCoord 0

generateNewPos :: IO (Float, Float)
generateNewPos = do x <- randomRIO (-100, 100) :: IO Float
                    y <- randomRIO (-100, 100) :: IO Float
                    let x' = fromInteger $ (10*) $ (`quot` 5) $ round x :: Float
                    let y' = fromInteger $ (10*) $ (`quot` 5) $ round y :: Float
                    return (x', y')

main :: IO ()
main = do applePos <- generateNewPos
          playIO inWindowSettings black 15 (newGame applePos) draw handleInput stepGame

draw :: Game -> IO Picture
draw game = 
  return $ drawApple  game <> drawSnake game <> drawStatus game <> drawScore  game <> drawGrid

changeDir :: Direction -> Game -> IO Game
changeDir dir game = return $ game {snakeDir = dir}

toggleStatus :: Game -> IO Game
toggleStatus = undefined

handleInput :: Event -> Game -> IO Game
handleInput (EventKey (SpecialKey KeyUp)    Down _ _) = changeDir DUp
handleInput (EventKey (SpecialKey KeyDown)  Down _ _) = changeDir DDown
handleInput (EventKey (SpecialKey KeyLeft)  Down _ _) = changeDir DLeft
handleInput (EventKey (SpecialKey KeyRight) Down _ _) = changeDir DRight
handleInput _ = return

moveSnake :: Game -> Float -> Float -> IO Game
moveSnake game ox oy
    | applePos game /= head positions = return $ game { snakePos = move ox oy positions
                                                        : init positions}
    | otherwise = return $ game { snakePos = move ox oy positions : positions }
    where
        move :: Float -> Float -> [Point] -> Point
        move offsetx offsety ((x, y):_) = (x + offsetx, y + offsety)
        move _       _       []         = error "Something horrible has happened!"
        positions = snakePos game

addHead :: Game -> IO Game -- Based on direction appends new block and pops off end
addHead game = case snakeDir game of
    DDown  -> moveSnake game 0 (-10)
    DUp    -> moveSnake game 0 10
    DRight -> moveSnake game 10 0
    DLeft  -> moveSnake game (-10) 0

stepGame :: Float -> Game -> IO Game
stepGame _ game = do
    game' <- addHead game
    if applePos game /= head (snakePos game)
        then return game'
        else do newPos <- generateNewPos
                return $ game' {applePos = newPos, score = 1 + score game}

drawSnake :: Game -> Picture
drawSnake Game{..} = 
  -- Check if I need to regenerate the entire list or just the head
  mconcat [translate x y p | (p, (x, y)) <- zip (body $ length snakePos) snakePos]
    where
        body :: Int -> [Picture] -- Generates body of snake based on number of positions
        body 0 = []
        body x = color green (rectangleSolid 5 5) : body (x - 1)

drawScore :: Game -> Picture
drawScore Game{..} = translate 0.0 150.0
                   $ scale 0.25 0.25
                   $ color white
                   $ text
                   $ "Score: " ++ show score

drawApple :: Game -> Picture
drawApple Game{..} = translate x y $ color red $ rectangleSolid 5 5
    where
        x = fst applePos
        y = snd applePos

drawStatus :: Game -> Picture
drawStatus Game{..} = blank

drawGrid :: Picture
drawGrid = blank
