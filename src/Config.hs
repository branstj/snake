module Config
    ( inWindowSettings
    , fullScreenSettings
    , screenWidth
    , screenHeight
    , halfScreenWidth
    , halfScreenHeight
    ) where

import Graphics.Gloss

-- Display Settings --
screenWidth, screenHeight :: Int
screenWidth = 500
screenHeight = 500

halfScreenWidth, halfScreenHeight :: Float
halfScreenWidth = fromIntegral screenWidth / 2
halfScreenHeight = fromIntegral screenHeight / 2

-- These are only needed if the game is played in a window
screenPosH, screenPosW :: Int
screenPosW = 0
screenPosH = 0

inWindowSettings :: Display
inWindowSettings = InWindow "Snake" (screenWidth, screenHeight) (screenPosW, screenPosH)

fullScreenSettings :: Display
fullScreenSettings = FullScreen (screenWidth, screenHeight)
