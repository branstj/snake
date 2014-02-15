module Config
    ( inWindowSettings
    , fullScreenSettings
    ) where

import Graphics.Gloss

-- Display Settings --
screenWidth, screenHeight :: Int
screenWidth = 500
screenHeight = 500

-- These are only needed if the game is played in a window
screenPosH, screenPosW :: Int
screenPosW = 0
screenPosH = 0

inWindowSettings :: Display
inWindowSettings = InWindow "Snake" (screenWidth, screenHeight) (screenPosW, screenPosH)

fullScreenSettings :: Display
fullScreenSettings = FullScreen (screenWidth, screenHeight)
