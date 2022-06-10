module Main where

import Animation
    ( Animation
    , Angle(..)
    , CourseMap(..)
    , BallState(..)
    , Coordinate(..)
    , GameState(..)
    , Result(..)
    , hole1Course
    , defaultSt
    , next
    , draw
    , runAnimation
    , getSpeedVector
    , getStroke
    )

import Control.Concurrent (threadDelay)
--import System.Random (randomRIO)
--import System.Random (randomRIO)
import System.Exit (ExitCode(..), exitSuccess)

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

putInitialState :: Animation CourseMap BallState ()
putInitialState = do
    (Course _ _ _ _ ball) <- ask
    --posX <- lift $ lift $ randomRIO (0, size)
    --posY <- lift $ lift $ randomRIO (0, size)
    lift $ put $ defaultSt {position = ball}

animate :: Animation CourseMap BallState ()
animate = do
    draw
    (BallSt _ speed _ _ status) <- lift get
    case status of 
        Playing -> if speed <= 0 then getStroke else next
        Done _ -> lift $ lift exitSuccess
    lift $ lift $ threadDelay 500000
    animate

        

mainAnimation :: Animation CourseMap BallState ()
mainAnimation = do
    putInitialState
    animate

main :: IO ()
main = runAnimation hole1Course defaultSt mainAnimation
