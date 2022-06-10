module Animation.State where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

import Animation.Env (CourseMap(..))
import Animation.Type (Animation, Coordinate(..), GameState(..), Result(..), sumCoord)

import System.IO ( hFlush, stdout ) 

data BallState = BallSt {
                        position :: Coordinate, 
                        speed :: Int, 
                        angle :: Angle, 
                        hits :: Int,
                        status :: GameState
                        }

data Angle = N | NE | E | SE | S | SW | W | NW


flush :: IO ()
flush = hFlush stdout

getStroke :: Animation CourseMap BallState ()
getStroke = do
   lift $ lift $ putStr "Shot strength (1-50): "
   lift $ lift flush
   speed <- lift $ lift readLn --Change to getLine and add a Function that reads incorrect input as a default speed (or ask again maybe)
   --speed <- getSpeed -- lift $ lift getLine --From the print above to here should be getSpeed
   --if speed `elem` [1..50] then .... else getSpeed
   lift $ lift $ putStr "Angle (N|NE|E|SE|S|SW|W|NW): "
   lift $ lift flush
   strAngle <- lift $ lift getLine
   prevBall <- lift get
   lift $ put $ prevBall {angle = readAngle strAngle, speed = speed, hits = hits prevBall + 1} 

readAngle :: String -> Angle
readAngle strAng
            |strAng == "N"  = N
            |strAng == "NE" = NE
            |strAng == "NW" = NW
            |strAng == "S"  = S
            |strAng == "SE" = SE
            |strAng == "SW" = SW
            |strAng == "E"  = E
            |strAng == "W"  = W
            |otherwise      = S

getSpeedVector :: Angle -> Coordinate
getSpeedVector S = Coord 0 1
getSpeedVector SE = Coord 1 1
getSpeedVector E = Coord 1 0
getSpeedVector NE = Coord 1 (-1)
getSpeedVector N = Coord 0 (-1)
getSpeedVector NW = Coord (-1) (-1)
getSpeedVector W = Coord (-1) 0
getSpeedVector SW = Coord (-1) 1


getXSpeed :: BallState -> Int
getXSpeed (BallSt _ spd ang _ _) = (x (getSpeedVector ang)) * spd

getYSpeed :: BallState -> Int
getYSpeed (BallSt _ spd ang _ _) = (y (getSpeedVector ang)) * spd

--Default BallState, no speed (should be part of the course)
defaultSt :: BallState
defaultSt = BallSt (Coord 1 1) 0 N 0 Playing

getmXCoord :: Maybe Coordinate -> Int
getmXCoord x = case x of 
            Nothing -> 9999
            Just (Coord x y) -> x

getmYCoord :: Maybe Coordinate -> Int
getmYCoord y = case y of 
            Nothing -> 9999
            Just (Coord x y) -> y

findXbyY :: Int -> Coordinate -> Int
findXbyY y (Coord cX cY) = if cY == y then cX else 9999

next :: Animation CourseMap BallState ()
next = do
    course <- ask
    prevBallSt <- lift get
    lift (put (nextInternal course prevBallSt))

--(Course size [obstacles] (Coord xHole yHole) parNumber)
nextInternal :: CourseMap -> BallState -> BallState
nextInternal course prevBallSt = 
    BallSt {position = newPos, speed = newSpd, angle = angle prevBallSt, hits = hits prevBallSt, status = gameStatus}

    where
    
    validatePos :: Coordinate -> Coordinate -> Coordinate
    validatePos pCoord sVector
        | sumCoord pCoord sVector `elem` obstacles course = pCoord
        | x (sumCoord pCoord sVector) > courseSize course || y (sumCoord pCoord sVector) > courseSize course = pCoord
        | x (sumCoord pCoord sVector) < 0 || y (sumCoord pCoord sVector) < 0 = pCoord
        | otherwise = sumCoord pCoord sVector

    newPos :: Coordinate
    newPos = validatePos (position prevBallSt) speedVector
    
    gameStatus = gameSt (holePos course) newPos (hits prevBallSt) (parHits course) newSpd

    gameSt :: Coordinate -> Coordinate -> Int -> Int -> Int -> GameState
    gameSt holePos newPos hits parHits speed
        | newPos == holePos && speed <= 2 = Done (getResult hits parHits)
        | otherwise = Playing
    
    getResult :: Int -> Int -> Result
    getResult hits par 
        |hits == 1 = HoleIn1
        |hits == 2 = Eagle
        |par - hits == 1 = Birdie
        |par == hits = Par
        |hits - par == 1 = Bogey
        |otherwise = DBogey

    speedVector :: Coordinate
    speedVector = getSpeedVector (angle prevBallSt)

    prevX :: Int
    prevX = x (position prevBallSt)
    prevY :: Int
    prevY = y (position prevBallSt)
    newSpd :: Int
    newSpd = (speed prevBallSt) - 1

    --newAng :: Angle
    --newAng = undefined

    --newHits :: Int
    --newHits = (hits prevBallSt) + 1
    