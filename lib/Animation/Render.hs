module Animation.Render where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

import Animation.State (BallState(..), getmXCoord, getmYCoord, findXbyY )
import Animation.Env (CourseMap(..))
import Animation.Type (Animation, Coordinate(..), GameState(..), Result(..))

--Main drawing function
draw :: Animation CourseMap BallState ()
draw = do
    scene <- drawAll
    lift (lift (putStrLn scene))

-- Gets all the data packed into a string so we can print it
drawAll :: Animation CourseMap BallState String
drawAll = do
    course <- ask
    ballSt <- lift get
    return (drawInternal course ballSt)

--This will contain the string to be printed
drawInternal :: CourseMap -> BallState -> String
drawInternal course ballSt = drawCourse (courseSize course) (position ballSt) (obstacles course) (holePos course) (hits ballSt) (parHits course) (speed ballSt) (status ballSt)

--Creates the string from the individual lines
drawCourse :: Int -> Coordinate -> [Coordinate] -> Coordinate -> Int -> Int -> Int -> GameState -> String
drawCourse size (Coord ballX ballY) courseObstacles (Coord holeX holeY) hits parHits speed status =
    case status of
        Playing         ->
                        unlines
                            (["           CLI Golf Course"] ++ [" "] ++ ["Course 1 - Par " ++ show parHits]
                            ++[drawLine '-' '-' size Nothing [] 9999]        --Top line
                            ++ mappedPositions                                --uses lineMaker to make the rest
                            ++ [drawLine '-' '-' size Nothing [] 9999]      --Bottom line
                            ++ ["Hits: " ++ show hits ++ "/" ++ show parHits])
                        
                        where   positions = [0..size]            
                                mappedPositions = map lineMaker positions
                                
                                lineMaker y 
                                    |(y == ballY) && (y == holeY) = drawLine '|' ' ' size (Just (Coord ballX ballY)) (map (findXbyY y) courseObstacles) holeX
                                    |y == ballY = drawLine '|' ' ' size (Just (Coord ballX ballY)) (map (findXbyY y) courseObstacles) 9999
                                    |y == holeY = drawLine '|' ' ' size Nothing (map (findXbyY y) courseObstacles) holeX
                                    |otherwise  = drawLine '|' ' ' size Nothing (map (findXbyY y) courseObstacles) 9999
        
        Done HoleIn1    -> holeInOneScreen
        
        Done Eagle      -> holeInOneScreen
        
        Done Birdie     -> holeInOneScreen
        
        Done Par        -> holeInOneScreen
        
        Done Bogey      -> holeInOneScreen
        
        Done DBogey     -> holeInOneScreen


drawLine    :: Char             --End character for the lateral borders
            -> Char             --Inner, empty character
            -> Int              --Size of the course
            -> Maybe Coordinate --Maybe coords for the ball
            -> [Int]            --List of x positions of objects in that line
            -> Int              --X coordinate of the hole  
            -> String           --returns the string to be printed

drawLine endChar innerChar size mBall obs mHole =
    let xPositions = [0..size]
        lineMakerX x
            | x == getmXCoord mBall = 'o'
            | x == mHole = 'P'
            | x `elem` obs = '*'
            | otherwise = innerChar

    in [endChar] ++ map lineMakerX xPositions ++ [endChar]


holeInOneScreen :: String
holeInOneScreen = 
        "='\\'                   .  .                        |>>>"
    ++  "\n   '\\'              .         ' .                   |"
    ++  "\n  O--         .                 'o                |"
    ++  "\n   '\\'       .                                      |"
    ++  "\n   /'\\'    .                                        |"
    ++  "\n  / '/'  .'                                         |"
    ++  "Hole in one!! Masterfully played"

birdieScreen :: String
birdieScreen = ""
{-  "                 __"           ++
    "\n              /'{> "         ++
    "\n          ____) (____"       ++
    "\n        //'--;   ;--'\\"     ++
    "\n       ///////\_/\\\\\\\\"   ++
    "\n              m m"           ++
    "\n That's a birdie!! Well done"
-}

parScreen :: String
parScreen = ""

eagleScreen :: String
eagleScreen = ""
{-    "
     _\|      __     |/_
   _-  \_   _/"->   _/  -_
   -_    `-'(   )`-'    _-
    `=.__.=-(   )-=.__.='
            |/-\|
            Y   Y
    "
-}
bogeyScreen :: String
bogeyScreen = ""

dBogeyScreen :: String
dBogeyScreen = ""
