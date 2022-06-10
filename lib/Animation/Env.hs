module Animation.Env where

import Animation.Type (Coordinate(..))
import Control.Monad.Trans.Reader (ask)

data CourseMap = Course {
                        courseSize :: Int, 
                        obstacles :: [Coordinate], 
                        holePos :: Coordinate, 
                        parHits :: Int, 
                        ballIniPos :: Coordinate
                        }

hole1Course :: CourseMap
hole1Course = Course { 
                     courseSize = 30,
                     obstacles = [Coord 2 4, Coord 6 8, Coord 3 6, Coord 1 25, Coord 1 8, Coord 4 5,
                     Coord 2 15, Coord 14 25, Coord 4 16, Coord 11 20, Coord 13 17, Coord 9 28, Coord 16 8, Coord 15 10, Coord 22 17],
                     holePos = Coord 18 25,
                     parHits = 5,
                     ballIniPos = Coord 8 3
                     }

