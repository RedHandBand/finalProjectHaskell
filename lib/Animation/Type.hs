module Animation.Type where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)

data Coordinate = Coord {x :: Int, y :: Int} deriving Eq

sumCoord :: Coordinate -> Coordinate -> Coordinate
sumCoord c1 c2 = Coord (x c1 + x c2) (y c1 + y c2)

data Result = HoleIn1 | Eagle | Birdie | Par | Bogey | DBogey

data GameState = Playing | Done Result -- should add |Menu |CourseSelect

type Animation course ballSt a = ReaderT course (StateT ballSt IO) a

runAnimation :: course -> ballSt -> Animation course ballSt a -> IO a
runAnimation course ballSt action = evalStateT (runReaderT action course) ballSt
