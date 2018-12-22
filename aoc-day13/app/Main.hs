{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Arrow ((&&&))
import Control.Monad (when, foldM, forM)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError, lift)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, readArray, writeArray, getAssocs)
import Data.Array.ST (STArray)
import Data.List (groupBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max(..))
import qualified Data.Set as S
import Debug.Trace

type Coord = (Int, Int) -- Y, X because it sorts best
data Direction = North | West | South | East deriving (Show, Enum)
data Turn = GoStraight | TurnLeft | TurnRight deriving (Show, Enum)
data Cart = Cart {heading :: Direction, plan :: Turn} deriving Show
data Curve = SEWN | NEWS deriving (Show, Enum)
data Terrain = Empty | Straight | Intersection | Curve Curve deriving Show
data Tile = Tile Terrain (Maybe Cart) deriving Show
type Grid s = STArray s Coord Tile

type Input = [(Coord, Tile)]

parseTile :: Char -> Maybe Tile
parseTile c = case c of
  ' ' -> tile Empty
  '|' -> tile Straight
  '-' -> tile Straight
  '+' -> tile Intersection
  '\\' -> tile (Curve SEWN)
  '/' -> tile (Curve NEWS)
  'v' -> cart South
  '^' -> cart North
  '>' -> cart East
  '<' -> cart West
  _ -> Nothing
  where tile = Just . flip Tile Nothing
        cart dir = Just . Tile Straight . Just $ Cart dir TurnLeft

nextPlan :: Turn -> Turn
nextPlan t = case t of
  TurnLeft -> GoStraight
  GoStraight -> TurnRight
  TurnRight -> TurnLeft

turn :: Turn -> Direction -> Direction
turn GoStraight x = x
turn TurnLeft x = case x of
  North -> West
  West -> South
  South -> East
  East -> North
turn TurnRight x = case x of
  North -> East
  East -> South
  South -> West
  West -> North

translate :: Direction -> Coord -> Coord
translate d (y, x) = case d of
  North -> (y - 1, x)
  East -> (y, x + 1)
  South -> (y + 1, x)
  West -> (y, x - 1)

bounce :: Curve -> Direction -> Direction
bounce SEWN d = case d of
  South -> East
  East -> South
  West -> North
  North -> West
bounce NEWS d = case d of
  North -> East
  East -> North
  West -> South
  South -> West

showGrid :: [(Coord, Tile)] -> String
showGrid = unlines . map showLine . map (map snd) . groupBy ((==) `on` (fst . fst))
  where showLine :: [Tile] -> String
        showLine = map showTile
        showTile :: Tile -> Char
        showTile (Tile _ (Just c)) = case heading c of
          West -> '<'
          North -> '^'
          East -> '>'
          South -> 'v'
        showTile (Tile t Nothing) = case t of
          Curve NEWS -> '/'
          Curve SEWN -> '\\'
          Intersection -> '+'
          _ -> ' '

eraseCart :: Grid s -> Coord -> ST s ()
eraseCart g pos = do
  Tile t _ <- readArray g pos
  writeArray g pos $ Tile t Nothing

type CartM s a = ExceptT Coord (ST s) a

moveOneCart :: Grid s -> Coord -> CartM s Coord
moveOneCart g pos = do
  Tile t (Just c@(Cart h p)) <- lift $ readArray g pos
  lift $ eraseCart g pos
  let pos' = translate h pos
  Tile t' c' <- lift $ readArray g pos'
  case c' of
    Just _ -> throwError pos'
    Nothing -> do
      let newCart = case t' of
            Intersection -> Cart (turn p h) (nextPlan p)
            Curve curve -> Cart (bounce curve h) p
            _ -> c
      lift . writeArray g pos' $ Tile t' (Just newCart)
      pure pos'

type CrashHandler s = Grid s -> S.Set Coord -> Coord -> Coord -> CartM s (S.Set Coord)

part1 :: forall s. CrashHandler s
part1 _ _ _ crash = throwError crash

part2 :: forall s. CrashHandler s
part2 g carts cart crash = pure . S.difference carts . S.fromList $ [cart, crash]

runOneTick :: CrashHandler s -> Grid s -> S.Set Coord -> CartM s (S.Set Coord)
runOneTick handler g carts = do
  carts' <- foldM moveOne carts . S.toList $ carts
  case S.toList carts' of
    [x] -> throwError x
    remainining -> pure carts'

  where moveOne carts cart = if cart `S.notMember` carts
          then pure carts
          else (do cart' <- moveOneCart g cart
                   pure (S.insert cart' (S.delete cart carts)))
               `catchError` handler g carts cart

debug = False

runUntilCrash :: CrashHandler s -> Grid s -> S.Set Coord -> ST s Coord
runUntilCrash handler g = go
  where go carts = do
          when debug $ do
            assocs <- getAssocs g
            trace (showGrid assocs)$ pure ()
          result <- runExceptT . runOneTick handler g $ carts
          case result of
            Left answer -> pure answer
            Right carts' -> go carts'

parse :: String -> Maybe Input
parse = go (0, 0) . lines
  where go :: Coord -> [String] -> Maybe Input
        go (y, x) [] = Just []
        go (y, x) ([]:xs) = go (y + 1, 0) xs
        go (y, x) ((c:cs):more) = do
          tile <- parseTile c
          (((y, x), tile) :) <$> go (y, x + 1) (cs:more)


main :: IO ()
main = do
  tiles <- fmap (fromMaybe [] . parse) $ getContents
  let bounds = (maximum . map fst $ coords, maximum . map snd $ coords)
      coords = map fst tiles
      answer = runST $ do
        a <- newArray ((0, 0), bounds) $ Tile Empty Nothing
        mapM_ (uncurry $ writeArray a) tiles
        assocs <- getAssocs a
        let hasCarts = S.fromList [ix | (ix, Tile _ (Just _)) <- assocs]
        (y, x) <- runUntilCrash (\_ _ _ crash -> throwError crash) a hasCarts
        pure ((x, y) :: Coord)
  print answer
