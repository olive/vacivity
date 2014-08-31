module Vacivity.Proc.BSPDungeon where

import Prelude hiding (any, concat, foldl)
import Control.Applicative
import Control.Monad.Random hiding (split)
import Data.List hiding (concat, foldl, any)
import Data.Maybe
import Data.Foldable
import qualified Data.Set as Set
import qualified Antiqua.Data.Array2d as A2D
import Antiqua.Utils
import Antiqua.Data.Coordinate
import Antiqua.Common
import Vacivity.Data.Tile

data NonEmpty

type Rect = (Int,Int,Int,Int)
data Tree = Node Rect | Tree Orientation [Tree] deriving Show

data Orientation = Horizontal | Vertical deriving Show
data Line = Line (Int,Int) (Int,Int) deriving Show

split :: RandomGen g => Tree -> Rand g Tree
split (Tree o ts) = Tree o <$> (sequence $ split <$> ts)
split n@(Node (x, y, w, h)) = do
    b :: Bool <- getRandom
    p :: Double <- getRandomR (0.33, 0.66)
    let ratio :: Double = fromIntegral w / fromIntegral h
    let o = if (ratio > 2)
            then Vertical
            else if (ratio < 0.5)
            then Horizontal
            else select Vertical Horizontal b
    let go Vertical = let w1 = floor $ fromIntegral w * p in
                      let w2 =  (subtract 1) $ floor $ fromIntegral w * (1 - p) in
                      if w1 < 3 || w2 < 3
                      then return n
                      else return $ Tree Vertical [ Node (         x, y, w1, h)
                                                  , Node (x + w1 + 1, y, w2, h)
                                                  ]
        go Horizontal = let h1 = floor $ fromIntegral h * p in
                        let h2 = ((subtract 1) . floor) $ fromIntegral h * (1 - p) in
                        if h1 < 3 || h2 < 3
                        then return n
                        else return $ Tree Horizontal [ Node (x,          y, w, h1)
                                                      , Node (x, y + h1 + 1, w, h2)
                                                      ]
    if w < 10 || h < 10
    then return n
    else go o

needSplit :: Tree -> Bool
needSplit (Tree _ ts) = any id (needSplit <$> ts)
needSplit (Node (_, _, w, h)) =
    let ratio :: Double = fromIntegral w / fromIntegral h in
    (w > 10 && h > 10) && (ratio > 2 || ratio < 0.5)

create :: RandomGen g => Rect -> Int -> Rand g (Set.Set XY, A2D.Array2d TileType)
create r@(x, y, w, h) n = do
    let sr = (x+2, y+2, w-2, h-2)
    tree <- doSplit n (Node sr)
    shrunk <- shrink tree
    let ct = connectAll shrunk
    return $ toCollisions r shrunk ct
    where doSplit :: RandomGen g => Int -> Tree -> Rand g Tree
          doSplit i t = do
              if i < 0 && (not . needSplit) t
              then return t
              else do t' <- split t
                      doSplit (i - 1) t'

getRandomTo :: RandomGen g => Int -> Rand g Int
getRandomTo i = do
    if i <= 0
    then return 0
    else getRandomR (0, i)

splitNum :: RandomGen g => Int -> Rand g (Int,Int)
splitNum i = do
    first <- getRandomTo i
    return (first, i - first)

shrinkRect :: RandomGen g => Rect -> Rand g Rect
shrinkRect (x, y, w, h) = do
    sx <- getRandomTo (w - 10)
    sy <- getRandomTo (h - 10)
    (sx1, sx2) <- splitNum sx
    (sy1, sy2) <- splitNum sy
    let result = (x + sx1
                 ,y + sy1
                 ,w - (sx1 + sx2)
                 ,h - (sy1 + sy2))
    return $ result

shrink :: RandomGen g => Tree -> Rand g Tree
shrink (Node n) = Node <$> shrinkRect n
shrink (Tree o ts) = Tree o <$> (sequence $ shrink <$> ts)

top :: Rect -> Line
top (x, y, w, _) = Line (x, y) (x + w, y)

bottom :: Rect -> Line
bottom (x, y, w, h) = Line (x, y + h) (x + w, y + h)

left :: Rect -> Line
left (x, y, _, h) = Line (x, y) (x, y + h)

right :: Rect -> Line
right (x, y, w, h) = Line (x + w, y) (x + w, y + h)

nearest :: Orientation -> Rect -> Rect -> (Line, Line)
nearest Vertical r1 r2 = (right r1, left r2)
nearest Horizontal r1 r2 = (bottom r1, top r2)

midPoint :: (Int,Int) -> (Int,Int) -> (Int,Int)
midPoint (x1, y1) (x2, y2) = ((x1 + x2) `quot` 2, (y1 + y2) `quot` 2)

connect :: Orientation -> Line -> Line -> [Line]
connect o (Line p1 p2) (Line q1 q2) =
    let m1@(m1x, m1y) = midPoint p1 p2 in
    let m2@(m2x, m2y) = midPoint q1 q2 in
    let (m3x, m3y) = midPoint m1 m2 in
    case o of
        Horizontal -> if abs (m1y - m2y) <= 2
                      then [ Line (m3x,m1y) (m3x,m2y) ]
                      else [ Line (m1x,m1y) (m1x,m3y)
                           , Line (m1x,m3y) (m2x,m3y)
                           , Line (m2x,m3y) (m2x,m2y)
                           ]
        Vertical -> if abs (m1x - m2x) <= 2
                    then [ Line (m1x,m3y) (m2x,m3y) ]
                    else [ Line (m1x,m1y) (m3x,m1y)
                         , Line (m3x,m1y) (m3x,m2y)
                         , Line (m3x,m2y) (m2x,m2y)
                         ]

center :: Rect -> (Int,Int)
center (x, y, w, h) = (x + (w `quot` 2), y + (h `quot` 2))

dist :: Rect -> Rect -> Int
dist r1 r2 =
    let (m1x, m1y) = center r1 in
    let (m2x, m2y) = center r2 in
    let two = 2 :: Int in
    (m1x - m2x) ^ two + (m1y - m2y) ^ two

closest :: [Rect] -> [Rect] -> (Rect, Rect)
closest rs1 rs2 =
    let xs = [ (dist r1 r2, (r1, r2)) | r1 <- rs1, r2 <- rs2 ] in
    let (best:_) = sortBy (\(x, _) (y, _) -> x `compare` y) xs in
    snd best

connectTree :: Orientation -> Tree -> Tree -> [Line]
connectTree o t1 t2 =
    let n1 = assembleRects t1 in
    let n2 = assembleRects t2 in
    let (r1, r2) = closest n1 n2 in
    let (l1, l2) = nearest o r1 r2 in
    connect o l1 l2

connectAll :: Tree -> [Line]
connectAll t =
    connectHelper [] t
    where connectHelper :: [Line] -> Tree -> [Line]
          connectHelper acc (Tree o ((Node r1):(Node r2):[])) =
              let (l1, l2) = nearest o r1 r2 in
              connect o l1 l2 ++ acc
          connectHelper acc (Tree o ts@(t1:t2:[]) ) =
              let cs = connectTree o t1 t2 in
              let rs = connectHelper acc <$> ts in
              cs ++ concat rs
          connectHelper acc _ = acc

assembleRects :: Tree -> [Rect]
assembleRects t =
    helper [] t
    where helper acc (Node r) = r:acc
          helper acc (Tree _ ts) = concat $ helper acc <$> ts

rectToPts :: Rect -> [(Int,Int)]
rectToPts (x, y, w, h) =
    [ (i, j) | i <- [x..(x + w - 1)], j <- [y..(y + h - 1)] ]

lineToPts :: Line -> [(Int,Int)]
lineToPts (Line (x1, y1) (x2, y2)) =
    let x0 = min x1 x2 in
    let y0 = min y1 y2 in
    rectToPts (x0, y0, abs (x1 - x2) + 1, abs (y1 - y2) + 1)

contains :: Rect -> XY -> Bool
contains (x, y, w, h) (p, q)
    | p >= x && p < x + w && q >= y && q < y + h = True
    | otherwise = False

toCollisions :: Rect -> Tree -> [Line] -> (Set.Set XY, A2D.Array2d TileType)
toCollisions r@(x, y, w, h) t ls =
    let rects = assembleRects t in
    let pts = concat $ (lineToPts <$> ls) ++ (rectToPts <$> rects) in
    let base = A2D.tabulate (w - x) (h - y) (const True) in
    let result = foldl (A2D.putv False) base pts in
    let fs = [ (i, j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0 ] in
    let ns p = catMaybes $ (A2D.get result . (p |+|)) <$> fs in
    let f p b = let hasSolid = any not (ns p) in
                if | (not . contains r) p -> Solid
                   | not b -> Free
                   | hasSolid -> Wall
                   | otherwise -> Solid
    in
    (Set.fromList pts , f A2D.<$*> result)




