module Vacivity.Proc.Dungeon where

import Control.Applicative
import Control.Monad
import Control.Monad.Random hiding (split)
import Data.List

import qualified Antiqua.Data.Array2d as A2D
import Antiqua.Utils


data NonEmpty

type Rect = (Int,Int,Int,Int)
data Tree = Node Rect | Tree Orientation [Tree] deriving Show
data Orientation = Horizontal | Vertical deriving Show
data Line = Line (Int,Int) (Int,Int) deriving Show

split :: RandomGen g => Tree -> Rand g Tree
split (Tree o ts) = do
    ts' <- sequence $ split <$> ts
    return $ Tree o ts'
split (Node (x, y, w, h)) = do
    b :: Bool <- getRandom
    p :: Double <- getRandomR (0.25, 0.75)
    let ratio :: Double = fromIntegral w / fromIntegral h
    let o = if (ratio > 3)
            then Vertical
            else if (ratio < 0.33)
            then Horizontal
            else select Vertical Horizontal b
    let go Vertical = let w1 = floor $ fromIntegral w * p in
                      let w2 =  (subtract 1) $ floor $ fromIntegral w * (1 - p) in
                      return $ Tree Vertical [ Node (x,      y, w1, h)
                                              , Node (x + w1 + 1, y, w2, h)
                                              ]
        go Horizontal = let h1 = floor $ fromIntegral h * p in
                        let h2 = (subtract 1) $ floor $ fromIntegral h * (1 - p) in
                        return $ Tree Horizontal [ Node (x,      y, w, h1)
                                                 , Node (x, y + h1 + 1, w, h2)
                                                 ]
    go o


create :: RandomGen g => Rect -> Int -> Rand g (A2D.Array2d Bool)
create r i = do
    tree <- foldM (\n _ -> split n) (Node r) [0..(i-1)]
    shrunk <- shrink tree
    let ct = connectAll shrunk
    return $ toCollisions r shrunk ct

shrinkRect :: RandomGen g => Rect -> Rand g Rect
shrinkRect (x, y, w, h) = do
    sx1 <- getRandomR (1, (w `quot` 4) - 1)
    sx2 <- getRandomR (1, (w `quot` 4) - 1)
    sy1 <- getRandomR (1, (h `quot` 4) - 1)
    sy2 <- getRandomR (1, (h `quot` 4) - 1)
    return $ (x + sx1, y + sy1, w - (sx1 + sx2), h - (sy1 + sy2))

shrink :: RandomGen g => Tree -> Rand g Tree
shrink (Node n) = do
    n' <- shrinkRect n
    return $ Node n'
shrink (Tree o ts) = do
    ts' <- sequence $ shrink <$> ts
    return $ Tree o ts'


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
        Horizontal ->
            [Line (m3x, m1y) (m3x, m2y)]
        Vertical ->
            [Line (m1x, m3y) (m2x, m3y)]

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

assembleRects :: Tree -> [Rect]
assembleRects t =
    helper [] t
    where helper acc (Node r) = r:acc
          helper acc (Tree _ ts) = concat $ helper acc <$> ts

rectToPts :: Rect -> [(Int,Int)]
rectToPts (x, y, w, h) =
    [ (i, j) | i <- [x..(x + w - 1)], j <- [y.. (y + h - 1)] ]

lineToPts :: Line -> [(Int,Int)]
lineToPts (Line (x1, y1) (x2, y2)) =
    rectToPts (x1, y1, x2 - x1 + 1, y2 - y1 + 1)

toCollisions :: Rect -> Tree -> [Line] -> A2D.Array2d Bool
toCollisions (x, y, w, h) t ls =
    let rects = assembleRects t in
    let pts = concat $ (lineToPts <$> ls) ++ (rectToPts <$> rects) in
    let base = A2D.tabulate (w - x) (h - y) (const True) in
    let result = foldl (\arr pt -> A2D.put arr pt False) base pts in
    result


