module Vacivity.FOV.ShadowCast where

import Prelude hiding (any, all, foldl)
import Data.Foldable
import qualified Data.Set as Set
import qualified Antiqua.Data.Array2d as A2D
import Antiqua.Common

type Mask = A2D.Array2d Bool

inRadius :: XY -> Int -> Bool
inRadius (x, y) r =
    x*x + y*y <= r*r

inRange :: XY -> (Int,Int,Int,Int) -> Bool
inRange (x, y) (rx, ry, rw, rh)
    | x >= rx && y >= ry && x < rx + rw && y < ry + rh = True
    | otherwise = False

isSolid :: Mask -> XY -> Bool
isSolid msk c = any not (A2D.get msk c)

calculate :: Mask -> XY -> Int -> Set.Set XY
calculate msk@(A2D.Array2d cols rows _) (sx, sy)  r =
    let size = (cols, rows) in
    let dirs = [ (i, j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0 ] in
    foldl (\lit (i, j) -> let l1 = castLight size 1 1.0 0.0 0 i j 0 msk lit in
                          let l2 = castLight size 1 1.0 0.0 i 0 0 j msk l1 in
                          l2
                      ) (Set.singleton (sx, sy)) dirs

 where castLight :: (Int,Int) -> Int -> Double -> Double -> Int -> Int -> Int -> Int -> Mask -> Set.Set XY -> Set.Set XY
       castLight size@(width,height) row start end xx xy yx yy mask l =
           if start < end
           then l
           else loop1 row start 0.0 False l
        where loop1 distance s ns blocked lit =
                  if distance > r || blocked
                  then lit
                  else let (s', ns', blocked', lit') = loop2 distance (-distance) (-distance) s ns blocked lit in
                       loop1 (distance + 1) s' ns' blocked' lit'
              loop2 d dy dx s ns blocked lit =
                  if dx > 0
                  then (s, ns, blocked, lit)
                  else let pos = (sx + dx * xx + dy * xy
                                 ,sy + dx * yx + dy * yy) in
                       let leftSlope = (fromIntegral dx - 0.5) / (fromIntegral dy + 0.5) in
                       let rightSlope = (fromIntegral dx + 0.5) / (fromIntegral dy - 0.5) in
                       if not (inRange pos (0, 0, width, height)) || s < rightSlope
                       then loop2 d dy (dx + 1) s ns blocked lit
                       else if end > leftSlope
                       then (s, ns, blocked, lit)
                       else let lit' = if inRadius (dx, dy) r
                                       then Set.insert pos lit
                                       else lit
                            in
                            if blocked
                            then if isSolid mask pos
                            then loop2 d dy (dx + 1) s rightSlope blocked lit'
                                   else loop2 d dy (dx + 1) ns ns False lit'
                              else if isSolid mask pos && d < r
                                   then let lit'' = castLight size (d + 1) s leftSlope xx xy yx yy mask lit' in
                                        loop2 d dy (dx + 1) s rightSlope True lit''
                                   else loop2 d dy (dx + 1) s ns blocked lit'


