{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Prelude hiding (any, all)
import Control.Applicative((<$>))
import Control.Monad.Random hiding (fromList)
import qualified Data.Array.ST as Array
import qualified Data.Array as Array
import qualified Graphics.UI.GLFW as GLFW
import Data.Foldable

import Antiqua.Game
import Antiqua.Common
import Antiqua.Utils
import Antiqua.Sound.Audio
import Antiqua.Data.CP437
import Antiqua.Data.Coordinate
import qualified Antiqua.Data.Array2d as A2D
import Antiqua.Graphics.Window
import Antiqua.Graphics.Assets
import qualified Antiqua.Graphics.Renderer as R
import Antiqua.Graphics.TileRenderer
import Antiqua.Graphics.Colors
import Antiqua.Graphics.Tile
import qualified Antiqua.Input.Controls as C

import Vacivity.Input.ControlMap
import Vacivity.Proc.BSPDungeon
import Vacivity.Data.Tile
import Vacivity.World

instance WindowSettings where
    width = 48*16
    height = 48*16
    title = "Vacivity"

data Player = Player XY

getPos :: Player -> XY
getPos (Player p) = p

class Renderable a where
    render :: a -> (XY, (Tile CP437))

instance Renderable Player where
    render pl = (getPos pl, Tile (:$) black white)

data GameState = GameState Player Dungeon R.Tileset

updatePlayer :: (Int,Int) -> Dungeon -> Player -> Player
updatePlayer dd (Dungeon arr _) (Player pos) =
    let newPos = pos |+| dd in
    Player $ if (any (not . isFree) . A2D.get arr) newPos
             then pos
             else newPos

instance Game GameState (ControlMap C.TriggerAggregate, Assets, Window) a where
    runFrame (GameState pl tr ts) (ctrl,_,_) rng =
        let z = C.zips 3 3 in
        let u = (select 0 (-1) . z . from ctrl) (Get :: Index 'CK'Up) in
        let d = (select u   1  . z . from ctrl) (Get :: Index 'CK'Down) in
        let l = (select 0 (-1) . z . from ctrl) (Get :: Index 'CK'Left) in
        let r = (select l   1  . z . from ctrl) (Get :: Index 'CK'Right) in
        let pl' = updatePlayer (r, d) tr pl in
        (GameState pl' tr ts, rng)

tr = empty (48, 48) (Tile C'Space black black)
instance Drawable GameState where
    draw (GameState pl (Dungeon ter mask) ts) tex = do
        let ren = R.Renderer tex ts
        let tf (DTile _ Free)  = Tile (:.) black white
            tf (DTile _ Wall)  = Tile (:#) black white
            tf (DTile _ Solid) = Tile C'Space black white
            tf (DTile _ Stair) = Tile C'X black red
            tf (DTile _ Spawn) = Tile C'S black yellow
        let ter' = tf <$> ter
        let mask' = fov (getPos pl) 15 mask
        let d t = mapFg (dim 2) t
        let getmsk pt = any id $ A2D.get mask' pt
        --let e = empty ((48+64),64) (Tile C'Space black black)
        let choose (pt, t) = (,) pt $ if getmsk pt
                                      then t
                                      else d t
        let lst = A2D.foldr ((:) . choose) [] ter' ++ [render pl]
        let rec [] e = snd <$> e
            rec (x:xs) e = do
              e' <- e
              rec xs $ (e' <+ x)
        let arrp = Array.runSTArray (rec lst tr)
        let doren z = R.render ren z
        doren $ Array.assocs arrp

enterLoop :: WindowSettings => IO ()
enterLoop = do
    win <- createWindow
    let mk t = C.mkTriggerAggregate [t]
    let ctrl = ControlMap (mk $ C.KeyTrigger GLFW.Key'Up,
                           mk $ C.KeyTrigger GLFW.Key'Down,
                           mk $ C.KeyTrigger GLFW.Key'Left,
                           mk $ C.KeyTrigger GLFW.Key'Right)
    tex <- loadTexture "16x16.png"
    let assets = undefined :: Assets
    let rng = mkStdGen 2
    let ter = create (0,0,width `div` 16, height `div` 16) 4
    let (spawn, dun) = evalRand (mkDungeon ter) rng
    let ts = R.Tileset 16 16 16 16
    let state = GameState (Player spawn) dun ts

    gs <- mkUpdater state (ctrl, assets, win) rng
    loop ctrl win gs tex rng

main :: IO ()
main = do
    runAudio enterLoop
