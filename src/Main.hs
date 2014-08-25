{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Applicative((<$>))
import Control.Monad.Random
import qualified Graphics.UI.GLFW as GLFW

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
import Vacivity.Proc.Dungeon
instance WindowSettings where
    width = (64+48)*16
    height = 64*16
    title = "Vacivity"


data Player = Player XY

class Renderable a where
    render :: a -> TR XY (Tile CP437) -> TR XY (Tile CP437)

instance Renderable Player where
    render (Player pos) tr = tr <+ (pos, Tile (:$) black white)

data GameState = GameState Player (A2D.Array2d Bool)

updatePlayer :: (Int,Int) -> Player -> Player
updatePlayer dd (Player pos) = Player (pos |+| dd)

instance Game GameState (ControlMap C.TriggerAggregate, Assets, Window) a where
    runFrame (GameState pl tr) (ctrl,_,_) rng =
        let u = (select 0 (-1) . C.isPressed . from ctrl) (Get :: Index 'CK'Up) in
        let d = (select u   1  . C.isPressed . from ctrl) (Get :: Index 'CK'Down) in
        let l = (select 0 (-1) . C.isPressed . from ctrl) (Get :: Index 'CK'Left) in
        let r = (select l   1  . C.isPressed . from ctrl) (Get :: Index 'CK'Right) in
        let pl' = updatePlayer (r, d) pl in
        (GameState pl' tr, rng)

instance Drawable GameState where
    draw (GameState pl ter) tex = do
        let ts = R.Tileset 16 16 16 16
        let ren = R.Renderer tex ts
        let ter' = select (Tile (:#) black white) (Tile (:!) black white) <$> ter
        let tr = render pl $ empty <+ ((0,0), Tile (:$) black white)
        R.render ren (A2D.foldl (<+) tr ter')

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
    let ter = evalRand (create (0,0,64 + 48,64) 4) rng
    let state = GameState (Player (10,10)) ter

    gs <- mkUpdater state (ctrl, assets, win) rng
    loop ctrl win gs tex rng

main :: IO ()
main = do
    runAudio enterLoop
