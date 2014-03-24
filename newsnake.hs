module Main where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Graphics.UI.GLUT

data Direction = UP | DOWN | LEFT | RIGHT
    deriving Eq

newtype Point = Pt { getPt :: (GLfloat,GLfloat) }
data Rectangle = Rect { minPt, maxPt :: Point }
type Food = Rectangle
type Snake = [Rectangle] 

type SnakeInfo = (IORef Snake, IORef Direction, IORef Food)

snakeColor :: Color4 GLfloat
snakeColor = Color4 0.506 0.928 0.4 0

foodColor :: Color4 GLfloat
foodColor = Color4 0.29 0.94 0.75 0

snakeSize :: GLfloat
snakeSize = 0.09

foodSize :: GLfloat
foodSize = 0.05

main :: IO ()
main = do
    getArgsAndInitialize
    win <- createWindow "Snake"
    snakeRef <- newIORef [(Rect (Pt(0,0)) (Pt(snakeSize,snakeSize)))]
    dirRef <- newIORef RIGHT
    foodRef <- newIORef spawnFood
    windowSize $= Size 300 300
    idleCallback $= Just (snakeLoop (snakeRef,dirRef,foodRef))
    keyboardMouseCallback $= Just (handleKeys dirRef)
    mainLoop

snakeLoop :: SnakeInfo -> DisplayCallback
snakeLoop (snakeRef,dirRef,foodRef) = do
    snake <- readIORef snakeRef
    dir <- readIORef dirRef
    clearColor $= Color4 1 1 1 0
    clear [ColorBuffer]
    mapM_ draw snake
    flush
    let newHead = moveHead dir (head snake)
    writeIORef snakeRef (moveSnake newHead snake)
    threadDelay 250000

moveHead :: Direction -> Rectangle -> Rectangle
moveHead LEFT (Rect(Pt(minx,miny)) (Pt(maxx,maxy))) = 
    Rect(Pt(minx-snakeSize,miny) Pt(maxx-snakeSize,maxy))
moveHead UP (Rect(Pt(minx,miny) Pt(maxx,maxy))) = 
    Rect(Pt(minx,miny+snakeSize) Pt(maxx,maxy+snakeSize))
moveHead DOWN (Rect(Pt(minx,miny) Pt(maxx,maxy))) = 
    Rect (Pt(minx,miny-snakeSize) Pt(maxx,maxy-snakeSize))
moveHead RIGHT (Rect(Pt(minx,miny) Pt(maxx,maxy))) = 
    Rect (Pt(minx+snakeSize,miny) Pt(maxx+snakeSize,maxy))

moveSnake :: Rectangle -> Snake -> Snake
moveSnake _ [] = []
moveSnake r (s:ss) = r : moveSnake s ss

draw :: Rectangle -> Color4 GLfloat -> IO ()
draw r c = do
    let (minx,miny) = getPt . minPt r
        (maxx,maxy) = getPt . maxPt r
    renderPrimitive Quads $ do
        color c
        vertex $ Vertex2 minx miny
        vertex $ Vertex2 maxx miny
        vertex $ Vertex2 maxx maxy
        vertex $ Vertex2 minx maxy

handleKeys :: IORef Direction -> KeyboardMouseCallback
handleKeys keyRef (Char k) Down _ _ = unless
    (k `notElem` "hjkl" $ 
        let newDir = case k of
            'h' -> LEFT
            'j' -> DOWN
            'k' -> UP
            'l' -> RIGHT
        in writeIORef keyRef newDir
handleKeys _ _ _ _ _ = return ()
