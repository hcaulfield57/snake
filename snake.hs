module Main where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Foreign.C.Types (CFloat(..))
import Graphics.UI.GLUT
import System.Exit
import System.Random

data Direction = UP | DOWN | LEFT | RIGHT
    deriving Eq

newtype Point = Pt { getPt :: (GLfloat,GLfloat) }
data Rectangle = Rect { minPt, maxPt :: Point }
type Food = (Rectangle,StdGen)
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
    _ <- getArgsAndInitialize
    _ <- createWindow "Snake"
    snakeRef <- newIORef [(Rect(Pt(0,0)) (Pt(snakeSize,snakeSize)))]
    dirRef <- newIORef RIGHT
    foodRef <- newIORef ((Rect(Pt(0.0,0.5)) (Pt(0+foodSize,0.5+foodSize))),mkStdGen 0)
    windowSize $= Size 300 300
    idleCallback $= Just (snakeLoop (snakeRef,dirRef,foodRef))
    keyboardMouseCallback $= Just (handleKeys dirRef)
    mainLoop

snakeLoop :: SnakeInfo -> DisplayCallback
snakeLoop (snakeRef,dirRef,foodRef) = do
    snake <- readIORef snakeRef
    dir <- readIORef dirRef
    food <- readIORef foodRef
    clearColor $= Color4 1 1 1 0
    clear [ColorBuffer]
    draw foodColor (fst food)
    mapM_ (draw snakeColor) snake
    flush
    checkCollision (snakeRef,dirRef,foodRef)
    let newHead = moveHead dir (head snake)
    writeIORef snakeRef (moveSnake newHead snake)
    threadDelay 250000

moveHead :: Direction -> Rectangle -> Rectangle
moveHead dir r = 
    let (minx,miny) = getPt . minPt $ r
        (maxx,maxy) = getPt . maxPt $ r
    in case dir of
        LEFT -> Rect(Pt(minx-snakeSize,miny)) (Pt(maxx-snakeSize,maxy))
        UP -> Rect(Pt(minx,miny+snakeSize)) (Pt(maxx,maxy+snakeSize))
        DOWN -> Rect(Pt(minx,miny-snakeSize)) (Pt(maxx,maxy-snakeSize))
        RIGHT -> Rect(Pt(minx+snakeSize,miny)) (Pt(maxx+snakeSize,maxy))

moveSnake :: Rectangle -> Snake -> Snake
moveSnake _ [] = []
moveSnake r (s:ss) = r : moveSnake s ss

draw :: Color4 GLfloat -> Rectangle -> IO ()
draw c r = do
    let (minx,miny) = getPt . minPt $ r
        (maxx,maxy) = getPt . maxPt $ r
    renderPrimitive Quads $ do
        color c
        vertex $ Vertex2 minx miny
        vertex $ Vertex2 maxx miny
        vertex $ Vertex2 maxx maxy
        vertex $ Vertex2 minx maxy

handleKeys :: IORef Direction -> KeyboardMouseCallback
handleKeys keyRef (Char k) Down _ _ = unless
    (k `notElem` "hjkl") $ 
        let newDir = case k of
             'h' -> LEFT
             'j' -> DOWN
             'k' -> UP
             'l' -> RIGHT
        in writeIORef keyRef newDir
handleKeys _ (Char 'q') _ _ _ = exitWith ExitSuccess
handleKeys _ _ _ _ _ = return ()

spawnFood :: Food -> Food
spawnFood (r,gen) = 
    let (pt,gen') = random gen :: (Integer,StdGen)
        loc = CFloat (fromIntegral (pt `mod` 1)) :: GLfloat
    in ((Rect(Pt(loc,loc)) (Pt(loc+foodSize,loc+foodSize))),gen')

checkCollision :: SnakeInfo -> IO ()
checkCollision (snakeRef,dirRef,foodRef) = do
    snake <- readIORef snakeRef
    dir <- readIORef dirRef
    food <- readIORef foodRef
    let (sminx,_) = getPt . minPt $ head snake
        (_,smaxy) = getPt . maxPt $ head snake
    when ((fst food) `inRect` (head snake)) $ do
        addSnake snakeRef dirRef
        writeIORef foodRef (spawnFood food)
    if sminx < (-1) || sminx > 1 || smaxy < -1 || smaxy > 1
        then exitWith ExitSuccess
        else return ()

inRect :: Rectangle -> Rectangle -> Bool
inRect r1 r2 = 
    let (minx1,_) = getPt . minPt $ r1
        (_,maxy1) = getPt . minPt $ r1
        (minx2,_) = getPt . minPt $ r2
        (_,maxy2) = getPt . minPt $ r2
    in if minx1 <= minx2 && maxy1 <= maxy2
       then True 
       else False

addSnake :: IORef Snake -> IORef Direction -> IO ()
addSnake snakeRef dirRef = do
    snake <- readIORef snakeRef
    dir <- readIORef dirRef
    let (minx,miny) = getPt . minPt $ head snake
        (maxx,maxy) = getPt . maxPt $ head snake
        newSnake = case dir of
         LEFT -> (Rect(Pt(minx,miny-snakeSize)) (Pt(maxx,maxy-snakeSize))) : snake
         UP -> (Rect(Pt(minx+snakeSize,miny)) (Pt(maxx+snakeSize,maxy))) : snake
         DOWN -> (Rect(Pt(minx-snakeSize,miny)) (Pt(maxx-snakeSize,maxy))) : snake
         RIGHT -> (Rect(Pt(minx,miny-snakeSize)) (Pt(maxx,maxy-snakeSize))) : snake
    writeIORef snakeRef newSnake
