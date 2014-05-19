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

newtype Point = Pt { getPt :: (GLfloat, GLfloat) }
    deriving Show -- DEBUG

data Rectangle = Rect { minPt, maxPt :: Point }
    deriving Show -- DEBUG

type Food = (Rectangle, StdGen)

type Snake = [Rectangle]

type GameInfo = (IORef Snake, IORef Direction, IORef Food)

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
    _        <- getArgsAndInitialize
    _        <- createWindow "Snake"
    snakeRef <- newIORef 
        [Rect (Pt (0, 0)) (Pt (snakeSize, snakeSize))]
    dirRef   <- newIORef RIGHT
    foodRef  <- newIORef
        (Rect (Pt (0.0, 0.5)) (Pt (foodSize, 0.5+foodSize)), mkStdGen 0)
    windowSize            $= Size 300 300
    displayCallback       $= initGame
    idleCallback          $= Just (gameLoop (snakeRef, dirRef, foodRef))
    keyboardMouseCallback $= Just (handleKeys dirRef)
    mainLoop

initGame :: DisplayCallback
initGame = do
    clearColor $= Color4 1 1 1 0
    clear [ColorBuffer]

gameLoop :: GameInfo -> DisplayCallback
gameLoop gameInfo@(snakeRef, dirRef, foodRef) = do
    snake <- readIORef snakeRef
    food  <- readIORef foodRef
    clearColor $= Color4 1 1 1 0
    clear [ColorBuffer]
    draw foodColor (fst food)
    mapM_ (draw snakeColor) snake
    -- DEBUG
    printDebug snake food
    -- END DEBUG
    flush
    checkCollision gameInfo
    threadDelay 250000

printDebug snake food = do
    putStrLn $ "Food = " ++ show food
    putStrLn $ "Snake = " ++ show snake

handleKeys :: IORef Direction -> KeyboardMouseCallback
handleKeys keyRef (Char k) Down _ _ =
    unless (k `notElem` "hjkl") $ 
        let newDir = case k of
                'h' -> LEFT
                'l' -> RIGHT
                'j' -> DOWN
                'k' -> UP
        in writeIORef keyRef newDir
handleKeys _ (Char 'q') _ _ _ = exitWith ExitSuccess
handleKeys _ _ _ _ _ = return ()

draw :: Color4 GLfloat -> Rectangle -> IO ()
draw col rect = do
    let (minx,miny) = getPt . minPt $ rect
        (maxx,maxy) = getPt . maxPt $ rect
    renderPrimitive Quads $ do
        color col
        vertex $ Vertex2 minx miny
        vertex $ Vertex2 maxx miny
        vertex $ Vertex2 maxx maxy
        vertex $ Vertex2 minx maxy

checkCollision :: GameInfo -> IO ()
checkCollision (snakeRef, dirRef, foodRef) = do
    snake <- readIORef snakeRef
    dir   <- readIORef dirRef
    food  <- readIORef foodRef
    -- ate food
    when (foodCollision (fst food) (head snake)) $ do
        -- DEBUG
        putStrLn $ "COLLISION AT: Food = " ++ show food ++ " Snake = " ++ show snake
        -- END DEBUG
        addSnake snakeRef dirRef
        spawnFood foodRef
    -- hit wall
    when (wallCollision (head snake)) $ do
        exitWith ExitSuccess
    -- hit self
    when (selfCollision (head snake) (tail snake)) $ do
        exitWith ExitSuccess
    let newHead = moveHead (head snake) dir
    writeIORef snakeRef (moveSnake newHead snake)

moveHead :: Rectangle -> Direction -> Rectangle
moveHead rect dir =
    let (minx,miny) = getPt . minPt $ rect
        (maxx,maxy) = getPt . maxPt $ rect
    in case dir of
        LEFT  -> Rect (Pt (minx-snakeSize,miny)) (Pt (maxx-snakeSize,maxy))
        RIGHT -> Rect (Pt (minx+snakeSize,miny)) (Pt (maxx+snakeSize,maxy))
        DOWN  -> Rect (Pt (minx,miny-snakeSize)) (Pt (maxx,maxy-snakeSize))
        UP    -> Rect (Pt (minx,miny+snakeSize)) (Pt (maxx,maxy+snakeSize))

moveSnake :: Rectangle -> Snake -> Snake
moveSnake _ [] = []
moveSnake rect (s:ss) = rect : moveSnake s ss

foodCollision :: Rectangle -> Rectangle -> Bool
foodCollision food rect = 
    let (minxf,_) = getPt . minPt $ food
        (_,maxyf) = getPt . maxPt $ food
        (minxs,_) = getPt . minPt $ rect
        (_,maxys) = getPt . maxPt $ rect
    in if minxf <= minxs && maxyf <= maxys
       then True
       else False

addSnake :: IORef Snake -> IORef Direction -> IO ()
addSnake snakeRef dirRef = do
    snake <- readIORef snakeRef
    dir   <- readIORef dirRef
    let (minx,miny) = getPt . minPt $ head snake
        (maxx,maxy) = getPt . maxPt $ head snake
        -- add new head to snake
        newSnake = case dir of
            LEFT  -> (Rect (Pt (minx-snakeSize,miny)) (Pt (maxx-snakeSize,maxy)))
                : snake
            RIGHT -> (Rect (Pt (minx+snakeSize,miny)) (Pt (maxx+snakeSize,maxy)))
                : snake
            UP    -> (Rect (Pt (minx,miny+snakeSize)) (Pt (maxx,maxy+snakeSize)))
                : snake
            DOWN  -> (Rect (Pt (minx,miny-snakeSize)) (Pt (minx,maxy-snakeSize)))
                : snake
    writeIORef snakeRef newSnake

spawnFood :: IORef Food -> IO ()
spawnFood foodRef = do
    (pt,gen) <- readIORef foodRef
    let (pt',gen') = random gen :: (Integer,StdGen)
        newPoint   = CFloat (fromIntegral (pt' `mod` 1)) :: GLfloat
    writeIORef foodRef (Rect (Pt (newPoint,newPoint)) (Pt (newPoint+foodSize
        , newPoint+foodSize)),gen')

wallCollision :: Rectangle -> Bool
wallCollision rect = 
    let (minx,_) = getPt . minPt $ rect
        (_,maxy) = getPt . maxPt $ rect
    in if minx < (-1) || minx > 1 || maxy < (-1) || maxy > 1
       then True
       else False

selfCollision :: Rectangle -> Snake -> Bool
selfCollision _ [] = False
selfCollision rect snake = 
    let (hminx,_) = getPt . minPt $ rect
        (_,hmaxy) = getPt . maxPt $ rect
        (tminx,_) = getPt . minPt $ (head snake)
        (_,tmaxy) = getPt . minPt $ (head snake)
    in if hminx == tminx && hmaxy == tmaxy
       then True
       else selfCollision rect (tail snake)
