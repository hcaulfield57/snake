import Control.Concurrent
import Data.IORef
import Graphics.UI.GLUT

data Direction = UP | DOWN | LEFT | RIGHT
    deriving Eq
newtype Point = Pt { getPt :: (GLfloat,GLfloat) }
data Rectangle = Rect { minPt,maxPt :: Point }
newtype Snake = Snake { getSnake :: Rectangle }

snakeColor :: Color4 GLfloat
snakeColor = Color4 0.506 0.928 0.4 0

snakeSize :: GLfloat
snakeSize = 0.09

main :: IO ()
main = do
    getArgsAndInitialize
    win <- createWindow "Snake"
    snakeRef <- newIORef [Snake (Rect (Pt(0,0)) (Pt(snakeSize,snakeSize)))]
    keyRef <- newIORef RIGHT
    windowSize $= Size 300 300
    displayCallback $= setup
    idleCallback $= (Just (snake snakeRef keyRef))
    keyboardMouseCallback $= (Just (handleKeys keyRef))
    mainLoop

setup :: DisplayCallback
setup = do
    clearColor $= (Color4 1 1 1 0)
    clear [ColorBuffer]
    flush

snake :: IORef [Snake] -> IORef Direction -> DisplayCallback
snake snakeRef keyRef = do
    snakeHead <- readIORef snakeRef
    keyDir <- readIORef keyRef
    clearColor $= (Color4 1 1 1 0)
    clear [ColorBuffer]
    mapM_ (drawRect . getSnake) snakeHead
    flush
    writeIORef snakeRef (moveSnake 
        (changeDirection keyDir (head snakeHead)) snakeHead)
    threadDelay 250000

moveSnake :: Rectangle -> [Snake] -> [Snake]
moveSnake _ [] = []
moveSnake r (s:ss) = Snake r : moveSnake (getSnake s) ss

changeDirection :: Direction -> Snake -> Rectangle
changeDirection LEFT (Snake(Rect(Pt(minx,miny)) (Pt(maxx,maxy)))) =
    Rect (Pt(minx-snakeSize,miny)) (Pt(maxx-snakeSize,maxy))
changeDirection UP (Snake(Rect(Pt(minx,miny)) (Pt(maxx,maxy)))) =
    Rect (Pt(minx,miny+snakeSize)) (Pt(maxx,maxy+snakeSize))
changeDirection DOWN (Snake(Rect(Pt(minx,miny)) (Pt(maxx,maxy)))) =
    Rect (Pt(minx,miny-snakeSize)) (Pt(maxx,maxy-snakeSize))
changeDirection RIGHT (Snake(Rect(Pt(minx,miny)) (Pt(maxx,maxy)))) =
    Rect (Pt(minx+snakeSize,miny)) (Pt(maxx+snakeSize,maxy))

drawRect :: Rectangle -> IO ()
drawRect (Rect (Pt(minx,miny)) (Pt(maxx,maxy))) =
    renderPrimitive Quads $ do
        color $ snakeColor
        vertex $ Vertex2 minx miny
        vertex $ Vertex2 maxx miny
        vertex $ Vertex2 maxx maxy
        vertex $ Vertex2 minx maxy

-- Key -> KeyState -> Modifiers -> Position -> IO ()
handleKeys :: IORef Direction -> KeyboardMouseCallback
handleKeys keyRef (Char k) Down _ _ = do
    let newDir = case k of
            'h' -> LEFT
            'j' -> DOWN
            'k' -> UP
            'l' -> RIGHT
    writeIORef keyRef newDir
handleKeys _ _ _ _ _ = return ()
