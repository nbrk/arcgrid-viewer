module GLHelpers where

import Graphics.UI.GLUT
import Control.Lens
import Control.Monad


-- | Construct and draw a 3-vertex from a triple of Float
vertex3f :: (Float, Float, Float) -> IO ()
vertex3f (x, y, z) = do
--  putStrLn $ "vertex3f: " ++ show (x,y,z)
  vertex $ Vertex3 x y (z :: GLfloat)

vertex3f' :: Float -> Float -> Float -> IO ()
vertex3f' x y z = vertex3f (x, y, z)

-- | Construct and draw a 2-vertex from a pair of Float
vertex2f :: (Float, Float) -> IO ()
vertex2f (x, y) = do
--  putStrLn $ "vertex2f: " ++ show (x,y)
  vertex $ Vertex2 x (y :: GLfloat)

-- | Construct and set the RGBA color
color4f :: (Float, Float, Float, Float) -> IO ()
color4f (r, g, b, a) = color $ Color4 r g b (a :: GLfloat)

color4f' :: Float -> Float -> Float -> Float -> IO ()
color4f' r g b a = color4f (r,g,b,a)


-- | Render a cuboid with a x-y base, extended in z-axe
renderCuboid :: Float -> Float -> IO ()
renderCuboid b z =
  preservingMatrix $ do
    renderPrimitive Polygon $ do
      vertex3f' 0 0 0
      vertex3f' 0 b 0
      vertex3f' b b 0
      vertex3f' b 0 0

      vertex3f' 0 0 0

      vertex3f' 0 0 z
      vertex3f' 0 b z
      vertex3f' b b z
      vertex3f' b 0 z

      vertex3f' 0 0 z



renderCuboid' :: Float -> Float  -> IO ()
renderCuboid' w z =
  preservingMatrix $ do
  renderPrimitive Quads $ do
    vertex $ Vertex3 w w z
    vertex $ Vertex3 w w 0
    vertex $ Vertex3 w 0 0
    vertex $ Vertex3 w 0 z
    vertex $ Vertex3 w w z
    vertex $ Vertex3 w w 0
    vertex $ Vertex3 0 w 0
    vertex $ Vertex3 0 w z
    vertex $ Vertex3 w w z
    vertex $ Vertex3 w 0 z
    vertex $ Vertex3 0 0 z
    vertex $ Vertex3 0 w z
    vertex $ Vertex3 0 w z
    vertex $ Vertex3 0 w 0
    vertex $ Vertex3 0 0 (0 :: Float)
    vertex $ Vertex3 0 0 z
    vertex $ Vertex3 w 0 z
    vertex $ Vertex3 w 0 0
    vertex $ Vertex3 0 0 (0 :: Float)
    vertex $ Vertex3 0 0 z
    vertex $ Vertex3 w w 0
    vertex $ Vertex3 w 0 0
    vertex $ Vertex3 0 0 (0 :: Float)
    vertex $ Vertex3 0 w 0


renderCube :: Float  -> IO ()
renderCube w = renderCuboid w w
