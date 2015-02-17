module GLHelper where

import Graphics.Rendering.OpenGL

black, white, blue, red, green :: Color4 GLfloat
black = Color4 0 0 0 1
white = Color4 1 1 1 1
blue = Color4 0 0 1 1
red = Color4 1 0 0 1
green = Color4 0 1 0 1

displayCross :: IO ()
displayCross =
  preservingMatrix $ do
    lineWidth $= 0.8
    let l = 10000
    renderPrimitive Lines $ do
      vertex (Vertex3 (-l) 0 0 :: Vertex3 GLfloat)
      vertex (Vertex3 l 0 0    :: Vertex3 GLfloat)
    renderPrimitive Lines $ do
      vertex (Vertex3 0 (-l) 0 :: Vertex3 GLfloat)
      vertex (Vertex3 0 l 0    :: Vertex3 GLfloat)
    renderPrimitive Lines $ do
      vertex (Vertex3 0 0 (-l) :: Vertex3 GLfloat)
      vertex (Vertex3 0 0 l    :: Vertex3 GLfloat)
