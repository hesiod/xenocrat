{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds #-}

module GL where

import Control.Monad
import Data.Map ((!))
import Linear
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Data.VectorSpace
import Graphics.Rendering.OpenGL
import Foreign.Storable
import qualified Graphics.UI.GLFW as GLFW

import Common

import RenderGL
import GLHelper
import Simulation

--instance AsUniform Int
--instance Uniform Int
--instance AsUniform a => AsUniform (V3 a)

displayState :: forall a. (AsUniform (V3 a), AsUniform (M44 a), Conjugate a, Epsilon a, Storable a, AsUniform a, RealFloat a, VectorSpace a, Fractional (Scalar a)) => GLState a -> GLFW.WindowRefreshCallback
displayState state w = forever $ do
  clear [ColorBuffer, DepthBuffer, StencilBuffer]

  camera <- get $ cam state
  scr <- get $ screen state
  s <- get $ bds state

  let sbv = msbv state
      (cS, cBs, cV) = sbv ! Cross
      (pS, pBs, pV) = sbv ! Planet
      (vS, vBs, vV) = sbv ! Vector
      cB = cBs ! ArrayBuffer
      pB = pBs ! ArrayBuffer
      vB = vBs ! ArrayBuffer

      zoom = recip 3e11 :: a
      arrowscale = 3 * recip 5 :: a
      tessI = 8*8 :: a
      tessO = 8*8 :: a
      noisiness = 0.2 :: a

      mkRot axis rad = m33_to_m44 . fromQuaternion $ axisAngle axis rad
      arrowrot = mkRot (V3 0 0 1) (deg2rad 50) :: M44 a
      scaleM = m33_to_m44 (identity !!* zoom)
      model = camMatrix camera
      view = Linear.lookAt (V3 0 0 0) (V3 0 0 (-1)) (V3 0 1 0)
      near = 0.0001
      far = 10
      fov = 0.8
      scr' = uncurry fromIntegral scr :: Screen a
      ar = uncurry (/) scr'
      proj = Linear.perspective fov ar near far
      transform = proj !*! view !*! model :: M44 a

  currentProgram $= Just (program cS)
  setUniform cS "transform" transform
  bindBuffer ArrayBuffer $= Just cB
  withVAO cV $ drawArrays Points 0 2

  currentProgram $= Just (program pS)
  setUniform pS "transform" transform
  setUniform pS "tess_inner" tessI
  setUniform pS "tess_outer" tessO
  setUniform pS "noisiness" noisiness
  setUniform pS "origin" $ V3 1 1 (1::a)
  let vertsI = fmap realToFrac <$> icosahedronTriangles :: [V3 a]
  bindBuffer ArrayBuffer $= Just pB
  replaceBuffer ArrayBuffer vertsI
  withVAO pV $ drawArrays Patches 0 (fromIntegral $ length vertsI)

  currentProgram $= Just (program vS)
  setUniform vS "transform" $ transform !*! scaleM
  setUniform vS "arrowrot" arrowrot
  setUniform vS "arrowscale" arrowscale
  let verts = fmap realToFrac <$> concatMap bodyVertices s :: [V3 a]
  bindBuffer ArrayBuffer $= Just vB
  replaceBuffer ArrayBuffer verts
  withVAO vV $ drawArrays Lines 0 (fromIntegral $ length verts)

  throwError

  GLFW.swapBuffers w
  GLFW.waitEvents
