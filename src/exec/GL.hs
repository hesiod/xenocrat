{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds #-}

module GL where

import Control.Concatenative
import Control.DeepSeq
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Bool
import Data.VectorSpace
import Data.Map (Map, (!))
import Linear
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW

import Common

import RenderGL
import GLHelper

displayState :: GLState -> GLFW.WindowRefreshCallback
displayState state w = forever $ do
  let (sbv, cam, bds, screen) = state

  clear [ColorBuffer, DepthBuffer, StencilBuffer]

  camera <- get cam
  scr <- get screen

  let (cS, cBs, cV) = sbv ! Cross
      (pS, pBs, pV) = sbv ! Planet
      (vS, vBs, vV) = sbv ! Vector
      cB = cBs ! ArrayBuffer
      pB = pBs ! ArrayBuffer
      vB = vBs ! ArrayBuffer

      zoom = recip 3e11 :: DT
      arrowscale = 0.6 :: DT
      tessI = 8*8 :: DT
      tessO = 8*8 :: DT
      noisiness = 0.2 :: DT

      mkRot axis rad = m33_to_m44 . fromQuaternion $ axisAngle axis rad
      arrowrot = mkRot (V3 0 0 1) (deg2rad 50) :: M44 DT
      scaleM = m33_to_m44 (identity !!* zoom)
      model = camMatrix camera
      view = Linear.lookAt (V3 0 0 0) (V3 0 0 (-1)) (V3 0 1 0)
      near = 0.0001
      far = 10
      fov = 0.8
      ar = uncurry (/) scr
      proj = Linear.perspective fov ar near far
      transform = proj !*! view !*! model :: M44 DT

  currentProgram $= Just (program cS)
  setUniform cS "transform" transform
  bindBuffer ArrayBuffer $= Just cB
  withVAO cV $ drawArrays Points 0 2

  currentProgram $= Just (program pS)
  setUniform pS "transform" transform
  setUniform pS "tess_inner" tessI
  setUniform pS "tess_outer" tessO
  setUniform pS "noisiness" noisiness
  setUniform pS "origin" $ V3 0 0 (0::DT)
  let vertsI = fmap realToFrac <$> icosahedronTriangles :: [V3 DT]
  bindBuffer ArrayBuffer $= Just pB
  replaceBuffer ArrayBuffer vertsI
  withVAO pV $ drawArrays Patches 0 (fromIntegral $ length vertsI)

  currentProgram $= Just (program vS)
  setUniform vS "transform" $ transform !*! scaleM
  setUniform vS "arrowrot" arrowrot
  setUniform vS "arrowscale" arrowscale
  s <- get bds
  print s
  let verts = fmap realToFrac <$> concatMap bodyVertices s :: [V3 DT]
  bindBuffer ArrayBuffer $= Just vB
  replaceBuffer ArrayBuffer verts
  withVAO vV $ drawArrays Lines 0 (fromIntegral $ length verts)

  throwError

  GLFW.swapBuffers w
  GLFW.waitEvents
