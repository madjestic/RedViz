--------------------------------------------------------------------------------
-- |
-- Module      :  Rendering
-- Copyright   :  (c) Vladimir Lopatin 2024
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling OpenGL buffers and rendering.
--
--------------------------------------------------------------------------------
module Graphics.RedViz.Rendering
  ( defaultUniforms
  , formatString
  , formatChar
  , formatText
  , renderObject
  , renderWidget
  , renderOutput
  , openWindow
  ) where

import Control.Monad
import Data.StateVar as SV
import Data.Text (Text)
import Foreign.C.Types
import Foreign.Ptr
import Graphics.Rendering.OpenGL as GL
import Linear.V2
import Linear.V4
import SDL hiding (Texture, normalize)

import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Entity hiding (uuid)
import Graphics.RedViz.Component
import Graphics.RedViz.Uniforms
import Graphics.RedViz.Widget
import Graphics.RedViz.Game
import Graphics.RedViz.Backend (BackendOptions(primitiveMode), ptSize)

import Lens.Micro

import Debug.Trace as DT

renderWidget :: Camera -> Uniforms -> Widget -> IO ()
renderWidget cam unis' wgt = case wgt of
  Empty                   -> do return ()
  Cursor  False _ _ _     -> do return ()
  Cursor  {} ->
    (\dr -> do
        bindUniforms cam unis' (formatDrw (format wgt) dr) 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements Triangles numIndices UnsignedInt nullPtr
    ) (head idrs) -- cursor font index is 75
        --) (head (DT.trace ("idrs :" ++ show (length idrs)) idrs)) -- cursor font index is 75
    where
      idrs :: [Drawable]
      idrs = concatMap drws $ concatMap renderables $ icons wgt
      --idrs = concatMap drws $ DT.trace ("renderable :" ++ show (concatMap renderable $ icons wgt )) $ concatMap renderable $ DT.trace ("icons wgt :" ++ show (length $ icons wgt )) $ icons wgt
        --where renderable cs = filter (\c -> case c of Objectable {} -> True; _-> False;) cs
  TextField False _ _ _ _   -> do return ()
  TextField _ s _ fmt _ ->
    mapM_
    (\dr -> do
        bindUniforms cam unis' dr 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements Triangles numIndices UnsignedInt nullPtr
        ) $ formatText fmt wdrs s (0,0)
  Selector _ icons' objs' fmt -> 
    mapM_
    (\obj -> do
        mapM_
          (\dr -> do
              bindUniforms cam unis' dr { u_xform = u_xform dr & translation .~ (xform . transformable $ obj)^.translation } 
              let (Descriptor triangles numIndices _) = descriptor dr
              bindVertexArrayObject $= Just triangles
              --drawElements (primitiveMode $ doptions dr) numIndices GL.UnsignedInt nullPtr
              drawElements Lines numIndices UnsignedInt nullPtr
          ) (formatDrws fmt $ drws . renderable $ icons'!!1)) objs'
  where
    wdrs = concatMap drws $ concatMap renderables $ fonts wgt      

formatDrw :: Format -> Drawable -> Drawable
--formatDrw fmt dr = dr
formatDrw _ dr = dr
  
renderObject :: Camera -> Uniforms -> Object -> IO ()
renderObject cam unis' obj = do
  mapM_ (\dr -> do
            bindUniforms cam unis' dr {u_xform = xform . transformable $ obj} 
            let (Descriptor triangles numIndices _) = descriptor dr
            bindVertexArrayObject $= Just triangles
            GL.pointSize $= (ptSize . backend . renderable $ obj)
            drawElements (primitiveMode . backend . renderable $ obj) numIndices UnsignedInt nullPtr
        ) (drws . renderable $ obj)

openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear                    
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality          
       when (renderQuality /= SDL.ScaleLinear) $                    
         putStrLn "Warning: Linear texture filtering not enabled!"

    let config = 
          OpenGLConfig { glColorPrecision     = V4 8 8 8 0
                       , glDepthPrecision     = 24
                       , glStencilPrecision   = 8
                       , glMultisampleSamples = 4
                       , glProfile            = Core Normal 4 5
                       }
     
    window <- SDL.createWindow
              title
              SDL.defaultWindow
              { SDL.windowInitialSize     = V2 sizex sizey
              , SDL.windowGraphicsContext = OpenGLContext config }

    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window
  
renderOutput :: Window -> GameSettings -> (Game, Maybe Bool) -> IO Bool
renderOutput _ _ ( _,Nothing) = SDL.quit >> return True
--renderOutput window gs (g,_) = do
renderOutput window _ (g,_) = do
  let
  clearColor $= Color4 0.0 0.0 0.0 1.0
  GL.clear [ColorBuffer, DepthBuffer]

  --GL.pointSize $= 10.0
  GL.blend $= Enabled
  GL.depthMask $= Enabled
  depthFunc $= Just Less
  cullFace  $= Just Back

  mapM_ (renderObject (head $ cams g) (unis g)) (objs g)
  mapM_ (renderWidget (head $ cams g) (unis g)) (wgts g)

  glSwapWindow window >> return False  
