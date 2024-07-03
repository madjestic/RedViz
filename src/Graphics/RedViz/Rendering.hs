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
import Linear.Metric (norm)
import SDL hiding (Texture, normalize)

import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Entity hiding (uuid)
import Graphics.RedViz.Component
import Graphics.RedViz.Uniforms
import Graphics.RedViz.Widget
import Graphics.RedViz.Game
import Graphics.RedViz.Backend as BO (Options(primitiveMode), ptSize, blendFunc)

import Lens.Micro

import Debug.Trace as DT
import Data.Maybe (listToMaybe, fromMaybe)

renderWidget :: Camera -> Uniforms -> Widget -> IO ()
renderWidget cam unis' wgt = case wgt of
  Empty                   -> do return ()
  Cursor  False _ _ _ _   -> do return ()
  Cursor  _ _ _ fmt _ ->
    (\dr -> do
        bindUniforms cam unis' (formatDrw (format wgt) dr) 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements Triangles numIndices UnsignedInt nullPtr
    ) (fromMaybe (error "No Cursor : empty list!") (listToMaybe idrs) ) -- cursor font index is 75
    where
      idrs :: [Drawable]
      idrs = scaleDrws fmt $ concatMap drws $ concatMap renderables $ icons wgt
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
          ) (scaleDrws fmt $ drws . renderable $ icons'!!1)) objs'
  InfoField False _ _ _ _   -> do return ()
  InfoField _ s _ fmt _ ->
    mapM_
    (\dr -> do
        bindUniforms cam unis' dr 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements Triangles numIndices UnsignedInt nullPtr
        ) $ scaleDrws fmt $ formatText fmt wdrs
             (["  velocity :" ++ show cam_vel ++ "\n"] ++
              ["  speed    : " ++ show cam_spd ]
             ) (0,0)
    where
      cam_vel = u_cam_vel unis'
      cam_spd = norm $ (\(x,y,z) -> V3 x y z) . u_cam_vel $ unis'

  --_ -> error "ERROR : Unknown widget type!"  
  where
    wdrs = concatMap drws $ concatMap renderables $ fonts wgt      

formatDrw :: Format -> Drawable -> Drawable
--formatDrw fmt dr = dr
formatDrw _ dr = dr
  
renderObject :: Camera -> Uniforms -> Object -> IO ()
renderObject cam unis' obj = do
  mapM_ (\dr -> do
            GL.blendFunc $= (BO.blendFunc . backend . renderable $ obj)
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

-- TODO: I need separate pipelines for this
renderOutput :: Window -> GameSettings -> (Game, Maybe Bool) -> IO Bool
renderOutput _ _ ( _,Nothing) = SDL.quit >> return True
renderOutput window _ (g,_) = do
  let
  clearColor   $= Color4 0.0 0.0 0.0 1.0
  GL.clear [ColorBuffer, DepthBuffer]

  --GL.pointSize $= 10.0
  GL.blend     $= Enabled
  GL.depthMask $= Enabled
  depthFunc    $= Just Less
  cullFace     $= Just Back
  mapM_ (renderObject (head $ cams g) (unis g)) (objs g)--(tail $ objs g)

  GL.blendFunc $= (OneMinusDstColor, OneMinusSrcAlpha)
  mapM_ (renderWidget (head $ cams g) (unis g)) (wgts g)

  glSwapWindow window >> return False  
