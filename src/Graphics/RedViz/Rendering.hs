--------------------------------------------------------------------------------
-- |
-- Module      :  Rendering
-- Copyright   :  (c) Vladimir Lopatin 2025
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
import Graphics.GL.Functions
import Linear.V2
import Linear.V4
import SDL hiding (Texture, normalize)
import Lens.Micro
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.ByteString.Char8 as BS
import Linear as L
import Data.List
import Foreign.Marshal.Array (withArray)
import Foreign.Storable
import Graphics.RedViz.Utils (encodeStringUUID, word32ToInt, intToWord32)

import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Entity hiding (uuid)
import Graphics.RedViz.Component
import Graphics.RedViz.Uniforms
import Graphics.RedViz.Widget
import Graphics.RedViz.Game
import Graphics.RedViz.Backend as BO (Options(primitiveMode), ptSize, blendFunc)
import Graphics.RedViz.Texture as T
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))
import Graphics.GL (glDeleteTextures)
import Graphics.RedViz.Component (Component(Obscurable))
import Graphics.RedViz.Entity (obscurable)
import Graphics.RedViz.LoadShaders
import Graphics.Rendering.OpenGL (TextureObject)

--import Debug.Trace as DT

renderWidget :: Camera -> Uniforms -> Widget -> IO ()
renderWidget cam unis' wgt = case wgt of
  Empty                   -> do return ()
  Cursor  False _ _ _ _   -> do return ()
  Cursor  _ _ _ fmt _ -> 
    (\dr -> do
        GL.blendFunc $= (OneMinusDstColor, OneMinusSrcAlpha) 
        bindUniforms cam unis' (formatDrw (format wgt) dr) 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements Triangles numIndices UnsignedInt nullPtr
    ) (fromMaybe (error "No Cursor : empty list!") (listToMaybe idrs) ) -- listToMaybe == head! (first element!), duh!
    where
      idrs :: [Drawable]
      idrs = scaleDrws fmt $ concatMap drws $ concatMap renderables $ icons wgt
  Gizmo  False _ _ _ _ -> do return ()
  Gizmo  _ _ _ fmt _ -> 
    (\dr -> do
        GL.blendFunc $= (SrcColor, OneMinusSrcAlpha)
        bindUniforms cam unis' (formatDrw (format wgt) dr) 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements Triangles numIndices UnsignedInt nullPtr
    ) (fromMaybe (error "No Gizmo : empty list!") (listToMaybe idrs) ) -- cursor font index is 75
    where
      idrs :: [Drawable]
      idrs = concatMap drws $ concatMap renderables $ icons wgt
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

renderNoShadows :: GameSettings -> Object -> Drawable -> IO ()
renderNoShadows gs obj dr = do 
  let (Descriptor triangles numIndices program) = descriptor dr
  currentProgram $= Just program
  mapM_ (bindTextures program) $ dtxs dr
  currentProgram $= Just program

  bindFramebuffer Framebuffer $= defaultFramebufferObject
  viewport $= (Position 0 0, Size (fromIntegral $ resX gs) (fromIntegral $ resY gs))
  bindVertexArrayObject $= Just triangles
  drawElements Triangles numIndices UnsignedInt nullPtr

debugView :: TextureObject -> IO ()
debugView depthTextureObject = do
  let planeVertices = [
          Vector3 (-10) (-10) 0, Vector3 10 (-10) 0, Vector3 10 10 0,
          Vector3 (-10) (-10) 0, Vector3 10 10 0, Vector3 (-10) 10 0 :: Vector3 GLfloat ]

  -- Create VBOs
  planeVBO <- genObjectName
  bindBuffer ArrayBuffer $= Just planeVBO
  withArray planeVertices $ \ptr ->
    bufferData ArrayBuffer $= (fromIntegral $ length planeVertices * sizeOf (undefined :: Vector3 GLfloat), ptr, StaticDraw)

  -- Create VAO for plane
  planeVAO <- genObjectName
  bindVertexArrayObject $= Just planeVAO
  bindBuffer ArrayBuffer $= Just planeVBO
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
  vertexAttribArray (AttribLocation 0) $= Enabled

  let
    lightRayDirection = L.normalize $ V3 (-1) (-1) (-1 :: GLfloat)
    lightDir = negate lightRayDirection -- Towards the light
    lightView = L.lookAt (V3 0 0 10) (V3 (-2) (0.3) 0) (V3 0 1 0) :: M44 GLfloat
    lightProjection = L.ortho (-20) 20 (-20) 20 5 15
    lightViewProjection = lightProjection !*! lightView
    cameraView = L.lookAt (V3 4 5 5) (V3 0 0 0) (V3 0 1 0) :: M44 GLfloat
    cameraProjection = L.perspective (pi/4) (800/600) 0.1 100
    modelViewProjection = cameraProjection !*! cameraView

  mainProgram  <- createShaderProgram mainVertexShaderSrc (Just mainFragmentShaderSrc)
  depthMask $= Disabled -- TODO: enable
  currentProgram $= Just mainProgram
  mvpLoc <- SV.get (uniformLocation mainProgram "modelViewProjection")
  modelViewProjection' <- m44GLfloatToGLmatrixGLfloat modelViewProjection
  uniform mvpLoc $= modelViewProjection'

  lightVPLoc' <- SV.get (uniformLocation mainProgram "lightViewProjection")
  lightViewProjection' <- m44GLfloatToGLmatrixGLfloat lightViewProjection
  uniform lightVPLoc' $= lightViewProjection'

  lightDirLoc <- get (uniformLocation mainProgram "lightDir")
  uniform lightDirLoc $= v3GLfloatToVertex3GLfloat lightDir
  
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just depthTextureObject
  shadowMapLoc <- uniformLocation mainProgram "shadowMap" -- not here
  uniform shadowMapLoc  $= (0 :: GLint)

  bindVertexArrayObject $= Just planeVAO
  drawArrays Triangles 0 6  -- Draw plane

    where
      mainVertexShaderSrc :: String
      mainVertexShaderSrc = unlines [
          "#version 330 core",
          "uniform mat4 modelViewProjection;",
          "uniform mat4 lightViewProjection;",
          "in vec3 position;",
          "out vec4 lightSpacePos;",
          "void main() {",
          "    gl_Position = modelViewProjection * vec4(position, 1.0);",
          "    lightSpacePos = lightViewProjection * vec4(position, 1.0);",
          "}"
          ]
       
      mainFragmentShaderSrc :: String
      mainFragmentShaderSrc = unlines [
          "#version 330 core",
          "uniform sampler2D shadowMap;",
          "uniform vec3 lightDir;",
          "uniform int objectType;",  -- Added uniform to identify the object
          "in vec4 lightSpacePos;",
          "out vec4 color;",
          "void main() {",
          "    if (objectType == 1) {",  -- Triangle case
          "        color = vec4(1.0, 0.0, 0.0, 1.0);",  -- Set color to red
          "    } else {",  -- Plane case
          "        vec3 projCoords = lightSpacePos.xyz / lightSpacePos.w;",
          "        projCoords = projCoords * 0.5 + 0.5;",
          "        float shadowMapDepth = texture(shadowMap, projCoords.xy).r;",
          "        float fragmentDepth = projCoords.z;",
          "        float bias = 0.005;",
          "        float shadow = fragmentDepth > shadowMapDepth + bias ? 1.0 : 0.0;",
          "        vec3 normal = vec3(0.0, 0.0, 1.0);",
          "        float diffuse = max(dot(normal, lightDir), 0.0);",
          "        float intensity = shadow > 0.5 ? 0.2 : diffuse;",
          "        color = vec4(intensity, intensity, intensity, 1.0);",
          "    }",
          "}"
          ]
       
renderWithShadows :: GameSettings -> Object -> Drawable -> IO ()
renderWithShadows gs obj dr = do
  -- | Manage depth texture and FBO

  let shadowMapSize = Size 1024 1024
      shadowMapSize'= TextureSize2D 1024 1024

      depthTextureObject = (\(Obscurable _ maybedtx)
                             -> case maybedtx of
                                  Just dtx -> (\(_,(_,txo)) -> txo) dtx
                                  Nothing -> error "Obscurable txo value is invalid :" -- ++ show maybedtx
                           ) $ obscurable obj

      (Descriptor triangles numIndices program) = descriptor dr
      -- Define matrices
      lightRayDirection = L.normalize $ V3 (-1) (-1) (-1 :: GLfloat)
      lightDir = negate lightRayDirection -- Towards the light
      lightView = L.lookAt (V3 0 0 10) (V3 (-2) (0.3) 0) (V3 0 1 0) :: M44 GLfloat
      lightProjection = L.ortho (-20) 20 (-20) 20 5 15
      lightViewProjection = lightProjection !*! lightView
      cameraView = L.lookAt (V3 4 5 5) (V3 0 0 0) (V3 0 1 0) :: M44 GLfloat
      cameraProjection = L.perspective (pi/4) (800/600) 0.1 100
      modelViewProjection = cameraProjection !*! cameraView

  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just depthTextureObject
  texImage2D Texture2D NoProxy 0 DepthComponent32f shadowMapSize' 0 (PixelData DepthComponent Float nullPtr)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  
  fbo <- genObjectName
  bindFramebuffer Framebuffer $= fbo
  framebufferTexture2D Framebuffer DepthAttachment Texture2D depthTextureObject 0
  drawBuffer $= NoBuffers
  readBuffer $= NoBuffers
  status <- get (framebufferStatus Framebuffer)
  unless (status == Complete) $ error "FBO not complete"
  
  -- | DepthMap pass
  bindFramebuffer Framebuffer $= fbo
  viewport $= (Position 0 0, shadowMapSize)
  GL.clear [DepthBuffer]

  let depthProgram = ((\(Obscurable maybeProgram _)
                            -> case maybeProgram of
                              Just program -> program
                              Nothing -> error "Obscurable program is empty"
                          ) $ obscurable obj)

  currentProgram $= Just depthProgram

  lightVPLoc <- SV.get (uniformLocation depthProgram "lightViewProjection")
  lightViewProjection' <- m44GLfloatToGLmatrixGLfloat lightViewProjection
  uniform lightVPLoc $= lightViewProjection'

  bindVertexArrayObject $= Just triangles
  drawElements Triangles numIndices UnsignedInt nullPtr

  bindFramebuffer Framebuffer $= defaultFramebufferObject
  viewport $= (Position 0 0, Size 800 600)
  GL.clear [ColorBuffer, DepthBuffer]
  -- | End of DepthMap pass

  debugView depthTextureObject
 
  -- | Main Pass
  currentProgram $= Just program
  let depthTexture = T.Texture "shadowMap" "shadowMap" (encodeStringUUID "shadowMap")
      maxTexId = fromMaybe 0 . listToMaybe . sortBy (comparing Down) $ fst <$> dtxs dr
  mapM_ (bindTextures program) $ dtxs dr ++ [(maxTexId+1, (depthTexture, depthTextureObject))] -- add shadow map (depthMap) texture to the binding call
  currentProgram $= Just program

  bindFramebuffer Framebuffer $= defaultFramebufferObject
  viewport $= (Position 0 0, Size (fromIntegral $ resX gs) (fromIntegral $ resY gs))
  bindVertexArrayObject $= Just triangles
  drawElements Triangles numIndices UnsignedInt nullPtr
  -- | End of Main Pass

render :: GameSettings -> Object -> Drawable -> IO ()
render gs obj dr = do
  case not . null $ obscurables obj of
    False -> renderNoShadows gs obj dr
    True  -> renderWithShadows gs obj dr

renderObject :: GameSettings -> Camera -> Uniforms -> Object -> IO ()
renderObject gs cam unis' obj = do
  mapM_ (\dr -> do
            GL.blendFunc $= (BO.blendFunc . backend . renderable $ obj)
            bindUniforms cam unis' dr {u_xform = xform . transformable $ obj} 
            render gs obj dr 
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

v3GLfloatToVertex3GLfloat :: V3 GLfloat -> Vertex3 GLfloat
v3GLfloatToVertex3GLfloat v3 = Vertex3 (v3^._x) (v3^._y) (v3^._z)

    -- Flatten matrix to column-major list for newMatrix helper
m44GLfloatToGLfloat :: M44 GLfloat -> [GLfloat]
m44GLfloatToGLfloat m = let (V4 c0 c1 c2 c3) = m in concatMap (\(V4 x y z w) -> [x,y,z,w]) [c0,c1,c2,c3]

m44GLfloatToGLmatrixGLfloat :: M44 GLfloat -> IO (GLmatrix GLfloat)
m44GLfloatToGLmatrixGLfloat mtx0 = do
  xform1 <- newMatrix ColumnMajor $ m44GLfloatToGLfloat mtx0
  return xform1

renderOutput :: Window -> GameSettings -> (Game, Maybe Bool) -> IO Bool
renderOutput _ _ ( _,Nothing) = SDL.quit >> return True
renderOutput window gs (game0, Just skipSwap) = do -- Just skipSwap window swap
  let
  clearColor   $= Color4 0.0 0.0 0.0 1.0
  GL.clear [ColorBuffer, DepthBuffer]

  --GL.pointSize $= 10.0
  GL.blend     $= Enabled
  GL.depthMask $= Enabled
  depthFunc    $= Just Less
  cullFace     $= Just Back

  mapM_ (renderObject gs (head $ cams game0) (unis game0)) (objs game0)
  mapM_ (renderWidget    (head $ cams game0) (unis game0)) (wgts game0)

  if skipSwap then return False else glSwapWindow window >> return False
