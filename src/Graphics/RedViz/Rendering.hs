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
import Graphics.Rendering.OpenGL as GL hiding (Light, position)
import Data.Ord (comparing, Down (..))
import Linear.V2
import Linear.V4
import SDL hiding (Texture, normalize)
import Lens.Micro
import Data.Maybe (listToMaybe, fromMaybe)
import Linear as L
import Data.List
import Foreign.Marshal.Array (withArray)
import Foreign.Storable
import Data.IORef (readIORef)

import Graphics.RedViz.Utils (encodeStringUUID, double2M44GLfloat)
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Entity hiding (uuid)
import Graphics.RedViz.Component
import Graphics.RedViz.Uniforms
import Graphics.RedViz.Widget
import Graphics.RedViz.Game
import Graphics.RedViz.AppInput
import Graphics.RedViz.Backend as BO (Options(primitiveMode), ptSize, blendFunc)
import Graphics.RedViz.Texture as T
import Graphics.RedViz.LoadShaders
import System.Directory (doesFileExist)
import Debug.Trace as DT

renderWidget :: Camera -> Uniforms -> Widget -> IO ()
renderWidget cam unis' wgt = case wgt of
  Empty                   -> do return ()
  Cursor  False _ _ _ _   -> do return ()
  Cursor  _ _ _ fmt _ -> 
    (\dr -> do
        GL.blendFunc $= (OneMinusDstColor, OneMinusSrcAlpha) 
        bindUniforms cam unis' (formatDrw (format wgt) dr) Nothing Nothing
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
        bindUniforms cam unis' (formatDrw (format wgt) dr) Nothing Nothing
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
        bindUniforms cam unis' dr Nothing Nothing
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements Triangles numIndices UnsignedInt nullPtr
        ) $ formatText fmt wdrs s (0,0)
  Selector _ icons' objs' fmt -> 
    mapM_
    (\obj -> do
        mapM_
          (\dr -> do
              bindUniforms cam unis' dr { u_xform = u_xform dr & translation .~ (xform . transformable $ obj)^.translation } Nothing Nothing
              let (Descriptor triangles numIndices _) = descriptor dr
              bindVertexArrayObject $= Just triangles
              --drawElements (primitiveMode $ doptions dr) numIndices GL.UnsignedInt nullPtr
              drawElements Lines numIndices UnsignedInt nullPtr
          ) (scaleDrws fmt $ drws . renderable $ icons'!!1)) objs'
  InfoField False _ _ _ _   -> do return ()
  InfoField _ s _ fmt _ ->
    mapM_
    (\dr -> do
        bindUniforms cam unis' dr Nothing Nothing
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

renderNoShadows :: GameSettings -> Drawable -> IO ()
renderNoShadows gs dr = do 
  let (Descriptor triangles numIndices program) = descriptor dr
  currentProgram $= Just program
  mapM_ (bindTextureUniform program) $ dtxs dr
  currentProgram $= Just program

  bindFramebuffer Framebuffer $= defaultFramebufferObject
  viewport $= (Position 0 0, Size (fromIntegral $ resX gs) (fromIntegral $ resY gs))
  bindVertexArrayObject $= Just triangles
  drawElements Triangles numIndices UnsignedInt nullPtr

debugView :: TextureObject -> AppInput -> Camera -> Light -> IO ()
debugView depthTextureObject input0 cam light = do
  (x0,y0,z0) <- readIORef (sliderLightRayDirectionRef input0)
  (x1,y1,z1) <- readIORef (eyeRef    input0)
  (cx,cy,cz) <- readIORef (centerRef input0)
  (x3,y3,z3) <- readIORef (upRef     input0)
  (l, r, b)  <- readIORef (lrbRef    input0)
  (t, n, f)  <- readIORef (tnfRef    input0)
  (s)        <- readIORef (scalarRef input0)
  let planeVertices = [
          Vector3 (-1) (-1) 0, Vector3 1 (-1) 0, Vector3 1 1 0,
          Vector3 (-1) (-1) 0, Vector3 1 1 0, Vector3 (-1) 1 0 :: Vector3 GLfloat ]

  -- Create VBOs
  planeVBO <- genObjectName
  bindBuffer ArrayBuffer   $= Just planeVBO
  withArray planeVertices  $ \ptr ->
    bufferData ArrayBuffer $= (fromIntegral $ length planeVertices * sizeOf (undefined :: Vector3 GLfloat), ptr, StaticDraw)

  -- Create VAO for plane
  planeVAO <- genObjectName
  bindVertexArrayObject  $= Just planeVAO
  bindBuffer ArrayBuffer $= Just planeVBO
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
  vertexAttribArray (AttribLocation 0) $= Enabled

  let
    lightRayDirection = L.normalize $ V3 (x0) (y0) (z0 :: GLfloat)
    lightDir = negate lightRayDirection -- Towards the light
    lightView = L.lookAt (realToFrac <$> position (lightable light)) (V3 cx cy cz) (V3 x3 y3 z3) :: M44 GLfloat
    lightProjection = L.ortho (-1.5) 1.5 (-1.5) 1.5 9 11
    lightViewProjection = (lightProjection !*! lightView) ^* V4 s s s 1 --  (undefined :: GLfloat)
    cameraView = xform  . transformable $ cam -- TODO: Double -> Float
    cameraProjection = L.perspective (pi/4) (800/600) 0.1 10
    modelViewProjection = cameraProjection !*! cameraView

  mainProgram  <- createShaderProgram debugVertexShaderSrc (Just debugFragmentShaderSrc)
  depthMask $= Disabled -- TODO: enable
  currentProgram $= Just mainProgram
  mvpLoc <- SV.get (uniformLocation mainProgram "modelViewProjection")
  --modelViewProjection' <- m44GLfloatToGLmatrixGLfloat (double2M44GLfloat modelViewProjection)
  modelViewProjection' <- m44GLfloatToGLmatrixGLfloat (double2M44GLfloat modelViewProjection)
  uniform mvpLoc $= modelViewProjection'

  -- do print $ lightViewProjection

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
      debugVertexShaderSrc :: String
      debugVertexShaderSrc = unlines [
          "#version 330 core",
          --"uniform mat4 modelViewProjection;",
          "uniform mat4 lightViewProjection;",
          "in vec3 position;",
          "out vec4 lightSpacePos;",
          "void main() {",
          "    gl_Position   = vec4(position, 1.0);",
          "    lightSpacePos = lightViewProjection * vec4(position, 1.0);",
          "}"
          ]
       
      debugFragmentShaderSrc :: String
      debugFragmentShaderSrc = unlines [
          "#version 330 core",
          "uniform sampler2D shadowMap;",
          "uniform vec3 lightDir;",
          "in vec4 lightSpacePos;",
          "out vec4 color;",
          "void main() {",
          "  vec3 projCoords = lightSpacePos.xyz / lightSpacePos.w;",
          "  projCoords      = lightSpacePos.xyz * 0.5 + 0.5;",
          "  vec3  shadow    = texture(shadowMap, projCoords.xy).rgb;",
          "  color = vec4(shadow, 1);",
          "}"
          ]

-- | Shadow Depth Map Texture Size x, y ebobo
dmx = 512
dmy = dmx
       
renderWithShadows :: GameSettings -> AppInput -> Camera -> Light -> Uniforms -> Object -> Drawable -> IO ()
renderWithShadows gs input0 cam light unis' obj dr = do
  (x0,y0,z0) <- readIORef (sliderLightRayDirectionRef input0)
  (x1,y1,z1) <- readIORef (eyeRef    input0)
  (cx,cy,cz) <- readIORef (centerRef input0)
  (x3,y3,z3) <- readIORef (upRef     input0)
  (l, r, b)  <- readIORef (lrbRef    input0)
  (t, n, f)  <- readIORef (tnfRef    input0)
  (s)        <- readIORef (scalarRef input0)
  -- | Manage depth texture and FBO

  let shadowMapSize = Size dmx dmy
      shadowMapSize'= TextureSize2D dmx dmy

      depthTextureObject = (\(Obscurable _ _ maybedtx)
                             -> case maybedtx of
                                  Just dtx -> (\(_,(_,txo)) -> txo) dtx
                                  Nothing -> error "Obscurable txo value is invalid :" -- ++ show maybedtx
                           ) $ obscurable obj

      (Descriptor triangles numIndices program) = descriptor dr
      lightView = L.lookAt (realToFrac <$> position (lightable light)) (V3 0 0 0) (V3 0 1 0) :: M44 GLfloat

      lightProjection = L.ortho (-1.5) 1.5 (-1.5) 1.5 9 11
      lightViewProjection = (lightProjection !*! lightView) -- ^* V4 s s s 1 --  (undefined :: GLfloat)

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
  GL.clear [DepthBuffer, ColorBuffer]

  let depthProgram = ((\(Obscurable maybeProgram _ _)
                            -> case maybeProgram of
                              Just program -> program
                              Nothing -> error "Obscurable program is empty"
                      ) $ obscurable obj)

  currentProgram $= Just depthProgram

  bindUniforms cam unis' (dr {u_xform = xform . transformable $ obj}) (Just (obscurable obj)) Nothing

  lightVPLoc <- SV.get (uniformLocation depthProgram "lightViewProjection") -- TODO: Move to Main pass uniform binding
  lightViewProjection' <- m44GLfloatToGLmatrixGLfloat lightViewProjection
  uniform lightVPLoc $= lightViewProjection'

  bindVertexArrayObject $= Just triangles
  drawElements Triangles numIndices UnsignedInt nullPtr

  bindFramebuffer Framebuffer $= defaultFramebufferObject
  viewport $= (Position 0 0, Size 800 600)
  GL.clear [ColorBuffer, DepthBuffer]
  -- | End of DepthMap pass

  when (menuApp input0) $ debugView depthTextureObject input0 cam light
 
  -- | Main Pass
  currentProgram $= Just program

  let unis'' = unis' {u_scale = s}
  bindUniforms cam unis'' (dr {u_xform = xform . transformable $ obj}) Nothing (Just ((\obj' -> (obscurable obj'){ projection = Just (fmap (fmap realToFrac ) lightViewProjection)}) obj))

  let depthTexture = T.Texture "shadowMap" "shadowMap" (fromIntegral dmx) (fromIntegral dmy) (encodeStringUUID "shadowMap")
      maxTexId = fromMaybe 0 . listToMaybe . sortBy (comparing Down) $ fst <$> dtxs dr
  mapM_ (bindTextureUniform program) $ dtxs dr ++ [(maxTexId+1, (depthTexture, depthTextureObject))] -- add shadow map (depthMap) texture to the binding call
  -- TODO: writeTexture should be moved to the Main pass, not here!
  -- Check if the file "sukanah.png" already exists before writing
  fileExists <- doesFileExist "sukanah.png"
  unless fileExists $ do
    writeTexture (maxTexId+1, (depthTexture, depthTextureObject)) "sukanah.png"
  currentProgram $= Just program

  bindFramebuffer Framebuffer $= defaultFramebufferObject
  viewport $= (Position 0 0, Size (fromIntegral $ resX gs) (fromIntegral $ resY gs))
  bindVertexArrayObject $= Just triangles
  drawElements Triangles numIndices UnsignedInt nullPtr
  -- | End of Main Pass

render :: GameSettings -> AppInput -> Camera -> Light -> Uniforms -> Object -> Drawable -> IO ()
render gs input0 cam light unis' obj dr = do
  case not . null $ obscurables obj of
    False -> renderNoShadows gs dr
    True  -> renderWithShadows gs input0 cam light unis' obj dr 

renderObject :: GameSettings -> AppInput -> Camera -> Light -> Uniforms -> Object -> IO ()
renderObject gs input0 cam light unis' obj = do
  mapM_ (\dr -> do
            GL.blendFunc $= (BO.blendFunc . backend . renderable $ obj)
            bindUniforms cam unis' (dr {u_xform = xform . transformable $ obj}) Nothing Nothing
            render gs input0 cam light unis' obj dr 
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

renderOutput :: Window -> GameSettings -> ((Game, AppInput), Maybe Bool) -> IO Bool
renderOutput _ _ ( _,Nothing) = SDL.quit >> return True
renderOutput window gs ((game0, input0), Just skipSwap) = do -- Just skipSwap window swap
  let
  clearColor   $= Color4 0.0 0.0 0.0 1.0
  GL.clear [ColorBuffer, DepthBuffer]

  --GL.pointSize $= 10.0
  GL.blend     $= Enabled
  GL.depthMask $= Enabled
  depthFunc    $= Just Less
  cullFace     $= Just Back

  mapM_ (renderObject gs input0 (head $ cams game0) (head $ lgts game0) (unis game0)) (objs game0)
  mapM_ (renderWidget           (head $ cams game0) (unis game0)) (wgts game0)

  if skipSwap then return False else glSwapWindow window >> return False
