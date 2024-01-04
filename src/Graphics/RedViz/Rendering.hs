--------------------------------------------------------------------------------
-- |
-- Module      :  Rendering
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling OpenGL buffers and rendering.
--
--------------------------------------------------------------------------------
--{-# LANGUAGE DeriveGeneric #-}

module Graphics.RedViz.Rendering
  ( bindTexture
  , formatText
  , formatString
  , formatChar
  , Uniforms (..)
  , defaultUniforms
  , renderObject
  , renderWidget
  ) where

import GHC.Generics
import GHC.Float
import Data.Maybe
import Data.Foldable as DF
import Data.StateVar as SV
import Data.UUID
import Foreign.C.Types
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Projection as LP        (infinitePerspective)
import Lens.Micro
import SDL (Point)

import Graphics.RedViz.Backend
import Graphics.RedViz.Camera
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.GLUtil  (readTexture, texture2DWrap)
import Graphics.RedViz.Object hiding (uuid)
import Graphics.RedViz.Texture ( Texture(path, uuid) )
import Graphics.RedViz.Transformable
import Graphics.RedViz.Widget

data Uniforms
  =  Uniforms
     { u_time  :: Double
     , u_res   :: (CInt, CInt)
     , u_cam   :: M44 Double
     , u_cam_a :: Double
     , u_cam_f :: Double
     , u_cam_ypr   :: (Double, Double, Double)
     , u_cam_yprS  :: (Double, Double, Double)
     , u_cam_vel   :: (Double, Double, Double)
     , u_cam_accel :: (Double, Double, Double)
     } deriving Show

bindUniforms :: Camera -> Uniforms -> Drawable -> IO ()  
bindUniforms cam' unis' dr =  
  do
    let
      u_xform'  = u_xform  dr
      d'        = descriptor dr :: Descriptor
      u_cam'    = (xform.ctransform) cam'
      u_mouse'  = (0,0)
      (Uniforms u_time' u_res' _ u_cam_a' u_cam_f' u_ypr' u_yprS' u_vel' u_accel') = unis'
      (Descriptor _ _ u_prog') = d'

    currentProgram $= Just u_prog'

    let u_mouse0      = Vector2 (realToFrac $ fst u_mouse') (realToFrac $ snd u_mouse') :: Vector2 GLfloat
    location0         <- SV.get (uniformLocation u_prog' "u_mouse'")
    uniform location0 $= u_mouse0

    let resX          = fromIntegral $ fromEnum $ fst u_res' :: Double
        resY          = fromIntegral $ fromEnum $ snd u_res' :: Double
        u_res         = Vector2 (realToFrac resX) (realToFrac resY) :: Vector2 GLfloat

    location1         <- SV.get (uniformLocation u_prog' "u_resolution")
    uniform location1 $= u_res
    
    location2         <- SV.get (uniformLocation u_prog' "u_time")
    uniform location2 $= (double2Float u_time' :: GLfloat)

    let apt = u_cam_a' -- aperture
        foc = u_cam_f' -- focal length
        proj =
          LP.infinitePerspective
          (2.0 * atan ( apt/foc/2.0 )) -- FOV
          (resX/resY)                  -- Aspect
          0.01                         -- Near

    persp             <- newMatrix RowMajor $ toList' proj   :: IO (GLmatrix GLfloat)
    location3         <- SV.get (uniformLocation u_prog' "persp")
    uniform location3 $= persp

    camera            <- newMatrix RowMajor $ toList' u_cam' :: IO (GLmatrix GLfloat)
    location4         <- SV.get (uniformLocation u_prog' "camera")
    uniform location4 $= camera

    -- | Compensate world space xform with camera position
    -- = Object Position - Camera Position
    xform             <- newMatrix RowMajor $ toList' (inv44 (identity & translation .~ u_cam'^.translation) !*! u_xform') :: IO (GLmatrix GLfloat)
    location5         <- SV.get (uniformLocation u_prog' "xform")
    uniform location5 $= xform

    let sunP = Vector3 299999999999.0 0.0 0.0 :: Vector3 GLfloat
    location7 <- SV.get (uniformLocation u_prog' "sunP")
    uniform location7 $= sunP
    
    let ypr  =
          Vector3
          (double2Float $ u_ypr'^._1)
          (double2Float $ u_ypr'^._2)
          (double2Float $ u_ypr'^._3)
          :: Vector3 GLfloat
    location8        <- SV.get (uniformLocation u_prog' "ypr")
    uniform location8 $= ypr

    let yprS =
          Vector3
          (double2Float $ u_yprS'^._1)
          (double2Float $ u_yprS'^._2)
          (double2Float $ u_yprS'^._3)
          :: Vector3 GLfloat
    location9        <- SV.get (uniformLocation u_prog' "yprS")
    uniform location9 $= yprS


    let vel  =
          Vector3
          (double2Float $ u_vel'^._1)
          (double2Float $ u_vel'^._2)
          (double2Float $ u_vel'^._3)
          :: Vector3 GLfloat
    location10        <- SV.get (uniformLocation u_prog' "vel")
    uniform location10 $= vel

    let accel  =
          Vector3
          (double2Float $ u_accel'^._1)
          (double2Float $ u_accel'^._2)
          (double2Float $ u_accel'^._3)
          :: Vector3 GLfloat
    location11        <- SV.get (uniformLocation u_prog' "accel")
    uniform location11 $= accel

    -- || Set Transform Matrix
    let tr :: [GLfloat]
        tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 1 ]

    transform <- newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location12 <- SV.get (uniformLocation u_prog' "transform")
    uniform location12 $= transform

    -- | Allocate Textures
    texture Texture2D        $= Enabled
    mapM_ allocateTextures (dtxs dr) -- TODO: this is ignored, should bind an appropriate texture

    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing
      where        
        toList' = fmap realToFrac.concat.(fmap DF.toList.DF.toList) :: V4 (V4 Double) -> [GLfloat]

allocateTextures :: (Int, (Texture, TextureObject)) -> IO ()
allocateTextures (txid, (_, txo)) =
  do
    activeTexture $= TextureUnit (fromIntegral txid)
    textureBinding Texture2D $= Just txo
    return ()

defaultUniforms :: Uniforms
defaultUniforms = 
  Uniforms
  { u_time  = 0.0
  , u_res   = (800,600)
  , u_cam   = identity :: M44 Double
  , u_cam_a = 50.0
  , u_cam_f = 100.0
  , u_cam_ypr   = (0,0,0)
  , u_cam_yprS  = (0,0,0)
  , u_cam_vel   = (0,0,0)
  , u_cam_accel = (0,0,0) }

bindTexture :: [(UUID, GLuint)] -> Texture -> IO (Texture, TextureObject)
bindTexture hmap tx =
  do
    putStrLn $ "Binding Texture : " ++ show tx ++ " at TextureUnit : " ++ show txid
    texture Texture2D        $= Enabled
    print $ "tx : " ++ show tx
    print $ "txid : " ++ show txid
    activeTexture            $= TextureUnit txid
    --activeTexture            $= TextureUnit (DT.trace ("bindTexture.txid : " ++ show txid) txid)
    tx0 <- loadTex $ path tx --TODO : replace that with a hashmap lookup?
    textureBinding Texture2D $= Just tx0
    return (tx, tx0)
      where
        txid = fromMaybe 0 (lookup (uuid tx) hmap)

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
    where
      idrs :: [Drawable]
      idrs = concatMap drws (icons wgt)
  TextField False _ _ _ _   -> do return ()
  TextField _ s _ fmt _ ->
    mapM_
    (\dr -> do
        bindUniforms cam unis' dr 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements Triangles numIndices UnsignedInt nullPtr
        ) $ formatText fmt wdrs s (0,0)
  Selector _ icons' objs' -> 
    mapM_
    (\obj -> do
        mapM_
          (\dr -> do
              bindUniforms cam unis' dr {u_xform = xform (transform obj)} 
              let (Descriptor triangles numIndices _) = descriptor dr
              bindVertexArrayObject $= Just triangles
              --drawElements (primitiveMode $ doptions dr) numIndices GL.UnsignedInt nullPtr
              drawElements (Lines) numIndices UnsignedInt nullPtr
          ) (drws (icons'!!1))) objs'
  where
    wdrs = concatMap drws (fonts wgt)      


formatDrw :: Format -> Drawable -> Drawable
formatDrw fmt dr = dr

  
renderObject :: Camera -> Uniforms -> Object -> IO ()
renderObject cam unis' obj = do
  mapM_ (\dr -> do
            bindUniforms cam unis' dr {u_xform = xform (transform obj)} 
            let (Descriptor triangles numIndices _) = descriptor dr
            bindVertexArrayObject $= Just triangles
            drawElements Triangles numIndices UnsignedInt nullPtr
        ) (drws obj)

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    texture2DWrap            $= (Repeated, ClampToEdge)
    textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
    blend                    $= Enabled
    blendFunc                $= (SrcAlpha, OneMinusSrcAlpha)
    generateMipmap' Texture2D
    return t
