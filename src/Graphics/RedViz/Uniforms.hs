--------------------------------------------------------------------------------
-- |
-- Module      :  Uniforms
-- Copyright   :  (c) Vladimir Lopatin 2024
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A basic camera structure.
--
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Graphics.RedViz.Uniforms where

import Data.Foldable as DF
import Data.StateVar as SV
import Graphics.Rendering.OpenGL hiding (get)
import Lens.Micro
import Linear.Matrix
import Linear.Projection as LP        (infinitePerspective)
import Linear.V4
import GHC.Float
import Control.Concurrent
import Data.Binary as DB
import GHC.Generics

import Graphics.RedViz.Entity
import Graphics.RedViz.Component
import Graphics.RedViz.Drawable
import Graphics.RedViz.Descriptor
import Graphics.RedViz.LoadShaders
import Graphics.RedViz.Material
import Graphics.RedViz.Texture
import Graphics.RedViz.Utils ()

debug :: Bool
#ifdef DEBUGSHADERS
debug = True
#else
debug = False
#endif

data Uniforms
  =  Uniforms
     { u_time  :: Double
     , u_res   :: (Int, Int)
     , u_cam   :: M44 Double
     , u_cam_a :: Double
     , u_cam_f :: Double
     , u_cam_ypr   :: (Double, Double, Double)
     , u_cam_yprS  :: (Double, Double, Double)
     , u_cam_vel   :: (Double, Double, Double)
     , u_cam_accel :: (Double, Double, Double)
     , u_scale :: Float
     } deriving (Show, Generic, Binary)

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
  , u_cam_accel = (0,0,0)
  , u_scale = 1.0 }

bindUniforms :: Camera -> Uniforms -> Drawable -> IO ()  
bindUniforms cam' unis' dr =  
  do
    let
      u_xform'  = u_xform  dr
      d'        = descriptor dr :: Descriptor
      u_cam'    = xform . transformable $ cam'
        -- where transformable = case transformables of [] -> defaultTransformable; _ -> head transformables
        --         where transformables = filter (\c -> case c of (Transformable{}) -> True; _ -> False; ) (cmps cam')
      u_mouse'  = (0,0) :: (Int, Int)
      (Uniforms u_time' u_res' _ u_cam_a' u_cam_f' u_ypr' u_yprS' u_vel' u_accel' u_scale') = unis'
      (Descriptor _ _ u_prog') = d'

    program' <- if debug then debugShaders dr else return u_prog'
    currentProgram $= Just program'
    
    let u_mouse0      = Vector2 (realToFrac $ fst u_mouse') (realToFrac $ snd u_mouse') :: Vector2 GLfloat
    location0         <- SV.get (uniformLocation u_prog' "u_mouse'")
    uniform location0 $= u_mouse0

    let resX          = fromIntegral $ fromEnum $ fst u_res' :: Double
        resY          = fromIntegral $ fromEnum $ snd u_res' :: Double
        u_res         = Vector2 (realToFrac resX) (realToFrac resY) :: Vector2 GLfloat

    location1         <- SV.get (uniformLocation u_prog' "u_resolution")
    uniform location1 $= u_res
    --print $ "u_res : " ++ show (u_res')
    
    location2         <- SV.get (uniformLocation u_prog' "u_time")
    uniform location2 $= (double2Float u_time' :: GLfloat)
    --print $ "u_time : " ++ show (u_time')

    let apt = u_cam_a' -- aperture
        foc = u_cam_f' -- focal length
        proj =
          LP.infinitePerspective
          (atan ( apt/foc/1.2 )) -- FOV
          (resX/resY)            -- Aspect
          0.01                   -- Near

    persp             <- newMatrix RowMajor $ toList' proj   :: IO (GLmatrix GLfloat)
    location3         <- SV.get (uniformLocation u_prog' "persp")
    uniform location3 $= persp

    --print $ "camera : " ++ show (u_cam')
    camera            <- newMatrix RowMajor $ toList' u_cam' :: IO (GLmatrix GLfloat)
    location4         <- SV.get (uniformLocation u_prog' "camera")
    uniform location4 $= camera

    -- | Compensate world space xform with camera position
    -- = Object Position - Camera Position
    xform0            <- newMatrix RowMajor $ toList' u_xform' :: IO (GLmatrix GLfloat)
    location5         <- SV.get (uniformLocation u_prog' "xform0")
    uniform location5 $= xform0

    xform             <- newMatrix RowMajor $ toList' (inv44 (identity & translation .~ u_cam'^.translation) !*! u_xform') :: IO (GLmatrix GLfloat)
    location6         <- SV.get (uniformLocation u_prog' "xform")
    uniform location6 $= xform

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

    location13        <- SV.get (uniformLocation u_prog' "u_scale")
    uniform location13 $= ( u_scale' :: GLfloat)

          -- | Allocate Textures
    texture Texture2D        $= Enabled
    mapM_ allocateTextures (dtxs dr) -- TODO: this is ignored, should bind an appropriate texture

    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing
      where        
        toList' = fmap realToFrac.DF.concat.(fmap DF.toList.DF.toList) :: V4 (V4 Double) -> [GLfloat]

debugShaders :: Drawable -> IO Program
debugShaders dr = do
  threadDelay 10000
  loadShaders
    [ ShaderInfo VertexShader   (FileSource (vertShader . material $ dr))   
    , ShaderInfo FragmentShader (FileSource (fragShader . material $ dr)) ]
