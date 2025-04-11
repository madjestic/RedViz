--------------------------------------------------------------------------------
-- |
-- Module      :  Descriptor
-- Copyright   :  (c) Vladimir Lopatin 2024
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A basic structure for passing to graphics driver.
--
--------------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Graphics.RedViz.Descriptor where

import Foreign (sizeOf)  
import Foreign.Marshal.Array (withArray)  
import GHC.Ptr
import Graphics.Rendering.OpenGL.GL hiding (get)
import GHC.Generics
import Data.Binary
import Data.Hashable

import Graphics.RedViz.Material as R
import Graphics.RedViz.GLTF
import Graphics.RedViz.LoadShaders

import Graphics.Rendering.OpenGL.GL.VertexArrayObjects (VertexArrayObject (..))
import Graphics.Rendering.OpenGL.GL.Shaders.Program (Program (..))

instance Hashable VertexArrayObject where
  hashWithSalt = hashWithSalt

instance Binary VertexArrayObject where
  put (VertexArrayObject v) = do
    put v
  get = do
    v <- get
    return (VertexArrayObject v)

instance Hashable Program where
  hashWithSalt = hashWithSalt

instance Binary Program where
  put (Program v) = do
    put v
  get = do
    v <- get
    return (Program v)

data Descriptor =
     Descriptor VertexArrayObject NumArrayIndices Program
     deriving (Eq, Generic, Hashable)

instance Binary Descriptor where
  put (Descriptor t i p) = do
    put t
    put i 
    put p 
  get = do
    t <- get 
    i <- get 
    p <- get
    return (Descriptor t i p)

instance Show Descriptor where
  show (Descriptor t i p) =
    "Descriptor: " ++ "\n" ++
    "\t" ++ show t ++ "\n" ++
    "\t" ++ show i ++ "\n" ++
    "\t" ++ show p ++ "\n"

toDescriptorMat :: FilePath -> IO [(Descriptor, R.Material)]
toDescriptorMat file = do
  (stuff, mats) <- loadGltf file -- "models/pighead.gltf"
  mats' <- mapM fromGltfMat mats
  ds    <- mapM (\((vs, idx), mat) -> toDescriptor idx vs mat) $ zip (concat stuff) mats'
  return $ zip ds mats'

toDescriptor :: [GLfloat] -> [GLenum] -> R.Material -> IO Descriptor
toDescriptor vs idx mat =  
  do
    -- | VAO
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length vs
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length idx
    withArray idx $ \ptr ->
      do
        let indexSize = fromIntegral $ numIndices * sizeOf (0 :: GLenum)
        bufferData ElementArrayBuffer $= (indexSize, ptr, StaticDraw)
        
    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 9 * floatSize

    -- | Positions
    let vPosition = AttribLocation 0
        posOffset = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | Colors
    let vaRGBA     = AttribLocation 1
        rgbaOffset = 3 * floatSize
    vertexAttribPointer vaRGBA  $=
        (ToFloat, VertexArrayDescriptor 4 Float stride (bufferOffset rgbaOffset))
    vertexAttribArray vaRGBA    $= Enabled

    -- | UV
    let uvCoords = AttribLocation 2
        uvOffset = 7 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled

    -- || Shaders
    -- print $ "mat : " ++ show mat
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource $ vertShader mat),
        ShaderInfo FragmentShader (FileSource $ fragShader mat)]
    currentProgram $= Just program

    -- || Unload buffers
    bindVertexArrayObject         $= Nothing

    return $ Descriptor triangles (fromIntegral numIndices) program

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
