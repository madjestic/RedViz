--------------------------------------------------------------------------------
-- |
-- Module      :  LoadShaders
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Utilities for shader handling, adapted from LoadShaders.cpp which is (c) The
-- Red Book Authors.
--
--------------------------------------------------------------------------------


module Graphics.RedViz.LoadShaders (
   ShaderSource(..), ShaderInfo(..), loadShaders, createShaderProgram
) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import System.Directory
import Graphics.Rendering.OpenGL
import Debug.Trace as DT

--------------------------------------------------------------------------------

-- | The source of the shader source code.

data ShaderSource =
     ByteStringSource B.ByteString
     -- ^ The shader source code is directly given as a 'B.ByteString'.
   | StringSource String
     -- ^ The shader source code is directly given as a 'String'.
   | FileSource FilePath
     -- ^ The shader source code is located in the file at the given 'FilePath'.
   deriving ( Eq, Ord, Show )

getSource :: ShaderSource -> IO B.ByteString
getSource (ByteStringSource bs) = return bs
getSource (StringSource str) = return $ packUtf8 str
getSource (FileSource path) = B.readFile path

--------------------------------------------------------------------------------

-- | A description of a shader: The type of the shader plus its source code.

data ShaderInfo = ShaderInfo ShaderType ShaderSource
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | Create a new program object from the given shaders, throwing an
-- 'IOException' if something goes wrong.

loadShaders :: [ShaderInfo] -> IO Program
loadShaders infos =
   createProgram `bracketOnError` deleteObjectName $ \program -> do
      --_ <- DT.trace ("loadShaders.hs: Loading Shader Program :" ++ show infos) $ return ()
      loadCompileAttach program infos
      linkAndCheck program
      return program

linkAndCheck :: Program -> IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

loadCompileAttach :: Program -> [ShaderInfo] -> IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) =
  createShader shType `bracketOnError` deleteObjectName $ \shader ->
  do
    --_ <- DT.trace ("Loading Shader Program" ++ show program ++ show source) $ return ()
    src        <- getSource source
    isLib      <- doesFileExist "./mat/share/lib.glsl"
    isHgSDFLib <- doesFileExist "./mat/share/hg_sdf.glsl"
                
    let
      mfs
        | isLib      = Just "./mat/share/lib.glsl"
        | otherwise  = DT.trace "warning: ./mat/share/lib.glsl not found"
                       $ Nothing
      mfs'
        | isHgSDFLib = Just "./mat/share/hg_sdf.glsl"
        | otherwise  = DT.trace "warning: ./mat/share/hg_sdf.glsl not found"
                       $ Nothing

    sharedLib <- case mfs of
      Just fs -> do getSource (FileSource fs)
      Nothing -> return . BS.pack $ ""

    hgSDFLib  <- case mfs' of
      Just fs -> do getSource (FileSource fs)
      Nothing -> return . BS.pack $ ""

    let
      version   = head (BS.lines src)
      shaderSrc = tail (BS.lines src)
      src'      = BS.unlines $ version : sharedLib : hgSDFLib : shaderSrc
     
    shaderSourceBS shader $= src'
    compileAndCheck shader
    attachShader program shader
    loadCompileAttach program infos

compileAndCheck :: Shader -> IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

checked :: (t -> IO ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> IO ()
checked action getStatus getInfoLog message object = do
   action object
   ok <- get (getStatus object)
   unless ok $ do
      infoLog <- get (getInfoLog object)
      fail (message ++ " log: " ++ infoLog)

-- Create shader program
createShaderProgram :: String -> Maybe String -> IO Program
createShaderProgram vsSrc fsSrcM = do
    program <- createProgram
    vs <- createShader VertexShader
    shaderSourceBS vs $= BS.pack vsSrc
    compileShader vs
    attachShader program vs
    
    case fsSrcM of
        Just fsSrc -> do
            fs <- createShader FragmentShader
            shaderSourceBS fs $= BS.pack fsSrc
            compileShader fs
            attachShader program fs
        Nothing -> return ()
    
    linkProgram program
    return program  
