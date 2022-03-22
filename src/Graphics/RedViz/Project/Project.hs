{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Project.Project 
  ( Project  (..)
  , ProjectCamera (..)
  , pTransform
  , pApt
  , pFoc
  , pMouseS
  , pKeyboardRS
  , pKeyboardTS
  , Model    (..)
  , name
  , resx
  , resy
  , camMode
  , models
  , objects
  , background
  , PreObject (..)
  , pname
  , modelIDXs
  , uuid 
  , solvers
  , solverAttrs
  , fonts
  , cameras
  , Graphics.RedViz.Project.Project.read
  , write
  , defaultProject
  , GUI' (..)
  , gui
  , Widget' (..)
  , widgets
  , Format' (..)
  , alignment
  , voffset  
  , hoffset  
  , soffset  
  , ssize      
  ) where

import Control.Lens hiding (Empty)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy as B hiding (drop, pack)
import Data.Maybe                       (fromMaybe)
import Data.Sort                        (sortOn)                              
import Data.Text ( Text, pack )
import Data.UUID

import Graphics.RedViz.Project.Model

-- import Debug.Trace as DT

data PreObject
  =  PreObject
     {
       _pname       :: String
     , _ptype       :: String
     , _uuid        :: UUID
     , _modelIDXs   :: [Int]
     , _solvers     :: [String]
     , _solverAttrs :: [[Double]]
     } deriving Show

$(makeLenses ''PreObject)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PreObject

data ProjectCamera
  =  ProjectCamera
     { _pcname      :: String
     , _pApt        :: Double
     , _pFoc        :: Double
     , _pTransform  :: [Float]
     , _pMouseS     :: Double -- | mouse    "sensitivity"
     , _pKeyboardRS :: Double -- | keyboard "sensitivity"
     , _pKeyboardTS :: Double
     } deriving Show
$(makeLenses ''ProjectCamera)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ProjectCamera

data Format'
  =  Format'
     { _alignment :: String
     , _voffset   :: Double
     , _hoffset   :: Double
     , _soffset   :: Double -- scale Offset
     , _ssize     :: Double -- scale Size
     } deriving Show
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Format'
$(makeLenses ''Format')

data Widget'
  =  TextField'
     { _active :: Bool
     , _text   :: [String]
     , _format :: Format' }
  |  FPS'
     { _active :: Bool
     , _format :: Format' }
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Widget'

instance Show Widget' where
  show (TextField' b f t) = show "TextField" ++ show(b, f, t)
  show (FPS' b f)         = show "FPS" ++ show (b, f)

data GUI'
  =  GUI'
     {
       _widgets :: [Widget']
     , _fonts   :: [Model]
     } deriving Show
$(makeLenses ''GUI')
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''GUI'

data Project
  =  Project
     {
       _name       :: String
     , _resx       :: Int
     , _resy       :: Int
     , _camMode    :: String
     , _models     :: [Model] -- is that used?
     , _objects    :: [PreObject]
     , _background :: [PreObject]
     , _gui        :: GUI'
     , _cameras    :: [ProjectCamera]
     } deriving Show
$(makeLenses ''Project)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Project

emptyGUI :: GUI'
emptyGUI = GUI' [] []

emptyProject :: Project
emptyProject =
  Project "foobar" (-1) (-1) "AbsoluteLocation" [] [] [] emptyGUI []

defaultFonts :: [Model]
defaultFonts =
  [
    (Model   "models/fnt_0.bgeo")
  , (Model   "models/fnt_1.bgeo")
  , (Model   "models/fnt_2.bgeo")
  , (Model   "models/fnt_3.bgeo")
  , (Model   "models/fnt_4.bgeo")
  , (Model   "models/fnt_5.bgeo")
  , (Model   "models/fnt_6.bgeo")
  , (Model   "models/fnt_7.bgeo")
  , (Model   "models/fnt_8.bgeo")
  , (Model   "models/fnt_9.bgeo")
  , (Model   "models/fnt_a.bgeo")
  , (Model   "models/fnt_b.bgeo")
  , (Model   "models/fnt_c.bgeo")
  , (Model   "models/fnt_d.bgeo")
  , (Model   "models/fnt_e.bgeo")
  , (Model   "models/fnt_f.bgeo")
  , (Model   "models/fnt_g.bgeo")
  , (Model   "models/fnt_h.bgeo")
  , (Model   "models/fnt_i.bgeo")
  , (Model   "models/fnt_j.bgeo")
  , (Model   "models/fnt_k.bgeo")
  , (Model   "models/fnt_l.bgeo")
  , (Model   "models/fnt_m.bgeo")
  , (Model   "models/fnt_n.bgeo")
  , (Model   "models/fnt_o.bgeo")
  , (Model   "models/fnt_p.bgeo")
  , (Model   "models/fnt_q.bgeo")
  , (Model   "models/fnt_r.bgeo")
  , (Model   "models/fnt_s.bgeo")
  , (Model   "models/fnt_t.bgeo")
  , (Model   "models/fnt_u.bgeo")
  , (Model   "models/fnt_v.bgeo")
  , (Model   "models/fnt_w.bgeo")
  , (Model   "models/fnt_x.bgeo")
  , (Model   "models/fnt_y.bgeo")
  , (Model   "models/fnt_z.bgeo")
  , (Model   "models/fnt_plus.bgeo")
  , (Model   "models/fnt_minus.bgeo")
  , (Model   "models/fnt_equal.bgeo")
  , (Model   "models/fnt_GT.bgeo")
  , (Model   "models/fnt_comma.bgeo")
  , (Model   "models/fnt_dot.bgeo")
  , (Model   "models/fnt_question.bgeo")
  , (Model   "models/fnt_exclam.bgeo")
  , (Model   "models/fnt_space.bgeo")
  , (Model   "models/fnt_asterics.bgeo")
  , (Model   "models/fnt_slash.bgeo")
  , (Model   "models/fnt_semicolon.bgeo")
  , (Model   "models/fnt_quote.bgeo")  
  ]

defaultFormat' :: Format'
defaultFormat' =  Format' "CC" (-0.4) 0.0 0.085 1.0

defaultGUI' :: GUI'
defaultGUI' =
  GUI'
  [ TextField' True ["Hello, World!"] defaultFormat'
  , FPS' True defaultFormat' ]
  defaultFonts

-- defaultWidget' :: Widget'
-- defaultWidget' =
--   TextField ["Hello, World!"]

defaultProject :: Project
defaultProject =
  Project
  "Test Project"
  800
  600
  "AbsoluteLocation"
  [ (Model   "models/box.bgeo")]
  [ (PreObject
    "Box"
    ""
    nil
    [0]
    ["rotate", "translate"]
    [[0,0,0,0,0,1000]
    ,[1000,0,0]]
    )
  ]
  []
  defaultGUI'
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-10,
     0, 0, 0, 1])
    1.0
    1.0
    1.0
  ]

write :: Project -> FilePath -> IO ()
write prj fileOut =
  B.writeFile fileOut $ encodePretty' config prj
  where
    config = defConfig { confCompare = comp }

comp :: Text -> Text -> Ordering
comp = keyOrder . (fmap pack) $ ["name", "resx", "resy", "camMode", "models", "objects", "background", "pname", "uuid ", "modelIDXs", "solvers", "solverAttrs", "fonts", "cameras", "pApt", "pFoc", "pTransform", "pMouseS", "pKeyboardRS", "pKeyboardTS"]

read :: FilePath -> IO Project
read filePath =
  do
    d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String Project)
    let name'     = (_name     . fromEitherDecode) d
        resx'     = (_resx     . fromEitherDecode) d
        resy'     = (_resy     . fromEitherDecode) d
        camMode'  = (_camMode  . fromEitherDecode) d
        models'   = (_models   . fromEitherDecode) d
        preObjs'  = (_objects  . fromEitherDecode) d
        bgrObjs'  = (_background . fromEitherDecode) d
        gui'      = (_gui      . fromEitherDecode) d
--        fonts'    = (_fonts    . fromEitherDecode) d
        cameras'  = (_cameras  . fromEitherDecode) d
    return $
      Project
      name'
      resx'
      resy'
      camMode'
      models'
      (sortOn (view uuid ) preObjs')
      (sortOn (view uuid ) bgrObjs')
      gui'
--      fonts'
      cameras'
      
      where
        fromEitherDecode = fromMaybe emptyProject . fromEither
        fromEither d =
          case d of
            Right pt -> Just pt            
            _ -> Nothing
