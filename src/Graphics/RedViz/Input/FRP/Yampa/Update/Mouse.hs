{-# LANGUAGE Arrows #-}

module Graphics.RedViz.Input.FRP.Yampa.Update.Mouse
  ( updateMouse
  ) where

import FRP.Yampa

import Graphics.RedViz.Input.FRP.Yampa.AppInput
import Graphics.RedViz.Input.Mouse

updateMouse :: SF AppInput (Mouse, [Event (Double, Double)])
updateMouse =
  proc input -> do
    lmbE <- lbpPos       -< input
    rmbE <- rbpPos       -< input
    mmovE <- mouseMoving -< input

    mpos' <- mousePos     -< input
    rpos' <- mouseRelPos  -< input

    let
      events = [lmbE, rmbE, mmovE]
      mouse' =
        Mouse
        (case isEvent lmbE of
           True -> Just $ fromEvent lmbE
           _    -> Nothing)
        (case isEvent rmbE of
           True -> Just $ fromEvent rmbE
           _    -> Nothing)
        mpos'
        rpos'
        (isEvent mmovE)
        []
    returnA -< (mouse', events)
    --returnA -< (mouse, DT.trace ("mouse :" ++ show events) events)    

