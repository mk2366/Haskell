{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))


bodyElement =  el "div" $ do
    rec
      el "h2" $ text "Some Tracing"
      let group = "g"
      let dynAttrs = styleMap <$> dynColor
      evRad1 <- radioBtn "orange" group Orange dynAttrs
      evRad2 <- radioBtn "green" group Green dynAttrs
      evRad3 <- radioBtn "red" group Red dynAttrs
      let evRadio = (T.pack . show) <$> leftmost [evRad1, evRad2, evRad3]
  
      -- added line:
      let evRadioT = traceEvent ("Clicked rb in group " <> T.unpack group) evRadio
  
      -- modified line: evRadioT instead of evRadio 
      dynColor <- holdDyn "lightgrey" evRadioT
  
    return ()