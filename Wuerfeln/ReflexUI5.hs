{-# LANGUAGE OverloadedStrings #-}

module ReflexUI5 (  headUI5
                  , buttonUI5) where

import           Reflex.Dom
import qualified Data.Map as Map
import qualified Data.Text as T


headUI5 :: MonadWidget t m => m ()
headUI5 = do
             elAttr "meta" (Map.fromList [
                ("http-equiv", "X-UA-Compatible"),
                ("content", "IE=edge")
                 ]) $ blank
             el "title" $ text "UI5 Web Components"
             elAttr "script" (Map.fromList [
                 ("type", "module"),
                 ("src", "https://sap.github.io/ui5-webcomponents/resources/sap/ui/webcomponents/main/bundle.esm.js")
                  ]) $ blank
             elAttr "script" (Map.fromList [
                 ("nomodule", ""),
                 ("src", "https://sap.github.io/ui5-webcomponents/resources/sap/ui/webcomponents/main/bundle.es5.js")
                  ]) $ blank

buttonUI5 :: DomBuilder t m => T.Text -> m (Event t ())
buttonUI5 t = do
  (e, _) <- element "ui5-button" def $ text t
  return $ domEvent Click e

inputUI5 :: MonadWidget t m => InputUI5config t -> m (InputUI5 t)
inputUI5 config = do 
    (e, _) <- elDynAttr' "ui5-input" blank
    let eChange = domEvent Change e
        eSubmit = domEvent Submit e
    return InputUI5 

-- Types
data InputUI5 t = InputUI5 {
      _textInput_value          :: Dynamic t T.Text
    , _textInput_change         :: Event t T.Text
--    , _textInput_liveChange     :: Event t Word
    , _textInput_submit         :: Event t T.Text
--    , _textInput_hasFocus       :: Dynamic t Bool
--    , _textInput_builderElement :: InputElement EventResult GhcjsDomSpace t
}

data InputUI5config t = InputUI5config {
      _textInputConfig_inputType    :: T.Text
    , _textInputConfig_initialValue :: T.Text
    , _textInputConfig_setValue     :: Event t T.Text
    , _textInputConfig_attributes   :: Dynamic t (Map.Map T.Text T.Text)
}

