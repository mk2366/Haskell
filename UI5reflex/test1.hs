{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
import           Reflex.Dom
import qualified Data.Map as Map
import           Prelude (IO, (=<<), ($), return, Maybe(..))
import qualified Data.Text as T

main :: IO ()
main = mainWidgetWithHead headUI5 body

body :: MonadWidget t m => m ()
body = do 
    ev1  <- buttonUI5 "Web Components are great"
    el "br" blank
    text "Number of clicks: "
    dyn1 <- count ev1
    display dyn1

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