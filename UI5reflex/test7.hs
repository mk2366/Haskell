{-# LANGUAGE OverloadedStrings, RecursiveDo #-}
import           Reflex.Dom
import qualified Data.Text as T


main :: IO ()
main = mainWidget $ do
                let dText = constDyn "ToDo"
                todoItem dText 



todoItem :: MonadWidget t m 
         => Dynamic t T.Text 
         -> m ()
todoItem dText = elClass "div" "todo-item" $ mdo

  (e, _) <- elDynClass' "div" dClass $
    dynText dText

  let 
    eDoubleClick = domEvent Dblclick e

  eRemove <- button "Remove"

  dClass <- holdDyn "" . leftmost $ [
              ""        <$ eDoubleClick
            , "removed" <$ eRemove
            ]

  pure ()
