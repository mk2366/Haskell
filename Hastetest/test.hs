-- This program will ask the user for their name, and echo a greeting back into
-- this text box as a comment.

import Haste
import Haste.DOM

main = do
  name <- prompt "Hello user, what is your name?"
  withElem "hastebox" $ \box -> do
    oldtext <- getProp box "value"
    setProp box "value" (oldtext ++ "\n-- Hello " ++ name)
