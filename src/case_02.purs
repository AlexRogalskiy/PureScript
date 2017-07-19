module Main2 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

data BasicData = SomeConstructor
instance showBasicData :: Show BasicData where
	show SomeConstructor = "SomeConstructor"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  --log "Hello sailor!"
  log $ show SomeConstructor
