module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Foreign.Class (class AsForeign, class IsForeign, write)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Global.Unsafe(unsafeStringify)

newtype MyRecords = MyRecord {a :: Int}
derive instance genericMyRecord :: Generic MyRecord _
instance showMyRecord :: Show MyRecord where
	show = genericShow
instance isForeignMyRecord :: IsForeign MyRecord where
	read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}
instance asForeignMyRecord :: AsForeign MyRecord where
	write = toForeigntGeneric $ defaultOptions {unwrapSingleConstructors = true}

toJSONString = write >> unsafeStringify

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  log $ toJSONString (MyRecord {a : 1})