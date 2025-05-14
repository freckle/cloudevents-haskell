{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Freckle.CloudEvent.V1.Event.Lens where

import Freckle.CloudEvent.V1.Event.Data (CloudEvent)
import Control.Lens (makeLensesFor)

makeLensesFor
  [ ("ceId", "_ceId")
  , ("ceSource", "_ceSource")
  , ("ceSpecversion", "_ceSpecversion")
  , ("ceType", "_ceType")
  , ("ceDatacontenttype", "_ceDatacontenttype")
  , ("ceDataschema", "_ceDataschema")
  , ("ceSubject", "_ceSubject")
  , ("ceTime", "_ceTime")
  , ("ceData", "_ceData")
  , ("ceData_base64", "_ceData_base64")
  , ("extensionAttributes", "_extensionAttributes")
  ]
  ''CloudEvent
