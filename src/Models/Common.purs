module App.Models.Common where

import Foundation

--import Data.Argonaut.Encode
--import Data.UUID (parseUUID, genUUID, GENUUID, UUID)


type EntityId = Maybe String

devjwt :: String 
devjwt = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InNpbW9uQGhvbGlzdGljc3lzdGVtcy5jby51ayIsInVpZCI6MiwibW9kZSI6InJ3IiwiY2hhbm5lbCI6Il91c2VyXzIiLCJyb2xlIjoicG9zdGdyZXMifQ.Sqd1G9qnBoac6xuO46gvcZz9llBSXKyEqJuui7jvPds"


{-
instance uuidIsForeign :: IsForeign UUID where
  read val = parseUUID val
-}