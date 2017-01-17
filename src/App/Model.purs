module App.Model (
  module App.Model
, module App.Models.User
, module App.Models.Message
, module App.Models.Notification
)

where

import Foundation 

import Data.Argonaut.Core
import Data.Argonaut.Encode


import App.Models.User
import App.Models.Message
import App.Models.Notification


data StatusMessage = StatusMessage {
  type_  :: MessageType
, title  :: String
, detail :: Maybe String
}

data MessageType = InfoMessage | WarnMessage | ErrorMessage
instance showMessageType :: Show MessageType where 
  show InfoMessage  = "info"
  show WarnMessage  = "warn"
  show ErrorMessage = "error"