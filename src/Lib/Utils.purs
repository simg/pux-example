module App.Utils

where

import Foundation

import Control.Monad.Except

import Data.Argonaut.Encode
import Data.StrMap as SM

import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL

import Pux (App, Config, CoreEffects, EffModel, noEffects, mapEffects, mapState, fromSimple, renderToDOM, start, Update)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Html (Html, (!), (#), h1, h2, h3, nav, section, div, a, ul, li, p, span, text, html, button)
import Pux.Html.Attributes
import Pux.Router (sampleUrl, link)

import WebSocket


foreign import foreignStringify :: Foreign -> String


--TODO: rationalise the api call methods into a single one (mostly like apiCall)
apiRequest :: forall e. String -> String -> Maybe String -> Maybe String -> Aff (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e) (AffjaxResponse String)
apiRequest host endPoint jwt content = affjax $ defaultRequest 
    { 
      method = Left POST
    , url = "http://" <> host <> "/" <> endPoint
    , content = Just $ fromMaybe "{}" content
    , headers = [ ContentType applicationJSON, RequestHeader "Authorization" $ "Bearer " <> (fromMaybe "" jwt) ] -- , Accept $ MediaType "*/*" ]
    }

apiCall :: forall e. Method -> String -> String -> Maybe String -> Maybe String -> Aff (ajax :: AJAX | e) (AffjaxResponse String)
apiCall method host endPoint jwt content = affjax $ defaultRequest 
    { 
      method = Left method
    , url = "http://" <> host <> "/" <> endPoint
    , content = Just $ fromMaybe "{}" content
    , headers = [ ContentType applicationJSON, RequestHeader "Authorization" $ "Bearer " <> (fromMaybe "" jwt) ] -- , Accept $ MediaType "*/*" ]
    }     

{-
apiGet :: forall e. String -> String -> Maybe String -> Maybe String -> Aff (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e) (AffjaxResponse String)
apiGet host endPoint jwt queryString = affjax $ defaultRequest 
    { 
      method = Left GET
    , url = "http://" <> host <> "/" <> endPoint <> query
    --, content = Just $ fromMaybe "{}" content
    , headers = [ ContentType applicationJSON, RequestHeader "Authorization" $ "Bearer " <> (fromMaybe "" jwt) ] -- , Accept $ MediaType "*/*" ]
    }
    where 
      query = case queryString of
                Just str -> "/?" <> str
                Nothing  -> ""
-}

apiRequest2 :: forall e. String -> String -> Maybe String -> Maybe String -> Aff (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e) (AffjaxResponse Foreign)
apiRequest2 host endPoint jwt content = affjax $ defaultRequest 
    { 
      method = Left POST
    , url = "http://" <> host <> "/" <> endPoint
    , content = Just $ fromMaybe "{}" content
    , headers = [ ContentType applicationJSON, RequestHeader "Authorization" $ "Bearer " <> (fromMaybe "" jwt) ] -- , Accept $ MediaType "*/*" ]
    }    


baseUrl :: String
baseUrl = baseDomain <> ":" <> basePort

baseWS :: String
baseWS = baseDomain <> ":" <> basePort

baseDomain :: String
baseDomain = "dev.sophont.co.uk"
--baseDomain = "192.168.12.1"

basePort :: String
basePort = "3000"


{-
parseJsonUUID :: String -> ExceptT (NonEmptyList ForeignError) UUID
parseJsonUUID u = exceptNoteA ((Identity <<< parseUUID) u) $ NEL.singleton $ JSONError "invalid uuid"
-}


-- | Add a conditional version of Argonaut's extend / ~> that does not encode null values
maybeExtend :: forall a. EncodeJson a => JAssoc -> a -> Json
maybeExtend (Tuple k v) = case isNull v of 
  false -> foldJsonObject
              (jsonSingletonObject k v)
              (SM.insert k v >>> fromObject)
              <<< encodeJson
  true -> encodeJson

-- | Infix version of maybeExtend
infixr 6 maybeExtend as ~>~