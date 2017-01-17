module App.Models.User where

import Foundation

import Data.Argonaut.Encode 
import Data.String as Str

import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL


import App.Models.Common
import App.Utils



-------------------------------------------------------------------------------------------------------
-- | Data 
-------------------------------------------------------------------------------------------------------

data User = User {
  id_   :: EntityId
, name  :: String
, pass  :: Maybe String
, email :: Maybe String
, icon  :: Maybe String
, role  :: UserRole
}
derive instance genericUser :: Generic User
instance eqUser :: Eq User where eq = gEq


instance encodeUser :: EncodeJson User where
  encodeJson (User a) 
    =   "id"    := a.id_
    ~>~ "name"  := a.name
    ~>  "pass"  := "foobar" --TODO: need to think about this
    ~>  "email" := a.email
    ~>  "icon"  := a.icon
    ~>  "role"  := "postgres" --TODO:roles
    ~>  jsonEmptyObject

instance userIsForeign :: IsForeign User where
  read val = do
    id_   <- readString =<< readProp "id"    val
    name  <- readString =<< readProp "name"  val
    pass  <- readString =<< readProp "pass"  val
    email <- readString =<< readProp "email" val
    icon  <- readString =<< readProp "icon"  val
    --roles <- readProp "roles" val
    pure $ User { id_:Just id_, name:name, pass:Just pass, email:Just email, icon:Just icon, role:Admin }

data UserRole = Anonymous | Admin | Contributor
derive instance genericUserOption :: Generic UserRole


data UserListFilterOptions = UserListFilterOptions {
  id_  :: Maybe String
, name :: Maybe String
}
derive instance genericUserListFilterOptions :: Generic UserListFilterOptions

{-
instance encodeUserListFilterOptions :: EncodeJson UserListFilterOptions where
  encodeJson (UserListFilterOptions a) 
    =   "id" : a.id_
    ~>~ "name" := a.name
    ~>~ jsonEmptyObject
-}

-------------------------------------------------------------------------------------------------------
-- | API
-------------------------------------------------------------------------------------------------------

-- TODO: find a way to plumb this in properly

saveUser :: forall e. User -> Aff (ajax :: AJAX, console :: CONSOLE | e) (Either String User)
saveUser (User user) = do
  res <- apiCall method baseUrl endPoint (Just devjwt) $ Just <<< show $ encodeJson (User user)
  liftEff $ log $ "save user"
  pure $ case Str.length res.response of 
           0 -> Right (User user) -- TODO: possibly get an updated user from the api
           _ -> Left "Kind of Success"
  where 
    endPoint = case user.id_ of 
                 Just uid -> "users?id=eq." <> uid
                 Nothing  -> "users"
    method  = case user.id_ of
                 Just _ -> PATCH
                 Nothing -> POST 

fetchUser :: forall e. String -> Aff (ajax :: AJAX, console :: CONSOLE | e) (Either String User)
fetchUser uid = do
  --res <- apiGet baseUrl "users" (Just devjwt) (Just $ "id=" <> uid)
  res <- apiCall GET baseUrl endpoint (Just devjwt) Nothing
  case runExcept ( readJSON res.response :: F (Array User) ) of
    Left err -> do
      liftEff $ log $ "invalid user list" <> res.response <> " : " <> show err
      pure $ Left $ "invalid user list" <> res.response <> " : " <> show err
    Right users ->
      case head users of --TODO: this is clumsy
        Just user -> pure $ Right user
        Nothing -> pure $ Left "user not found"
  where
    endpoint = "users?id=eq." <> uid

fetchUsers :: forall e. UserListFilterOptions -> Aff (ajax :: AJAX, console :: CONSOLE | e) (Either String (Array User))
fetchUsers (UserListFilterOptions lo) = do
  --res <- apiGet baseUrl "users" Nothing Nothing --(Just <<< show $ encodeJson o)
  res <- apiCall GET baseUrl "users" (Just devjwt) Nothing
  case runExcept ( readJSON res.response :: F (Array User) ) of
    Left err -> do
      liftEff $ log $ "invalid user list" <> res.response <> " : " <> show err
      pure $ Left $ "invalid user list" <> res.response <> " : " <> show err
    Right users -> do
      liftEff $ log $ "valid users"                 
      pure $ Right users
  




  
  