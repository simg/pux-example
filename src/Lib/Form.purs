module App.Form

where

import Foundation

import Prelude (($), (+), (-), const, show, map)
import Pux.Html (Html, form, div, p, ul, li, span, button, text, input, select, option, label)
import Pux.Html.Attributes (className, type_, name, checked, id_, htmlFor, value, maxLength, action, method)
import Pux.Html.Attributes (label) as A
import Pux.Html.Events (FormEvent, onClick, onInput, onChange)

type State = {
  elements :: Array Field
}

data Action
  = UpdateFieldValue Int FormEvent
  --| AddListItem 
  --| RemoveListItem Int

data Field 
    = Group GroupOptions (Array Field)
    | Text TextBoxOptions (Maybe String)
    | YesNo YesNoOptions (Maybe Boolean)
    | MultiChoice MultiChoiceOptions (Array String)
    | Rating RatingOptions (Maybe Int)
    --| Select (Maybe Int) (Array String)
    --| SortSelect (Maybe Int) (Array String)
    --| NumberBox Int
--derive instance genericField :: Generic Field

newtype FieldError = FieldError {
  pos :: Int
, err :: String
}

type NumberRange = {
  min :: Maybe Int
, max :: Maybe Int
}

type GroupOptions = {
  label :: String
}

type FieldOptions = {
  label :: String
}

type TextBoxOptions = {
  label :: String
, maxLength :: Int
}

type YesNoOptions = {
  label :: String
}


type RatingOptions = {
  label :: String
, range :: NumberRange
}

type MultiChoiceOptions = {
  label :: String  
, maxChoices :: Maybe Int
, options :: Array Choice
}

type Choice = {
  value :: Maybe String
, desc :: String
}


update :: Action -> State -> State
update action form = case action of
    UpdateFieldValue i ev -> 
      case form.elements !! i of -- TODO: need to be able to nest forms
        Just (Text o v ) -> form { elements = fromMaybe form.elements $ updateAt i (Text o (Just ev.target.value)) form.elements }
        Just (MultiChoice o v) -> form { elements = fromMaybe form.elements $ updateAt i (MultiChoice o [ev.target.value]) form.elements }
        _ -> form


view :: State -> Html Action
view form = div [] $ mapWithIndex (renderField "") form.elements

--TODO: reconsider this
fieldStringValue :: Field -> String 
fieldStringValue f = case f of
  Text o v -> case v of
                Just v -> v
                Nothing -> "null"
  YesNo o v -> case v of 
                Just v -> show v
                Nothing -> "null"
  MultiChoice o v -> show v
  Rating o v -> case v of
                  Just v -> show v
                  Nothing -> "null"
  Group o fs -> "null"

renderField :: String -> Int -> Field -> Html Action
renderField ref i f = 
  case f of 
    Text o v -> div [ className "field" ] [
                  label [ htmlFor refName ] [ text o.label ]
                , input [ type_ "text"
                    , value (fromMaybe "" v)
                    , maxLength (show o.maxLength)
                    , name $ refName
                    , onChange (UpdateFieldValue i)
                    ] []
                ]
    YesNo o v -> div [ className "field" ] [ 
                  select [] [ option [] [ text "" ] 
                          , option [] [ text "Yes" ]
                          , option [] [ text "No" ] 
                          ]
                ]
    MultiChoice o vs -> div [ className "field" ] [
                          p [] [ text o.label ]
                        , ul [] $ mapWithIndex (\j opt -> li [] [ input [
                                                       type_ "radio" 
                                                     , name refName --<> "_" <> (show j)
                                                     , id_ (refId j)
                                                     , onChange (UpdateFieldValue i)
                                                     , checked $ isChecked (optVal opt) vs
                                                     , value (optVal opt) 
                                                     ] [] 
                                                  , label [ htmlFor (refId j) ] [ text opt.desc ]
                                                  ]) o.options
                        ]
    Rating o r -> div [ className "field" ] [ text ("Rate from " <> show o.range.min <> " to " <> show o.range.max) ]
    Group o fs ->  div [] $ mapWithIndex (renderField refName) fs

    where
      refName = ref <> "_" <> show i
      refId j = refName <> "_" <> show j
      optVal o = fromMaybe o.desc o.value
      isChecked v vs = v `elem` vs