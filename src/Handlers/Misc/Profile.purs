module Handlers.Profile where

import Foundation
import Handlers.Common

import App.Form as Form

-------------------------------------------------------------------------------------------
-- State 
-------------------------------------------------------------------------------------------

type State = {
  formState :: Form.State
} 

data Action = Submit
            | ChildForm Form.Action
            | Nop


type Profile = {
  uid :: Int
, name :: String
, email :: String
, icon :: String
}

init :: State
init = { formState: formInit Nothing }


formInit :: Maybe Profile -> Form.State
formInit profile = { elements: [
    --Form.Group { label: "Personal Details" } [
      Form.Text { label:"Name", maxLength:100 } (_.name <$> profile)
    , Form.Text { label:"Email address", maxLength:100 } (_.email <$> profile)
    , Form.MultiChoice { label:"Industries in which you operate"
                       , maxChoices:Nothing
                       , options:[ { value:Nothing, desc:"Manufacturing" }
                                 , { value:Nothing, desc:"Construction" }
                                 , { value:Nothing, desc:"Retail" }
                                 , { value:Nothing, desc:"Real Estate" }
                                 , { value:Nothing, desc:"Conglomerate" }
                                 ]
                       } [] 

    , Form.MultiChoice { label: "Size of your organisation"
                       , maxChoices:Just 1
                       , options: [ { value:Nothing, desc:"Small: 1-10 staff" }
                                  , { value:Nothing, desc:"Expanding: 10-30 staff" }
                                  , { value:Nothing, desc:"Medium: 30-100 staff" }
                                  , { value:Nothing, desc:"Large: 100-300 staff" }
                                  , { value:Nothing, desc:"Very large: 300-5000 staff" }
                                  , { value:Nothing, desc:"Multi-national / Government: 5000+ staff" }
                                  ] 
                      } []
    , Form.MultiChoice { label: "Your interests"
                       , maxChoices:Nothing
                       , options: [ { value:Nothing, desc:"Digital Transformation" }
                                  , { value:Nothing, desc:"Big Data" }
                                  , { value:Nothing, desc:"Information Assurance" }
                                  ]
                      } []
     , Form.MultiChoice { label: "Your role"
                        , maxChoices:Nothing
                        , options: [
                            { value:Nothing, desc:"Administrative" }
                          , { value:Nothing, desc:"Executive" }
                          , { value:Nothing, desc:"Technical" }
                          , { value:Nothing, desc:"Sales" }
                          , { value:Nothing, desc:"Operations" }
                          ]
                        } []
     , Form.MultiChoice { label: "Your Technical Level" 
                       , maxChoices:Just 1
                       , options: [
                           { value:Nothing, desc:"Beginner" }
                         , { value:Nothing, desc:"Intermediate" }
                         , { value:Nothing, desc:"Advanced" }
                         , { value:Nothing, desc:"IT Professional" }
                         , { value:Nothing, desc:"Hacker" }
                         ]
                       } []
     , Form.MultiChoice { label: "Your organisations digital capabilities"
                        , maxChoices:Nothing
                        , options: [
                            { value:Nothing, desc:"Customer Relationship system" }
                          , { value:Nothing, desc:"Company-wide email" }
                          , { value:Nothing, desc:"Website" }
                          , { value:Nothing, desc:"Content Management system" }
                          , { value:Nothing, desc:"Intranet" }
                          , { value:Nothing, desc:"Resource Planning" }
                          , { value:Nothing, desc:"Fax machine" }
                          , { value:Nothing, desc:"Don't know" }
                          ]
                        } []
     , Form.YesNo { label: "Do you have an approved budget for your project?" } Nothing
     , Form.MultiChoice { label: "What is the budget for your project?"
                        , maxChoices:Just 1
                        , options: [
                            { value:Nothing, desc:"Less than £1K" }
                          , { value:Nothing, desc:"£1K - £5K" }
                          , { value:Nothing, desc:"£5K - £10K" }
                          , { value:Nothing, desc:"£10K - £20K" }
                          , { value:Nothing, desc:"£20K - £50K" }
                          , { value:Nothing, desc:"£50K - £100K" }
                          ]
                        } []
     , Form.MultiChoice { label: "Where do you operate?"
                        , maxChoices: Nothing
                        ,  options: [
                             { value:Nothing, desc:"UK" }
                           , { value:Nothing, desc:"EMEA" }
                           , { value:Nothing, desc:"USA" }
                           , { value:Nothing, desc:"ASIAPAC" }
                           ]
                        } []
     , Form.YesNo { label: "Do you have a current project in mind" } Nothing
     , Form.MultiChoice { label: "What are you hoping to achieve with this project?"
                        , maxChoices: Nothing
                        , options: [
                            { value:Nothing, desc:"Cost savings" }
                          , { value:Nothing, desc:"Brand improvement" }
                          , { value:Nothing, desc:"Quality improvement" }
                          , { value:Nothing, desc:"Customer Loyalty" }
                          , { value:Nothing, desc:"Recruitment" }
                          , { value:Nothing, desc:"Sales" }
                          ]
                        } []
     , Form.MultiChoice { label: "What are the main challenges facing your organisation"
                        , maxChoices: Just 3
                        , options: [
                            { value:Nothing, desc:"Recruitment" }
                          , { value:Nothing, desc:"Sales" }
                          , { value:Nothing, desc:"Customer Loyalty" }
                          , { value:Nothing, desc:"Quality Control" }
                          , { value:Nothing, desc:"Information Distribution" }
                          , { value:Nothing, desc:"All of the above" }
                          ]
                        } []
  ]}

-------------------------------------------------------------------------------------------
-- State Transformations
-------------------------------------------------------------------------------------------

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update Submit state = 
  noEffects $ state 

update (ChildForm n) state = 
  noEffects $ state { formState = Form.update n state.formState } 

update Nop state = 
  noEffects $ state  

-------------------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------------------

view :: State -> Html Action
view state = 
  div [] [ 
    h1 [] [ text "Profile" ]
  , p [] [ text "Please tell us about yourself" ]
  , map ChildForm $ Form.view $ spy state.formState
  , button [ onClick (const Submit) ] [ text "Save" ]
  ]  

                          

  