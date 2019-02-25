module Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Data.HTTP.Method (Method(..), CustomMethod)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXResponse
import Network.HTTP.RequestHeader as RH

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | MakeRequest a

initialState :: State
initialState = {
  loading: false,
  username: "",
  result: Nothing
}

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

render :: State -> H.ComponentHTML Query
render st =
  HH.form_ $
    [ HH.h1_ [ HH.text "Lookup Okta user" ]
    , HH.fieldset
      [ HP.class_ (H.ClassName "fieldset") ]
      [ HH.div
        [ HP.class_ (H.ClassName "fieldset-flex") ]
        [ HH.input
          [ HP.class_ (H.ClassName "text-input")
          , HP.type_ HP.InputText
          , HP.name "name"
          , HP.required true
          , HP.value st.username
          , HE.onValueInput (HE.input SetUsername)
          ]
        , HH.label
          [ HP.for "name"
          , HP.class_ (H.ClassName "label")
          ]
          [ HH.text "Enter Username"]
        ]
      ]
    , HH.button
        [ HP.disabled st.loading
        , HP.type_ HP.ButtonButton
        , HP.class_ (H.ClassName "button" )
        , HE.onClick (HE.input_ MakeRequest)
        ]
        [ HH.text "Fetch info" ]
    , HH.button
      [ HP.type_ HP.ButtonButton
      , HP.class_ (H.ClassName "button" )
      --, HE.onClick (HE.input_ MakeRequest)
      ]
      [ HH.text "Add User" ]
    , HH.p_
        [ HH.text (if st.loading then "Working..." else "") ]
    , HH.div_
        case st.result of
          Nothing -> []
          Just res ->
            [ HH.h2_
                [ HH.text "Found Response:" ]
            , HH.pre_
                [ HH.code_ [ HH.text res ] ]
            ]
    ]

eval :: Query ~> H.ComponentDSL State Query Void Aff
eval = case _ of
  SetUsername username next -> do
    H.modify_ (_ { username = username, result = Nothing :: Maybe String })
    pure next
  MakeRequest next -> do
    username <- H.gets _.username
    H.modify_ (_ { loading = true })
    response <- H.liftAff $ oktaUsers username
    H.modify_ (_ { loading = false, result = Just response.response })
    pure next

oktaUsers :: String -> AX.Affjax String
oktaUsers query = AX.affjax AXResponse.string (oktaRequest { url = "/api/v1/users?q=" <> query })


oktaRequest :: AX.AffjaxRequest
oktaRequest =
  { method: Left GET
  , url: "/"
  , headers: []
  , content: Nothing
  , username: Nothing
  , password: Nothing
  , withCredentials: true
  }
