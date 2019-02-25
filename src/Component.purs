module Component where

import Prelude

import DOM.HTML.Indexed as DOM
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXResponse
import Network.HTTP.RequestHeader as RH
import Nim.Styles as Styles

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | FetchUsers a
  | AddUser a

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

{- Button -}
data OButtonType = OPrimary | OSecondary | ODanger | OSecondaryDanger
type OButtonOption f = { btnLabel :: String
                         , btnDisabled :: Boolean
                         , btnClicked :: HQ.Action f
                         , btnClass :: OButtonType
                         }

oButton :: forall f p. OButtonOption f -> HH.HTML p (f Unit)
oButton btnOpt =
  HH.button
  [ HP.disabled btnOpt.btnDisabled
  , HP.type_ HP.ButtonButton
  , HP.class_ (H.ClassName ("button" <> btnClassName))
  , HE.onClick (HE.input_ btnOpt.btnClicked)
  ]
  [ HH.text btnOpt.btnLabel ]
  where btnClassName = case btnOpt.btnClass of
          ODanger -> " is-button-danger"
          OSecondary -> " is-button-secondary"
          OSecondaryDanger -> " is-button-danger is-button-secondary"
          _ -> ""

{- Form -}
{- TextInput -}
type OTextInputOpt a f = { label :: String
                       , name :: String
                       , value :: String
                       , action :: (a -> HQ.Action f)
                       }
oTextInput textInputOpt =
  HH.fieldset
      [ HP.class_ Styles.fieldset ]
      [ HH.div
        [ HP.class_ Styles.fieldsetFlex ]
        [ HH.input
          [ HP.class_ Styles.textInput
          , HP.type_ HP.InputText
          , HP.name textInputOpt.name
          , HP.required true
          , HP.value textInputOpt.value
          , HE.onValueInput (HE.input textInputOpt.action)
          ]
        , HH.label
          [ HP.for textInputOpt.name
          , HP.class_ Styles.label
          ]
          [ HH.text textInputOpt.label ]
        ]
      ]

{- Table -}

render :: State -> H.ComponentHTML Query
render st =
  HH.form_ $
    [ HH.h1_ [ HH.text "Lookup Okta user" ]
    , (oTextInput { label: "Enter Username", name: "name", value: st.username, action: SetUsername })
    , (oButton { btnDisabled : st.loading, btnLabel : "Fetch Users", btnClicked: FetchUsers, btnClass: OPrimary })
    , (oButton { btnDisabled : false, btnLabel : "Add User", btnClicked : FetchUsers, btnClass: OSecondary })
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
  AddUser next -> do
    pure next
  FetchUsers next -> do
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
