module Component where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXResponse

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
    , initializer: Nothing
    , finalizer: Nothing
    }

render :: State -> H.ComponentHTML Query () Aff
render st =
  HH.form_ $
    [ HH.h1_ [ HH.text "Lookup GitHub user" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter username:" ]
        , HH.input
            [ HP.value st.username
            , HE.onValueInput (HE.input SetUsername)
            ]
        ]
    , HH.button
        [ HP.disabled st.loading
        , HP.type_ HP.ButtonButton
        , HE.onClick (HE.input_ MakeRequest)
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text (if st.loading then "Working..." else "") ]
    , HH.div_
        case st.result of
          Nothing -> []
          Just res ->
            [ HH.h2_
                [ HH.text "Response:" ]
            , HH.pre_
                [ HH.code_ [ HH.text res ] ]
            ]
    ]

eval :: Query ~> H.HalogenM State Query () Void Aff
eval = case _ of
  SetUsername username next -> do
    H.modify_ (_ { username = username, result = Nothing :: Maybe String })
    pure next
  MakeRequest next -> do
    username <- H.gets _.username
    H.modify_ (_ { loading = true })
    response <- H.liftAff $ AX.get AXResponse.string ("https://api.github.com/users/" <> username)
    H.modify_ (_ { loading = false, result = Just response.response })
    pure next
