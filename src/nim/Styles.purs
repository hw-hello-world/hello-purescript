module Nim.Styles where

import Data.Semigroup ((<>))
import Halogen (ClassName(..))

button :: ClassName
button = ClassName "button"
buttonDanger = ClassName "is-button-danger"
buttonSecondary = ClassName "is-button-secondary"
buttonSecondaryDanger = buttonDanger <> buttonSecondary


fieldset = ClassName "fieldset"
fieldsetFlex = ClassName "fieldset-flex"

label = ClassName "label"
textInput = ClassName "text-input"
