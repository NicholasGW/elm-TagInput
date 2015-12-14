module TagInput where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import StartApp.Simple as StartApp
import Json.Decode as Json
import Debug exposing (..)


type alias Tag = { id: Int, text: String}
type alias Model = { tags: List Tag , text: String }
type Action = Edit String | Add | Remove Int | NoOp

model: Model
model = {
        tags = []
        , text = ""
      }


update: Action -> Model -> Model
update action model =
  case action of

    Edit string ->
       { model | text = string }

    Add ->
      let
        id = (List.length model.tags) + 1
        text = model.text
        newTag = { id = id, text = text }
      in
        { model |
            tags = newTag :: model.tags,
            text = model.text
        }

    Remove id ->
      { model | tags = List.filter (\tag -> tag.id /= id) model.tags }

    NoOp ->
      model


isEnter: Int -> Action
isEnter keycode =
  case keycode of
    13 ->
      Add
    _ ->
      NoOp

tagsToHtml: List Tag -> Signal.Address Action -> List Html
tagsToHtml tags address =
  List.map (\tag -> div [onClick address (Remove tag.id)] [text tag.text]) tags

view: Signal.Address Action -> Model -> Html
view address model =
  div [] (input [ on "input" targetValue (\str -> Signal.message address (Edit str)),
                  on "keypress" keyCode (\code -> Signal.message address (isEnter code)),
                  value model.text] []
                   :: tagsToHtml model.tags address)

main =
    StartApp.start { model = model, update = update, view = view }
