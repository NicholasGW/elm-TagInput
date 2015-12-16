module TagInput where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import StartApp.Simple as StartApp
import Json.Decode as Json


type alias Tag = { id: Int, text: String}
type alias Model = { tags: List Tag , inputText: String }
type Action = Edit String | Add | Remove Int | NoOp

model: Model
model = {
        tags = []
        , inputText = ""
      }


update: Action -> Model -> Model
update action model =
  case action of

    Edit string ->
       { model | inputText = string }

    Add ->
      let
        id = (List.length model.tags) + 1
        text = model.inputText
        newTag = { id = id, text = text }
      in
        { model |
            tags = newTag :: model.tags,
            inputText = ""
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

onEdit: Signal.Address Action -> (String -> Action) -> Attribute
onEdit address value =
  on "input" targetValue (\str -> Signal.message address (value str))

onEnter: Signal.Address Action -> Attribute
onEnter address =
  on "keydown" keyCode (\key -> Signal.message address (isEnter key))

onRemove: Signal.Address Action -> Int -> Attribute
onRemove address id =
    onClick address (Remove id)

tagToHtml: Signal.Address Action -> Tag -> Html
tagToHtml address tag =
    div [onRemove address tag.id] [text tag.text]


view: Signal.Address Action -> Model -> Html
view address model =
  div [] (input [ onEdit address Edit,
                  onEnter address,
                  value model.inputText] []
                   :: List.map (tagToHtml address) model.tags )

main =
    StartApp.start { model = model, update = update, view = view }
