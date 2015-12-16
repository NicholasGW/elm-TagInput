module TagInput where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (..)
import StartApp.Simple as StartApp
import Json.Decode as Json


type alias Tag = { id : Int,
                   text : String
                 }
type alias Model = { tags: List Tag,
                     inputText: String
                   }
type Action
     = NoOp
     | Add
     | Edit String
     | Remove Int


model: Model
model = { tags = [],
          inputText = ""
        }


update: Action -> Model -> Model
update action model =
  case action of

    NoOp -> model

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

    Edit string ->
       { model | inputText = string }

    Remove id ->
      { model | tags = List.filter (\tag -> tag.id /= id) model.tags }


view: Address Action -> Model -> Html
view address model =
  div [] (tagInput address model.inputText :: tagList address model.tags)


tagInput: Address Action -> String -> Html
tagInput address inputText =
    input [ onEnter address,
            onEdit address Edit,
            value inputText] []

tagList: Address Action -> List Tag -> List Html
tagList address tags =
  List.map (tagToHtml address) tags

isEnter: Int -> Action
isEnter keycode =
  case keycode of
    13 ->
      Add
    _ ->
      NoOp

onEdit: Address Action -> (String -> Action) -> Attribute
onEdit address value =
  on "input" targetValue (\str -> Signal.message address (value str))

onEnter: Address Action -> Attribute
onEnter address =
  on "keydown" keyCode (\key -> Signal.message address (isEnter key))

onRemove: Address Action -> Int -> Attribute
onRemove address id =
    onClick address (Remove id)

tagToHtml: Address Action -> Tag -> Html
tagToHtml address tag =
    div [onRemove address tag.id] [text tag.text]


main =
    StartApp.start { model = model, update = update, view = view }
