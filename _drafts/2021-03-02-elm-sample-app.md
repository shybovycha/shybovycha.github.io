---
layout: post
title: "ShootThem! revival"
date: '2020-11-13T13:46:24+11:00'
tags: [frontend, functional-programming, elm]
---

## Step 1: skeleton

```hs
module Main exposing (..)

import Browser
import Html exposing (Html, text)

type alias Model = ...

type Msg = ...

init : Model
init = ...

view : Model -> Html Msg
view model = text "Hello, translations!"

update : Msg -> Model -> Model
update msg model =
  case msg of
    ...

main = Browser.sandbox { init = init, update = update, view = view }
```

Note that the above code won't work

## Step 2: define the model

```hs
module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, text)

type alias Translations = Dict String String

type alias Model = Translations

type Msg = ...

init : Model
init = Dict.empty

view : Model -> Html Msg
view model = text "Hello, translations!"

update : Msg -> Model -> Model
update msg model =
  case msg of
    ...

main = Browser.sandbox { init = init, update = update, view = view }
```

The above code still won't work

Elm has its own type for maps, [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict).

## Step 3: define model interactions

```hs
module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, text)

type alias Translations = Dict String String

type alias Model = Translations

type Msg = UpdateTranslation String String

init : Model
init = Dict.empty

view : Model -> Html Msg
view model = text "Hello, translations!"

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTranslation key value ->
      Dict.update key (\_ -> Just value) model

main = Browser.sandbox { init = init, update = update, view = view }
```

The above code would work for the first time

Using [Dict.update](https://package.elm-lang.org/packages/elm/core/latest/Dict#update) having a signature of
`update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v`, or in other words,
`update : Key -> (updateValueForTheKeyFn : Maybe Value -> Maybe Value) -> (Dict Key Value) -> (Dict Key Value)`, which takes

* a key in a map
* a function that would be called with `Maybe Value`, depending on whether the (key, value) combination exists
in a map, and returning a new value for the entry
* the current map

and returns the new map

## Step 4: define the representation

```hs
module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text, input)
import Html.Attributes
import List

type alias Translations = Dict String String

type alias Model = Translations

type Msg = UpdateTranslation String String

init : Model
init = Dict.empty

view : Model -> Html Msg
view model =
  let
    children = List.map (\(key, value) -> div [] [div [] [text key], div [] [input [Html.Attributes.value value] []]]) (Dict.toList model)
  in
    div [] children

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTranslation key value ->
      Dict.update key (\_ -> Just value) model

main = Browser.sandbox { init = init, update = update, view = view }

```

Using [`Dict.toList`](https://package.elm-lang.org/packages/elm/core/latest/Dict#toList), which converts a map to
a list of pairs `(key, value)` representing the map entries, and the 
[`List.map`](https://package.elm-lang.org/packages/elm/core/latest/List#map), we can display the 
translations page

Just for the sake of tests, let's have some test data in the initial state:

```hs
init = Dict.fromList [("hello", "cześć"), ("world", "świat")]
```

## Step 5: small refactoring

```hs
translationForm : Translations -> Html Msg
translationForm model =
  let
    translationEntries = Dict.toList model
    rows = List.map (\(key, value) -> translationRow key value) translationEntries
  in
    div [] rows

translationRow : String -> String -> Html Msg
translationRow key value =
  div [] [
    div [] [text key],
    div [] [
      input [Html.Attributes.value value] []
    ]
  ]

view : Model -> Html Msg
view model = 
  let
    translations_form = translationForm model
  in
    div [] [translations_form]
```

## Step 6: CSS

```hs
translationForm : Translations -> Html Msg
translationForm model =
  let
    translationEntries = Dict.toList model
    rows = List.map (\(key, value) -> translationRow key value) translationEntries
  in
    div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column"] rows

translationRow : String -> String -> Html Msg
translationRow key value =
  div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "row"] [
    div [] [text key],
    div [] [
      input [Html.Attributes.value value] []
    ]
  ]

view : Model -> Html Msg
view model =
  let
    translations_form = translationForm model
  in
    div [] [translations_form]
```

## Step X: JSON

```hs
module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text, input, button)
import Html.Attributes
import Html.Events
import List
import Http
import Json.Decode

type alias Translations = Dict String String

type alias Model = Loading
  | LoadedTranslations (Result Http.Error Translations)
--  | Saving
--  | Saved (Result Http.Error String)

type Msg = LoadTranslations
  | UpdateTranslation String String 
--   | SaveTranslations

-- Helpers

decodeTranslations : Json.Decoder Translations
decodeTranslations = Json.Decoder.dict Json.Decoder.string

loadTranslations : Cmd Msg
loadTranslations = 
  Http.get {
    url = "https://elm-test.free.beeceptor.com",
    expect = Http.expectJson LoadedTranslations Json.Decode.
  }

translationForm : Translations -> Html Msg
translationForm model =
  let
    translationEntries = Dict.toList model
    rows = List.map (\(key, value) -> translationRow key value) translationEntries
  in
    div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column"] rows

translationRow : String -> String -> Html Msg
translationRow key value =
  div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "row"] [
    div [] [text key],
    div [] [
      input [
        Html.Attributes.value value,
        Html.Events.onInput (\newValue -> UpdateTranslation key newValue)
      ] []
    ]
  ]
  
-- Application specifics

init : Model
init = Loading -- Dict.fromList [("hello", "cześć"), ("world", "świat")] -- Dict.empty

view : Model -> Html Msg
view model = 
  let
    translations_form = translationForm model
  in
    div [] [
      translations_form,
      button [Html.Events.onClick SaveForm] [text "Save"]
    ]
    

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadTranslations ->
      (model, )
    UpdateTranslation key value ->
      (Dict.update key (\_ -> Just value) model, Cmd.none)

main = Browser.sandbox { init = init, update = update, view = view }

```

## Step X+1: fetching JSON

```hs
module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text, input, button)
import Html.Attributes
import Html.Events
import List
import Http
import Json.Decode exposing (Decoder)

type alias Translations = Dict String String

type alias Model = Translations

type Msg = LoadTranslations
  | LoadedTranslations (Result Http.Error Translations)
--  | UpdateTranslation String String

-- Helpers

decodeTranslations : Decoder Translations
decodeTranslations = Json.Decode.dict Json.Decode.string

loadTranslations : Cmd Msg
loadTranslations = 
  Http.get {
    url = "https://elm-test.free.beeceptor.com/translations",
    expect = Http.expectJson LoadedTranslations decodeTranslations
  }

translationForm : Translations -> Html Msg
translationForm model =
  let
    translationEntries = Dict.toList model
    rows = List.map (\(key, value) -> translationRow key value) translationEntries
  in
    div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column"] rows

translationRow : String -> String -> Html Msg
translationRow key value =
  div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "row"] [
    div [] [text key],
    div [] [
      input [
        Html.Attributes.value value
--        , Html.Events.onInput (\newValue -> UpdateTranslation key newValue)
      ] []
    ]
  ]
  
-- Application specifics

init : () -> (Model, Cmd Msg)
init _ = (Dict.empty, loadTranslations)

view : Model -> Html Msg
view model = div [] [ translationForm model ]
    

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadTranslations ->
      (model, loadTranslations)
    LoadedTranslations (Ok translations) ->
      (translations, Cmd.none)
    LoadedTranslations (Err httpError) ->
      (model, Cmd.none)
--    UpdateTranslation key value ->
--      (Dict.update key (\_ -> Just value) model, Cmd.none)

main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }

```

## Step X+2: the loading state

```hs
module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text, input, button)
import Html.Attributes
import Html.Events
import List
import Http
import Json.Decode exposing (Decoder)

type alias Translations = Dict String String

type Model = Loading
  | Error
  | Loaded Translations

type Msg = LoadTranslations
  | LoadedTranslations (Result Http.Error Translations)
--  | UpdateTranslation String String

-- Helpers

decodeTranslations : Decoder Translations
decodeTranslations = Json.Decode.dict Json.Decode.string

loadTranslations : Cmd Msg
loadTranslations = 
  Http.get {
    url = "https://elm-test.free.beeceptor.com/translations",
    expect = Http.expectJson LoadedTranslations decodeTranslations
  }

translationForm : Translations -> Html Msg
translationForm model =
  let
    translationEntries = Dict.toList model
    rows = List.map (\(key, value) -> translationRow key value) translationEntries
  in
    div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column"] rows

translationRow : String -> String -> Html Msg
translationRow key value =
  div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "row"] [
    div [] [text key],
    div [] [
      input [
        Html.Attributes.value value
--        , Html.Events.onInput (\newValue -> UpdateTranslation key newValue)
      ] []
    ]
  ]
  
-- Application specifics

init : () -> (Model, Cmd Msg)
init _ = (Loading, loadTranslations)

view : Model -> Html Msg
view model =
  case model of
    Loading -> div [] [text "Loading..."]
    Loaded translations -> div [] [ translationForm translations ]
    Error -> div [] [text "Error loading translations!"]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadTranslations ->
      (Loading, loadTranslations)
    LoadedTranslations (Ok translations) ->
      (Loaded translations, Cmd.none)
    LoadedTranslations (Err httpError) ->
      (Error, Cmd.none)
--    UpdateTranslation key value ->
--      (Dict.update key (\_ -> Just value) model, Cmd.none)

main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }

```

## Step X+4: send JSON

```hs
module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text, input, button)
import Html.Attributes
import Html.Events
import List
import Http
import Json.Decode exposing (Decoder)
import Json.Encode

type alias Translations = Dict String String

type Model = Loading
  | Error
  | Loaded Translations

type Msg = LoadTranslations
  | LoadedTranslations (Result Http.Error Translations)
  | UpdateTranslation String String
  | SaveTranslations
  | SavedTranslations (Result Http.Error ())

-- Http helpers

decodeTranslations : Decoder Translations
decodeTranslations = Json.Decode.dict Json.Decode.string

loadTranslations : Cmd Msg
loadTranslations = 
  Http.get {
    url = "https://elm-test.free.beeceptor.com/translations",
    expect = Http.expectJson LoadedTranslations decodeTranslations
  }
  
serializeTranslations : Translations -> Json.Encode.Value
serializeTranslations translations = (Json.Encode.dict identity Json.Encode.string translations)
  
saveTranslations : Translations -> Cmd Msg
saveTranslations translations =
  Http.post {
    url = "https://elm-test.free.beeceptor.com/translations",
    body = Http.jsonBody (serializeTranslations translations),
    expect = Http.expectWhatever SavedTranslations
  }

-- View helpers

translationPage : Translations -> Html Msg
translationPage translations = 
  div [] [
    translationForm translations,
    button [Html.Events.onClick SaveTranslations] [text "Save"]
  ]

translationForm : Translations -> Html Msg
translationForm translations =
  let
    translationEntries = Dict.toList translations
    rows = List.map (\(key, value) -> translationRow key value) translationEntries
  in
    div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column"] rows

translationRow : String -> String -> Html Msg
translationRow key value =
  div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "row"] [
    div [] [text key],
    div [] [
      input [
        Html.Attributes.value value,
        Html.Events.onInput (\newValue -> UpdateTranslation key newValue)
      ] []
    ]
  ]
  
-- Application specifics

init : () -> (Model, Cmd Msg)
init _ = (Loading, loadTranslations)

view : Model -> Html Msg
view model =
  case model of
    Loading -> div [] [text "Loading..."]
    Loaded translations -> div [] [ translationPage translations ]
    Error -> div [] [text "Error loading translations!"]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadTranslations ->
      (Loading, loadTranslations)
    LoadedTranslations (Ok translations) ->
      (Loaded translations, Cmd.none)
    LoadedTranslations (Err httpError) ->
      (Error, Cmd.none)
    UpdateTranslation key value ->
      case model of
        Loaded translations -> (Loaded (Dict.update key (\_ -> Just value) translations), Cmd.none)
        _ -> (model, Cmd.none)
    SaveTranslations ->
      case model of
        Loaded translations -> (model, saveTranslations translations)
        _ -> (model, Cmd.none)
    SavedTranslations (Ok ()) ->
      (model, Cmd.none)
    SavedTranslations (Err httpError) ->
      (model, Cmd.none)

main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }

```

## Step X+5: add tri-state to the "Save" button

```hs
module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text, input, button)
import Html.Attributes
import Html.Events
import List
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Delay

type alias Translations = Dict String String

type SaveButtonState = IsSaving | WasSaved | NoSaving

type Model = Loading
  | Error
  | Loaded Translations
  | Saving Translations
  | Saved Translations

type Msg = LoadTranslations
  | LoadedTranslations (Result Http.Error Translations)
  | UpdateTranslation String String
  | SaveTranslations
  | SavedTranslations (Result Http.Error ())

-- Http helpers

decodeTranslations : Decoder Translations
decodeTranslations = Json.Decode.dict Json.Decode.string

loadTranslations : Cmd Msg
loadTranslations =
  Http.get {
    url = "https://elm-test.free.beeceptor.com/translations",
    expect = Http.expectJson LoadedTranslations decodeTranslations
  }

serializeTranslations : Translations -> Json.Encode.Value
serializeTranslations translations = (Json.Encode.dict identity Json.Encode.string translations)

saveTranslations : Translations -> Cmd Msg
saveTranslations translations =
  Http.post {
    url = "https://elm-test.free.beeceptor.com/translations",
    body = Http.jsonBody (serializeTranslations translations),
    expect = Http.expectWhatever SavedTranslations
  }

-- View helpers

translationPage : Translations -> SaveButtonState -> Html Msg
translationPage translations saveButtonState =
  div [] [
    translationForm translations,
    translationPageControls saveButtonState
  ]

translationPageControls : SaveButtonState -> Html Msg
translationPageControls saveButtonState =
  case saveButtonState of
    IsSaving -> div [] [ button [Html.Events.onClick SaveTranslations, Html.Attributes.disabled True] [text "Saving..."] ]
    WasSaved -> div [] [ button [Html.Events.onClick SaveTranslations, Html.Attributes.disabled True] [text "✔️ Saved!"] ]
    _ -> div [] [ button [Html.Events.onClick SaveTranslations] [text "Save"] ]

translationForm : Translations -> Html Msg
translationForm translations =
  let
    translationEntries = Dict.toList translations
    rows = List.map (\(key, value) -> translationRow key value) translationEntries
  in
    div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column"] rows

translationRow : String -> String -> Html Msg
translationRow key value =
  div [Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "row"] [
    div [] [text key],
    div [] [
      input [
        Html.Attributes.value value,
        Html.Events.onInput (\newValue -> UpdateTranslation key newValue)
      ] []
    ]
  ]

-- Application specifics

init : () -> (Model, Cmd Msg)
init _ = (Loading, loadTranslations)

view : Model -> Html Msg
view model =
  case model of
    Loading -> div [] [text "Loading..."]
    Loaded translations -> div [] [ translationPage translations NoSaving ]
    Saving translations -> div [] [ translationPage translations IsSaving ]
    Saved translations -> div [] [ translationPage translations WasSaved ]
    Error -> div [] [text "Error loading translations!"]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadTranslations ->
      (Loading, loadTranslations)
    LoadedTranslations (Ok translations) ->
      (Loaded translations, Cmd.none)
    LoadedTranslations (Err httpError) ->
      (Error, Cmd.none)
    UpdateTranslation key value ->
      case model of
        Loaded translations -> (Loaded (Dict.update key (\_ -> Just value) translations), Cmd.none)
        _ -> (model, Cmd.none)
    SaveTranslations ->
      case model of
        Loaded translations -> (Saving translations, saveTranslations translations)
        _ -> (model, Cmd.none)
    SavedTranslations (Ok ()) ->
      case model of
        Saving translations -> (Saved translations, Delay.after 2000 (LoadedTranslations (Ok translations)))
        _ -> (model, Cmd.none)
    SavedTranslations (Err httpError) ->
      case model of
        Saving translations -> (Loaded translations, Cmd.none)
        _ -> (model, Cmd.none)

main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }

```

Before sending data to the server, change the text of the "Save" button to "Saving...".
Then use [andrewMacmurray/elm-delay](https://package.elm-lang.org/packages/andrewMacmurray/elm-delay/latest/) to change the text of the "Save" button from "Saving..." to "Saved!" with a timeout after the data was successfully received.