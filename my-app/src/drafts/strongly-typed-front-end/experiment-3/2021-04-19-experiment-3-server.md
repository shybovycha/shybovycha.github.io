---
layout: post
title: 'Strongly-typed front-end: experiment 3, server communication, in Elm'
date: '2021-04-19T12:58:15+0700'
published: false
---

## AJAX requests

In Elm, there is a `Cmd` monad, which allows for any sort of side effects (including AJAX requests). Using `Cmd` requires changing not only the `update` function (which handles application state mutations), but also the `main` function (which initializes the application):

```elm
update : Msg -> Model -> Model
-- becomes
update : Msg -> Model -> (Model, Cmd Msg)
```

```elm
main = Browser.sandbox { init = init, update = update, view = view }

-- becomes

main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
```

Then any state modification must return a pair of a new state and an instance of `Cmd` monad, even if it does nothing:

```elm
update msg model =
  case msg of
    LoadedData -> ({ model | loaded = True }, Cmd.none)
```

Then, Elm provides a `Http` module with all the functionality required:

```elm
import Http

loadData : Cmd Msg
loadData = 
  Http.get {
    url = "http://localhost:3000/",
    expect = Http.expectWhatever LoadedData
  }
```

The expect param for the `Http.get` function (`Http.post` is supported out-of-the-box and for anything else there is `Http.request`) tells Elm how to process (parse) the response. For now, the `Http.expectWhatever LoadedData` call will tell Elm to dispatch a `LoadedData` message whenever anything but an error is received from server.

After the response is parsed, the `LoadedData` message will be dispatched. But the type of this message is again complex, to count for errors in the response (this will also be covered later):

```elm
type Msg = LoadedData (Result Http.Error (Dict String String))
```

The `Result` type is essentially a well-known `Either` monad, containing `Left` or `Right` part, which in this example are `Http.Error` and `Dict String String`, correspondingly.

## Handling error responses

As mentioned above, the message dispatched when the response is received is essentially an `Either` monad containing _either_ an error instance (`Http.Error`) or a parsed response. As `Either` in Haskell, `Result` in Elm has `Ok` (similar to `Right`) and `Err` (similar to `Left`) variations. These can be easily matched within the `update` function:

```elm
update : Msg -> Model -> (Model, Cmd)
update msg model =
  case msg of
    LoadedData (Ok data) -> ({ model | data = data }, Cmd.none)
    LoadedData (Err error) -> ({ model | error = error }, Cmd.none)
```

Similarly, `Http.Error` passed as a parameter to `Err` in case of error can be matched against:

```elm
type Error = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String
```

## Handling JSON requests & responses

As described above, the `expect` param configures how the server response is parsed. It could also be `Http.expectJson` or `Http.expectString`. Using JSON is relatively simple:

```elm
expect = Http.expectJson LoadedData decodeData
```

This will still dispatch the same message, `LoadedData`, but will also use `decodeData` function to parse the response and send it alongside the `LoadedData` message.

The `decodeData` function might look something like this:

```elm
import Json.Decode exposing (Decoder)

decodeData : Decoder (Dict String String)
decodeData = Json.Decode.dict Json.Decode.string
```

The `Json.Decode.dict` function parses response as a `Dict String` a where a is the `Dict`'s value' type. In this example, we pass `Json.Decode.string` which will be used to decode each value for the `Dict`.

**TODO:** what will happen if the response can not be parsed using the specified decoder?

## [TODO] Sending files

## [TODO] Sending form data
