---
layout: post
title: 'Pure functional frontend'
date: '2024-05-16T19:00:00+10:00'
---

Idea: separate IO and business logic and make that logic referentially transparent.
The benefits? Straightforward logic, less of a need for tests, easier refactoring, less runtime errors.

How we write applications in React nowadays:

```jsx
export const MyComponent = () => {
    const { data } = useMyComponentData();
    const { action } = useMyAction();

    return (
        <>
            <span>{data.field1}</span>
            <button onClick={action}>Do action</button>
        </>
    );
}
```

A lot of the time these components mix in both business logic and representation.
Hence it makes it especially hard to separate the two or have a clear flow of data.

My guess is that this approach serves few purposes:

* only loading the data needed for the component
* only loading the data when the component is being rendered or explicitly requests the data
* encapsulating everything in the component itself (both the data, its actions and presentation)

Let's see how Elm suggests solving this:

```elm
type Msg = DoAction | MyComponentDataLoaded (Result Http.Error MyComponentData)

type alias ApplicationState =
    { myComponentData = MyComponentData }

type Model
    = Loading
    | Failure
    | Loaded ApplicationState

init : () -> (Model, Cmd Msg)
init _ =
    ( Loading
    , Http.get
        { url = "/loadData"
        , expect = Http.expectString MyComponentDataLoaded
        }
    )

MyComponent data = [
    span [] [ text data.field1 ],
    button [ onClick DoAction ] [ text "Do action" ]
]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MyComponentDataLoaded result ->
            case result of
                Ok data ->
                    (Loaded data, Cmd.none)
                Err _ ->
                    (Failure, Cmd.none)

view state =
    div []
        [
            case state.MyComponentData of
                Nothing ->
                    div [] []
                Just data ->
                    MyComponent state.MyComponentData
        ]
```

A bit verbose, but at least one can tell when the data will be loaded, how it will be handled, where it will be passed.
It is just a bit cumbersome to have it all in one place - Elm does not really handle multiple components well.
