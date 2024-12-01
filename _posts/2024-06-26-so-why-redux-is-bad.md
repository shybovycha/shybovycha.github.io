---
layout: post
title: "So why Redux is bad?"
date: '2024-06-26T21:15:24+10:00'
---

Redux is generally considered a bad choice for state management on front-end.
But check out an average application' React application (or a few fragments of it):

```jsx
import { Provider, useAtom } from 'jotai';
import { useQuery } from 'react-query';

const useInfo = () => {
  const { data, error, isLoading } = useQuery({
    queryKey: [ 'info' ],
    queryFn: () => fetch('/info').then(r => r.json()),
    staleTime: Infinity,
  });

  return {
    info: data,
    isLoading,
    error,
  };
};

const HomeWithoutProvider = () => {
  const { raiseToast } = useToast();

  const [ initialRender, setInitialRender ] = useState(false);

  const [ pageType, setPageType ] = useAtom(pageTypeAtom);

  const { info } = useInfo();

  useEffect(() => {
    if (info.isNewVersionAvailable) {
      raiseToast({
        // ...
      });
    }
  }, [info]);

  return (
    <div>...</div>
  );
};

const Home = () => (
  <Provider>
    <HomeWithoutProvider />
  </Provider>
);

const Routes = () => (
  <BrowserRouter>
    <Suspense fallback={<Loader />}>
      <Route element={<Home />} path="/" />
    </Suspense>
  </BrowserRouter>
);

const root = React.createRoot(document.getElementById('root'));

root.render(
  <StrictMode>
    <StyleThemeProvider>
      <ToastProvider>
        <GlobalErrorHandler>
          <ReactQueryProvider>
            <Routes />
          </ReactQueryProvider>
        </GlobalErrorHandler>
      </ToastProvider>
    </StyleThemeProvider>
  </StrictMode>
);
```

Usually most of the components (`Home`, `Routes`) and hooks (`useInfo`) are in separate files,
but for the sake of simplicity I combined them all into one code block.

What I find suboptimal with this code is that it has at least three obvious different state management systems:

* `jotai` for shared atoms (pieces of global state)
* `React.useState` for internal component state
* various `React.Context`s (`StyleThemeProvider`, `ToastProvider`, `GlobalErrorHandler`, etc.)

On top of those, there are less obvious state management systems:

* `react-router` uses internal router state, which could be treated as global application state
* `react-query` uses its internal cache for each query
* `react-hook-form` uses the form state of a component' ancestor (which could be declared on any level above the current component)

In the pursuit of encapsulation and reducing the boilerplate, front-end developers came up with all of these solutions aiming to solve the problem of managing application state.

So what exactly is this problem? And what are the issues all of the aforementioned solutions try to address?

As I see it, there are two competing camps:

- containing the logic in small reusable chunks (hooks, components)
- sharing chunks of state between different parts of the application

There are some side-tracks like dealing with asynchronous actions (like fetching the data from server),
changing the state of external components (like showing a toast message), reducing the unnecessary re-renders.

Back in the day, Redux seemingly addressed these areas to a degree. Redux implements Flux architecture, which was
compared to MVC (Model-View-Controller) architecture back in the day:

<img src="/images/why-redux-is-bad/mvc-architecture.webp" loading="lazy" alt="Data flow in MVC architecture">
<img src="/images/why-redux-is-bad/flux-architecture.webp" loading="lazy" alt="Data flow in Flux architecture">

It became especially popular after Angular.JS' MVVM (Model-View-ViewModel) architecture implementation was considered slow with its
dirty checks and constant re-rendering.

It could be said that Redux is being shipped in recent versions of React itself - with the use of [`useReducer()`](https://react.dev/reference/react/useReducer) hook. One would rarely use Redux on its own, often sticking to a somewhat opinionated stack of reselect (for derived state), react-redux (to `connect()` components to the store) and redux-thunk or redux-sagas (for asynchronous `dispatch()` calls).

The aforementioned component could be implemented with the "conventional" (old) Redux approach like so:

```jsx
import { createStore, combineReducers, applyMiddleware } from 'redux';
import { connect } from 'react-redux';
import { createSelector } from 'reselect';
import { thunk } from 'redux-thunk';

const infoReducer = (state = { isNewVersionAvailable: false }, action) => {
  switch (action.type) {
    case 'INFO_LOADED':
      return { ...state, ...action.payload };

    default:
      return state;
  }
};

const pageDataReducer = (state = { pageType: undefined }, action) => {
  switch (action.type) {
    case 'SET_PAGE_TYPE':
      return { ...state, pageType: action.pageType };

    default:
      return state;
  }
};

const toastReducer = (state = { isOpen: false, content: undefined }, action) => {
  switch (action.type) {
    case 'SHOW_TOAST':
      return { ...state, isOpen: true, content: action.payload };

    default:
      return state;
  }
};

const rootReducer = combineReducers({
  info: infoReducer,
  pageData: pageDataReducer,
  toast: toastReducer,
});

const store = createStore(rootReducer, applyMiddleware(thunk));

// Meet thunks.
// A thunk in this context is a function that can be dispatched to perform async
// activity and can dispatch actions and read state.
// This is an action creator that returns a thunk:
const loadInfoAction = () =>
  (dispatch) =>
    fetch('/info')
      .then(r => r.json)
      .then(payload => dispatch({ type: 'INFO_LOADED', payload }));

const raiseToastAction = (content) =>
  (dispatch) =>
    dispatch({ type: 'SHOW_TOAST', payload: content });

const setPageTypeAction = (pageType) =>
  (dispatch) =>
    dispatch({ type: 'SET_PAGE_TYPE', pageType });

const Home = ({ info, pageType, loadInfo, raiseToast, setPageType }) => {
  useEffect(() => {
    loadInfo();
  }, []);

  useEffect(() => {
    if (info.isNewVersionAvailable) {
      raiseToast({
        // ...
      });
    }
  }, [info]);

  return (
    <div>...</div>
  );
};

const mapStateToProps = ({ info, pageData: { pageType } }) => ({ info, pageType });

const mapDispatchToProps = (dispatch) => ({
  raiseToast: (content) => dispatch(raiseToastAction(content)),

  setPageType: (pageType) => dispatch(setPageTypeAction(content)),

  loadInfo: () => dispatch(loadInfoAction()),
});

const HomeContainer = connect(mapStateToProps, mapDispatchToProps)(Home);

const Routes = () => (
  <BrowserRouter>
    <Suspense fallback={<Loader />}>
      <Route element={<HomeContainer />} path="/" />
    </Suspense>
  </BrowserRouter>
);

const root = React.createRoot(document.getElementById('root'));

root.render(
  <Routes />
);
```

In my opinion, Redux is not suitable for complex projects for a few reasons:

- it combines all states (both local and global) in one big messy furball; managing it is quite a hurdle
    - as the project complexity grows, one can not just change a piece of state or selectors without affecting the entirety of the project (and teams)
    - combining reducers into one supermassive function makes any state update unreasonably long and complex process (remember: each reducer returns a new state instance; now imagine having even a hundred of reducers, each of which returns a new state)
- it is easy for a component to be re-rendered on any change to the state; a lot of effort goes into making sure selectors are well memoized and not re-calculated
- asynchronous actions are a big unsolved mystery (are you going to use thunks, flux, sagas or something else?)

On a flip side, the idea itself could actually bring a lot of positives if cooked properly:

- there is only one possible flow of data: via `dispatch()` call, through the reducers and back to the components connected to the store via component props
    - this is supposed to make following the data (e.g. debugging the application) easy
- components are pretty much stateless at this point, encapsulated and not having side effects leaking everywhere
- logic is nicely separated from the representation and is encapsulated in the reducers (and maybe, to a small extent, in selectors)

[Elm](https://elm-lang.org/) utilizes the language features and its own runtime combined with Redux-like architecture to improve some aspects of the more traditional pure JS way of things, where there are only opinionated libraries and no one way of doing things.

Consider Elm architecture and how it compares to Redux:

- all the states are still combined into one big cauldron of chaos
- by default, any component is just a function returning an array; the entire application will be rerendered on each state change, which is still suboptimal, since literally all components are connected to the store
- asynchronous actions are handled separately by the runtime in a similar way to synchronous actions; each action returns a new state and a command (triggering the asynchronous processing)
    - since commands are handled by the runtime and there's a handful of commands, all of them will (eventually) circle back to dispatching messages just like components do, following the same one-way data flow
- reducers are a lot faster, since they are essentially a big `switch..case` statement (which is cheap)

The above component could be re-implemented in Elm as follows:

```elm
import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map, field, bool)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { info : Maybe (Result Http.Error Info)
  , toast : Maybe Toast
  , pageType : PageType
  }

type alias Info =
  { quote : String
  , source : String
  , author : String
  , year : Int
  }

type alias Toast =
  { content : String }

type PageType = Page1 | Page2

init : () -> (Model, Cmd Msg)
init _ =
  ({ info = Nothing, toast = Nothing, pageType = Page1 }, loadInfo)

type Msg
  = GotInfo (Result Http.Error Info)
  | ShowToast Toast
  | SetPageType PageType

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotInfo result ->
      case result of
        Ok info ->
          ({ model | info = Just (Ok info) }, Cmd.none)

        Err e ->
          ({ model | info = Just (Err e) }, Cmd.none)

    ShowToast t ->
      ({ model | toast = Just t }, Cmd.none)

    SetPageType p ->
      ({ model | pageType = p }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Demo App" ]
    , viewInfo model.info
    , viewToast model.toast
    ]

viewInfo : Maybe (Result Http.Error Info) -> Html Msg
viewInfo mbInfoResult =
  case mbInfoResult of
    Nothing ->
      text "Loading..."

    Just infoResult ->
      case infoResult of
        Err _ ->
          div []
            [ text "Could not load info" ]

        Ok info ->
          div []
            [ text "App loaded" ]

viewToast : Maybe Toast -> Html Msg
viewToast mbToast =
  case mbToast of
    Nothing ->
      div [] []

    Just toast ->
      div [] [ text toast.content ]

loadInfo : Cmd Msg
loadInfo =
  Http.get
    { url = "/info"
    , expect = Http.expectJson GotInfo infoDecoder
    }

infoDecoder : Decoder Info
infoDecoder =
  map Info
    (field "isNewVersionAvailable" bool)
```

The good bits are:

- forcing to handle all possible actions (messages) and results (HTTP success and error scenarios)
- expressive language features (union types, strong typing, records, switch-case expressions) ensure robust code (as in this code does not leave room for mistakes like null/undefined/unhandled exceptions/unhandled code path/wrong value type)
- no leeway for various ways to get things done (as in there is only one way to handle HTTP requests, only one way to handle asynchronous message dispatches, only one way to parse HTTP responses)

But if Redux spreads things apart compared to modern React, Elm feels like it spreads things further apart by handling effect results separately (like sending HTTP request, parsing HTTP response and processing the result by dispatching another message).

One other example would be [PureScript](https://www.purescript.org/) (or rather [Halogen](https://github.com/purescript-halogen/purescript-halogen)). Purescript itself elevates the complexity to the skies and beyond, by making you run around with monads like a headless chicken. Consider "simple" example of sending a HTTP request:

```purescript
module Main where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Action
  = SetUsername String
  | MakeRequest Event

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = { loading: false, username: "", result: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form
    [ HE.onSubmit \ev -> MakeRequest ev ]
    [ HH.h1_ [ HH.text "Look up GitHub user" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter username:" ]
        , HH.input
            [ HP.value st.username
            , HE.onValueInput \str -> SetUsername str
            ]
        ]
    , HH.button
        [ HP.disabled st.loading
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text $ if st.loading then "Working..." else "" ]
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

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  SetUsername username -> do
    H.modify_ _ { username = username, result = Nothing }

  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.username
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    H.modify_ _ { loading = false, result = map _.body (hush response) }
```

Now add the [`halogen-store`](https://github.com/thomashoneyman/purescript-halogen-store/) package to the mix to make use of Redux-like state management:

```purescript
module Main where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Halogen.Store.Monad (class MonadStore, updateStore, runStoreT)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Select (selectAll)
import Effect.Aff (launchAff_)

data StoreAction
  = StoreSetUsername String
  | StoreMakeRequest
  | StoreReceiveResponse (Maybe String)

reduce :: State -> StoreAction -> State
reduce store = case _ of
  StoreSetUsername username ->
    store { username = username, result = Nothing }
  StoreMakeRequest ->
    store { loading = true }
  StoreReceiveResponse response ->
    store { loading = false, result = response }

initialStore :: State
initialStore = { username: "", loading: false, result: Nothing }

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  root <- runStoreT initialStore reduce component
  void $ runUI root unit body

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Action
  = SetUsername String
  | MakeRequest Event
  | ReceiveState (Connected State Unit)

deriveState :: Connected State Unit -> State
deriveState { context: { username, loading, result }, input: _ } =
  { username: username
  , loading: loading
  , result: result
  }

component :: forall query output m. MonadAff m => MonadStore StoreAction State m => H.Component query Unit output m
component =
  connect selectAll $ H.mkComponent
    { initialState: deriveState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< ReceiveState
      }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form
    [ HE.onSubmit \ev -> MakeRequest ev ]
    [ HH.h1_ [ HH.text "Look up GitHub user" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter username:" ]
        , HH.input
            [ HP.value st.username
            , HE.onValueInput \str -> SetUsername str
            ]
        ]
    , HH.button
        [ HP.disabled st.loading
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text $ if st.loading then "Working..." else "" ]
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

handleAction :: forall output m. MonadAff m => MonadStore StoreAction State m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  SetUsername username -> do
    updateStore $ StoreSetUsername username

  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.username
    updateStore $ StoreMakeRequest
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    updateStore $ StoreReceiveResponse (map _.body (hush response))

  ReceiveState input ->
    H.put $ deriveState input
```

The really nice things about this approach are:

- components could be self-sufficient, as opposed to Elm:
  - they can have both internal state and communicate with the external application via `Aff`
  - they can be extracted into separate modules, making them actually reusable components
- state selectors and connecting to a store are seamlessly implemented based on existing Halogen tools (subscriptions)
- it feels like you do not have to worry about state growing big, since each component explicitly declares which parts of that messy furball it needs (derives)

The bad news is that everything relies on monads and transformers - lifting, mapping, flat-mapping are just the very tip of the iceberg. Once you hit some mysterious error - it is quite tricky to understand what is going on. Unlike Elm, which has really nicely structured, formatted and presented both error message, its location and ways to fix it.

Just looking at the type definitions is nauseaing at best:

```purescript
handleAction :: forall output m. MonadAff m => MonadStore StoreAction State m => Action -> H.HalogenM State Action () output m Unit
```

And then there is this bit, lifting everything to the same monad and then mapping and flat-mapping it to get the response body:

```purescript
MakeRequest event -> do
  H.liftEffect $ Event.preventDefault event
  username <- H.gets _.username
  updateStore $ StoreMakeRequest
  response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
  updateStore $ StoreReceiveResponse (map _.body (hush response))
```

And do not forget that this monad is merely describing the computation, you will have to run it at some point:

```purescript
main = runHalogenAff do
  body <- HA.awaitBody
  root <- runStoreT initialStore reduce component
  let ui = runUI root unit body
  ui
```

The example on `halogen-store` suggests using `launchAff_`, but then you will have to cast the return value type to match the monad of the `main` function (`Effect Unit`) or lift `runStoreT` to the `Effect Unit` monad - whichever you find suitable:

```purescript
main = launchAff_ do
  body <- HA.awaitBody
  root <- runStoreT initialStore reduce component
  let ui = runUI root unit body
  void ui
```

But having to worry about all these intricacies actually strengthens the point that PureScript is not for the faint-harted - Elm prevails here.

The other drawback is that mixing logic in both component and the reducers is weird - it is not clear from the Flux architecture where the side-effects should live - like network calls, asynchronous actions, actions triggering other actions. Redux is known for suffering from all of these areas.

Elm solves this nicely with commands.

Halogen kind of takes a step backwards from Elm - it does have subscriptions, but it does not prevent you from issuing side effects from the `handleActions`. And `halogen-store` does not have a recipe for complex chained actions.

Ultimately, I don't think Redux is bad - the idea to have a full visibility into all possible application interactions is scary in a complex project and it is hard to come up with a clean way to work around it, but single-way data flow is actually nice.

Interesting how developers went from _"we don't want Angular.js dirty checks - it is not clear where the data is flowing"_ to _"we don't want a single point of contention for all application interactions"_.

In my eyes, the four technologies (modern-day React, Redux, Elm and Purescript) all come with their own pros and massive cons and there is no good or one-size-fits-all solution among them. And none of them ultimately solves the problem of managing application state and interactions in a non-bloated way. Maybe Angular or React 19 have an answer?
