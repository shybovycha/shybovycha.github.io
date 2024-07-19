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

In the pursuit of encapsulation, front-end developers came up with all of these solutions aiming to solve the problem
of managing application state.

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
- asynchronous actions are a big unresolved mystery (are you going to use thunks, flux, sagas or something else?)

On a flip side, the idea itself could actually bring a lot of positives if cooked properly:

- there is only one possible flow of data: via `dispatch()` call, through the reducers and back to the components connected to the store via component props
    - this is supposed to make following the data (e.g. debugging the application) easy
- components are pretty much stateless at this point, encapsulated and not having side effects leaking everywhere
- logic is nicely separated from the representation and is encapsulated in the reducers (and maybe, to a small extent, in selectors)

Elm utilizes the language features and its own runtime combined with Redux-like architecture to improve some aspects of the more traditional pure JS way of things, where there are only opinionated libraries and no one way of doing things.

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
