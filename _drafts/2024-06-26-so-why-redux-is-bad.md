---
layout: post
title: "So why Redux is bad?"
date: '2024-06-26T21:15:24+10:00'
---

Redux is generally considered a bad choice for state management on front-end.
But check out an average application' React application (or a few fragments of it):

```jsx
import { Provider, useAtom, useSetAtom } from 'jotai';

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

Back in the day, Redux seemingly addressed these areas to a degree. In my opinion, Redux is not suitable for
complex projects for a few reasons:

- it combines all states (both local and global) in one big messy furball; managing it is quite a hurdle
    - as the project complexity grows, one can not just change a piece of state or selectors without affecting the entirety of the project (and teams)
    - combining reducers into one supermassive function makes any state update unreasonably long and complex process (remember: each reducer returns a new state instance; now imagine having even a hundred of reducers, each of which returns a new state)
- it is easy for a component to be re-rendered on any change to the state; a lot of effort goes into making sure selectors are well memoized and not re-calculated
- asynchronous actions are a big unresolved mystery (are you going to use thunks, flux, sagas or something else?)

On a flip side, the idea itself could actually bring a lot of positives if cooked properly.
Consider Elm architecture and how it compares to Redux:

- all the states are still combined into one big cauldron of chaos
- by default, any component is just a function returning an array; the entire application will be rerendered on each state change, which is still suboptimal
- asynchronous actions are handled separately by the runtime in a similar way to synchronous actions; each action returns a new state and a command (triggering the asynchronous processing)
- reducers are a lot faster, since they are essentially a big `switch..case` statement (which is cheap)
