---
layout: post
title: How not to use monads
date: '2023-01-31T11:23:24+11:00'
---

Recently I have watched a video titled ["Optionals and errors in Haskell & Rust - monads by example"](https://www.youtube.com/watch?v=c_F1o_so2MQ).
In the video, the author makes a simple application simulating the retrieval of a HTML document via URL and checking
if the document contains the title. This is a half-simulated experience, aimed at the beginners to demonstrate how the error
handling is done in Rust and Haskell.

However, author in his code has made quite a few bad decisions.
And that's what I want to make a focus of this blog: how one should **not** use monads and handle errors in Haskell.

Author's original source looks like this:

```hs
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

import Control.Monad (forM_)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

data Error = Error (String) deriving (Show)

data Doc = Doc {head :: Maybe Head}

data Head = Head {title :: Maybe String}

data Summary = Summary
  { title :: Maybe String,
    ok :: Bool
  }
  deriving (Show)

readDoc :: String -> Either Error Doc
readDoc url =
  if
      | isInfixOf "fail" url -> Left $ Error $ printf "Bad read of %s" url
      | otherwise ->
          Right $
            if
                | isInfixOf "head-missing" url -> Doc {head = Nothing}
                | isInfixOf "title-missing" url ->
                    Doc {head = Just Head {title = Nothing}}
                | isInfixOf "title-empty" url ->
                    Doc {head = Just Head {title = Just ""}}
                | otherwise ->
                    Doc
                      { head =
                          Just Head {title = Just $ printf "Title of %s" url}
                      }

buildSummary :: Doc -> Summary
buildSummary doc =
  Summary {title = doc.head >>= (.title), ok = True}

readAndBuildSummary :: String -> Summary
readAndBuildSummary url = case readDoc url of
  Left err -> Summary {title = Nothing, ok = False}
  Right doc -> buildSummary doc

isTitleNonEmpty :: Doc -> Maybe Bool
isTitleNonEmpty doc = do
  head <- doc.head
  title <- head.title
  return $ not $ null title

isTitleNonEmpty' :: Doc -> Maybe Bool
isTitleNonEmpty' doc = not <$> null <$> (doc.head >>= (.title))

isTitleNonEmpty'' :: Doc -> Maybe Bool
isTitleNonEmpty'' doc = not . null <$> (doc.head >>= (.title))

readWhetherTitleNonEmpty :: String -> Either Error (Maybe Bool)
readWhetherTitleNonEmpty url = do
  doc <- readDoc url
  return $ isTitleNonEmpty doc

readWhetherTitleNonEmpty' :: String -> Either Error (Maybe Bool)
readWhetherTitleNonEmpty' url = isTitleNonEmpty <$> readDoc url

main :: IO ()
main = do
  let urls = ["good", "title-empty", "title-missing", "head-missing", "fail"]
  forM_ urls $ \url -> do
    putStrLn $ printf "Checking \"https://%s/\":" url
    -- Summary.
    let summary = readAndBuildSummary url
    putStrLn $ printf "  Summary: %s" $ show summary
    putStrLn $ printf "  Title: %s" $ fromMaybe "" summary.title
    -- Has title.
    let hasTitle = readWhetherTitleNonEmpty url
    -- let hasTitleSure = either (const False) id $ fromMaybe False <$> hasTitle
    let hasTitleSure = fromMaybe False $ either (const Nothing) id hasTitle
    putStrLn $
      printf "  Has title: %s vs %s" (show hasTitle) (show hasTitleSure)
```

He then proceeded to wrapping this code in `IO` monad and ended up with the following:

```hs
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

import Control.Monad (forM_)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

data Error = Error (String) deriving (Show)

data Doc = Doc {head :: Maybe Head}

data Head = Head {title :: Maybe String}

data Summary = Summary
  { title :: Maybe String,
    ok :: Bool
  }
  deriving (Show)

-- readDoc

readDoc :: String -> IO (Either Error Doc)
readDoc url =
  return
    if
        | isInfixOf "fail" url -> Left $ Error $ printf "Bad read of %s" url
        | otherwise ->
            Right $
              if
                  | isInfixOf "head-missing" url -> Doc {head = Nothing}
                  | isInfixOf "title-missing" url ->
                      Doc {head = Just Head {title = Nothing}}
                  | isInfixOf "title-empty" url ->
                      Doc {head = Just Head {title = Just ""}}
                  | otherwise ->
                      Doc
                        { head =
                            Just Head {title = Just $ printf "Title of %s" url}
                        }

-- buildSummary

buildSummary :: Doc -> Summary
buildSummary doc =
  Summary {title = doc.head >>= (.title), ok = True}

-- readAndBuildSummary

readAndBuildSummary :: String -> IO Summary
readAndBuildSummary url = do
  docOrError <- readDoc url
  return $ case docOrError of
    Left err -> Summary {title = Nothing, ok = False}
    Right doc -> buildSummary doc

readAndBuildSummary' :: String -> IO Summary
readAndBuildSummary' url =
  readDoc url >>= \docOrError -> return $ case docOrError of
    Left err -> Summary {title = Nothing, ok = True}
    Right doc -> buildSummary doc

readAndBuildSummary'' :: String -> IO Summary
readAndBuildSummary'' url =
  readDoc url <&> \case
    Left err -> Summary {title = Nothing, ok = True}
    Right doc -> buildSummary doc

-- isTitleNonEmpty

isTitleNonEmpty :: Doc -> Maybe Bool
isTitleNonEmpty doc = do
  head <- doc.head
  title <- head.title
  return $ not $ null title

isTitleNonEmpty' :: Doc -> Maybe Bool
isTitleNonEmpty' doc = not <$> null <$> (doc.head >>= (.title))

isTitleNonEmpty'' :: Doc -> Maybe Bool
isTitleNonEmpty'' doc = not . null <$> (doc.head >>= (.title))

-- readWhetherTitleNonEmpty

readWhetherTitleNonEmpty :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty url = do
  docOrError <- readDoc url
  return $ do
    doc <- docOrError
    return $ isTitleNonEmpty doc

readWhetherTitleNonEmpty' :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty' url =
  readDoc url >>= \docOrError ->
    return $ docOrError >>= \doc -> return $ isTitleNonEmpty doc

readWhetherTitleNonEmpty'' :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty'' url = (isTitleNonEmpty <$>) <$> readDoc url

readWhetherTitleNonEmpty''' :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty''' url = readDoc url <&> (<&> isTitleNonEmpty)

-- main

main :: IO ()
main = do
  let urls = ["good", "title-empty", "title-missing", "head-missing", "fail"]
  forM_ urls $ \url -> do
    putStrLn $ printf "Checking \"https://%s/\":" url
    -- Summary.
    summary <- readAndBuildSummary url
    putStrLn $ printf "  Summary: %s" $ show summary
    putStrLn $ printf "  Title: %s" $ fromMaybe "" summary.title
    -- Has title.
    hasTitle <- readWhetherTitleNonEmpty url
    -- let hasTitleSure = either (const False) id $ fromMaybe False <$> hasTitle
    let hasTitleSure = fromMaybe False $ either (const Nothing) id hasTitle
    putStrLn $
      printf "  Has title: %s vs %s" (show hasTitle) (show hasTitleSure)
```

I see a few issues with this code:

1. the abuse of `IO` monad
2. the excessive (an I mean unnecessary) use of `Maybe` monad
3. too many unnecessary `readDoc` calls (bear in mind: it is supposed to be a network call + document parse, so quite a heavy function)

## Improvements

My suggestions on how to make their code better:

1. only stick to `IO` where the function is actually interacting with the external world;
  in this case this would be either `main` or `readDoc` (when it actually makes a network call)
2. eliminate the abuse of `Maybe Bool` - in the functions where the `Maybe Bool` is used (`isTitleNonEmpty` and its variations)
  just `Bool` would suffice
3. only call `readDoc` once and use the result down the pipeline, no need to call it multiple times
  (`readAndBuildSummary`, `readWhetherTitleNonEmpty`)
4. reduce the nested `if` statements to guard expressions
5. replace the `case eitherValue` expressions with the `either` function
6. remove the dead / unused / unnecessary code
  (like those duplicated functions - `readWhetherTitleNonEmpty'`, `readWhetherTitleNonEmpty''` and `readWhetherTitleNonEmpty'''`)

### Use `IO` monad only for the functions which interact with the outside world

`IO` monad tells compiler and the people who read the code the function operates outside of the program internals.
Like actually performing input-output, working with network, operating system, etc.

You can think of any such function as an unsafe part of an application.

In the example above, most of the functions do not comply with that - they are simply a side-effect of poor monad operations
and could be easily changed as follows:

```hs
isTitleNonEmpty :: Doc -> Maybe Bool
isTitleNonEmpty doc = do
  head <- doc.head
  title <- head.title
  return $ not $ null title

buildSummary :: Doc -> Summary
buildSummary doc =
  Summary {title = doc.head >>= (.title), ok = True}

-- there is absolutely no need for this function
readWhetherTitleNonEmpty :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty url = do
  docOrError <- readDoc url
  return $ do
    doc <- docOrError
    return $ isTitleNonEmpty doc

-- this is also redundant
readAndBuildSummary :: String -> IO Summary
readAndBuildSummary url = do
  docOrError <- readDoc url
  return $ case docOrError of
    Left err -> Summary {title = Nothing, ok = False}
    Right doc -> buildSummary doc

-- this is the (reduced) existing code
main__old :: IO ()
main__old = do
  let url = "good"

  putStrLn $ printf "Checking \"https://%s/\":" url

  summary <- readAndBuildSummary url

  putStrLn $ printf "  Summary: %s" $ show summary
  putStrLn $ printf "  Title: %s" $ fromMaybe "" summary.title

  hasTitle <- readWhetherTitleNonEmpty url

-- note how the document is only passed down to the functions which actually operate on the document
-- the only two functions using `IO` monad here are `main` and `readDoc`, as both perform input-output operations
main :: IO ()
main = do
  let url = "good"

  putStrLn $ printf "Checking \"https://%s/\":" url

  docOrError <- readDoc url

  let summary = case docOrError of
    Left err -> Summary {title = Nothing, ok = False}
    Right doc -> buildSummary doc

  putStrLn $ printf "  Summary: %s" $ show summary
  putStrLn $ printf "  Title: %s" $ fromMaybe "" summary.title

  let hasTitle = isTitleNonEmpty doc
```

### Reduce calls to `IO` functions

Pure functions must return the exact same value for the same arguments, regardless of how many times they are called.

`IO` (and a lot of other monads, like `Reader`, `Writer`, etc.) work around this rule by changing the outside world.
So even though a function _technically_ returns the same _value_ every time, the side-effect of an `IO` monad can
potentially change something elsewhere, outside of the function itself.

This makes the programs non-pure and might cause a lot of negative impact. These side effects are what is thought to be
the root of all evils in (functional) programming. They make it hard to figure out what a program actually does.
They make the functions and programs hard to test and prove to be correct.

From the performance perspective, if a function actually does change the outside world, it can't be memoized and
it won't be optimized by the compiler to only use its result, computed once.

Think calling a function which tries to create a user in a system or drop a database table. This would happen on every call.
If you call such function twice, it will most likely fail (due to database failures, hopefully).

But what if the function _creates_ data instead and there are no checks for duplicates in the external system?
Like appending a log line. If such function was to be called multiple times, it can quickly bloat the storage.
This is a potential security issue as well.

So instead of calling such functions (recap: functions returning `IO` monad), why not call them once and store the result?
Then the entire program can operate on the stored result value.

Of course, there are plethora of cases where such behaviour (calling such functions multiple times) is actually desired.
But in the sample code from above, this is not the case.

Check the transformed code from the previous point how this solution is utilized:

```hs
main :: IO ()
main = do
  let url = "good"

  putStrLn $ printf "Checking \"https://%s/\":" url

  -- store the result of fetching a document by its URL
  docOrError <- readDoc url

  -- work on the stored value instead of sending an extra HTTP request
  let summary = case docOrError of
    Left err -> Summary {title = Nothing, ok = False}
    Right doc -> buildSummary doc

  putStrLn $ printf "  Summary: %s" $ show summary
  putStrLn $ printf "  Title: %s" $ fromMaybe "" summary.title

  -- work on the stored value again, preventing one more HTTP request
  let hasTitle = case docOrError of
    Left err -> False
    Right doc -> isTitleNonEmpty doc
```

### Eliminate the abuse of `Maybe Bool`

There's simply no need to go overboard with `Maybe` in methods which simply check whether something is present or not.

`Maybe Bool` introduces a tri-state:

* `Just False`
* `Just True`
* `Nothing`

In cases where this is desired, is much better to represent each of the state with a semantic value:

```hs
type HeaderPresence = NoHeader | EmptyHeader | HasHeader
```

This way the code is much easier to follow and debug.

For the sample in question even this approach is redundant - the function simply checks whether the value (title) is present or not,
so a simple `Bool` would suffice:

```hs
isTitleNonEmpty__old :: Doc -> Maybe Bool
isTitleNonEmpty__old doc = do
  head <- doc.head
  title <- head.title
  return $ not $ null title

isTitleNonEmpty :: Doc -> Bool
isTitleNonEmpty doc =
  not $ null title
  where
    title = doc.head doc >>= head.title

main__old :: IO ()
main__old = do
  let url = "good"

  docOrError <- readDoc url

  hasTitle <- case docOrError of
    Left err -> None
    Right doc -> isTitleNonEmpty__old doc

  let hasTitleSure = fromMaybe False $ either (const Nothing) id hasTitle

  putStrLn $
    printf "  Has title: %s vs %s" (show hasTitle) (show hasTitleSure)

main :: IO ()
main = do
  let url = "good"

  putStrLn $ printf "Checking \"https://%s/\":" url

  docOrError <- readDoc url

  -- a simple True | False value, no need to mess with Maybe
  let hasTitle = case docOrError of
    Left err -> False
    Right doc -> isTitleNonEmpty doc

  -- note: no need to wrap & unwrap Maybe and Either
  putStrLn $ printf "  Has title: %s" (show hasTitle)
```

### Reduce the nested `if` statements - use guard expressions

The original `readDoc` function uses a rather unnecessarily bulky `if` statement:

```hs
readDoc :: String -> IO (Either Error Doc)
readDoc url =
  return
    if
        | isInfixOf "fail" url -> Left $ Error $ printf "Bad read of %s" url
        | otherwise ->
            Right $
              if
                  | isInfixOf "head-missing" url -> Doc {head = Nothing}
                  | isInfixOf "title-missing" url ->
                      Doc {head = Just Head {title = Nothing}}
                  | isInfixOf "title-empty" url ->
                      Doc {head = Just Head {title = Just ""}}
                  | otherwise ->
                      Doc
                        { head =
                            Just Head {title = Just $ printf "Title of %s" url}
                        }
```

Haskell provides a built-in language feature for just this case, which really does make code cleaner a lot:

```hs
readDoc :: String -> IO (Either Error Doc)

readDoc url
  | isInfixOf "fail" url = return $ Left $ Error $ printf "Bad read of %s" url
  | isInfixOf "head-missing" url = return $ Right Doc { head = Nothing }
  | isInfixOf "title-missing" url = return $ Right Doc { head = Just Head { title = Nothing } }
  | isInfixOf "title-empty" url = return $ Right Doc { head = Just Head { title = Just "" } }
  | otherwise = return $ Right Doc { head = Just Head { title = Just $ printf "Title of %s" url } }
```

### Use existing functions on monads instead of `case` statements

In my experience, for the most part, you want to simply apply a function to a `Maybe` or an `Either`.

And, again, for the most part, you either ignore one of the states of the monad (be it `Nothing` or `Left`), or apply a different function to it.

There are handy helper functions for these situations which do make the code cleaner -
[`fromMaybe`](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.Maybe.html#fromMaybe) and
[`either`](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.Either.html#either).

Consider few examples:

```hs
hasTitle__old <- case docOrError of
  Left err -> None
  Right doc -> isTitleNonEmpty__old doc

hasTitle <- either (const None) (isTitleNonEmpty__old)
```

```hs
let summary__old = case docOrError of
  Left err -> Summary {title = Nothing, ok = False}
  Right doc -> buildSummary doc

-- needs a nice helper function
invalid_summary = Summary { title = Nothing, ok = False }

let summary = either invalid_summary buildSummary
```

```hs
let hasTitle__old = case docOrError of
  Left err -> False
  Right doc -> isTitleNonEmpty doc

let hasTitle = either (const False) isTitleNonEmpty
```

## End result

Here's what the code looks like when applying all of the above suggestions and tidying up the code:

```hs
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Monad (forM_)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

newtype Error = Error String deriving (Show)

newtype Doc = Doc {_head :: Maybe Head}

newtype Head = Head {title :: Maybe String}

data Summary = Summary
  { title :: Maybe String,
    ok :: Bool
  }
  deriving (Show)

-- helpers for Repl.It is meh with language extensions

docHead :: Doc -> Maybe Head
docHead = _head

headTitle :: Head -> Maybe String
headTitle = title

summaryTitle :: Summary -> Maybe String
summaryTitle = title

-- implementations

readDoc :: String -> IO (Either Error Doc)
readDoc url
  | "fail" `isInfixOf` url = return $ Left $ Error $ printf "Bad read of %s" url
  | "head-missing" `isInfixOf` url = return $ Right Doc {_head = Nothing}
  | "title-missing" `isInfixOf` url = return $ Right Doc {_head = Just Head {title = Nothing}}
  | "title-empty" `isInfixOf` url = return $ Right Doc {_head = Just Head {title = Just ""}}
  | otherwise = return $ Right Doc {_head = Just Head {title = Just $ printf "Title of %s" url}}

buildSummary :: Doc -> Summary
buildSummary doc =
  Summary {title = docHead doc >>= headTitle, ok = True}

invalidSummary :: Summary
invalidSummary = Summary {title = Nothing, ok = False}

readAndBuildSummary :: Either Error Doc -> Summary
readAndBuildSummary = either invalidSummary buildSummary

readAndBuildTitle :: Summary -> String
readAndBuildTitle summary = fromMaybe "" (summaryTitle summary)

isTitleNonEmpty :: Doc -> Bool
isTitleNonEmpty doc =
  not $ null title
  where
    title = docHead doc >>= headTitle

readWhetherTitleNonEmpty :: Either Error Doc -> Bool
readWhetherTitleNonEmpty = either (const False) isTitleNonEmpty

-- main

main :: IO ()
main = do
  let urls = ["good", "title-empty", "title-missing", "head-missing", "fail"]

  forM_ urls $ \url -> do
    putStrLn $ printf "Checking \"https://%s/\":" url

    docOrError <- readDoc url

    -- Summary
    let summary = readAndBuildSummary docOrError
    let title = readAndBuildTitle summary

    putStrLn $ printf "  Summary: %s" $ show summary
    putStrLn $ printf "  Title: %s" $ title

    -- Has title
    let hasTitle = readWhetherTitleNonEmpty docOrError

    putStrLn $ printf "  Has title: %s" (show hasTitle)
```


