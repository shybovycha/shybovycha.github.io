---
layout: post
title: "How not to use monads"
---

Recently I have watched a video titled ["Optionals and errors in Haskell & Rust - monads by example"]().
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

My suggestions on how to make this code better:

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

Here's what the code looks like when applying all of the above:

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

readDoc :: String -> Either Error Doc
readDoc url
  | "fail" `isInfixOf` url = Left $ Error $ printf "Bad read of %s" url
  | "head-missing" `isInfixOf` url = Right Doc {_head = Nothing}
  | "title-missing" `isInfixOf` url = Right Doc {_head = Just Head {title = Nothing}}
  | "title-empty" `isInfixOf` url = Right Doc {_head = Just Head {title = Just ""}}
  | otherwise = Right Doc {_head = Just Head {title = Just $ printf "Title of %s" url}}

buildSummary :: Doc -> Summary
buildSummary doc =
  Summary {title = docHead doc >>= headTitle, ok = True}

readAndBuildSummary :: Either Error Doc -> Summary
readAndBuildSummary = either (const Summary {title = Nothing, ok = False}) buildSummary

isTitleNonEmpty :: Doc -> Bool
isTitleNonEmpty doc =
  not $ null title
  where
    title = docHead doc >>= headTitle

readWhetherTitleNonEmpty :: Either Error Doc -> Bool
readWhetherTitleNonEmpty = either (const False) isTitleNonEmpty

readAndBuildTitle :: Summary -> String
readAndBuildTitle summary = fromMaybe "" (summaryTitle summary)

-- main

main :: IO ()
main = do
  let urls = ["good", "title-empty", "title-missing", "head-missing", "fail"]

  forM_ urls $ \url -> do
    putStrLn $ printf "Checking \"https://%s/\":" url

    let docOrError = readDoc url

    -- Summary
    let summary = readAndBuildSummary docOrError
    let title = readAndBuildTitle summary

    putStrLn $ printf "  Summary: %s" $ show summary
    putStrLn $ printf "  Title: %s" $ title

    -- Has title
    let hasTitle = readWhetherTitleNonEmpty docOrError

    putStrLn $ printf "  Has title: %s" (show hasTitle)
```


