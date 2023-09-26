# SoftWar in Elm

```hs
module Main exposing (..)

import Browser
import List
import Random
import Random.List
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type Card = Employee Int
  | Treat Int
  | Traitor
  | SickLeave
  | PairProgramming
  | GoodClientRelationships
  | Necromantic
  | IQuit
  | GitPull
  | GitPush
  | Flush
  | Nope

type alias Player =
  { hand : List Card,
    table : List Card
  }

-- ------

type alias Model =
  { players : List Player,
    drawPile : List Card,
    discardPile : List Card
  }

allCards : List Card
allCards =
  [ -- 12x booster (+1)
    -- 4x promotion (+2)
    -- 4x faker (=0)
    -- 4x pair programming
    -- 4x sick leave
    -- 4x traitor (*-1)
    -- 10x employee (2)
    -- 12x employee (4)
    -- 10x employee (6)
    -- 4x flush
    -- 4x git pull
    -- 4x git push --force
    -- 4x iQuit
    -- 5x necromantic
    -- 5x nope
    -- 4x good client relationships
  ]

init : Model
init = { players = [], drawPile = allCards, discardPile = [] }

-- UPDATE

type Msg
  = NewGame Int -- number of players (mb scenario?..)
  | ShuffleDrawPile cards
  -- | PlayCard Player Card Maybe (Player, Int) -- playCard me myCard maybeTargetPlayerAndTablePosition
  -- BETTER: Play_iQuit | Play_Employee Int | Play_Nope, etc.

shuffleDeck : List Card -> Random.Generator List Card
shuffleDeck cards = Random.List.shuffle cards

-- generateNewPlayer : List Card -> (List Card, List Card) -- existing_allCards -> (newPlayerHand, new_allCards)
-- generateNewPlayer existingCards =
--   let
--     new_playerHand = Random.uniform (Employee 2) existingCards
--     new_allCards = List.filter (\card -> List.any ((==) card) new_playerHand) existingCards
--   in
--     (new_allCards, new_playerHand)

-- appendNewPlayer : List Player -> (List Card, List Card) -> (List Card, List Player)
-- appendNewPlayer old_players newPlayerData =
--   let
--     (new_playerHand, new_allCards) = newPlayerData
--     newPlayer = Player new_playerHand []
--   in
--     (new_allCards, newPlayer :: old_players)

-- generateNewTable : Int -> (List Card, List Player)
-- generateNewTable numPlayers =
--   let
--     players1 = List.repeat numPlayers 0
--   in
--     List.foldl (\_ (old_allCards, old_players) -> appendNewPlayer old_players (generateNewPlayer old_allCards)) (allCards, []) players1

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewGame numPlayers ->
      Random.generate (\new_allCards -> GeneratePlayers numPlayers new_allCards) (shuffleDeck allCards)

    GeneratePlayers numPlayers new_allCards ->
      let
        (new_players, allCards2) = List.foldl (\_ (players, cards) -> (Player (List.take 10 cards) [], List.drop 10 cards) ([], allCards2)) (List.repeat numPlayers 0)
      in
        ({ model | drawPile = new_allCards, players = new_players }, Cmd.none)

      --   (new_allCards, players) = generateNewTable numPlayers
      -- in
      --   ({ model | players = players, drawPile = new_allCards, discardPile = [] }, Cmd.none)

    -- PlayCard player card maybeTargetPlayerAndTheirTablePosition ->
    --   case card of
    --     Employee ep -> model
    --     Treat ep -> model
    --     Traitor -> model
    --     SickLeave -> model
    --     PairProgramming -> model
    --     GoodClientRelationships -> model
    --     Necromantic -> model
    --     IQuit -> model
    --     GitPull -> model
    --     GitPush -> model
    --     Flush -> model
    --     Nope -> model

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ text "hello world"
    ]
```