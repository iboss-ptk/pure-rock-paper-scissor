module Main where

import CSS (px)
import CSS.Font (fontSize)
import CSS.Geometry (paddingBottom, paddingTop)
import CSS.TextAlign (center, textAlign)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Plus (empty)
import Data.List (List, head, (:))
import Data.Maybe (Maybe(..))
import Data.RPS (RPS(..), Result(..), Round(..), Score(..), against, totalScore)
import Data.Strategy (nextThrowWithRandom)
import Prelude hiding (div)
import Pux (App, EffModel, CoreEffects, start)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (div, img)
import Text.Smolder.HTML.Attributes (src, width)
import Text.Smolder.Markup (text, (!), (#!))

data Event = Player RPS | ConcludeRound Round

type State = List Round

-- | Return a new state (and effects) from each event
foldp :: Event -> State -> EffModel State Event (random :: RANDOM)
foldp (Player playerThrow) previousRounds = {
  state: previousRounds ,
  effects: [ concludeRound ] }
  where
    concludeRound = do
      botThrow <- liftEff $ nextThrowWithRandom previousRounds
      pure $ Just $ ConcludeRound (Round botThrow playerThrow)

foldp (ConcludeRound round) previousRounds = { state: round : previousRounds , effects: [] }

-- | Return markup from the state
view :: State -> HTML Event
view rounds =
  div do
    viewScore rounds
    viewLastestRound rounds
    viewSelectThrow

viewSelectThrow :: HTML Event
viewSelectThrow = div ! style styling $ do
    img ! src "rock.png" ! width "70" #! onClick (const $ Player Rock)
    img ! src "paper.png" ! width "70" #! onClick (const $ Player Paper)
    img ! src "scissors.png" ! width "70" #! onClick (const $ Player Scissor)
  where
    styling = do
      textAlign center

viewLastestRound :: State -> HTML Event
viewLastestRound rounds = div ! style styling $
  case head rounds of
    Nothing -> text ""
    Just (Round bot player) -> do
      div $ imgOf bot
      div $ imgOf player
      div $ text $ case player `against` bot of
        Win -> "You win!"
        Lose -> "You lose!"
        Tie -> "It's a tie!"
  where
    styling = do
      paddingTop (50.0 # px)
      paddingBottom (50.0 # px)
      fontSize (20.0 # px)
      textAlign center

    imgOf Rock = img ! src "rock.png" ! width "200"
    imgOf Paper = img ! src "paper.png" ! width "200"
    imgOf Scissor = img ! src "scissors.png" ! width "200"

viewScore :: State -> HTML Event
viewScore rounds = div ! style styling $ text $
  case totalScore rounds of
    Score a b -> "Bot " <> (show a) <> " : " <> (show b) <> " You"
  where
    styling = do
      fontSize (30.0 # px)
      textAlign center

type WebApp = App (DOMEvent -> Event) Event State
-- | Start and render the app

main :: State -> Eff (CoreEffects (random :: RANDOM)) WebApp
main state = do
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input

  pure app

initialState :: State
initialState = empty

