module Types where

import qualified Graphics.UI.GLFW             as GLFW
import qualified Graphics.Rendering.OpenGL.GL as GL
import Control.Monad.State.Lazy
import Control.Lens
import Data.Monoid

import Graphics.UI.HamGui.Types

data Program = Program {
    _program       :: Maybe GL.Program,
    _bufferArray   :: Maybe GL.BufferObject,
    _bufferElement :: Maybe GL.BufferObject,
    _bufferStorage :: Maybe GL.BufferObject
  }
$(makeLenses ''Program)

data InputEvent =
    KeyEvent GLFW.Key
  | CharEvent Char
  | MouseEvent Double Double
  | ResizeEvent Int Int
$(makePrisms ''InputEvent)

data E = EA | EB deriving Show
$(makePrisms ''E)

data Event = GameEvent Char | UIEvent Int | UIEvent2 E deriving Show
$(makePrisms ''Event)

newtype EventBus = EventBus { _bus :: [Event] } deriving Show
$(makeLenses ''EventBus)

data GameState = GameState
  {
    _windowHandle :: GLFW.Window,
    _hamGuiState  :: HamGuiData,
    _programMain  :: Program,
    _programHG    :: Program,
    _eventBus     :: EventBus
  }
makeLenses ''GameState

type Game a = StateT GameState IO a

liftGUI :: HamGui a -> Game a
liftGUI = zoom hamGuiState

dispatchEvent :: Event -> Game ()
dispatchEvent = modifying (eventBus . bus) . (:)

getEventsOf :: Getting Any Event a -> Game [Event]
getEventsOf prism = do
  uses (eventBus . bus) (\b -> b ^.. traverse . (filtered $ has prism))

consumeEventsOf :: Getting Any Event a -> Game [Event]
consumeEventsOf prism = do
  result <- uses (eventBus . bus) (\b -> b ^.. traverse . (filtered $ has prism))
  without_result <- uses (eventBus . bus) (\b -> b ^.. traverse . (filtered $ not . has prism))
  eventBus . bus .= without_result
  pure result

clearEvents :: Game ()
clearEvents = eventBus . bus .= mempty