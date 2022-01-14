module Propellor.Types.Result where

import System.Console.ANSI
import qualified Data.Semigroup as Sem
import Data.Monoid
import Prelude

-- | There can be three results of satisfying a Property.
data Result = NoChange | MadeChange | FailedChange
	deriving (Read, Show, Eq)

instance Sem.Semigroup Result where
	FailedChange <> _ = FailedChange
	_ <> FailedChange = FailedChange
	MadeChange <> _ = MadeChange
	_ <> MadeChange = MadeChange
	NoChange <> NoChange = NoChange

instance Monoid Result where
	mempty = NoChange
	mappend = (Sem.<>)

class ToResult t where
	toResult :: t -> Result

instance ToResult Bool where
	toResult False = FailedChange
	toResult True = MadeChange

instance ToResult Result where
	toResult = id

-- | Results of actions, with color.
class ActionResult a where
	getActionResult :: a -> (String, ColorIntensity, Color)

instance ActionResult Bool where
	getActionResult False = ("failed", Vivid, Red)
	getActionResult True = ("done", Dull, Green)

instance ActionResult Result where
	getActionResult NoChange = ("ok", Dull, Green)
	getActionResult MadeChange = ("done", Vivid, Green)
	getActionResult FailedChange = ("failed", Vivid, Red)
