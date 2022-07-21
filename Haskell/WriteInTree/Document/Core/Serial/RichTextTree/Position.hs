module WriteInTree.Document.Core.Serial.RichTextTree.Position
(
	Position, PositionFields (..),
	HasPosition (..), Positioned (..), PositionedMb (..), 
	position_error, without_position, maybefy_positioned, fill_position,
	show_position,
)
where

import Fana.Prelude
import Data.Default.Class

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base


type Text = Base.String

type Position = [Text]

show_position :: Position -> Accu.Accumulated Text
show_position pos = 
	Fold.foldr' (<>) (Accu.single "at ") 
		(List.intersperse (Accu.single " : ") (map Fana.show (List.reverse pos)))

data PositionFields = PositionFields 
	{
		field_source_path :: Position
		-- ^ absolute position of the node in the picture tree, with node text values.
	}
	deriving (Eq)

instance Default PositionFields where def = PositionFields def

class HasPosition p where get_position :: p -> Position

instance HasPosition Position where get_position = id

data Positioned e = Positioned
	{ position :: Position
		-- ^ the position in reverse order [towards trunk].
	, positionedValue :: e
	}
	deriving (Functor, Foldable, Traversable)

instance HasPosition (Positioned e) where get_position = position

position_error :: HasPosition a => a -> e -> Positioned e
position_error = get_position >>> Positioned

-- | maybe positioned.
data PositionedMb e = PositionedMb
	{ position_mb :: Maybe Position
		-- ^ the position in reverse order [towards trunk].
	, value_in_PositionedMb :: e
	}
	deriving (Functor, Foldable, Traversable)	

without_position :: e -> PositionedMb e
without_position e = PositionedMb Nothing e

maybefy_positioned :: Positioned e -> PositionedMb e
maybefy_positioned (Positioned pos val) = PositionedMb (Just pos) val

fill_position :: HasPosition a => a -> PositionedMb e -> Positioned e
fill_position a (PositionedMb pos val) = Positioned (Base.fromMaybe (get_position a) pos) val

instance Fana.Showable Text e => Fana.Showable Text (PositionedMb e) where
	show pe = let
		show_pos :: Position -> Accu.Accumulated Text
		show_pos p = show_position p <> (Accu.single " : ")
		pos :: Accu.Accumulated Text
		pos = Base.maybe mempty show_pos (position_mb pe)
		in pos <> Fana.show (value_in_PositionedMb pe)

instance Fana.Showable Text e => Fana.Showable Text (Positioned e) where
	show = maybefy_positioned >>> Fana.show
