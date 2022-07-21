module WriteInTree.Document.Core.Serial.RichTextTree.Position
(
	PositionAtLevel (..), Position, PositionFields (..), ofPositionFields_ordinal,
	HasPosition (..), Positioned (..), PositionedMb (..), 
	position_error, without_position, maybefy_positioned, fill_position, 
)
where

import Fana.Prelude
import Data.Default.Class

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Categories.Lens as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Fana.Serial.Print.Show as Show
import qualified Prelude as Base


type Text = Base.String


-- | the position of an element among its siblings.
data PositionAtLevel = PositionAtLevel { text :: Text } deriving (Eq)

instance Fana.Showable Text PositionAtLevel where
	show pos = Show.from_ShowS (("\"" <>) <<< (text pos <>) <<< ("\"" <>))

type Position = [PositionAtLevel]

instance Fana.Showable Text Position where
	show pos = 
		Fold.foldr' (<>) (Accu.single "at ") 
			(List.intersperse (Accu.single " : ") (map Fana.show (List.reverse pos)))

data PositionFields = PositionFields 
	{ field_ordinal :: () 
		-- ^ relative position [the ordinal] of the node among its siblings.
	, field_source_path :: Position
		-- ^ absolute position of the node in the picture tree, with node text values.
	}
	deriving (Eq)

ofPositionFields_ordinal :: Optic.Lens' () PositionFields
ofPositionFields_ordinal = Optic.lens_from_get_set field_ordinal (\ e c -> c { field_ordinal = e })

instance Default PositionFields where def = PositionFields def def

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
		show_pos p = Fana.show p <> (Accu.single " : ")
		pos :: Accu.Accumulated Text
		pos = Base.maybe mempty show_pos (position_mb pe)
		in pos <> Fana.show (value_in_PositionedMb pe)

instance Fana.Showable Text e => Fana.Showable Text (Positioned e) where
	show = maybefy_positioned >>> Fana.show
