module WriteInTree.Document.Core.Serial.Position
(
	Count,
	Position, inPositioned_position,
	HasPosition (..), Positioned (..), 
	prefix_error_message_with_position, prefix_error_message_with_position_from,
)
where

import Fana.Data.Tree.SerializeHight (Count)
import Fana.Prelude

import qualified Fana.Data.Function as Fn
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type Text = Base.String
type Position = Count

class HasPosition p where get_position :: p -> Position

instance HasPosition Position where get_position = id

prefix_error_message_with_position :: Position -> Fn.Endo Text
prefix_error_message_with_position p em = "at line " <> Base.show p <> ":\n" <> em

prefix_error_message_with_position_from :: HasPosition p => p -> Fn.Endo Text
prefix_error_message_with_position_from = get_position >>> prefix_error_message_with_position

show_position :: Position -> Text
show_position p = prefix_error_message_with_position p ""

data Positioned e =
	Positioned
	{ position :: Position
		-- ^ the position in reverse order [towards trunk].
	, positionedValue :: e
	}
	deriving (Eq, Functor, Foldable, Traversable)


inPositioned_position :: Optic.Lens' Position (Positioned e)
inPositioned_position = Optic.lens_from_get_set position (\ p w -> w { position = p })

instance HasPosition (Positioned e) where get_position = position
