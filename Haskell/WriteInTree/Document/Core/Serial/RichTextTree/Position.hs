module WriteInTree.Document.Core.Serial.RichTextTree.Position
(
	Position,
	HasPosition (..), Positioned (..), PositionedMb (..),
	position_error_in_piso,
	position_error, without_position, maybefy_positioned, fill_position,
	show_position,
)
where

import Fana.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base


type Text = Base.String

type Position = [Text]

show_position :: Position -> Accu.Accumulated Text
show_position =
	List.reverse >>>
	map Fana.show >>>
	List.intersperse (Accu.single ": ") >>>
	Fold.fold >>>
	(Accu.single "\nat "<>)

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

position_error_in_piso ::
	forall e pr pp dr dp .
	HasPosition pp =>
	Optic.PartialIso e pr pp dr dp ->
	Optic.PartialIso (Positioned e) pr pp dr dp
position_error_in_piso (Optic.PartialIso render parse) =
	let
		new_parse :: pp -> Either (Positioned e) dp
		new_parse = (get_position >>> Positioned >>> Bifunctor.first) <*> parse
		in Optic.PartialIso render new_parse

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
		show_pos p = show_position p <> (Accu.single ":\n")
		pos :: Accu.Accumulated Text
		pos = Base.maybe mempty show_pos (position_mb pe)
		in pos <> Fana.show (value_in_PositionedMb pe)

instance Fana.Showable Text e => Fana.Showable Text (Positioned e) where
	show = maybefy_positioned >>> Fana.show
