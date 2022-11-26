module WriteInTree.Document.Core.Serial.Link.Individual
(
	meta_node_name,
	MetaNodeName (..),
	ParseError,
	render, parse',
)
where


import Data.Tree (Tree (..))
import Fana.Prelude
import WriteInTree.Document.Core.Data (Link)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))

import qualified Data.Bifunctor as BiFr
import qualified Data.Tree as Tree
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Enum as Serial
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNodeTextStructure as Ntt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String

data MetaNodeName = MnLink deriving (Base.Enum, Base.Bounded)

type ParseError' = Accu.Accumulated Text
type ParseError = Pos.Positioned ParseError'

data DestinationType = Internal | External deriving (Base.Enum, Base.Bounded)

render_DestinationType :: DestinationType -> Text
render_DestinationType = \case
	Internal -> "internal"
	External -> "external"

layer_destination_type :: Optic.PartialIso' (Accu.Accumulated Text) Text DestinationType
layer_destination_type =
	let
		error_description = "link destination type not recognized"
		in 
			Optic.piso_convert_error_with_low (const (const error_description)) 
				(Serial.enum render_DestinationType)

children_number_error_message :: Accu.Accumulated Text
children_number_error_message =
	Accu.single "a link node must have exactly 2 children [target type, target address]"

meta_node_name :: Text
meta_node_name = "links-to"

parse_core' :: [Tree Text] -> Either ParseError' Link
parse_core' =
	map Tree.rootLabel >>>
	\case
		[destination_type, address] ->
			let
				build :: DestinationType -> Link
				build =
					\case
						Internal -> Data.LIn address
						External -> Data.LEx address
				in map build (Optic.piso_interpret layer_destination_type destination_type)
		_ -> Left children_number_error_message

parse :: Tree Text -> Maybe (Either ParseError' Link)
parse (Node trunk children) =
	if trunk == Ntt.render_exceptional meta_node_name
		then Just (parse_core' children)
		else Nothing

parse' :: Tree (Positioned Text) -> Maybe (Either ParseError Link)
parse' tree =
	map (BiFr.first (Pos.position_error (Tree.rootLabel tree)))
		(parse (map Pos.positionedValue tree))

render :: Link -> Tree Text
render d =
	let
		(dt, addr) =
			case d of
				Data.LIn a -> (Internal, a)
				Data.LEx a -> (External, a)
		in Node (Ntt.render_exceptional meta_node_name)
			(map (flip Node []) [render_DestinationType dt, addr])
