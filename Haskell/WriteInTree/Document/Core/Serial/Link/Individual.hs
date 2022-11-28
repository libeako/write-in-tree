module WriteInTree.Document.Core.Serial.Link.Individual
(
	meta_node_name,
	MetaNodeName (..),
	render, parse,
)
where


import Data.Tree (Tree (..))
import Fana.Prelude
import WriteInTree.Document.Core.Data (Link)
import WriteInTree.Document.Core.Serial.Position 
	(Positioned (..), prefix_error_message_with_position_from)

import qualified Data.Bifunctor as BiFr
import qualified Data.List as List
import qualified Data.Tree as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Enum as Serial
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as Ntt
import qualified WriteInTree.Document.Core.Serial.Position as Pos


type Text = Base.String

data MetaNodeName = MnLink deriving (Base.Enum, Base.Bounded)

data DestinationType = Internal | External deriving (Base.Enum, Base.Bounded)

render_DestinationType :: DestinationType -> Text
render_DestinationType = \case
	Internal -> "internal"
	External -> "external"

layer_destination_type :: Optic.PartialIso' Text Text DestinationType
layer_destination_type =
	let
		error_description = "link destination type not recognized"
		in 
			Optic.piso_convert_error_with_low (const (const error_description)) 
				(Serial.enum render_DestinationType)

children_number_error_message :: Text
children_number_error_message =
	"a link node must have exactly 2 arguments [target type, target address]"

meta_node_name :: Text
meta_node_name = "links-to"

parse_link_from_words :: [Text] -> Either Text Link
parse_link_from_words =
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

parse_link_tree :: [Text] -> Either Text Link
parse_link_tree arguments = 
	let 
		words :: [Text]
		words = fold (map List.words arguments)
		in parse_link_from_words words

parse_branch_on_link :: Tree Text -> Maybe (Either Text Link)
parse_branch_on_link (Node trunk children) =
	map (List.dropWhile (== ' ') >>> (: fold (map toList children)) >>> parse_link_tree)
		(List.stripPrefix (Ntt.render_exceptional meta_node_name) trunk)

parse :: Tree (Positioned Text) -> Maybe (Either Text Link)
parse tree =
	map (BiFr.first (prefix_error_message_with_position_from (Tree.rootLabel tree)))
		(parse_branch_on_link (map Pos.positionedValue tree))

render :: Link -> Tree Text
render d =
	let
		(dt, addr) =
			case d of
				Data.LIn a -> (Internal, a)
				Data.LEx a -> (External, a)
		in Node (List.intercalate " " [Ntt.render_exceptional meta_node_name, render_DestinationType dt, addr]) []
