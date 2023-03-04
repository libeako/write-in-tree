module WriteInTree.Document.Core.Serial.Link.InTree
(
	serialize,
)
where

import Data.Tree (Tree (..), Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data (Inline, Link)
import WriteInTree.Document.Core.Serial.Position (Positioned (..))

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as InNode
import qualified WriteInTree.Document.Core.Serial.Link.Individual as Individual


type Text = Base.String

render :: Tree (Inline InNode.Structure) -> Tree InNode.Structure
render (Node trunk children) =
	let
		trunk_rendered :: InNode.Structure
		trunk_rendered = Data.ilVisual trunk
		children_rendered :: [Tree InNode.Structure]
		children_rendered = map render children
		in
			case Data.ilLink trunk of
				Nothing -> Node trunk_rendered children_rendered
				Just link -> Node trunk_rendered (Individual.render link : children_rendered)

type ParseChildrenSituation =
	(Maybe Link, Forest (Positioned (InNode.Structure)) {- <- the rest of the children -})

parse_children :: Forest (Positioned InNode.Structure) -> Either Text ParseChildrenSituation
parse_children children =
	case children of
		[] -> Right (Nothing, [])
		first : rest ->
			maybe
				(Right (Nothing, children))
				(map (\ l -> (Just l, rest)))
				(Individual.parse first)

parse' :: Tree (Positioned InNode.Structure) -> Either Text (Tree (Positioned (Inline InNode.Structure)))
parse' (Node trunk children) =
	let
		from_situation :: ParseChildrenSituation -> Either Text (Tree (Positioned (Inline InNode.Structure)))
		from_situation (l, rest_of_children) =
			map (Node (map (flip Data.Inline l) trunk)) (traverse parse rest_of_children)
		in parse_children children >>= from_situation

parse :: Tree (Positioned InNode.Structure) -> Either Text (Tree (Positioned (Inline InNode.Structure)))
parse = parse'

serialize :: 
	Optic.PartialIso Text
		(Forest InNode.Structure) (Forest (Positioned InNode.Structure))
		(Forest (Inline InNode.Structure)) (Forest (Positioned (Inline InNode.Structure)))
serialize = Optic.lift_piso (Optic.PartialIso render parse)
