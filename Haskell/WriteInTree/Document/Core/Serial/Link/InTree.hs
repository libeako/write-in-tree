module WriteInTree.Document.Core.Serial.Link.InTree
(
	serialize,
)
where

import Fana.Prelude
import WriteInTree.Document.Core.Data (Inline, Link, TreeA, ForestA)
import WriteInTree.Document.Core.Serial.Position (Positioned (..))

import qualified Fana.Data.Tree.ChildrenWithInfo as ForestA
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as InNode
import qualified WriteInTree.Document.Core.Serial.Link.Individual as Individual


type Text = Base.String

render :: TreeA (Inline InNode.Structure) -> TreeA InNode.Structure
render (ForestA.Node trunk children) =
	let
		trunk_rendered :: InNode.Structure
		trunk_rendered = Data.ilVisual trunk
		children_rendered :: ForestA InNode.Structure
		children_rendered = (map >>> map) render children
		in
			case Data.ilLink trunk of
				Nothing -> ForestA.Node trunk_rendered children_rendered
				Just link -> 
					ForestA.Node trunk_rendered 
						(map (Individual.render link :)children_rendered)

type ParseChildrenSituation =
	(Maybe Link, ForestA (Positioned (InNode.Structure)) {- <- the rest of the children -})

parse_children :: ForestA (Positioned InNode.Structure) -> Either Text ParseChildrenSituation
parse_children i@(a, children) =
	case children of
		[] -> Right (Nothing, i)
		first : rest ->
			maybe
				(Right (Nothing, i))
				(map (\ l -> (Just l, (a, rest))))
				(Individual.parse first)

parse :: TreeA (Positioned InNode.Structure) -> Either Text (TreeA (Positioned (Inline InNode.Structure)))
parse (ForestA.Node trunk children) =
	let
		from_situation :: ParseChildrenSituation -> Either Text (TreeA (Positioned (Inline InNode.Structure)))
		from_situation (l, rest_of_children) =
			map
				(ForestA.Node (map (flip Data.Inline l) trunk))
				((traverse >>> traverse) parse rest_of_children)
		in parse_children children >>= from_situation

serialize_tree ::
	Optic.PartialIso Text
		(TreeA InNode.Structure) (TreeA (Positioned InNode.Structure))
		(TreeA (Inline InNode.Structure)) (TreeA (Positioned (Inline InNode.Structure)))
serialize_tree =
	Optic.PartialIso
		render
		parse

serialize ::
	Optic.PartialIso Text
		(ForestA InNode.Structure) (ForestA (Positioned InNode.Structure))
		(ForestA (Inline InNode.Structure)) (ForestA (Positioned (Inline InNode.Structure)))
serialize =
	 (Optic.lift_piso >>> Optic.lift_piso)
		serialize_tree
