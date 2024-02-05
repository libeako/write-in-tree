module WriteInTree.Document.Core.Serial.Id.Forest
(
	serialize,
)
where


import Data.Tree (Tree, Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Position
import qualified WriteInTree.Document.Core.Serial.Id.Node as Node

import qualified Fana.Data.Function as Fn
import qualified Fana.Data.List as List
import qualified Fana.Data.Tree.ChildrenWithInfo as ForestA
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Data.Tree as Tree
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as InNode


type Error = Text
type NodeH = Either Address InNode.Structure

render_tree_to_renderable_lines :: TreeA InNode.Structure -> Tree (Either Address InNode.Structure)
render_tree_to_renderable_lines (ForestA.Node t c) = Tree.Node (Right t) (render_forest_to_renderable_lines c)

render_forest_to_renderable_lines ::
	ForestA InNode.Structure ->
	Forest (Either Address InNode.Structure)
render_forest_to_renderable_lines (ForestA.Forest mba c) = 
	let
		appender :: Fn.Endo (Forest (Either Address InNode.Structure))
		appender =
			case mba of
				Nothing -> id
				Just a -> (Tree.Node (Left a) [] :)
		in (map render_tree_to_renderable_lines c)

parse_tree_from_parsed_lines ::
	Tree (Either (Positioned Address) (Positioned InNode.Structure)) ->
	Either (Positioned Address) (TreeA (Positioned InNode.Structure))
parse_tree_from_parsed_lines (Tree.Node t c) = 
	case t of
		Left a -> Left a
		Right nt -> Right (ForestA.Node nt (parse_forest_from_parsed_lines c))

parse_forest_from_parsed_lines ::
	Forest (Either (Positioned Address) (Positioned InNode.Structure)) ->
	ForestA (Positioned InNode.Structure)
parse_forest_from_parsed_lines =
	map parse_tree_from_parsed_lines >>> partitionEithers >>>
	\ (addresses, normal_subtrees) -> ForestA.Forest (map positionedValue (List.first addresses)) normal_subtrees

render :: ForestA InNode.Structure -> Forest InNode.Structure
render = render_forest_to_renderable_lines >>> (map >>> map) Node.render

parse :: Forest (Positioned InNode.Structure) -> Either Error (ForestA (Positioned InNode.Structure))
parse = (traverse >>> traverse) Node.parse_positioned >>> map parse_forest_from_parsed_lines

serialize :: Optic.PartialIso Error 
	(Forest InNode.Structure) (Forest (Positioned InNode.Structure))
	(ForestA InNode.Structure) (ForestA (Positioned InNode.Structure)) 
serialize = Optic.PartialIso render parse
