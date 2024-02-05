module WriteInTree.Document.Core.Serial.Id
(
	test,
)
where


import Data.Tree (Tree, Forest)
import Fana.Develop.Test.Define (Test)
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Position

import qualified Fana.Data.Function as Fn
import qualified Fana.Data.List as List
import qualified Fana.Data.Tree.ChildrenWithInfo as ForestA
import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Data.List as List
import qualified Data.Tree as Tree
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as InNode

type Error = Text

parse_id_value :: [Text] -> Either Error Address
parse_id_value =
	\case
		[] -> Left "no id value given"
		[i] -> Right (Address i)
		_ -> Left "id value would be only one word"

id_instruction_meta_text :: Text
id_instruction_meta_text = "id"

render_id_instruction_words :: Address -> [Text]
render_id_instruction_words a = [id_instruction_meta_text, unwrapPageAddress a]

parse_id_instruction_words :: [Text] -> Either Error (Maybe Address)
parse_id_instruction_words =
	\case
		[] -> pure Nothing
		h : t ->
			if h == id_instruction_meta_text 
				then map Just (parse_id_value t)
				else pure Nothing

render_id_instruction :: Address -> Text
render_id_instruction = render_id_instruction_words >>> List.intercalate " "

parse_id_instruction :: Text -> Either Error (Maybe Address)
parse_id_instruction = List.words >>> parse_id_instruction_words


type NodeH = Either Address InNode.Structure

render_node :: NodeH -> InNode.Structure
render_node = either (render_id_instruction >>> InNode.Meta) id

parse_node :: InNode.Structure -> Either Error NodeH
parse_node i =
	case i of
		InNode.Norm _ -> Right (Right i)
		InNode.Meta t ->
			let
				decide :: Maybe Address -> NodeH
				decide = maybe (Right i) Left
				in map decide (parse_id_instruction t)

serialize_node :: Optic.PartialIso' Text InNode.Structure NodeH
serialize_node = Optic.PartialIso render_node parse_node


parse_positioned_node :: Positioned InNode.Structure -> Either Error (Either (Positioned Address) (Positioned InNode.Structure))
parse_positioned_node (Positioned p i) =
	let
		on_success :: NodeH -> Either (Positioned Address) (Positioned InNode.Structure)
		on_success =
			\case
				Left a -> Left (Positioned p a)
				Right e -> Right (Positioned p e)
		in map on_success (parse_node i)

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
render = render_forest_to_renderable_lines >>> (map >>> map) render_node

parse :: Forest (Positioned InNode.Structure) -> Either Error (ForestA (Positioned InNode.Structure))
parse = (traverse >>> traverse) parse_positioned_node >>> map parse_forest_from_parsed_lines

serialize :: Optic.PartialIso Error 
	(Forest InNode.Structure) (Forest (Positioned InNode.Structure))
	(ForestA InNode.Structure) (ForestA (Positioned InNode.Structure)) 
serialize = Optic.PartialIso render parse


{- ----------------- TESTS ------------------- -}

test :: Test
test = Test.single "id line" 
	(
		Optic.test_piso' 
			[InNode.Meta "id blabla dfg"] 
			[Left (Address "blabla"), Right (InNode.Meta "idblabla")] 
			serialize_node
	)
