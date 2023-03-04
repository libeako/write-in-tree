module WriteInTree.Document.Core.Serial.Paragraph
(
	serialize,
)
where

import Data.Tree (Tree, Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Position (Positioned (..))

import qualified Data.Tree as Tree
import qualified Fana.Data.Tree.OfBase as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as InNode


meta_node_name :: Text
meta_node_name = "paragraph"

serialize_tree ::
	Optic.Iso
		(Tree (Inline InNode.Structure))
		(Tree (Positioned (Inline InNode.Structure)))
		(Tree (Paragraph InNode.Structure))
		(Tree (Positioned (Paragraph InNode.Structure)))
serialize_tree =
	let
		render' :: Paragraph InNode.Structure -> Tree (Inline InNode.Structure)
		render' =
			\case
				[] -> Tree.Node (Inline (InNode.Meta meta_node_name) Nothing) []
				[single] -> Tree.Node single []
				many -> 
					Tree.Node
						(Inline (InNode.Meta meta_node_name) Nothing)
						(map (flip Tree.Node []) many)
		render :: Tree (Paragraph InNode.Structure) -> Tree (Inline InNode.Structure)
		render (Tree.Node input_trunk children) =
			Optic.fn_up Tree.children_in_tree (<> map render children) (render' input_trunk)
		parse :: Tree (Positioned (Inline InNode.Structure)) -> Tree (Positioned (Paragraph InNode.Structure))
		parse (Tree.Node input_trunk children) = 
			let
				normal = Tree.Node (map (: []) input_trunk) (map parse children)
				in
					case ilVisual (positionedValue input_trunk) of
						InNode.Norm _ -> normal
						InNode.Meta text -> 
							case text == meta_node_name of
								False -> normal
								True -> 
									let
										cut_children :: [Inline InNode.Structure]
										cut_children = map (Tree.rootLabel >>> positionedValue) children
										in Tree.Node (map (const cut_children) input_trunk) []
		in Optic.Iso render parse

serialize ::
	Optic.Iso
		(Forest (Inline InNode.Structure))
		(Forest (Positioned (Inline InNode.Structure)))
		(Forest (Paragraph InNode.Structure))
		(Forest (Positioned (Paragraph InNode.Structure)))
serialize = Optic.lift_iso serialize_tree
