module WriteInTree.Document.Core.Serial.Page.Serialize
(
	ParseError, layer,
)
where

import Control.Monad ((>=>))
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Tree (Tree, Forest, rootLabel)
import Fana.Math.Algebra.Monoid.Accumulate (Accumulated)
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Page.Data (Page, PageContent)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..), address_of_Labels, labels_has_class)
import WriteInTree.Document.Core.Serial.RichTextTree.Position 
	(PositionedMb, position_error, maybefy_positioned, without_position)

import qualified Data.Bifunctor as BiFr
import qualified Data.List as List
import qualified Data.Tree as Tree
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Key.LensToMaybeElement as Map
import qualified Fana.Data.Key.Map.Interface as Map
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Basic


type PageMap = StringyMap.Map Base.Char PageContent

render_sub :: PageMap -> Tree Node -> Tree Node
render_sub page_map (Tree.Node trunk children) =
	case (labels_has_class "wit-page" (nodeLabels trunk), Basic.ilLink (nodeContent trunk)) of
		(True, Just (Basic.LIn address)) ->
			let
				page =
					fromMaybe (Base.error "site render: page address not found")
						(Map.get_at address page_map)
				in render_page page_map page
		_ -> Tree.Node trunk (map (render_sub page_map) children)

render_page :: PageMap -> PageContent -> Tree Node
render_page page_map (title_node, children) =
	Tree.Node title_node (map (render_sub page_map) children)

render :: Tree Page -> Tree Node
render pages =
	let
		page_map :: PageMap
		page_list :: [(Text, PageContent)]
		page_list = map (BiFr.first unwrapPageAddress) (toList pages)
		page_map =
			fromRight (Base.error "site render: repetition in page addresses") 
				(Map.from_list_of_uniques page_list)
		in render_page page_map (snd (rootLabel pages))


type ParseError = PositionedMb (Accumulated Text)
type ParseFailable = Either ParseError

get_address_of_node :: Node -> ParseFailable (Maybe PageAddress)
get_address_of_node node =
	case nodePageTrunkStatus node of
		Basic.IsNotPageTrunk -> Right Nothing
		Basic.IsPageTrunk -> 
			case address_of_Labels (nodeLabels node) of
				Nothing ->
					let 
						error = Acc.single "page trunk node does not have address."
						positioned_error = maybefy_positioned (position_error node error)
						in Left positioned_error
				Just address -> Right (Just address)

type AddressedNode = (Maybe PageAddress, Node)

addressify_node :: Node -> ParseFailable AddressedNode
addressify_node node = map (Pair.before node) (get_address_of_node node)

addressify :: Tree Node -> ParseFailable (Tree AddressedNode)
addressify = traverse addressify_node


parse_page :: PageAddress -> Node -> Forest AddressedNode -> Tree Page
parse_page address trunk_node children =
	let
		main_content :: [Tree Node]
		sub_pages :: [Forest Page]
		(main_content, sub_pages) = List.unzip (map parse_sub_tree children)
		in Tree.Node (address, (trunk_node, main_content)) (fold sub_pages)

parse_sub_tree :: Tree AddressedNode -> (Tree Node, Forest Page)
parse_sub_tree (Tree.Node (address_mb, trunk_node) children) =
	let
		as_sub_content :: (Tree Node, Forest  Page)
		as_sub_content =
			let (sub_contents, sub_pages) = List.unzip (map parse_sub_tree children)
				in (Tree.Node trunk_node sub_contents, fold sub_pages)
		as_page :: PageAddress -> (Tree Node, Forest Page)
		as_page address =
			let	link_content = Tree.Node (link_to_subpage address trunk_node) []
				in (link_content, [parse_page address trunk_node children])
		in maybe as_sub_content as_page address_mb

link_to_subpage :: PageAddress -> Node -> Node
link_to_subpage address =
	Optic.fill Basic.link_in_Node
		(Just (Basic.LIn (unwrapPageAddress address)))

parse_from_addressed_tree :: Tree AddressedNode -> ParseFailable (Tree Page)
parse_from_addressed_tree (Tree.Node (address_mb, title_node) children) =
	case address_mb of
		Nothing -> Left (without_position (Acc.single "document trunk is not page trunk"))
		Just address -> Right (parse_page address title_node children)

parse :: Tree Node -> ParseFailable (Tree Page)
parse = addressify >=> parse_from_addressed_tree

layer :: Optic.PartialIso' ParseError (Tree Node) (Tree Page)
layer = Optic.PartialIso render parse
