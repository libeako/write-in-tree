module WriteInTree.Document.Core.Serial.Page.Serialize
(
	layer,
)
where

import Control.Monad ((>=>))
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Tree (Tree, Forest, rootLabel)
import Fana.Math.Algebra.Monoid.Accumulate (Accumulated)
import Fana.Prelude
import WriteInTree.Document.Core.Data (nodeLabels, nodePageTrunkStatus, nodeLabels, nodeContent)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..), address_of_Labels, labels_has_class)
import WriteInTree.Document.Core.Serial.RichTextTree.Position 
	(PositionedMb, position_error, maybefy_positioned, without_position)

import qualified Data.Bifunctor as BiFr
import qualified Data.Tree as Tree
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Key.LensToMaybeElement as Map
import qualified Fana.Data.Key.Map.Interface as Map
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Basic


type Text = Base.String

type ParseError = PositionedMb (Accumulated Text)
type ParseFailable = Either ParseError

type NodeT = Basic.Node Text
type PageContent = Tree NodeT
type Page = (PageAddress, PageContent)
type SubPages = Forest Page

get_address_of_node :: NodeT -> ParseFailable (Maybe PageAddress)
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

addressify_node :: NodeT -> ParseFailable (Maybe PageAddress, NodeT)
addressify_node node = map (Pair.before node) (get_address_of_node node)

addressify :: Tree NodeT -> ParseFailable (Tree (Maybe PageAddress, NodeT))
addressify = traverse addressify_node

gather_pages_partially :: 
	Tree (Maybe PageAddress, NodeT) -> 
	Forest (PageAddress, Tree (Maybe PageAddress, NodeT))
gather_pages_partially tree@(Tree.Node (address_mb, trunk) children) =
	let
		child_results = fold (map gather_pages_partially children)
		in 
			case address_mb of
				Nothing -> child_results
				Just address -> [Tree.Node (address, tree) child_results]
gather_all_pages :: 
	Tree (Maybe PageAddress, NodeT) -> 
	ParseFailable (Tree (PageAddress, Tree (Maybe PageAddress, NodeT)))
gather_all_pages tree@(Tree.Node (address_mb, trunk) children) =
	let
		child_results = fold (map gather_pages_partially children)
		in 
			case address_mb of
				Nothing -> Left (without_position (Acc.single "document trunk is not page trunk"))
				Just address -> Right (Tree.Node (address, tree) child_results)

link_to_subpage :: PageAddress -> NodeT -> NodeT
link_to_subpage address =
	Optic.fill Basic.link_in_Node
		(Just (Basic.LIn (unwrapPageAddress address)))

replace_subpages_by_link :: Tree (Maybe PageAddress, NodeT) -> Tree NodeT
replace_subpages_by_link (Tree.Node (address_mb, trunk) children) =
	case address_mb of
		Just address -> Tree.Node (link_to_subpage address trunk) []
		Nothing -> Tree.Node trunk (map replace_subpages_by_link children)

cut_off_sub_pages_of_page :: Tree (Maybe PageAddress, NodeT) -> PageContent
cut_off_sub_pages_of_page (Tree.Node (_, trunk) children) = 
	Tree.Node (trunk) (map replace_subpages_by_link children)

type PageMap = StringyMap.Map Base.Char PageContent

render_sub :: PageMap -> PageContent -> Tree NodeT
render_sub page_map (Tree.Node trunk children) =
	case (labels_has_class "wit-page" (nodeLabels trunk), Basic.ilLink (nodeContent trunk)) of
		(True, Just (Basic.LIn address)) -> 
			let
				page = 
					fromMaybe (Base.error "site render: page address not found")
						(Map.get_at address page_map)
				in render_sub page_map page
		_ -> Tree.Node trunk (map (render_sub page_map) children)

render :: Tree Page -> Tree NodeT
render pages =
	let
		page_map :: PageMap
		page_list :: [(Text, PageContent)]
		page_list = map (BiFr.first unwrapPageAddress) (toList pages)
		page_map = 
			fromRight (Base.error "site render: repetition in page addresses") 
				(Map.from_list_of_uniques page_list)
		in render_sub page_map (snd (rootLabel pages))

parse :: Tree NodeT -> ParseFailable (Tree Page)
parse = (addressify >=> gather_all_pages) >>> map (map (map cut_off_sub_pages_of_page))

layer :: Optic.PartialIso' ParseError (Tree NodeT) (Tree Page)
layer = Optic.PartialIso render parse
