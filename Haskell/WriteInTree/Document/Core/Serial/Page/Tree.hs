-- | write-in-tree output data format.
module WriteInTree.Document.Core.Serial.Page.Tree
(
	SubPageTarget (..),
	CrossLinkTarget (..),
	LinkInternalTarget (..),
	Link, Inline, Paragraph, Node, Structure,
	UserAddressMap,
	Page (..), Site (..),
	get_page_of_Site_at, get_CrossLinkTarget_page,

	title_of_section, title_of_page, id_of_page, is_inline_a_page_break, get_subpages_of_page,
	
	melt_pages_to_single, compile_site, layer
)
where

import Control.Arrow ((&&&))
import Control.Monad.State.Lazy (State, evalState)
import Data.Ord (max)
import Data.Tree (Tree)
import Data.Array (array, (!))
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import Prelude (String, Int, (+))

import qualified Control.Monad.State.Lazy as State
import qualified Data.Array as Array
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Tree as Tree
import qualified Fana.Data.HeteroPair as HePair
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as UI
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String
type PageKey = Int
page_key_start :: PageKey
page_key_start = 1
type Array = Array.Array PageKey
type KeyedPage i = (PageKey, Page i)

type PagePath = [String]

data SubPageTarget id_u =
	SubPageTarget { sptPage :: Page id_u, sptId :: Text }
	deriving (Eq)

type LinkInternalTarget (id_u :: Type) = Either (SubPageTarget id_u) id_u

type Link (id_u :: Type) = UI.Link (LinkInternalTarget id_u)
type Inline (id_u :: Type) = UI.Inline (LinkInternalTarget id_u)
type Paragraph (id_u :: Type) = UI.Paragraph (LinkInternalTarget id_u)
type Node (id_u :: Type) = UI.Node id_u (LinkInternalTarget id_u)

type Structure (id_u :: Type) = Tree.Tree (Node id_u)

data Page (id_u :: Type) = Page
	{
	pagePathToTrunk :: [Node id_u],
	pageContent :: Structure id_u,
	pageIsTrunk :: Bool
	}
	deriving (Eq)

{-| Link address to page but not to sub-page. -}
data CrossLinkTarget = CrossLinkTarget { cltPage :: PageKey } deriving (Eq)
	
type UserAddressMap id_u = Map.Map id_u CrossLinkTarget

data Site (id_u :: Type) = Site
	{
	siteMainPage :: Page id_u,
	siteAllPages :: Array (Page id_u),
	siteUserAddressMap :: UserAddressMap id_u,
	sitePageMap :: Map.Map Text (Page id_u)
	}
	deriving (Eq)

get_page_of_Site_at :: Site id_u -> PageKey -> Page id_u
get_page_of_Site_at site key = siteAllPages site ! key

get_CrossLinkTarget_page :: Site id_u -> CrossLinkTarget -> Page id_u
get_CrossLinkTarget_page site = cltPage >>> get_page_of_Site_at site

make_Site :: forall id_u . Page id_u -> Array (Page id_u)-> UserAddressMap id_u -> Site id_u
make_Site main_page all_pages ua_map =
	let
		page_map =
			let
				make_key_value_pair :: Page id_u -> (Text, Page id_u)
				make_key_value_pair = id_of_page &&& id
				in Map.fromList (map make_key_value_pair (Fold.toList (get_subpages_of_page main_page)))
	in Site main_page all_pages ua_map page_map

is_link_a_page_break :: Link id_u -> Bool
is_link_a_page_break =
	\case
		UI.LIn lit -> either (const True) (const False) lit
		UI.LEx _ -> False

is_inline_a_page_break :: Inline id_u -> Bool
is_inline_a_page_break = UI.ilLink >>> maybe False is_link_a_page_break

title_of_section :: Node idts -> String
title_of_section = Optic.to_list UI.texts_in_Node >>> Fold.concat

title_of_page :: Page idts -> String
title_of_page = pageContent >>> Tree.rootLabel >>> title_of_section


-- optics :

of_Structure_SubPageTarget ::
	Optic.Traversal
		(SubPageTarget i) (SubPageTarget i)
		(Structure i) (Structure i)
of_Structure_SubPageTarget =
	Category2.identity >**>^ Optic.prism_Left >**>^ UI.internal_address_in_tree

get_direct_subpages_of_page :: Page i -> [Page i]
get_direct_subpages_of_page =
	pageContent >>>
	Optic.to_list of_Structure_SubPageTarget >>>
	map sptPage

get_subpages_of_page :: Page i -> Tree (Page i)
get_subpages_of_page trunk =
	Tree.Node trunk (map get_subpages_of_page (get_direct_subpages_of_page trunk))

content_in_Page :: Optic.Lens' (Structure idts) (Page idts)
content_in_Page = Optic.lens_from_get_set pageContent (\ c p -> p { pageContent = c })


trunk_node_of_page :: Page idts -> Node idts
trunk_node_of_page = pageContent >>> Tree.rootLabel

both_id_of_page :: Page idts -> (Text, Maybe idts)
both_id_of_page = trunk_node_of_page >>> UI.both_id_of_node

id_of_page :: Page id_u -> Text
id_of_page = trunk_node_of_page >>> UI.nodeIdAuto


-- compilation

-- gather addresses by user

gather_addresses_by_user_in_Node :: Node id_u -> Maybe (id_u, Node id_u)
gather_addresses_by_user_in_Node n = map (HePair.before n) (UI.uid_of_node n)

gather_addresses_by_user_in_Structure :: Structure id_u -> [(id_u, Node id_u)]
gather_addresses_by_user_in_Structure = 
	Fold.foldMap (gather_addresses_by_user_in_Node >>> Fold.toList)

gather_InternalLinkTargets_in_Page ::
	forall id_u .
	(PageKey, Page id_u) -> Either (Pos.PositionedMb (Accu.Accumulated Text)) [(id_u, CrossLinkTarget)]
gather_InternalLinkTargets_in_Page (page_key, page) =
	let
		structure = pageContent page
		make_one :: Node id_u -> Either (Pos.PositionedMb (Accu.Accumulated Text)) CrossLinkTarget
		make_one node =
			if UI.nodeIdAuto node == UI.nodeIdAuto (Tree.rootLabel structure)
				then Right (CrossLinkTarget page_key)
				else
					Left
						(
						Pos.maybefy_positioned
							(
							Pos.position_error
								(UI.nodeWitSource node)
								(Accu.single "non-page link target")
							)
						)
		in (traverse >>> traverse) make_one (gather_addresses_by_user_in_Structure structure)

gather_InternalLinkTargets_in_Pages ::
	Base.Ord id_u =>
	Array (Page id_u) ->
	Either (Pos.PositionedMb (Accu.Accumulated Text)) (Map.Map id_u (CrossLinkTarget))
gather_InternalLinkTargets_in_Pages pages = 
	 (map (Fold.fold >>> Map.fromList) (traverse gather_InternalLinkTargets_in_Page (Array.assocs pages)))

-- divide to pages

-- | creates an output clone of the node 
-- which will be just a link to the page that the input node is a trunk of.
page_node_as_link :: forall id_u . Page id_u -> Node id_u -> Node id_u
page_node_as_link page trunk_node =
	let
		changer :: Node id_u -> Node id_u
		changer =
			id
			>>> Optic.fill UI.inNode_idu_source_mb Nothing
			>>> Optic.fill UI.links_in_Node 
				(Just (UI.LIn (Left (SubPageTarget page (UI.nodeIdAuto trunk_node)))))
		in changer trunk_node


divide_to_pages :: forall i . [Node i] -> Bool -> Structure i -> State PageKey (Structure i, [Tree (KeyedPage i)])
divide_to_pages path_to_trunk may_treat_as_page_trunk whole_structure =
	let
		trunk_node :: Node i
		trunk_node = Tree.rootLabel whole_structure
		in
			if may_treat_as_page_trunk && UI.nodeIsSeparatePage trunk_node
				then 
					let
						make_the_tree :: Tree (KeyedPage i) -> (Structure i, [Tree (KeyedPage i)])
						make_the_tree page_tree@(Tree.Node (trunk_page_id, trunk_page :: Page i) _) = 
							(Tree.Node (page_node_as_link trunk_page trunk_node) [], [page_tree])
						in map make_the_tree (divide_to_pages_from_page path_to_trunk whole_structure)
				else
					let
						children = Tree.subForest whole_structure
						recursive_call = divide_to_pages (trunk_node : path_to_trunk) True
						sub_results :: State PageKey [(Structure i, [Tree (KeyedPage i)])]
						sub_results = traverse recursive_call children
						merge_the_subresult_pairs :: 
							([Structure i], [[Tree (KeyedPage i)]]) -> (Structure i, [Tree (KeyedPage i)])
						merge_the_subresult_pairs (page_contents, sub_pages) =
							(Tree.Node trunk_node page_contents, Fold.concat sub_pages)
						merge_the_subresults :: [(Structure i, [Tree (KeyedPage i)])] -> (Structure i, [Tree (KeyedPage i)])
						merge_the_subresults = List.unzip >>> merge_the_subresult_pairs
						in map merge_the_subresults sub_results

divide_to_pages_from_page :: [Node i] -> Structure i -> State PageKey (Tree (KeyedPage i))
divide_to_pages_from_page path_to_trunk whole_structure = 
	do
		(new_structure, subtrees) <- divide_to_pages path_to_trunk False whole_structure
		current_key <- State.get
		State.modify (+ 1)
		let new_page = Page path_to_trunk new_structure (List.null path_to_trunk)
		pure (Tree.Node (current_key, new_page) subtrees)

compile_site ::
	forall i .
	Base.Ord i =>
	Structure i -> Either (Pos.PositionedMb (Accu.Accumulated Text)) (Site i)
compile_site input_structure = 
	let
		pages_tree :: Tree (KeyedPage i)
		pages_tree = evalState (divide_to_pages_from_page [] input_structure) page_key_start
		Tree.Node (_, main_page) _  = pages_tree
		all_pages :: Array (Page i)
		all_pages = 
			let 
				pairs = Fold.toList pages_tree
				keys = map fst pairs
				max_key = Fold.foldl' max page_key_start keys
				in array (page_key_start, max_key) pairs
		user_address_map :: Either (Pos.PositionedMb (Accu.Accumulated Text)) (UserAddressMap i)
		user_address_map = gather_InternalLinkTargets_in_Pages all_pages
		in map (make_Site main_page all_pages) user_address_map

melt_pages_to_single :: forall id_u . Structure id_u -> UI.StructureAsTree id_u id_u
melt_pages_to_single (Tree.Node trunk children) =
	let
		melted_children = map melt_pages_to_single children
		trunk_node_inline = UI.nodeContent trunk
		make_result_with_link :: Maybe (UI.Link id_u) -> UI.StructureAsTree id_u id_u
		make_result_with_link link =
			let
				new_trunk_node_inline :: UI.Inline id_u
				new_trunk_node_inline = UI.Inline (UI.ilVisual trunk_node_inline) link
				new_trunk_node :: UI.Node id_u id_u
				new_trunk_node = Optic.fill UI.inNode_content new_trunk_node_inline trunk
				in Tree.Node new_trunk_node melted_children			
		in
			case UI.ilLink trunk_node_inline of
				Nothing -> make_result_with_link Nothing
				Just link ->
					case link of
						UI.LEx addr -> make_result_with_link (Just (UI.LEx addr))
						UI.LIn addr ->
							case addr of
								Left (SubPageTarget sub_page _) ->
									melt_pages_to_single (pageContent sub_page)
								Right a -> make_result_with_link (Just (UI.LIn a))

render :: Site UI.NodeIdU -> UI.StructureAsTree UI.NodeIdU UI.NodeIdU
render = siteMainPage >>> pageContent >>> melt_pages_to_single

parse ::
	UI.StructureAsTree UI.NodeIdU UI.NodeIdU ->
	Either (Pos.PositionedMb (Accu.Accumulated Text)) (Site UI.NodeIdU)
parse =
	let
		rightify :: UI.StructureAsTree UI.NodeIdU UI.NodeIdU -> Structure UI.NodeIdU
		rightify = Optic.fn_up UI.internal_address_in_tree Right
		in rightify >>> compile_site

layer ::
	Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text))
		(UI.StructureAsTree UI.NodeIdU UI.NodeIdU) (Site UI.NodeIdU)
layer = Optic.PartialIso render parse
