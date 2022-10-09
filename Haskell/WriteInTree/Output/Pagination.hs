-- | write-in-tree output data format.
module WriteInTree.Output.Pagination
(
	SubPageTarget (..),
	CrossLinkTarget (..),
	LinkInternalTarget (..),
	Inline,
	Link,
	Paragraph,
	Node,
	Structure,
	UserAddressMap,
	Page (..),
	Site (..),

	title_of_section, title_of_page,
	id_of_page,
	is_inline_a_page_break,
	
	compile_site, compile_document,
)
where

import Control.Arrow ((&&&))
import Data.Foldable
import Fana.Prelude
import Prelude (String)

import qualified Data.Bifunctor as BiFr
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Tree as Tree
import qualified Fana.Data.HeteroPair as HePair
import qualified Fana.Data.Tree.Leaf as Lt
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as UI


type Text = Base.String

type PagePath = [String]

data SubPageTarget id_u = SubPageTarget { sptPage :: Page id_u, sptId :: Text }

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

{-| Link address to page but not to sub-page. -}
data CrossLinkTarget idts = CrossLinkTarget
	{ 
	cltPage :: Page idts, 
	cltInPage :: Maybe (Node idts)
	}
type UserAddressMap id_u = Map.Map id_u (CrossLinkTarget id_u)

data Site (id_u :: Type) = Site
	{
	sitePages :: Tree.Tree (Page id_u),
	siteUserAddressMap :: UserAddressMap id_u,
	sitePageMap :: Map.Map Text (Page id_u)
	}

make_Site :: forall id_u . Tree.Tree (Page id_u) -> UserAddressMap id_u -> Site id_u
make_Site pages ua_map = 
	let 
		page_map = 
			let
				make_key_value_pair :: Page id_u -> (Text, Page id_u)
				make_key_value_pair = id_of_page &&& id
				in Map.fromList (map make_key_value_pair (Fold.toList pages))
	in Site pages ua_map page_map

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

gather_InternalLinkTargets_in_Page :: forall id_u . Page id_u -> [(id_u, CrossLinkTarget id_u)]
gather_InternalLinkTargets_in_Page page = 
	let
		structure = pageContent page
		make_one :: Node id_u -> CrossLinkTarget id_u
		make_one node = 
			CrossLinkTarget page 
				(
					if UI.nodeIdAuto node == UI.nodeIdAuto (Tree.rootLabel structure) 
						then Nothing else Just node
				)
		in (map >>> map) make_one (gather_addresses_by_user_in_Structure structure)

gather_InternalLinkTargets_in_Pages :: 
	Foldable pc => Base.Ord id_u => pc (Page id_u) -> Map.Map id_u (CrossLinkTarget id_u)
gather_InternalLinkTargets_in_Pages pages = 
	Map.fromList (Fold.foldMap gather_InternalLinkTargets_in_Page pages)

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
				(
					Just (UI.LIn (Left (SubPageTarget page (UI.nodeIdAuto trunk_node))))
					-- this feels an ugly solution, but i hope will do it for now
				)
		in changer trunk_node

divide_to_pages :: 
	forall id_u . [Node id_u] -> Bool -> Structure id_u -> 
	(Structure id_u, Lt.Tree [] (Tree.Tree (Page id_u)))
divide_to_pages path_to_trunk may_treat_as_page_trunk whole_structure =
	let
		trunk_node :: Node id_u
		trunk_node = Tree.rootLabel whole_structure
		in
			if may_treat_as_page_trunk && UI.nodeIsSeparatePage trunk_node
				then 
					let
						(trunk_page, pages) = divide_to_pages_from_page path_to_trunk whole_structure
						in (Tree.Node (page_node_as_link trunk_page trunk_node) [], Lt.leaf pages)
				else
					let
						children = Tree.subForest whole_structure
						sub_results = 
							map 
								(divide_to_pages (trunk_node : path_to_trunk) True) 
								children
						merge_sub_results :: 
							[(Structure id_u, Lt.Tree [] (Tree.Tree (Page id_u)))] -> 
							(Structure id_u, Lt.Tree [] (Tree.Tree (Page id_u)))
						merge_sub_results srs = 
							let (sub_structures, sub_page_trees) = List.unzip srs
								in (Tree.Node trunk_node sub_structures, Lt.joint sub_page_trees)
						in merge_sub_results sub_results

divide_to_pages_from_page :: [Node id_u] -> Structure id_u -> (Page id_u, Tree.Tree (Page id_u))
divide_to_pages_from_page path_to_trunk whole_structure = 
	let
		raw_result = BiFr.second Fold.toList (divide_to_pages path_to_trunk False whole_structure)
		new_structure = Base.fst raw_result
		page = Page path_to_trunk new_structure (List.null path_to_trunk)
		in (page, Tree.Node page (Base.snd raw_result))


compile_site :: forall id_u . Base.Ord id_u => Structure id_u -> Site id_u
compile_site input_structure = 
	let
		pages :: Tree.Tree (Page id_u)
		pages = snd (divide_to_pages_from_page [] input_structure)
		user_address_map :: UserAddressMap id_u
		user_address_map = gather_InternalLinkTargets_in_Pages pages
		in make_Site pages user_address_map

compile_document :: UI.Document UI.NodeIdU UI.NodeIdU -> Site UI.NodeIdU
compile_document =
	let
		rightify :: UI.StructureAsTree UI.NodeIdU UI.NodeIdU -> Structure UI.NodeIdU
		rightify = Optic.fn_up UI.internal_address_in_tree Right
		in UI.docTree >>> rightify >>> compile_site
