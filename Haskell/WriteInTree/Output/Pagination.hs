-- | write-in-tree output data format.
module WriteInTree.Output.Pagination
(
	InternalLinkTarget (..),
	Inline,
	Link,
	Paragraph,
	Node,
	Structure,
	AI, AO,
	UserAddressMap,
	Page (..),
	Site (..),

	title_of_section, title_of_page,
	id_of_page,
	is_inline_a_page_break,
	
	compile_site,
)
where

import Control.Arrow ((&&&))
import Data.Default.Class (Default (..))
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

type LinkInternalTarget (id_u :: Type) = Either Text id_u

type AI = ()
data AO = AO {}

instance Default AO where def = AO

type Link (id_u :: Type) = UI.Link (LinkInternalTarget id_u)
type Inline (id_u :: Type) = UI.Inline (LinkInternalTarget id_u)
type Paragraph (id_u :: Type) = UI.Paragraph (LinkInternalTarget id_u)
type Node a (id_u :: Type) = UI.Node a id_u (LinkInternalTarget id_u)

type Structure a (id_u :: Type) = Tree.Tree (Node a id_u)

data Page (id_u :: Type) = Page 
	{
	pagePathToTrunk :: [Node AI id_u],
	pageContent :: Structure AO id_u,
	pageIsTrunk :: Bool
	}

data InternalAddress = InternalAddress { iaPage :: Text, iaInPage :: Maybe Text }
data InternalLinkTarget idts = InternalLinkTarget 
	{ 
	iltPage :: Page idts, 
	iltInPage :: Maybe (Node AO idts)
	}
type UserAddressMap id_u = Map.Map id_u (InternalLinkTarget id_u)

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

title_of_section :: Node a idts -> String
title_of_section = Optic.to_list UI.texts_in_Node >>> Fold.concat

title_of_page :: Page idts -> String
title_of_page = pageContent >>> Tree.rootLabel >>> title_of_section


-- optics :

content_in_Page :: Optic.Lens' (Structure AO idts) (Page idts)
content_in_Page = Optic.lens_from_get_set pageContent (\ c p -> p { pageContent = c })


trunk_node_of_page :: Page idts -> Node AO idts
trunk_node_of_page = pageContent >>> Tree.rootLabel

both_id_of_page :: Page idts -> (Text, Maybe idts)
both_id_of_page = trunk_node_of_page >>> UI.both_id_of_node

id_of_page :: Page id_u -> Text
id_of_page = trunk_node_of_page >>> UI.nodeIdAuto


-- compilation

-- gather addresses by user

gather_addresses_by_user_in_Node :: Node a id_u -> Maybe (id_u, Node a id_u)
gather_addresses_by_user_in_Node n = map (HePair.before n) (UI.uid_of_node n)

gather_addresses_by_user_in_Structure :: Structure a id_u -> [(id_u, Node a id_u)]
gather_addresses_by_user_in_Structure = 
	Fold.foldMap (gather_addresses_by_user_in_Node >>> Fold.toList)

gather_InternalLinkTargets_in_Page :: forall id_u . Page id_u -> [(id_u, InternalLinkTarget id_u)]
gather_InternalLinkTargets_in_Page page = 
	let
		structure = pageContent page
		make_one :: Node AO id_u -> InternalLinkTarget id_u
		make_one node = 
			InternalLinkTarget page 
				(
					if UI.nodeIdAuto node == UI.nodeIdAuto (Tree.rootLabel structure) 
						then Nothing else Just node
				)
		in (map >>> map) make_one (gather_addresses_by_user_in_Structure structure)

gather_InternalLinkTargets_in_Pages :: 
	Foldable pc => Base.Ord id_u => pc (Page id_u) -> Map.Map id_u (InternalLinkTarget id_u)
gather_InternalLinkTargets_in_Pages pages = 
	Map.fromList (Fold.foldMap gather_InternalLinkTargets_in_Page pages)

-- divide to pages

-- | creates an output clone of the node 
-- which will be just a link to the page that the input node is a trunk of.
page_node_as_link :: Node AI id_u -> Node AO id_u
page_node_as_link trunk_node = 
	let 
		changer :: Node AI id_u -> Node AO id_u
		changer = 
			id
			>>> Optic.fill UI.inNode_idu_source_mb Nothing
			>>> Optic.fill UI.links_in_Node 
				(
					Just (UI.LIn (Left (UI.nodeIdAuto trunk_node)))
					-- this feels an ugly solution, but i hope will do it for now
				)
			>>> Optic.fill UI.ofNode_additional def
		in changer trunk_node

divide_to_pages :: 
	forall id_u . [Node AI id_u] -> Bool -> Structure AI id_u -> 
	(Structure AO id_u, Lt.Tree [] (Tree.Tree (Page id_u)))
divide_to_pages path_to_trunk may_treat_as_page_trunk whole_structure =
	let
		trunk_node :: Node AI id_u
		trunk_node = Tree.rootLabel whole_structure
		trunk_node_new :: Node AO id_u
		trunk_node_new = Optic.fill UI.ofNode_additional def trunk_node
		in
			if may_treat_as_page_trunk && UI.nodeIsSeparatePage trunk_node
				then 
					let
						pages = divide_to_pages_from_page path_to_trunk whole_structure
						in (Tree.Node (page_node_as_link trunk_node) [], Lt.leaf pages)
				else
					let
						children = Tree.subForest whole_structure
						sub_results = 
							map 
								(divide_to_pages (trunk_node : path_to_trunk) True) 
								children
						merge_sub_results :: 
							[(Structure AO id_u, Lt.Tree [] (Tree.Tree (Page id_u)))] -> 
							(Structure AO id_u, Lt.Tree [] (Tree.Tree (Page id_u)))
						merge_sub_results srs = 
							let (sub_structures, sub_page_trees) = List.unzip srs
								in (Tree.Node trunk_node_new sub_structures, Lt.joint sub_page_trees)
						in merge_sub_results sub_results

divide_to_pages_from_page :: [Node AI id_u] -> Structure AI id_u -> Tree.Tree (Page id_u)
divide_to_pages_from_page path_to_trunk whole_structure = 
	let
		raw_result = BiFr.second Fold.toList (divide_to_pages path_to_trunk False whole_structure)
		new_structure = Base.fst raw_result
		page = Page path_to_trunk new_structure (List.null path_to_trunk)
		in Tree.Node page (Base.snd raw_result)


compile_site :: forall id_u . Base.Ord id_u => Structure AI id_u -> Site id_u
compile_site input_structure = 
	let
		pages :: Tree.Tree (Page id_u)
		pages = divide_to_pages_from_page [] input_structure
		user_address_map :: UserAddressMap id_u
		user_address_map = gather_InternalLinkTargets_in_Pages pages
		in make_Site pages user_address_map
