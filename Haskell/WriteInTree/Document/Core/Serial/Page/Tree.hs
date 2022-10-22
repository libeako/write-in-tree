-- | write-in-tree output data format.
module WriteInTree.Document.Core.Serial.Page.Tree
(
	SubPageTarget (..),
	CrossLinkTarget (..),
	LinkInternalTarget (..),
	Link, Inline, Paragraph, Node, Structure,
	Page (..),
	PageKey, Site (..),
	get_page_of_Site_at, get_CrossLinkTarget_page,

	title_of_section, title_of_page, is_inline_a_page_break, page_addresses_in_site, text_content_in_site,
	node_in_site,
	
	melt_pages_to_single, compile_site, layer
)
where

import Prelude (String, Int, (+))

import Control.Monad.State.Lazy (State, evalState)
import Data.Ord (max)
import Data.Tree (Tree)
import Data.Array (array, (!))
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))

import qualified Control.Monad.State.Lazy as State
import qualified Data.Array as Array
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Tree as Tree
import qualified Fana.Data.Tree.OfBase as Tree
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as UI
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Elem as LabelElem
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String
type PageKey = Int
page_key_start :: PageKey
page_key_start = 1
type Array = Array.Array PageKey
type AllPages i = Array (Page i)

type PagePath = [String]

data SubPageTarget =
	SubPageTarget { sptPageKey :: PageKey }
	deriving (Eq)

type LinkInternalTarget (i :: Type) = Either SubPageTarget i

type Link (i :: Type) = UI.Link (LinkInternalTarget i)
type Inline (i :: Type) = UI.Inline (LinkInternalTarget i)
type Paragraph (i :: Type) = UI.Paragraph (LinkInternalTarget i)
type Node (i :: Type) = UI.Node i (LinkInternalTarget i)

type Structure (i :: Type) = Tree.Tree (Node i)

type PageContent i = Structure i
type KeyedPage i = (PageKey, Page i)
type KeyedPageContent i = (PageKey, PageContent i)

data Page (i :: Type) = Page
	{ pageAddress :: PageAddress
	, pageContent :: Structure i
	}
	deriving (Eq)

{-| Link address to page but not to sub-page. -}
data CrossLinkTarget = CrossLinkTarget { cltPage :: PageKey } deriving (Eq)
	
data Site (i :: Type) = Site
	{
	sitePageRelations :: Tree PageKey,
	siteAllPages :: AllPages i
	}
	deriving (Eq)

get_page_of_Site_at :: Site i -> PageKey -> Page i
get_page_of_Site_at site key = siteAllPages site ! key

get_main_page_of_Site :: Site i -> Page i
get_main_page_of_Site site =
	get_page_of_Site_at site (Tree.rootLabel (sitePageRelations site))

get_CrossLinkTarget_page :: Site i -> CrossLinkTarget -> Page i
get_CrossLinkTarget_page site = cltPage >>> get_page_of_Site_at site

make_Site :: forall i . Tree PageKey -> AllPages i -> Site i
make_Site page_relations all_pages = Site page_relations all_pages

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
	Optic.Traversal SubPageTarget SubPageTarget (Structure i) (Structure i)
of_Structure_SubPageTarget =
	Category2.identity >**>^ Optic.prism_Left >**>^ UI.internal_address_in_link_in_tree

content_in_Page :: Optic.Lens' (Structure i) (Page i)
content_in_Page = Optic.lens_from_get_set pageContent (\ c p -> p { pageContent = c })

trunk_node_in_Page :: Optic.Lens' (Node i) (Page i)
trunk_node_in_Page = Tree.trunk_in_tree >**>^ content_in_Page

trunk_node_of_page :: Page i -> Node i
trunk_node_of_page = pageContent >>> Tree.rootLabel

pages_in_site :: Optic.Lens' (AllPages i) (Site i)
pages_in_site =
	Optic.lens_from_get_set
		siteAllPages
		(\ aps (Site relations _) -> Site relations aps)

trunk_node_in_site :: Optic.Traversal' (Node i) (Site i)
trunk_node_in_site =
	Category2.identity >**>^
	trunk_node_in_Page >**>^ Optic.from_Traversable >**>^ pages_in_site

page_addresses_in_site :: Optic.Traversal' (Maybe PageAddress) (Site i)
page_addresses_in_site = UI.page_addresses_in_Node >**>^ trunk_node_in_site

text_content_in_site :: Optic.Traversal' Text (Site i)
text_content_in_site =
	Category2.identity >**>^
	UI.texts_in_Tree  >**>^ content_in_Page >**>^ Optic.from_Traversable >**>^ pages_in_site

target_in_not_subpage_link_in_node :: Optic.Traversal' i (Node i)
target_in_not_subpage_link_in_node =
	Category2.identity >**>^
	Optic.prism_Right >**>^ UI.internal_address_in_link_in_node

node_in_site :: Optic.Traversal' (Node i) (Site i)
node_in_site =
	Category2.identity
	 >**>^ UI.node_in_tree
	 >**>^ content_in_Page
	 >**>^ Optic.from_Traversable
	 >**>^ pages_in_site

-- compilation

-- divide to pages

-- | creates an output clone of the node 
-- which will be just a link to the page that the input node is a trunk of.
page_node_as_link :: forall i . PageKey -> Node i -> Node i
page_node_as_link page_key trunk_node =
	let
		changer :: Node i -> Node i
		changer =
			Optic.fill UI.links_in_Node 
				(Just (UI.LIn (Left (SubPageTarget page_key))))
		in changer trunk_node


divide_to_pages :: forall i . Bool -> Structure i -> State PageKey (PageContent i, [Tree (KeyedPageContent i)])
divide_to_pages may_treat_as_page_trunk whole_structure =
	let
		trunk_node :: Node i
		trunk_node = Tree.rootLabel whole_structure
		in
			if may_treat_as_page_trunk && UI.nodeIsSeparatePage trunk_node
				then 
					let
						make_the_tree :: Tree (KeyedPageContent i) -> (PageContent i, [Tree (KeyedPageContent i)])
						make_the_tree page_tree@(Tree.Node (trunk_page_id, trunk_page :: PageContent i) _) = 
							(Tree.Node (page_node_as_link trunk_page_id trunk_node) [], [page_tree])
						in map make_the_tree (divide_to_pages_from_page whole_structure)
				else
					let
						children = Tree.subForest whole_structure
						recursive_call = divide_to_pages True
						sub_results :: State PageKey [(PageContent i, [Tree (KeyedPageContent i)])]
						sub_results = traverse recursive_call children
						merge_the_subresult_pairs :: 
							([PageContent i], [[Tree (KeyedPageContent i)]]) ->
							(PageContent i, [Tree (KeyedPageContent i)])
						merge_the_subresult_pairs (page_contents, sub_pages) =
							(Tree.Node trunk_node page_contents, Fold.concat sub_pages)
						merge_the_subresults ::
							[(PageContent i, [Tree (KeyedPageContent i)])] ->
							(PageContent i, [Tree (KeyedPageContent i)])
						merge_the_subresults = List.unzip >>> merge_the_subresult_pairs
						in map merge_the_subresults sub_results

divide_to_pages_from_page :: Structure i -> State PageKey (Tree (KeyedPageContent i))
divide_to_pages_from_page whole_structure = 
	do
		(new_structure, subtrees) <- divide_to_pages False whole_structure
		current_key <- State.get
		State.modify (+ 1)
		let new_page = new_structure
		pure (Tree.Node (current_key, new_page) subtrees)

compile_site ::
	forall i .
	PageContent i -> Either (Pos.PositionedMb (Accu.Accumulated Text)) (Site i)
compile_site input_structure = 
	let
		pages_tree :: Tree (KeyedPageContent i)
		pages_tree = evalState (divide_to_pages_from_page input_structure) page_key_start
		all_pages_content :: Array (PageContent i)
		all_pages_content = 
			let 
				pairs = Fold.toList pages_tree
				keys = map fst pairs
				max_key = Fold.foldl' max page_key_start keys
				in array (page_key_start, max_key) pairs
		make_page :: PageContent i -> Either (Pos.PositionedMb (Accu.Accumulated Text)) (Page i)
		make_page page_content =
			let
				page_trunk_label_elem = UI.nodeWitSource (Tree.rootLabel page_content)
				in
					case (LabelElem.ofElem_labels >>> Label.address_of_Labels) page_trunk_label_elem of
						Nothing -> Left (Pos.position_error_mb page_trunk_label_elem (Accu.single "page has no address"))
						Just address -> Right (Page address page_content)
		all_pages :: Either (Pos.PositionedMb (Accu.Accumulated Text)) (AllPages i)
		all_pages = traverse make_page all_pages_content
		relations = map fst pages_tree
		in map (make_Site relations) all_pages

melt_pages_to_single :: forall i . Site i -> Structure i -> UI.StructureAsTree i i
melt_pages_to_single site (Tree.Node trunk children) =
	let
		melted_children = map (melt_pages_to_single site) children
		trunk_node_inline = UI.nodeContent trunk
		make_result_with_link :: Maybe (UI.Link i) -> UI.StructureAsTree i i
		make_result_with_link link =
			let
				new_trunk_node_inline :: UI.Inline i
				new_trunk_node_inline = UI.Inline (UI.ilVisual trunk_node_inline) link
				new_trunk_node :: UI.Node i i
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
								Left (SubPageTarget sub_page_key) ->
									melt_pages_to_single site (pageContent (get_page_of_Site_at site sub_page_key))
								Right a -> make_result_with_link (Just (UI.LIn a))

render :: Site Text -> UI.StructureAsTree Text Text
render site =
	melt_pages_to_single site
		(pageContent (get_main_page_of_Site site))

parse ::
	UI.StructureAsTree Text Text ->
	Either (Pos.PositionedMb (Accu.Accumulated Text)) (Site Text)
parse =
	let
		rightify :: UI.StructureAsTree Text Text -> Structure Text
		rightify = Optic.fn_up UI.internal_address_in_link_in_tree Right
		in rightify >>> compile_site

layer ::
	Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text))
		(UI.StructureAsTree Text Text) (Site Text)
layer = Optic.PartialIso render parse
