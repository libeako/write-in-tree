module WriteInTree.Document.Core.Serial.Page.Tree
(
	layer
)
where

import Prelude (String, (+))

import Control.Monad.State.Lazy (State, evalState)
import Data.Ord (max)
import Data.Tree (Tree)
import Data.Array (array)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.Page.Data

import qualified Control.Monad.State.Lazy as State
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Tree as Tree
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as UI
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Elem as LabelElem
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


page_key_start :: PageKey
page_key_start = 1

type PagePath = [String]


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
			if may_treat_as_page_trunk && (UI.node_is_page_trunk trunk_node)
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
		in map (SiteStructure relations) all_pages

melt_pages_to_single :: forall i . Site i -> Structure i -> UI.StructureAsTree i
melt_pages_to_single site (Tree.Node trunk children) =
	let
		melted_children = map (melt_pages_to_single site) children
		trunk_node_inline = UI.nodeContent trunk
		make_result_with_link :: Maybe (UI.Link i) -> UI.StructureAsTree i
		make_result_with_link link =
			let
				new_trunk_node_inline :: UI.Inline i
				new_trunk_node_inline = UI.Inline (UI.ilVisual trunk_node_inline) link
				new_trunk_node :: UI.Node i
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

render :: Site Text -> UI.StructureAsTree Text
render site =
	melt_pages_to_single site
		(pageContent (get_main_page_of_Site site))

parse ::
	UI.StructureAsTree Text ->
	Either (Pos.PositionedMb (Accu.Accumulated Text)) (Site Text)
parse =
	let
		rightify :: UI.StructureAsTree Text -> Structure Text
		rightify = Optic.fn_up UI.internal_address_in_link_in_tree Right
		in rightify >>> compile_site

layer ::
	Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text))
		(UI.StructureAsTree Text) (Site Text)
layer = Optic.PartialIso render parse
