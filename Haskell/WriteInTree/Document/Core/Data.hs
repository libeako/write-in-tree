-- | The file containing the design intentions of the user, that is the document.
module WriteInTree.Document.Core.Data where


import Data.Maybe (catMaybes)
import Fana.Data.Identified (Identified)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import Prelude (String, Int)

import qualified Data.Tree as Tree
import qualified Data.Foldable as Fold
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Data.HeteroPair as Pair
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label


type Text = Base.String

-- | we are going to use "a" as name for of type parameter of addition information


data NodeIdUCore = NodeIdUCore
	{ nidun_a :: Text, nidun_u :: Text, nidun_path_to_trunk :: [String] }
-- | at this stage of the application this type is used instead of the user-given string alone.
type NodeIdU = Identified Int NodeIdUCore

-- | Can be internal or external.
data Link ia =
	  LIn ia -- ^ | Internal.
	| LEx String -- ^ | External.
	deriving (Eq)

data Inline ia e =
	Inline
	{ ilVisual :: e
	, ilLink :: Maybe (Link ia)
	}
	deriving (Eq, Functor, Foldable, Traversable)

type Paragraph ia = Inline ia Text

data Node a (id_u :: Type) ia =
	Node
	{
		nodeIdAuto :: Text,
		nodeWitSource :: Label.Elem id_u (),
		nodeContent :: (a, Paragraph ia),
		nodeIsSeparatePage :: Bool
	}
	deriving (Eq)

inNode_source :: 
	Optic.Lens 
		(Label.Elem id_u_1 ()) (Label.Elem id_u_2 ()) 
		(Node a id_u_1 ia) (Node a id_u_2 ia)
inNode_source = Optic.lens_from_get_set nodeWitSource (\ p w -> w { nodeWitSource = p })

inNode_idu :: Optic.Traversal e1 e2 (Node a e1 ia) (Node a e2 ia)
inNode_idu = Label.inElem_idu >**>^ inNode_source

inNode_idu_source_mb :: 
	Optic.Lens
		(Maybe id_u_1) (Maybe id_u_2)
		(Node a id_u_1 ia) (Node a id_u_2 ia)
inNode_idu_source_mb = Label.inLabel_id_source_mb >**>^ Label.inElem_labels >**>^ inNode_source 


uid_of_node :: Node a id_u li -> Maybe id_u
uid_of_node = nodeWitSource >>> Label.ofElem_labels >>> Label.id_of_Labels

both_id_of_node :: Node a id_u ia -> (Text, Maybe id_u)
both_id_of_node node = (nodeIdAuto node, uid_of_node node)

attach_its_uid_to_node :: Node a id_u ia -> Maybe (id_u, Node a id_u ia)
attach_its_uid_to_node n = map (Pair.before n) (uid_of_node n)


type StructureAsTree a (id_u :: Type) ia = Tree.Tree (Node a id_u ia)

data Document a (id_u :: Type) ia = Document
	{
	docTree :: StructureAsTree a id_u ia
	}
	deriving (Eq)


-- | collects all the user identifiers in the document with their nodes.
uids_int_doc_with_nodes :: Document a id_u ia -> [(id_u, Node a id_u ia)]
uids_int_doc_with_nodes = docTree >>> Fold.toList >>> map attach_its_uid_to_node >>> catMaybes


-- optics :

ofLink_internals :: 
	Optic.Iso 
		(Either ia1 String) (Either ia2 String)
		(Link ia1) (Link ia2)
ofLink_internals = let
	down = \case { LIn x -> Left x; LEx x -> Right x }
	up = Base.either LIn LEx
	in Optic.Iso down up

ofInline_internals ::
	Optic.Iso
		((e, Maybe (Link ia))) ((e, Maybe (Link ia)))
		(Inline ia e) (Inline ia e)
ofInline_internals = Optic.Iso (\ (Inline v l) -> (v, l)) (uncurry Inline)

visual_in_Inline :: Optic.Lens' e (Inline ia e)
visual_in_Inline = Optic.lens_from_get_set ilVisual (\ e c -> c { ilVisual = e })

link_in_Inline ::
	forall ia1 ia2 e .
	Optic.Lens
		(Maybe (Link ia1)) (Maybe (Link ia2)) 
		(Inline ia1 e) (Inline ia2 e)
link_in_Inline = Optic.lens_from_get_set ilLink (\ e c -> c { ilLink = e })

ofLink_additional :: forall a1 a2 ia . Optic.Traversal a1 a2 (Link ia) (Link ia)
ofLink_additional =
	let
		trav :: forall a . (a1 -> a a2) -> (Link ia -> Link ia)
		trav f =
			\case
				LIn x -> LIn x
				LEx x -> LEx x
		in Optic.Traversal (trav >>> map pure)

ofInline_additional :: forall ia e a1 a2 . Optic.Traversal a1 a2 (Inline ia e) (Inline ia e)
ofInline_additional = let
	from_link :: Optic.Traversal a1 a2 (Maybe (Link ia)) (Maybe (Link ia))
	from_link = Category2.empty >**>^ ofLink_additional >**>^ Optic.prism_Maybe
	from_internals :: Optic.Traversal a1 a2 (e, Maybe (Link ia)) (e, Maybe (Link ia))
	from_internals = from_link >**>^ Optic.lens_2
	in Category2.empty >**>^ from_internals >**>^ ofInline_internals

internal_address_in_Link :: Optic.Prism ia1 ia2 (Link ia1) (Link ia2)
internal_address_in_Link = 
	Optic.from_up_and_match LIn (\case { LIn ia -> Right ia; LEx t -> Left (LEx t) })

internal_address_in_Link' :: forall ia1 ia2 . Optic.AffineTraversal ia1 ia2 (Link ia1) (Link ia2)
internal_address_in_Link' = Category2.empty >**>^ internal_address_in_Link

internal_address_in_Inline :: 
	forall ia1 ia2 e . Optic.AffineTraversal ia1 ia2 (Inline ia1 e) (Inline ia2 e)
internal_address_in_Inline = 
	let 
		empty :: Optic.Empty Optic.AffineTraversal ia1 ia2
		empty = Category2.empty
	in 
		Category2.empty
		>**>^ internal_address_in_Link'
		>**>^ Optic.prism_Maybe 
		>**>^ link_in_Inline

ofParagraph_additional :: Optic.Traversal a1 a2 (Paragraph ia) (Paragraph ia)
ofParagraph_additional = ofInline_additional

inlines_in_Node :: 
	forall a id_u ia1 ia2 .
	Optic.Traversal (Inline ia1 Text) (Inline ia2 Text) (Node a id_u ia1) (Node a id_u ia2)
inlines_in_Node = Category2.empty >**>^ inNode_content_elem

links_in_Node :: 
	forall a id_u ia1 ia2 .
	Optic.Traversal
		(Maybe (Link ia1)) (Maybe (Link ia2))
		(Node a id_u ia1) (Node a id_u ia2)
links_in_Node = Category2.empty >**>^ link_in_Inline @ia1 @ia2 >**>^ inlines_in_Node

texts_in_Node :: forall u id_u ia . Optic.Traversal' Text (Node u id_u ia)
texts_in_Node = Category2.empty
	>**>^ visual_in_Inline
	>**>^ inNode_content_elem

wit_source_in_Node ::
	Optic.Lens
		(Label.Elem idts1 ()) (Label.Elem idts2 ())
		(Node a idts1 ia) (Node a idts2 ia)
wit_source_in_Node = Optic.lens_from_get_set nodeWitSource (\ e c -> c { nodeWitSource = e })

source_in_Node :: 
	Optic.Lens 
		(Label.Elem idts_1 ()) (Label.Elem idts_2 ()) 
		(Node a idts_1 li) (Node a idts_2 li)
source_in_Node = Optic.lens_from_get_set nodeWitSource (\ p w -> w { nodeWitSource = p })

inNode_content ::
	forall a1 a2 ia1 ia2 id_u .
	Optic.Lens
		(a1, (Paragraph ia1)) (a2, (Paragraph ia2))
		(Node a1 id_u ia1) (Node a2 id_u ia2)
inNode_content = Optic.lens_from_get_set nodeContent (\ p w -> w { nodeContent = p })

inNode_content_elem :: 
	forall u ia1 ia2 idts .
	Optic.Lens (Paragraph ia1) (Paragraph ia2) (Node u idts ia1) (Node u idts ia2)
inNode_content_elem = Category2.empty >**>^ Optic.lens_2 >**>^ inNode_content @u @u

separate_page_in_Node :: Optic.Lens' Bool (Node u idts li)
separate_page_in_Node = Optic.lens_from_get_set nodeIsSeparatePage (\ p w -> w { nodeIsSeparatePage = p })

idu_in_Node :: Optic.Traversal (id_u_1) (id_u_2) (Node u id_u_1 ia) (Node u id_u_2 ia)
idu_in_Node = Category2.empty >**>^ Label.inElem_idu >**>^ inNode_source

ofNode_additional :: Optic.Traversal a1 a2 (Node a1 id_u ia) (Node a2 id_u ia)
ofNode_additional = 
	Category2.empty 
	>**>^ Optic.product (Category2.empty, ofParagraph_additional) 
	>**>^ inNode_content

internal_address_in_node :: 
	forall u ia1 ia2 id_u .
	Optic.Traversal ia1 ia2 (Node u id_u ia1) (Node u id_u ia2)
internal_address_in_node = 
	Category2.empty
	>**>^ internal_address_in_Inline
	>**>^ inNode_content_elem

node_in_tree ::
	Optic.Traversal
		(Node u id_u_1 ia1) (Node u id_u_2 ia2)
		(StructureAsTree u id_u_1 ia1) (StructureAsTree u id_u_2 ia2)
node_in_tree = Optic.from_Traversable

inlines_in_Structure :: Optic.Traversal' (Inline ia Text) (StructureAsTree u id_u ia)
inlines_in_Structure = inlines_in_Node >**>^ node_in_tree

idu_in_tree ::
	Optic.Traversal
		(id_u_1) (id_u_2)
		(StructureAsTree u id_u_1 ia) (StructureAsTree u id_u_2 ia)
idu_in_tree = idu_in_Node >**>^ node_in_tree

internal_address_in_tree :: 
	forall u ia1 ia2 id_u .
	Optic.Traversal ia1 ia2 (StructureAsTree u id_u ia1) (StructureAsTree u id_u ia2)
internal_address_in_tree = internal_address_in_node >**>^ node_in_tree

tree_in_Document :: 
	Optic.Lens 
		(StructureAsTree u1 id_u1 ia1) (StructureAsTree u2 id_u2 ia2)
		(Document u1 id_u1 ia1) (Document u2 id_u2 ia2)
tree_in_Document = Optic.lens_from_get_set docTree (\ p w -> w { docTree = p })

