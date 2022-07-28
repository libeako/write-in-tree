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
data Link a ia = 
	  LIn (a, ia) -- ^ | Internal.
	| LEx (a, String) -- ^ | External.
	deriving (Eq)

type Link' al a ia = (a, (al, Link a ia))

-- | .
-- type parameter 'al' hold additional info specifically of links;
data Inline (al :: Type) a ia e = 
	Inline
	{ ilVisual :: e
	, ilLink :: Maybe (Link' al a ia)
	}
	deriving (Eq, Functor, Foldable, Traversable)

type Paragraph al a ia e = (a, Inline al a ia e)

data Node al a (id_u :: Type) ia e =
	Node
	{
		nodeIdAuto :: Text,
		nodeWitSource :: Label.Elem id_u (),
		nodeContent :: (a, Paragraph al a ia e),
		nodeIsSeparatePage :: Bool
	}
	deriving (Eq)

inNode_source :: 
	Optic.Lens 
		(Label.Elem id_u_1 ()) (Label.Elem id_u_2 ()) 
		(Node al a id_u_1 ia e) (Node al a id_u_2 ia e)
inNode_source = Optic.lens_from_get_set nodeWitSource (\ p w -> w { nodeWitSource = p })

inNode_idu :: Optic.Traversal e1 e2 (Node al a e1 ia e) (Node al a e2 ia e)
inNode_idu = Label.inElem_idu >**>^ inNode_source

inNode_idu_source_mb :: 
	Optic.Lens
		(Maybe (Label.Id id_u_1)) (Maybe (Label.Id id_u_2))
		(Node al a id_u_1 ia e) (Node al a id_u_2 ia e)
inNode_idu_source_mb = Label.inLabel_id_source_mb >**>^ Label.inElem_labels >**>^ inNode_source 


uid_of_node :: Node al a id_u li e -> Maybe id_u
uid_of_node = nodeWitSource >>> Label.ofElem_labels >>> Label.id_of_Labels >>> map Label.valueId

both_id_of_node :: Node al a id_u ia e -> (Text, Maybe id_u)
both_id_of_node node = (nodeIdAuto node, uid_of_node node)

attach_its_uid_to_node :: Node al a id_u ia e -> Maybe (id_u, Node al a id_u ia e)
attach_its_uid_to_node n = map (Pair.before n) (uid_of_node n)


type StructureAsTree al a (id_u :: Type) ia e = Tree.Tree (Node al a id_u ia e)

data Document al a (id_u :: Type) ia e = Document
	{
	docTree :: StructureAsTree al a id_u ia e
	}
	deriving (Eq)


-- | collects all the user identifiers in the document with their nodes.
uids_int_doc_with_nodes :: Document al a id_u ia e -> [(id_u, Node al a id_u ia e)]
uids_int_doc_with_nodes = docTree >>> Fold.toList >>> map attach_its_uid_to_node >>> catMaybes


-- optics :

ofLink_internals :: 
	Optic.Iso 
		(Either (a1, ia1) (a1, String)) (Either (a2, ia2) (a2, String))
		(Link a1 ia1) (Link a2 ia2)
ofLink_internals = let
	down = \case { LIn x -> Left x; LEx x -> Right x }
	up = Base.either LIn LEx
	in Optic.Iso down up

ofLink_additional :: Optic.Lens a1 a2 (Link a1 ia) (Link a2 ia)
ofLink_additional = Category2.empty >**>^ Optic.sum (Optic.lens_1, Optic.lens_1) >**>^ ofLink_internals

ofLink'_additional :: forall al ia a1 a2 . Optic.Traversal a1 a2 (Link' al a1 ia) (Link' al a2 ia)
ofLink'_additional = let
	trav :: forall app . Applicative app => (a1 -> app a2) -> (Link' al a1 ia -> app (Link' al a2 ia))
	trav e (a, (al, l)) = let
		result_part_2 :: app (al, Link a2 ia)
		result_part_2 = map (Pair.after al) (Optic.traverse ofLink_additional e l)
		in liftA2 (,) (e a) result_part_2
	in Optic.Traversal trav

ofInline_internals ::
	Optic.Iso
		((e, Maybe (Link' al a1 ia))) ((e, Maybe (Link' al a2 ia)))
		(Inline al a1 ia e) (Inline al a2 ia e)
ofInline_internals = Optic.Iso (\ (Inline v l) -> (v, l)) (uncurry Inline)

visual_in_Inline :: Optic.Lens' e (Inline al u ia e)
visual_in_Inline = Optic.lens_from_get_set ilVisual (\ e c -> c { ilVisual = e })

link_in_Inline ::
	forall a ia1 ia2 al1 al2 e .
	Optic.Lens
		(Maybe (Link' al1 a ia1)) (Maybe (Link' al2 a ia2)) 
		(Inline al1 a ia1 e) (Inline al2 a ia2 e)
link_in_Inline = Optic.lens_from_get_set ilLink (\ e c -> c { ilLink = e })

ofInline_additional :: forall al ia e a1 a2 . Optic.Traversal a1 a2 (Inline al a1 ia e) (Inline al a2 ia e)
ofInline_additional = let
	from_link :: Optic.Traversal a1 a2 (Maybe (Link' al a1 ia)) (Maybe (Link' al a2 ia))
	from_link = Category2.empty >**>^ ofLink'_additional >**>^ Optic.prism_Maybe
	from_internals ::
		Optic.Traversal a1 a2
			(e, Maybe (Link' al a1 ia)) (e, Maybe (Link' al a2 ia))
	from_internals = from_link >**>^ Optic.lens_2
	in Category2.empty >**>^ from_internals >**>^ ofInline_internals

ofInline_additional_to_link ::
	forall a ia e al1 al2 . Optic.Traversal al1 al2 (Inline al1 a ia e) (Inline al2 a ia e)
ofInline_additional_to_link =
	Category2.empty >**>^ Optic.lens_1 >**>^ Optic.lens_2 >**>^ Optic.prism_Maybe >**>^ link_in_Inline

internal_address_in_Link :: Optic.Prism (a, ia1) (a, ia2) (Link a ia1) (Link a ia2)
internal_address_in_Link = 
	Optic.from_up_and_match LIn (\case { LIn ia -> Right ia; LEx t -> Left (LEx t) })

internal_address_in_Link' :: forall a ia1 ia2 . Optic.AffineTraversal ia1 ia2 (Link a ia1) (Link a ia2)
internal_address_in_Link' = Category2.empty >**>^ Optic.lens_2 >**>^ internal_address_in_Link @a

internal_address_in_Inline :: 
	forall al a ia1 ia2 e . Optic.AffineTraversal ia1 ia2 (Inline al a ia1 e) (Inline al a ia2 e)
internal_address_in_Inline = 
	let 
		empty :: Optic.Empty Optic.AffineTraversal ia1 ia2
		empty = Category2.empty
	in 
		Category2.empty
		>**>^ internal_address_in_Link' @a
		>**>^ Optic.lens_2
		>**>^ Optic.lens_2
		>**>^ Optic.prism_Maybe 
		>**>^ link_in_Inline @a

ofParagraph_additional :: Optic.Traversal a1 a2 (Paragraph al a1 ia e) (Paragraph al a2 ia e)
ofParagraph_additional = Optic.product (Category2.empty, ofInline_additional)

ofParagraph_additional_to_link :: Optic.Traversal al1 al2 (Paragraph al1 a ia e) (Paragraph al2 a ia e)
ofParagraph_additional_to_link = 
	Category2.empty 
	>**>^ ofInline_additional_to_link 
	>**>^ Optic.lens_2

inlines_in_Node :: 
	forall al a id_u ia1 ia2 e .
	Optic.Traversal (Inline al a ia1 e) (Inline al a ia2 e) (Node al a id_u ia1 e) (Node al a id_u ia2 e)
inlines_in_Node = Category2.empty >**>^ Optic.lens_2 >**>^ inNode_content_elem

links_in_Node :: 
	forall al a id_u ia1 ia2 e .
	Optic.Traversal 
		(Maybe (Link' al a ia1)) (Maybe (Link' al a ia2))
		(Node al a id_u ia1 e) (Node al a id_u ia2 e)
links_in_Node = Category2.empty >**>^ link_in_Inline @a @ia1 @ia2 >**>^ inlines_in_Node

texts_in_Node :: forall u e id_u ia al . Optic.Traversal' e (Node al u id_u ia e)
texts_in_Node = Category2.empty
	>**>^ visual_in_Inline
	>**>^ Optic.lens_2
	>**>^ inNode_content_elem

wit_source_in_Node :: 
	Optic.Lens 
		(Label.Elem idts1 ()) (Label.Elem idts2 ()) 
		(Node al a idts1 ia e) (Node al a idts2 ia e)
wit_source_in_Node = Optic.lens_from_get_set nodeWitSource (\ e c -> c { nodeWitSource = e })

source_in_Node :: 
	Optic.Lens 
		(Label.Elem idts_1 ()) (Label.Elem idts_2 ()) 
		(Node al a idts_1 li e) (Node al a idts_2 li e)
source_in_Node = Optic.lens_from_get_set nodeWitSource (\ p w -> w { nodeWitSource = p })

inNode_content :: 
	forall a1 a2 e1 e2 ia1 ia2 id_u al1 al2 .
	Optic.Lens 
		(a1, (Paragraph al1 a1 ia1 e1)) (a2, (Paragraph al2 a2 ia2 e2))
		(Node al1 a1 id_u ia1 e1) (Node al2 a2 id_u ia2 e2)
inNode_content = Optic.lens_from_get_set nodeContent (\ p w -> w { nodeContent = p })

inNode_content_elem :: 
	forall u e ia1 ia2 idts al1 al2 .
	Optic.Lens 
		(Paragraph al1 u ia1 e) (Paragraph al2 u ia2 e) 
		(Node al1 u idts ia1 e) (Node al2 u idts ia2 e)
inNode_content_elem = Category2.empty >**>^ Optic.lens_2 >**>^ inNode_content @u @u

separate_page_in_Node :: Optic.Lens' Bool (Node al u idts li e)
separate_page_in_Node = Optic.lens_from_get_set nodeIsSeparatePage (\ p w -> w { nodeIsSeparatePage = p })

idu_in_Node :: 
	Optic.Traversal 
		(id_u_1) (id_u_2) (Node al u id_u_1 ia e) (Node al u id_u_2 ia e)
idu_in_Node = Category2.empty >**>^ Label.inElem_idu >**>^ inNode_source

ofNode_additional :: 
	Optic.Traversal a1 a2 (Node al a1 id_u ia e) (Node al a2 id_u ia e)
ofNode_additional = 
	Category2.empty 
	>**>^ Optic.product (Category2.empty, ofParagraph_additional) 
	>**>^ inNode_content

ofNode_additional_to_link :: 
	Optic.Traversal al1 al2 (Node al1 a id_u ia e) (Node al2 a id_u ia e)
ofNode_additional_to_link = Category2.empty >**>^ ofParagraph_additional_to_link >**>^ inNode_content_elem

internal_address_in_node :: 
	forall al u ia1 ia2 id_u e .
	Optic.Traversal ia1 ia2 (Node al u id_u ia1 e) (Node al u id_u ia2 e)
internal_address_in_node = 
	Category2.empty
	>**>^ internal_address_in_Inline >**>^ Optic.lens_2
	>**>^ inNode_content_elem

node_in_tree ::
	Optic.Traversal
		(Node al u id_u_1 ia1 e) (Node al u id_u_2 ia2 e)
		(StructureAsTree al u id_u_1 ia1 e) (StructureAsTree al u id_u_2 ia2 e)
node_in_tree = Optic.from_Traversable

inlines_in_Structure :: Optic.Traversal' (Inline al u ia e) (StructureAsTree al u id_u ia e)
inlines_in_Structure = inlines_in_Node >**>^ node_in_tree

idu_in_tree ::
	Optic.Traversal
		(id_u_1) (id_u_2)
		(StructureAsTree al u id_u_1 ia e) (StructureAsTree al u id_u_2 ia e)
idu_in_tree = idu_in_Node >**>^ node_in_tree

internal_address_in_tree :: 
	forall al u ia1 ia2 id_u e .
	Optic.Traversal ia1 ia2 (StructureAsTree al u id_u ia1 e) (StructureAsTree al u id_u ia2 e)
internal_address_in_tree = internal_address_in_node >**>^ node_in_tree

tree_in_Document :: 
	Optic.Lens 
		(StructureAsTree al1 u1 id_u1 ia1 e1) (StructureAsTree al2 u2 id_u2 ia2 e2)
		(Document al1 u1 id_u1 ia1 e1) (Document al2 u2 id_u2 ia2 e2)
tree_in_Document = Optic.lens_from_get_set docTree (\ p w -> w { docTree = p })

