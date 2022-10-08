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

data Inline ia =
	Inline
	{ ilVisual :: Text
	, ilLink :: Maybe (Link ia)
	}
	deriving (Eq)

type Paragraph ia = Inline ia

data Node (id_u :: Type) ia =
	Node
	{
		nodeIdAuto :: Text,
		nodeWitSource :: Label.Elem id_u (),
		nodeContent :: Paragraph ia,
		nodeIsSeparatePage :: Bool
	}
	deriving (Eq)

inNode_source :: 
	Optic.Lens 
		(Label.Elem id_u_1 ()) (Label.Elem id_u_2 ()) 
		(Node id_u_1 ia) (Node id_u_2 ia)
inNode_source = Optic.lens_from_get_set nodeWitSource (\ p w -> w { nodeWitSource = p })

inNode_idu :: Optic.Traversal e1 e2 (Node e1 ia) (Node e2 ia)
inNode_idu = Label.inElem_idu >**>^ inNode_source

inNode_idu_source_mb :: 
	Optic.Lens
		(Maybe id_u_1) (Maybe id_u_2)
		(Node id_u_1 ia) (Node id_u_2 ia)
inNode_idu_source_mb = Label.inLabel_id_source_mb >**>^ Label.inElem_labels >**>^ inNode_source 


uid_of_node :: Node id_u li -> Maybe id_u
uid_of_node = nodeWitSource >>> Label.ofElem_labels >>> Label.id_of_Labels

both_id_of_node :: Node id_u ia -> (Text, Maybe id_u)
both_id_of_node node = (nodeIdAuto node, uid_of_node node)

attach_its_uid_to_node :: Node id_u ia -> Maybe (id_u, Node id_u ia)
attach_its_uid_to_node n = map (Pair.before n) (uid_of_node n)


type StructureAsTree (id_u :: Type) ia = Tree.Tree (Node id_u ia)

data Document (id_u :: Type) ia =
	Document
	{
		docTree :: StructureAsTree id_u ia
	}
	deriving (Eq)


-- | collects all the user identifiers in the document with their nodes.
uids_int_doc_with_nodes :: Document id_u ia -> [(id_u, Node id_u ia)]
uids_int_doc_with_nodes = docTree >>> Fold.toList >>> map attach_its_uid_to_node >>> catMaybes


-- optics :

ofLink_internals :: 
	Optic.Iso 
		(Either ia1 String) (Either ia2 String)
		(Link ia1) (Link ia2)
ofLink_internals =
	let
		down = \case { LIn x -> Left x; LEx x -> Right x }
		up = Base.either LIn LEx
		in Optic.Iso down up

ofInline_internals ::
	Optic.Iso
		((Text, Maybe (Link ia))) ((Text, Maybe (Link ia)))
		(Inline ia) (Inline ia)
ofInline_internals = Optic.Iso (\ (Inline v l) -> (v, l)) (uncurry Inline)

visual_in_Inline :: Optic.Lens' Text (Inline ia)
visual_in_Inline = Optic.lens_from_get_set ilVisual (\ e c -> c { ilVisual = e })

link_in_Inline ::
	forall ia1 ia2 .
	Optic.Lens
		(Maybe (Link ia1)) (Maybe (Link ia2)) 
		(Inline ia1) (Inline ia2)
link_in_Inline = Optic.lens_from_get_set ilLink (\ e c -> c { ilLink = e })

internal_address_in_Link :: Optic.Prism ia1 ia2 (Link ia1) (Link ia2)
internal_address_in_Link =
	Optic.from_up_and_match LIn (\case { LIn ia -> Right ia; LEx t -> Left (LEx t) })

internal_address_in_Inline ::
	forall ia1 ia2 . Optic.AffineTraversal ia1 ia2 (Inline ia1) (Inline ia2)
internal_address_in_Inline =
	Category2.empty
	>**>^ internal_address_in_Link
	>**>^ Optic.prism_Maybe
	>**>^ link_in_Inline

links_in_Node ::
	forall id_u ia1 ia2 .
	Optic.Traversal
		(Maybe (Link ia1)) (Maybe (Link ia2))
		(Node id_u ia1) (Node id_u ia2)
links_in_Node = Category2.empty >**>^ link_in_Inline @ia1 @ia2 >**>^ inNode_content

texts_in_Node :: forall id_u ia . Optic.Traversal' Text (Node id_u ia)
texts_in_Node = Category2.empty
	>**>^ visual_in_Inline
	>**>^ inNode_content

wit_source_in_Node ::
	Optic.Lens
		(Label.Elem idts1 ()) (Label.Elem idts2 ())
		(Node idts1 ia) (Node idts2 ia)
wit_source_in_Node = Optic.lens_from_get_set nodeWitSource (\ e c -> c { nodeWitSource = e })

source_in_Node ::
	Optic.Lens
		(Label.Elem idts_1 ()) (Label.Elem idts_2 ()) 
		(Node idts_1 li) (Node idts_2 li)
source_in_Node = Optic.lens_from_get_set nodeWitSource (\ p w -> w { nodeWitSource = p })

inNode_content ::
	forall ia1 ia2 id_u .
	Optic.Lens
		(Paragraph ia1) (Paragraph ia2)
		(Node id_u ia1) (Node id_u ia2)
inNode_content = Optic.lens_from_get_set nodeContent (\ p w -> w { nodeContent = p })

separate_page_in_Node :: Optic.Lens' Bool (Node idts li)
separate_page_in_Node = Optic.lens_from_get_set nodeIsSeparatePage (\ p w -> w { nodeIsSeparatePage = p })

idu_in_Node :: Optic.Traversal (id_u_1) (id_u_2) (Node id_u_1 ia) (Node id_u_2 ia)
idu_in_Node = Category2.empty >**>^ Label.inElem_idu >**>^ inNode_source

internal_address_in_node ::
	forall ia1 ia2 id_u .
	Optic.Traversal ia1 ia2 (Node id_u ia1) (Node id_u ia2)
internal_address_in_node =
	Category2.empty
	>**>^ internal_address_in_Inline
	>**>^ inNode_content

node_in_tree ::
	Optic.Traversal
		(Node id_u_1 ia1) (Node id_u_2 ia2)
		(StructureAsTree id_u_1 ia1) (StructureAsTree id_u_2 ia2)
node_in_tree = Optic.from_Traversable

inlines_in_Structure :: Optic.Traversal' (Inline ia) (StructureAsTree id_u ia)
inlines_in_Structure = Category2.empty >**>^ inNode_content >**>^ node_in_tree

idu_in_tree ::
	Optic.Traversal
		(id_u_1) (id_u_2)
		(StructureAsTree id_u_1 ia) (StructureAsTree id_u_2 ia)
idu_in_tree = idu_in_Node >**>^ node_in_tree

internal_address_in_tree ::
	forall ia1 ia2 id_u .
	Optic.Traversal ia1 ia2 (StructureAsTree id_u ia1) (StructureAsTree id_u ia2)
internal_address_in_tree = internal_address_in_node >**>^ node_in_tree

tree_in_Document ::
	Optic.Lens
		(StructureAsTree id_u1 ia1) (StructureAsTree id_u2 ia2)
		(Document id_u1 ia1) (Document id_u2 ia2)
tree_in_Document = Optic.lens_from_get_set docTree (\ p w -> w { docTree = p })

