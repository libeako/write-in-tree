-- | The file containing the design intentions of the user, that is the document.
module WriteInTree.Document.Core.Data where


import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import Prelude (String)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Elem (inElem_labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..), inLabel_page_address)

import qualified Data.Tree as Tree
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Data.HeteroPair as Pair
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize as Label


type Text = Base.String


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

attach_its_uid_to_node :: Node id_u ia -> Maybe (id_u, Node id_u ia)
attach_its_uid_to_node n = map (Pair.before n) (uid_of_node n)


type StructureAsTree (id_u :: Type) ia = Tree.Tree (Node id_u ia)


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
	Category2.identity
	>**>^ internal_address_in_Link
	>**>^ Optic.prism_Maybe
	>**>^ link_in_Inline

links_in_Node ::
	forall id_u ia1 ia2 .
	Optic.Traversal
		(Maybe (Link ia1)) (Maybe (Link ia2))
		(Node id_u ia1) (Node id_u ia2)
links_in_Node = Category2.identity >**>^ link_in_Inline @ia1 @ia2 >**>^ inNode_content

texts_in_Node :: forall id_u ia . Optic.Traversal' Text (Node id_u ia)
texts_in_Node =
	Category2.identity
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
idu_in_Node = Category2.identity >**>^ Label.inElem_idu >**>^ inNode_source

internal_address_in_link_in_node ::
	forall ia1 ia2 id_u .
	Optic.Traversal ia1 ia2 (Node id_u ia1) (Node id_u ia2)
internal_address_in_link_in_node =
	Category2.identity
	>**>^ internal_address_in_Inline
	>**>^ inNode_content

page_addresses_in_Node :: Optic.Traversal' (Maybe PageAddress) (Node i ia)
page_addresses_in_Node = Category2.identity >**>^ inLabel_page_address >**>^ inElem_labels >**>^ inNode_source

node_in_tree ::
	Optic.Traversal
		(Node id_u_1 ia1) (Node id_u_2 ia2)
		(StructureAsTree id_u_1 ia1) (StructureAsTree id_u_2 ia2)
node_in_tree = Optic.from_Traversable

inlines_in_Structure :: Optic.Traversal' (Inline ia) (StructureAsTree id_u ia)
inlines_in_Structure = Category2.identity >**>^ inNode_content >**>^ node_in_tree

idu_in_tree ::
	Optic.Traversal
		(id_u_1) (id_u_2)
		(StructureAsTree id_u_1 ia) (StructureAsTree id_u_2 ia)
idu_in_tree = idu_in_Node >**>^ node_in_tree

internal_address_in_link_in_tree ::
	forall ia1 ia2 id_u .
	Optic.Traversal ia1 ia2 (StructureAsTree id_u ia1) (StructureAsTree id_u ia2)
internal_address_in_link_in_tree = internal_address_in_link_in_node >**>^ node_in_tree

texts_in_Tree :: forall id_u ia . Optic.Traversal' Text (StructureAsTree id_u ia)
texts_in_Tree = Category2.identity >**>^ texts_in_Node >**>^ node_in_tree
