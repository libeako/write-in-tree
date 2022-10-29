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

data IsPageTrunkStatus =
	IsPageTrunk | IsNotPageTrunk
	deriving (Eq)

status_from_is_page_trunk :: Bool -> IsPageTrunkStatus
status_from_is_page_trunk =
	\case
		True -> IsPageTrunk
		False -> IsNotPageTrunk

data Node ia =
	Node
	{
		nodeWitSource :: Label.Elem (),
		nodeContent :: Paragraph ia,
		nodePageTrunkStatus :: IsPageTrunkStatus
	}
	deriving (Eq)

inNode_source :: Optic.Lens (Label.Elem ()) (Label.Elem ()) (Node ia) (Node ia)
inNode_source = Optic.lens_from_get_set nodeWitSource (\ p w -> w { nodeWitSource = p })

node_is_page_trunk :: Node i -> Bool
node_is_page_trunk node = nodePageTrunkStatus node == IsPageTrunk


type StructureAsTree ia = Tree.Tree (Node ia)


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

link_in_Node ::
	forall ia1 ia2 .
	Optic.Lens (Maybe (Link ia1)) (Maybe (Link ia2)) (Node ia1) (Node ia2)
link_in_Node = Category2.identity >**>^ link_in_Inline @ia1 @ia2 >**>^ inNode_content

texts_in_Node :: forall ia . Optic.Traversal' Text (Node ia)
texts_in_Node =
	Category2.identity
	>**>^ visual_in_Inline
	>**>^ inNode_content

wit_source_in_Node ::
	Optic.Lens (Label.Elem ()) (Label.Elem ()) (Node ia) (Node ia)
wit_source_in_Node = Optic.lens_from_get_set nodeWitSource (\ e c -> c { nodeWitSource = e })

source_in_Node :: Optic.Lens (Label.Elem ()) (Label.Elem ()) (Node li) (Node li)
source_in_Node = Optic.lens_from_get_set nodeWitSource (\ p w -> w { nodeWitSource = p })

inNode_content ::
	forall ia1 ia2 .
	Optic.Lens (Paragraph ia1) (Paragraph ia2) (Node ia1) (Node ia2)
inNode_content = Optic.lens_from_get_set nodeContent (\ p w -> w { nodeContent = p })

separate_page_in_Node :: Optic.Lens' IsPageTrunkStatus (Node li)
separate_page_in_Node = Optic.lens_from_get_set nodePageTrunkStatus (\ p w -> w { nodePageTrunkStatus = p })

internal_address_in_link_in_node ::
	forall ia1 ia2 . Optic.Traversal ia1 ia2 (Node ia1) (Node ia2)
internal_address_in_link_in_node =
	Category2.identity
	>**>^ internal_address_in_Inline
	>**>^ inNode_content

page_addresses_in_Node :: Optic.Traversal' (Maybe PageAddress) (Node ia)
page_addresses_in_Node = Category2.identity >**>^ inLabel_page_address >**>^ inElem_labels >**>^ inNode_source

node_in_tree :: Optic.Traversal (Node ia1) (Node ia2) (StructureAsTree ia1) (StructureAsTree ia2)
node_in_tree = Optic.from_Traversable

inlines_in_Structure :: Optic.Traversal' (Inline ia) (StructureAsTree ia)
inlines_in_Structure = Category2.identity >**>^ inNode_content >**>^ node_in_tree

internal_address_in_link_in_tree ::
	forall ia1 ia2 .
	Optic.Traversal ia1 ia2 (StructureAsTree ia1) (StructureAsTree ia2)
internal_address_in_link_in_tree = internal_address_in_link_in_node >**>^ node_in_tree

texts_in_Tree :: forall ia . Optic.Traversal' Text (StructureAsTree ia)
texts_in_Tree = Category2.identity >**>^ texts_in_Node >**>^ node_in_tree
