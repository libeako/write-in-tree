-- | Basic layers of the design intentions of the user, that is the document.
module WriteInTree.Document.Core.Data where


import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import Prelude (String)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..), inLabel_page_address, Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Position, HasPosition, get_position)

import qualified Data.Tree as Tree
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type Text = Base.String


-- | Can be internal or external.
data Link ia =
	  LIn ia -- ^ | Internal.
	| LEx String -- ^ | External.
	deriving (Eq)

data Inline =
	Inline
	{ ilVisual :: Text
	, ilLink :: Maybe (Link Text)
	}
	deriving (Eq)

type Paragraph = Inline

data IsPageTrunkStatus =
	IsPageTrunk | IsNotPageTrunk
	deriving (Eq)

status_from_is_page_trunk :: Bool -> IsPageTrunkStatus
status_from_is_page_trunk =
	\case
		True -> IsPageTrunk
		False -> IsNotPageTrunk

data Node =
	Node
	{ nodePosition :: Position
	, nodeLabels :: Labels
	, nodeContent :: Paragraph
	, nodePageTrunkStatus :: IsPageTrunkStatus
	}
	deriving (Eq)

type StructureAsTree = Tree.Tree Node


-- optics :

inNode_position :: Optic.Lens' Position Node
inNode_position = Optic.lens_from_get_set nodePosition (\ p w -> w { nodePosition = p })

inNode_labels :: Optic.Lens' Labels Node
inNode_labels = Optic.lens_from_get_set nodeLabels (\ p w -> w { nodeLabels = p })

inNode_content :: Optic.Lens' Paragraph Node
inNode_content = Optic.lens_from_get_set nodeContent (\ p w -> w { nodeContent = p })

inNode_separate_page :: Optic.Lens' IsPageTrunkStatus Node
inNode_separate_page = Optic.lens_from_get_set nodePageTrunkStatus (\ p w -> w { nodePageTrunkStatus = p })

node_is_page_trunk :: Node -> Bool
node_is_page_trunk node = nodePageTrunkStatus node == IsPageTrunk


ofLink_internals :: 
	Optic.Iso 
		(Either ia1 String) (Either ia2 String)
		(Link ia1) (Link ia2)
ofLink_internals =
	let
		down = \case { LIn x -> Left x; LEx x -> Right x }
		up = Base.either LIn LEx
		in Optic.Iso down up

ofInline_internals :: Optic.Iso' ((Text, Maybe (Link Text))) Inline
ofInline_internals = Optic.Iso (\ (Inline v l) -> (v, l)) (uncurry Inline)

visual_in_Inline :: Optic.Lens' Text Inline
visual_in_Inline = Optic.lens_from_get_set ilVisual (\ e c -> c { ilVisual = e })

link_in_Inline :: Optic.Lens' (Maybe (Link Text)) Inline
link_in_Inline = Optic.lens_from_get_set ilLink (\ e c -> c { ilLink = e })

internal_address_in_Link :: Optic.Prism ia1 ia2 (Link ia1) (Link ia2)
internal_address_in_Link =
	Optic.from_up_and_match LIn (\case { LIn ia -> Right ia; LEx t -> Left (LEx t) })

internal_address_in_Inline :: Optic.AffineTraversal' Text Inline
internal_address_in_Inline =
	Category2.identity
	>**>^ internal_address_in_Link
	>**>^ Optic.prism_Maybe
	>**>^ link_in_Inline

link_in_Node :: Optic.Lens' (Maybe (Link Text)) Node
link_in_Node = Category2.identity >**>^ link_in_Inline  >**>^ inNode_content

texts_in_Node :: forall . Optic.Traversal' Text Node
texts_in_Node =
	Category2.identity
	>**>^ visual_in_Inline
	>**>^ inNode_content

internal_address_in_link_in_node :: Optic.Traversal' Text Node
internal_address_in_link_in_node =
	Category2.identity
	>**>^ internal_address_in_Inline
	>**>^ inNode_content

page_addresses_in_Node :: Optic.Traversal' (Maybe PageAddress) Node
page_addresses_in_Node = Category2.identity >**>^ inLabel_page_address >**>^ inNode_labels

node_in_tree :: Optic.Traversal' Node StructureAsTree
node_in_tree = Optic.from_Traversable

inlines_in_Structure :: Optic.Traversal' Inline StructureAsTree
inlines_in_Structure = Category2.identity >**>^ inNode_content >**>^ node_in_tree

internal_address_in_link_in_tree ::	Optic.Traversal' Text StructureAsTree
internal_address_in_link_in_tree = internal_address_in_link_in_node >**>^ node_in_tree

texts_in_Tree :: forall ia . Optic.Traversal' Text StructureAsTree
texts_in_Tree = Category2.identity >**>^ texts_in_Node >**>^ node_in_tree

------------- end of optics ----------------


instance HasPosition Node where
	get_position = nodePosition
