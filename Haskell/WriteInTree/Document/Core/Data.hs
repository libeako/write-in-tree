-- | Basic layers of the design intentions of the user, that is the document.
module WriteInTree.Document.Core.Data where


import Data.Tree (Tree, Forest)
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (String)
import WriteInTree.Document.Core.Serial.Position (Position, HasPosition, get_position)

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type Text = Base.String


-- | Can be internal or external.
data Link =
	  LIn Text -- ^ | Internal.
	| LEx String -- ^ | External.
	deriving (Eq)

data Inline e =
	Inline
	{ ilVisual :: e
	, ilLink :: Maybe Link
	}
	deriving (Eq, Functor, Foldable, Traversable)
type InlineT = Inline Text

type Paragraph = InlineT

data Node =
	Node
	{ nodePosition :: Position
	, nodeContent :: Paragraph
	}
	deriving (Eq)

type StructureAsTree = Tree Node

data PageAddress = 
	PageAddress { unwrapPageAddress :: Text }
	deriving Eq

type PageContentTree = Tree Node
type PageContentBulk = Forest Node
type PageTitle = Text
{-| (title, bulk content) -}
type PageContent = (PageTitle, PageContentBulk)
type Page = (PageAddress, PageContent)
type Site = Tree Page


title_of_page :: Page -> Text
title_of_page = snd >>> fst



-- optics :

inNode_position :: Optic.Lens' Position Node
inNode_position = Optic.lens_from_get_set nodePosition (\ p w -> w { nodePosition = p })

inNode_content :: Optic.Lens' Paragraph Node
inNode_content = Optic.lens_from_get_set nodeContent (\ p w -> w { nodeContent = p })

ofLink_internals :: Optic.Iso' (Either Text String) Link
ofLink_internals =
	let
		down = \case { LIn x -> Left x; LEx x -> Right x }
		up = Base.either LIn LEx
		in Optic.Iso down up

ofInline_internals :: Optic.Iso' ((e, Maybe Link)) (Inline e)
ofInline_internals = Optic.Iso (\ (Inline v l) -> (v, l)) (uncurry Inline)

visual_in_Inline :: Optic.Lens' e (Inline e)
visual_in_Inline = Optic.lens_from_get_set ilVisual (\ e c -> c { ilVisual = e })

link_in_Inline :: Optic.Lens' (Maybe Link) (Inline e)
link_in_Inline = Optic.lens_from_get_set ilLink (\ e c -> c { ilLink = e })

internal_address_in_Link :: Optic.Prism' Text Link
internal_address_in_Link =
	Optic.from_up_and_match LIn (\case { LIn ia -> Right ia; LEx t -> Left (LEx t) })

internal_address_in_Inline :: Optic.AffineTraversal' Text (Inline e)
internal_address_in_Inline =
	Category2.identity
	>**> Optic.to_AffineTraversal internal_address_in_Link
	>**> Optic.to_AffineTraversal Optic.prism_Maybe
	>**> Optic.to_AffineTraversal link_in_Inline

link_in_Node :: Optic.Lens' (Maybe Link) Node
link_in_Node = Category2.identity >**> link_in_Inline  >**> inNode_content

text_in_Node :: forall . Optic.Lens' Text Node
text_in_Node =
	Category2.identity
	>**> visual_in_Inline
	>**> inNode_content

internal_address_in_link_in_node :: Optic.Traversal' Text Node
internal_address_in_link_in_node =
	Category2.identity
	>**> Optic.to_Traversal internal_address_in_Inline
	>**> Optic.to_Traversal inNode_content

node_in_tree :: Optic.Traversal' Node StructureAsTree
node_in_tree = Optic.from_Traversable

inlines_in_Structure :: Optic.Traversal' InlineT StructureAsTree
inlines_in_Structure = Category2.identity >**> Optic.to_Traversal inNode_content >**> node_in_tree

internal_address_in_link_in_tree ::	Optic.Traversal' Text StructureAsTree
internal_address_in_link_in_tree = internal_address_in_link_in_node >**> node_in_tree

texts_in_Tree :: forall ia . Optic.Traversal' Text StructureAsTree
texts_in_Tree = Category2.identity >**> Optic.to_Traversal text_in_Node >**> node_in_tree

node_in_page_content :: Optic.Traversal' Node PageContent
node_in_page_content =
	Category2.identity
	>**> node_in_tree
	>**> Optic.from_Traversable
	>**> Optic.to_Traversal Optic.lens_2

node_in_page :: Optic.Traversal' Node Page
node_in_page =
	Category2.identity
	>**> node_in_page_content
	>**> Optic.to_Traversal Optic.lens_2

node_in_site :: Optic.Traversal' Node Site
node_in_site =
	Category2.identity
	>**> node_in_page
	>**> Optic.from_Traversable

text_content_in_page_content_bulk :: Optic.Traversal' Text PageContentBulk
text_content_in_page_content_bulk =
	Category2.identity
	>**> Optic.to_Traversal text_in_Node
	>**> Optic.to_Traversal node_in_tree
	>**> Optic.from_Traversable

internal_address_in_link_in_site :: Optic.Traversal' Text Site
internal_address_in_link_in_site = 
	Category2.identity
	>**> internal_address_in_link_in_tree
	>**> Optic.from_Traversable
	>**> Optic.to_Traversal Optic.lens_2
	>**> Optic.to_Traversal Optic.lens_2
	>**> Optic.from_Traversable


------------- end of optics ----------------


instance HasPosition Node where
	get_position = nodePosition
