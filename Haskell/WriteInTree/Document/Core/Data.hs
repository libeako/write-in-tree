-- | Basic layers of the design intentions of the user, that is the document.
module WriteInTree.Document.Core.Data where


import Data.Tree (Tree)
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (String)
import WriteInTree.Document.Core.Serial.Position (Position, HasPosition, get_position)

import qualified Fana.Data.Tree.ChildrenWithInfo as ForestA
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type Text = Base.String


{- link -}

-- | Can be internal or external.
data Link =
	  LIn Text -- ^ | Internal.
	| LEx String -- ^ | External.
	deriving (Eq)

either_in_Link :: Optic.Iso' (Either Text String) Link
either_in_Link =
	let
		down = \case { LIn x -> Left x; LEx x -> Right x }
		up = Base.either LIn LEx
		in Optic.Iso down up

internal_address_in_Link :: Optic.Prism' Text Link
internal_address_in_Link =
	Optic.from_up_and_match LIn (\case { LIn ia -> Right ia; LEx t -> Left (LEx t) })


{- inline -}

data Inline e =
	Inline
	{ ilVisual :: e
	, ilLink :: Maybe Link
	}
	deriving (Eq, Functor, Foldable, Traversable)
type InlineT = Inline Text

pair_in_Inline :: Optic.Iso' ((e, Maybe Link)) (Inline e)
pair_in_Inline = Optic.Iso (\ (Inline v l) -> (v, l)) (uncurry Inline)

visual_in_Inline :: Optic.Lens' e (Inline e)
visual_in_Inline = Optic.lens_from_get_set ilVisual (\ e c -> c { ilVisual = e })

link_in_Inline :: Optic.Lens' (Maybe Link) (Inline e)
link_in_Inline = Optic.lens_from_get_set ilLink (\ e c -> c { ilLink = e })

internal_address_in_Inline :: Optic.AffineTraversal' Text (Inline e)
internal_address_in_Inline =
	Category2.identity
	>**> Optic.to_AffineTraversal internal_address_in_Link
	>**> Optic.to_AffineTraversal Optic.prism_Maybe
	>**> Optic.to_AffineTraversal link_in_Inline


{- paragraph -}

type Paragraph e = Inline e
type ParagraphT = Paragraph Text


{- node -}

data Node =
	Node
	{ nodePosition :: Position
	, nodeContent :: ParagraphT
	}
	deriving (Eq)


position_in_Node :: Optic.Lens' Position Node
position_in_Node = Optic.lens_from_get_set nodePosition (\ p w -> w { nodePosition = p })

content_in_Node :: Optic.Lens' ParagraphT Node
content_in_Node = Optic.lens_from_get_set nodeContent (\ p w -> w { nodeContent = p })

text_in_Node :: forall . Optic.Traversal' Text Node
text_in_Node =
	Category2.identity
	>**> Optic.to_Traversal visual_in_Inline
	>**> Optic.to_Traversal content_in_Node

internal_address_in_Link_in_Node :: Optic.Traversal' Text Node
internal_address_in_Link_in_Node =
	Category2.identity
	>**> Optic.to_Traversal internal_address_in_Inline
	>**> Optic.to_Traversal content_in_Node


{- address -}

data Address = Address { unwrapPageAddress :: Text } deriving Eq


{- structure -}

type TreeA e = ForestA.Tree (Maybe Address) e
type ForestA e = ForestA.Forest (Maybe Address) e

type StructureAsTree = TreeA Node
type StructureAsForest = ForestA Node

inlines_in_Structure :: Optic.Traversal' InlineT StructureAsTree
inlines_in_Structure = 
	Category2.identity
	>**> Optic.to_Traversal content_in_Node
	>**> Optic.from_Traversable

text_content_in_PageContentBulk :: Optic.Traversal' Text StructureAsForest
text_content_in_PageContentBulk =
	Category2.identity
	>**> Optic.to_Traversal text_in_Node
	>**> Optic.from_Traversable
	>**> Optic.from_Traversable
	>**> Optic.from_Traversable


{- page -}

type PageTitle = Text
{-| (title, bulk content) -}
type PageContent = (PageTitle, StructureAsForest)


node_in_PageContent :: Optic.Traversal' Node PageContent
node_in_PageContent =
	Category2.identity
	>**> Optic.from_Traversable
	>**> Optic.from_Traversable
	>**> Optic.from_Traversable
	>**> Optic.to_Traversal Optic.lens_2


type Page = (Address, PageContent)

title_of_page :: Page -> Text
title_of_page = snd >>> fst

node_in_Page :: Optic.Traversal' Node Page
node_in_Page =
	Category2.identity
	>**> node_in_PageContent
	>**> Optic.to_Traversal Optic.lens_2


{- site -}

type Site = Tree Page


node_in_Site :: Optic.Traversal' Node Site
node_in_Site =
	Category2.identity
	>**> node_in_Page
	>**> Optic.from_Traversable

internal_address_in_Link_in_Site :: Optic.Traversal' Text Site
internal_address_in_Link_in_Site = 
	Category2.identity
	>**> internal_address_in_Link_in_Node
	>**> Optic.from_Traversable
	>**> Optic.from_Traversable
	>**> Optic.from_Traversable
	>**> Optic.to_Traversal Optic.lens_2
	>**> Optic.to_Traversal Optic.lens_2
	>**> Optic.from_Traversable


instance HasPosition Node where
	get_position = nodePosition
