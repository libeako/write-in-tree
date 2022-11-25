module WriteInTree.Document.Core.Serial.Page.Data
(
	Text,
	PageContentTree, PageContentBulk, PageTitle, PageContent, Page,
	Site,
	title_of_page, 
	text_content_in_page_content_bulk, 
	node_in_site,
)
where

import Data.Tree (Tree, Forest)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Optic.Concrete.Prelude (lens_2)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))
import WriteInTree.Document.Core.Data

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic


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

node_in_page_content :: Optic.Traversal' Node PageContent
node_in_page_content =
	Category2.identity
	>**>^ node_in_tree
	>**>^ Optic.from_Traversable
	>**>^ lens_2

node_in_page :: Optic.Traversal' Node Page
node_in_page =
	Category2.identity
	>**>^ node_in_page_content
	>**>^ lens_2

node_in_site :: Optic.Traversal' Node Site
node_in_site =
	Category2.identity
	>**>^ node_in_page
	>**>^ Optic.from_Traversable

text_content_in_page_content_bulk :: Optic.Traversal' Text PageContentBulk
text_content_in_page_content_bulk =
	Category2.identity
	>**>^ text_in_Node
	>**>^ node_in_tree
	>**>^ Optic.from_Traversable
