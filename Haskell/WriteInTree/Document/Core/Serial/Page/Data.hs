module WriteInTree.Document.Core.Serial.Page.Data
(
	Text,
	PageContentTree, PageContentBulk, PageTitle, PageContent, Page,
	Site,
	title_of_section, title_of_page, text_content_in_site,
	title_in_page, title_in_trunk_page,
	node_in_site,
)
where

import Prelude (String)

import Data.Tree (Tree, Forest)
import Fana.Data.Tree.OfBase (trunk_in_tree)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Optic.Concrete.Prelude (lens_2)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))
import WriteInTree.Document.Core.Data

import qualified Data.Foldable as Fold
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic


type PageContentTree = Tree Node
type PageContentBulk = Forest Node
type PageTitle = Node
{-| (title, bulk content) -}
type PageContent = (PageTitle, PageContentBulk)
type Page = (PageAddress, PageContent)
type Site = Tree Page

title_of_section :: Node -> String
title_of_section = Optic.to_list text_in_Node >>> Fold.concat

title_of_page :: Page -> Text
title_of_page = snd >>> fst >>> nodeContent >>> ilVisual


-- optics :

node_in_site :: Optic.Traversal' Node Site
node_in_site =
	Category2.identity
	>**>^ node_in_tree
	>**>^ Optic.from_Traversable
	>**>^ lens_2
	>**>^ lens_2
	>**>^ Optic.from_Traversable

text_content_in_site :: Optic.Traversal' Text Site
text_content_in_site = Category2.identity >**>^ text_in_Node  >**>^ node_in_site

title_in_page :: Optic.Lens' Text Page
title_in_page = Category2.identity >**>^ text_in_Node >**>^ Optic.lens_1 >**>^ Optic.lens_2

title_in_trunk_page :: Optic.Lens' Text Site
title_in_trunk_page = Category2.identity >**>^ title_in_page >**>^ trunk_in_tree
