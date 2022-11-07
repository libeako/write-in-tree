module WriteInTree.Document.Core.Serial.Page.Data
(
	Text,
	PageContent, Page,
	Site,
	title_of_section, title_of_page, text_content_in_site,
	node_in_site,
)
where

import Prelude (String)

import Data.Tree (Tree)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Optic.Concrete.Prelude (lens_2)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))
import WriteInTree.Document.Core.Data (Node, texts_in_Node, node_in_tree)

import qualified Data.Foldable as Fold
import qualified Data.Tree as Tree
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type Text = Base.String

type PageContent = Tree Node
type Page = (PageAddress, PageContent)
type Site = Tree Page

title_of_section :: Node -> String
title_of_section = Optic.to_list texts_in_Node >>> Fold.concat

title_of_page :: Page -> Text
title_of_page = snd >>> Tree.rootLabel >>> title_of_section


-- optics :

node_in_site :: Optic.Traversal' Node Site
node_in_site =
	Category2.identity
	 >**>^ node_in_tree
	 >**>^ lens_2
	 >**>^ Optic.from_Traversable

text_content_in_site :: Optic.Traversal' Text Site
text_content_in_site = Category2.identity >**>^ texts_in_Node  >**>^ node_in_site
