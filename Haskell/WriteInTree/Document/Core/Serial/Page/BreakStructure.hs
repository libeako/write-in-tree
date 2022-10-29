module WriteInTree.Document.Core.Serial.Page.BreakStructure
(
	Page (..), additional_info_in_page,
	PageContent (..),
	Child, 
	children_in_page_content, child_in_page_content,
	Page', PageContent',
	layer,
)
where

import Data.Tree (Tree)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import WriteInTree.Document.Core.Data (IsPageTrunkStatus (..))

import qualified Data.Tree as Tree
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Data


data Page a e =
	Page
	{ pageAdditionalInfo :: a
	, pageContent :: PageContent a e
	}

data PageContent a e =
	PageContent
	{ pcTrunk :: e
	, pcChildren :: [Child a e]
	}

type Child a e = Either (Page a e) (PageContent a e)

additional_info_in_page :: Optic.Lens' a (Page a e)
additional_info_in_page = Optic.lens_from_get_set pageAdditionalInfo (\ a (Page _ c) -> Page a c)

children_in_page_content ::
	Optic.Lens ([Child a1 e]) ([Child a2 e]) (PageContent a1 e) (PageContent a2 e)
children_in_page_content = Optic.lens_from_get_set pcChildren (\ c (PageContent t _) -> PageContent t c)

child_in_page_content ::
	Optic.Traversal (Child a1 e) (Child a2 e) (PageContent a1 e) (PageContent a2 e)
child_in_page_content =
	Category2.identity >**>^ Optic.from_Traversable >**>^ children_in_page_content


type Page' = Page ()
type PageContent' = PageContent ()
type Child' e = Child () e

break :: (e -> IsPageTrunkStatus) -> Tree e -> Child' e
break get_node_status (Tree.Node trunk children) =
	let
		breaked_content = PageContent trunk (map (break get_node_status) children)
		in case get_node_status trunk of
			IsPageTrunk -> Left (Page () breaked_content)
			IsNotPageTrunk -> Right (breaked_content)

page_anyway :: Child' e -> Page' e
page_anyway = either id (Page ())


layer ::
	Optic.Iso
		(Tree (Data.Node i1)) (Tree (Data.Node i2))
		(Tree (Data.Node i1)) (Page' (Data.Node i2))
layer = Optic.Iso id (break Data.nodePageTrunkStatus >>> page_anyway)
