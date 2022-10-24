module WriteInTree.Document.Core.Serial.Page.BreakStructure
(
	Page (..), additional_info_in_page,
	PageContent (..), children_in_page_content,
	Page', PageContent',
	layer,
)
where

import Data.Tree (Tree)
import Fana.Prelude
import WriteInTree.Document.Core.Data (IsPageTrunkStatus (..))

import qualified Data.Tree as Tree
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


type Forget e c m l = (m -> l) -> c m e -> c l e

forget_in_additional_info_in_page :: Forget e Page m l
forget_in_additional_info_in_page f (Page a c) =
	Page (f a) (forget_in_additional_info_in_page_content f c)

forget_in_additional_info_in_page_content :: Forget e PageContent m l
forget_in_additional_info_in_page_content =
	forget_in_additional_info_in_child >>> map >>> Optic.fn_up children_in_page_content

forget_in_additional_info_in_child :: Forget e Child m l
forget_in_additional_info_in_child f =
	bimap
		(forget_in_additional_info_in_page f)
		(forget_in_additional_info_in_page_content f)


type Page' = Page ()
type PageContent' = PageContent ()
type Child' e = Child () e

melt_child :: Child' e -> Tree e
melt_child = either melt_page melt_page_content

melt_page_content :: PageContent' e -> Tree e
melt_page_content (PageContent trunk children) = Tree.Node trunk (map melt_child children)

melt_page :: Page' e -> Tree e
melt_page (Page _ content) = melt_page_content content

break :: (e -> IsPageTrunkStatus) -> Tree e -> Child' e
break get_node_status (Tree.Node trunk children) =
	let
		breaked_content = PageContent trunk (map (break get_node_status) children)
		in case get_node_status trunk of
			IsPageTrunk -> Left (Page () breaked_content)
			IsNotPageTrunk -> Right (breaked_content)

page_anyway :: Child' e -> Page' e
page_anyway = either id (Page ())

layer :: Optic.Iso' (Tree (Data.Node i)) (Page' (Data.Node i))
layer = Optic.Iso melt_page (break Data.nodePageTrunkStatus >>> page_anyway)
