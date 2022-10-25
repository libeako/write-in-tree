module WriteInTree.Document.Core.Serial.Page.Data
(
	Text,
	SubPageTarget (..),
	CrossLinkTarget (..),
	LinkInternalTarget (..),
	Link, Inline, Paragraph, Node, Structure,
	Page (..), PageContent (..),
	PageKey, KeyedPageContent,
	Array, AllPages, SiteStructure (..), Site (..),
	get_page_of_Site_at, get_main_page_of_Site, get_CrossLinkTarget_page,

	title_of_section, title_of_page, is_inline_a_page_break, page_addresses_in_site, text_content_in_site,
	node_in_site,
)
where

import Prelude (String, Int)

import Data.Tree (Tree)
import Data.Array ((!))
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))

import qualified Data.Array as Array
import qualified Data.Foldable as Fold
import qualified Data.Tree as Tree
import qualified Fana.Data.Tree.OfBase as Tree
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as UI


type Text = Base.String
type PageKey = Int
type Array = Array.Array PageKey
type AllPages i = Array (Page i)

data SubPageTarget =
	SubPageTarget { sptPageKey :: PageKey }
	deriving (Eq)

type LinkInternalTarget (i :: Type) = Either SubPageTarget i

type Link (i :: Type) = UI.Link (LinkInternalTarget i)
type Inline (i :: Type) = UI.Inline (LinkInternalTarget i)
type Paragraph (i :: Type) = UI.Paragraph (LinkInternalTarget i)
type Node (i :: Type) = UI.Node (LinkInternalTarget i)

type Structure (i :: Type) = Tree.Tree (Node i)

type PageContent i = Structure i
type KeyedPage i = (PageKey, Page i)
type KeyedPageContent i = (PageKey, PageContent i)

data Page (i :: Type) =
	Page
	{ pageAddress :: PageAddress
	, pageContent :: Structure i
	}
	deriving (Eq)

{-| Link address to page but not to sub-page. -}
data CrossLinkTarget = CrossLinkTarget { cltPage :: PageKey } deriving (Eq)

data SiteStructure p =
	SiteStructure
	{
	sitePageRelations :: Tree PageKey,
	siteAllPages :: Array p
	}
	deriving (Eq)

type Site i = SiteStructure (Page i)

get_page_of_Site_at :: SiteStructure p -> PageKey -> p
get_page_of_Site_at site key = siteAllPages site ! key

get_main_page_of_Site :: SiteStructure p -> p
get_main_page_of_Site site =
	get_page_of_Site_at site (Tree.rootLabel (sitePageRelations site))

get_CrossLinkTarget_page :: Site i -> CrossLinkTarget -> Page i
get_CrossLinkTarget_page site = cltPage >>> get_page_of_Site_at site

make_Site :: Tree PageKey -> Array p -> SiteStructure p
make_Site page_relations all_pages = SiteStructure page_relations all_pages

is_link_a_page_break :: Link i -> Bool
is_link_a_page_break =
	\case
		UI.LIn lit -> either (const True) (const False) lit
		UI.LEx _ -> False

is_inline_a_page_break :: Inline i -> Bool
is_inline_a_page_break = UI.ilLink >>> maybe False is_link_a_page_break

title_of_section :: Node i -> String
title_of_section = Optic.to_list UI.texts_in_Node >>> Fold.concat

title_of_page :: Page i -> String
title_of_page = pageContent >>> Tree.rootLabel >>> title_of_section


-- optics :

of_Structure_SubPageTarget ::
	Optic.Traversal SubPageTarget SubPageTarget (Structure i) (Structure i)
of_Structure_SubPageTarget =
	Category2.identity >**>^ Optic.prism_Left >**>^ UI.internal_address_in_link_in_tree

content_in_Page :: Optic.Lens' (Structure i) (Page i)
content_in_Page = Optic.lens_from_get_set pageContent (\ c p -> p { pageContent = c })

trunk_node_in_Page :: Optic.Lens' (Node i) (Page i)
trunk_node_in_Page = Tree.trunk_in_tree >**>^ content_in_Page

trunk_node_of_page :: Page i -> Node i
trunk_node_of_page = pageContent >>> Tree.rootLabel

pages_in_site :: Optic.Lens (Array p1) (Array p2) (SiteStructure p1) (SiteStructure p2)
pages_in_site =
	Optic.lens_from_get_set
		siteAllPages
		(\ aps (SiteStructure relations _) -> SiteStructure relations aps)

trunk_node_in_site :: Optic.Traversal' (Node i) (Site i)
trunk_node_in_site =
	Category2.identity >**>^
	trunk_node_in_Page >**>^ Optic.from_Traversable >**>^ pages_in_site

page_addresses_in_site :: Optic.Traversal' (Maybe PageAddress) (Site i)
page_addresses_in_site = UI.page_addresses_in_Node >**>^ trunk_node_in_site

text_content_in_site :: Optic.Traversal' Text (Site i)
text_content_in_site =
	Category2.identity >**>^
	UI.texts_in_Tree  >**>^ content_in_Page >**>^ Optic.from_Traversable >**>^ pages_in_site

target_in_not_subpage_link_in_node :: Optic.Traversal' i (Node i)
target_in_not_subpage_link_in_node =
	Category2.identity >**>^
	Optic.prism_Right >**>^ UI.internal_address_in_link_in_node

node_in_site :: Optic.Traversal' (Node i) (Site i)
node_in_site =
	Category2.identity
	 >**>^ UI.node_in_tree
	 >**>^ content_in_Page
	 >**>^ Optic.from_Traversable
	 >**>^ pages_in_site
