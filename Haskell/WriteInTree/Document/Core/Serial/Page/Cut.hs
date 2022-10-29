module WriteInTree.Document.Core.Serial.Page.Cut
(
	layer,
)
where

import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))

import qualified Data.Tree as Tree
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Optic.Concrete.Categories.Lens as Optic
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Page.BreakStructure as BS
import qualified WriteInTree.Document.Core.Serial.Page.Count as Count
import qualified WriteInTree.Document.Core.Serial.Page.Data as PData


type PageKey = Count.Ordinal

loosen_link_in_node :: (Data.Node i) -> PData.Node i
loosen_link_in_node = Optic.fn_up Data.internal_address_in_link_in_node Right

-- | creates a clone of the node 
-- which will be just a link to the page that the input node is a trunk of.
page_trunk_node_as_link ::
	PageKey ->
	Data.Node i ->
	Data.Node (PData.LinkInternalTarget i)
page_trunk_node_as_link page_key =
	Optic.fill Data.link_in_Node 
		(Just (Data.LIn (Left (PData.SubPageTarget page_key))))

link_node_to_subpage :: BS.Page PageKey (Data.Node i) -> Data.Node (PData.LinkInternalTarget i)
link_node_to_subpage (BS.Page key content) = page_trunk_node_as_link key (BS.pcTrunk content)

cut_page_content :: BS.PageContent PageKey (Data.Node i) -> PData.Structure i
cut_page_content (BS.PageContent trunk children) =
	Tree.Node (loosen_link_in_node trunk) (map cut_child children)

cut_child :: BS.Child PageKey (Data.Node i) -> PData.Structure i
cut_child = either (link_node_to_subpage >>> flip Tree.Node []) cut_page_content

cut_page ::
	PData.SiteStructure (PageAddress, BS.Page PageKey (Data.Node i)) ->
	(PageAddress, BS.Page PageKey (Data.Node i)) -> PData.Page i
cut_page site (address, p) = PData.Page address (cut_page_content (BS.pageContent p))

cut_site :: PData.SiteStructure (PageAddress, BS.Page PageKey (Data.Node i)) -> PData.Site i
cut_site site@(PData.SiteStructure rs pages) = PData.SiteStructure rs (map (cut_page site) pages)


down_grade_link :: PData.Link i -> (Maybe PageKey, Maybe (Data.Link i))
down_grade_link =
	\case
		Data.LIn a ->
			either
				(PData.sptPageKey >>> Just >>> Pair.before Nothing)
				(Data.LIn >>> Just >>> Pair.after Nothing) a
		Data.LEx s -> (Nothing, Just (Data.LEx s))

down_grade_link_mb :: Maybe (PData.Link i) -> (Maybe PageKey, Maybe (Data.Link i))
down_grade_link_mb = maybe (Nothing, Nothing) down_grade_link

down_grade_node :: PData.Node i -> (Maybe PageKey, Data.Node i)
down_grade_node = Optic.lift_functory_function Data.link_in_Node down_grade_link_mb


melt_from_node :: PData.Site i -> PData.Structure i -> Data.StructureAsTree i
melt_from_node site (Tree.Node trunk sub_forest) =
	let
		(sub_page_key_mb, new_trunk) = down_grade_node trunk
		melt_sub_content = map (melt_from_node site)
		in
			case sub_page_key_mb of
				Nothing -> Tree.Node new_trunk (melt_sub_content sub_forest)
				Just sub_page_key ->
					let
						pure_trunk = Optic.fill Data.link_in_Node Nothing new_trunk
							-- ^ delete link
						sub_page = PData.get_page_of_Site_at site sub_page_key
						in Tree.Node pure_trunk (melt_sub_content (Tree.subForest (PData.pageContent sub_page)))

melt_from_page :: PData.Site i -> PageKey -> Data.StructureAsTree i
melt_from_page site page_key =
	let
		PData.Page _ content = PData.get_page_of_Site_at site page_key
		in melt_from_node site content

melt :: PData.Site i -> Data.StructureAsTree i
melt site = melt_from_page site (Tree.rootLabel (PData.sitePageRelations site))


layer :: Optic.Iso
	(Data.StructureAsTree i1)
	(PData.SiteStructure (PageAddress, BS.Page PageKey (Data.Node i2)))
	(PData.Site i1) (PData.Site i2)
layer = Optic.Iso melt cut_site
