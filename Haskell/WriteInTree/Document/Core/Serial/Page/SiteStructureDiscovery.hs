module WriteInTree.Document.Core.Serial.Page.SiteStructureDiscovery
(
	layer,
)
where

import Data.Array (array)
import Data.Tree (Tree, Forest)
import Fana.Prelude

import qualified Data.List as List
import qualified Data.Tree as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Page.BreakStructure as BS
import qualified WriteInTree.Document.Core.Serial.Page.Count as Count
import qualified WriteInTree.Document.Core.Serial.Page.Data as PData


type PageKey = Count.Ordinal
type Key = PageKey


discover_page_tree :: BS.Page a e -> Tree (BS.Page a e)
discover_page_tree trunk =
	Tree.Node trunk (discover_page_forest (BS.pageContent trunk))

discover_page_forest :: BS.PageContent a e -> Forest (BS.Page a e)
discover_page_forest = BS.pcChildren >>> map discover_page_forest_of_child >>> fold

discover_page_forest_of_child :: BS.Child a e -> Forest (BS.Page a e)
discover_page_forest_of_child = either (discover_page_tree >>> (: [])) discover_page_forest

parse :: BS.Page Key e -> PData.SiteStructure (BS.Page Key e)
parse trunk =
	let
		page_tree = discover_page_tree trunk
		page_array =
			let
				page_key_start :: PageKey
				page_key_start = 1
				pair_list = map (\ p -> (BS.pageAdditionalInfo p, p)) (toList page_tree)
				in array (page_key_start, List.length pair_list) pair_list
		in
			PData.SiteStructure
				(map BS.pageAdditionalInfo page_tree)
				page_array

layer ::
	Optic.Iso
		(Data.StructureAsTree i) (BS.Page Key e2)
		(Data.StructureAsTree i) (PData.SiteStructure (BS.Page Key e2))
layer = Optic.Iso id parse
