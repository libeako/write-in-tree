module WriteInTree.Document.Core.Serial.Page.Count
(
	Ordinal, layer,
)
where

import Control.Monad.State.Lazy (State, get, modify, evalState)
import Data.Bitraversable (bitraverse)
import Fana.Prelude
import Prelude ((+))

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Page.Data as PData
import qualified WriteInTree.Document.Core.Serial.Page.BreakStructure as BS

type Ordinal = PData.PageKey

type Page a e = BS.Page a e
type PageContent a e = BS.PageContent a e
type Child a e = BS.Child a e

type Count e c = c () e -> State Ordinal (c Ordinal e)

count_in_page :: Count e Page
count_in_page (BS.Page _ c) =
	do
		next <- get
		modify (+1)
		map (BS.Page next) (count_in_page_content c)
count_in_page_content :: Count e PageContent
count_in_page_content = Optic.traverse BS.child_in_page_content count_in_child
count_in_child :: Count e Child
count_in_child = bitraverse count_in_page count_in_page_content

count_all :: Page () e -> Page Ordinal e
count_all = count_in_page >>> flip evalState 1

layer ::
	Optic.Iso
		(Data.StructureAsTree i) (Page () e2)
		(Data.StructureAsTree i) (Page Ordinal e2)
layer = Optic.Iso id count_all
