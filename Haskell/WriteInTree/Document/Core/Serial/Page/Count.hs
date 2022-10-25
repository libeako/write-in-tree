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
import qualified WriteInTree.Document.Core.Serial.Page.Data as Data
import qualified WriteInTree.Document.Core.Serial.Page.BreakStructure as BS

type Ordinal = Data.PageKey

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

layer :: Optic.Iso (Page () e1) (Page () e2) (Page Ordinal e1) (Page Ordinal e2)
layer = Optic.Iso (BS.forget_in_additional_info_in_page (const ())) count_all
