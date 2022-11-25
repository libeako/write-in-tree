module WriteInTree.Document.Core.Serial.Page.Serialize
(
	layer,
)
where

import Data.Tree (Tree, Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Page.Data (PageContent)

import qualified Data.Tree as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic


parse_page :: Tree Node -> PageContent
parse_page (Tree.Node trunk children) = (trunk, children)

layer :: Optic.Iso (Forest Node) (Tree Node) PageContent PageContent
layer = Optic.Iso snd parse_page
