module WriteInTree.Document.Core.Serial.Page.Border
(
	NodeH, layer,
)
where

import Data.Tree (Tree)
import Fana.Prelude
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.ClassPrefix as Class
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Label


type Text = Base.String

type Paragraph = Data.Paragraph Text

-- | text value of node class signalling the separate page status
text_page_class :: Text
text_page_class = Class.class_prefix <> "page"

has_page_class :: Labels -> Bool
has_page_class = Label.labels_has_class text_page_class


type NodeH = Data.Node Text

render_from_node :: NodeH -> (Labels, Positioned Paragraph)
render_from_node i = (map >>> map) (const (Data.nodeContent i)) (Data.nodeWitSource i)

parse_into_node :: (Labels, Positioned Paragraph) -> NodeH
parse_into_node (l, Positioned pos par) = 
	Data.Node (l, Positioned pos ()) par (Data.status_from_is_page_trunk (has_page_class l))

layer :: Optic.Iso' (Tree (Labels, Positioned Paragraph)) (Tree NodeH)
layer = Optic.lift_iso (Optic.Iso render_from_node parse_into_node)
