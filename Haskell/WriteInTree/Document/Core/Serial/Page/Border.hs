module WriteInTree.Document.Core.Serial.Page.Border
(
	layer,
)
where

import Data.Tree (Tree)
import Fana.Prelude

import qualified Data.Char as Base
import qualified Data.Foldable as Fold
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.ClassPrefix as Class
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize as Label


type Text = Base.String

type Paragraph = Data.Paragraph Text

-- | text value of node class signalling the separate page status
text_page_class :: Text
text_page_class = Class.class_prefix <> "page"

has_page_class :: Label.Elem id e -> Bool
has_page_class = Label.elem_has_class text_page_class


type NodeH = Data.Node Text Text

render_from_node :: forall a . a ~ Label.Elem Text => NodeH -> (a (), Paragraph)
render_from_node i = (Data.nodeWitSource i, Data.nodeContent i)

parse_into_node :: forall a . a ~ Label.Elem Text => (a (), Paragraph) -> NodeH
parse_into_node (a, paragraph) =
	let
		new_node :: Data.Node Text Text
		new_node = Data.Node a paragraph (has_page_class a)
		in new_node

layer ::
	a ~ Label.Elem Text =>
	Optic.Iso' (Tree (a (), Paragraph)) (Tree NodeH)
layer = (Optic.lift_iso (Optic.Iso render_from_node parse_into_node))


-- other stuff:

translate_page_name_char :: Base.Char -> Base.Char
translate_page_name_char c = if Base.isAlphaNum c then c else '-'

translate_page_name :: Text -> Text
translate_page_name = map translate_page_name_char

pages :: Tree NodeH -> [NodeH]
pages = toList >>> Base.filter Data.nodeIsSeparatePage

address_of_page :: NodeH -> Text
address_of_page = Optic.to_list Data.texts_in_Node >>> Fold.fold >>> translate_page_name
