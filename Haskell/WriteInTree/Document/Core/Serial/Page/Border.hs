module WriteInTree.Document.Core.Serial.Page.Border
(
	layer,
)
where

import Data.Tree (Tree)
import Fana.Prelude

import qualified Data.Char as Base
import qualified Data.Foldable as Fold
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.ClassPrefix as Class
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


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

parse_into_node ::
	forall a . a ~ Label.Elem Text =>
	(a (), Paragraph) -> Either (Pos.Positioned (Accu.Accumulated Text)) NodeH
parse_into_node (a, paragraph) =
	let
		make :: Text -> Data.Node Text Text
		make id_a = Data.Node id_a a paragraph (has_page_class a)
		error_message :: Pos.Positioned (Accu.Accumulated Text)
		error_message = Pos.Positioned (Pos.get_position a) "node does not have an automatic identifier"
		in Base.maybe (Left error_message) Right (map make (Label.ofElem_auto_id a))

layer ::
	a ~ Label.Elem Text =>
	Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text)) (Tree (a (), Paragraph)) (Tree NodeH)
layer = 
	Optic.piso_convert_error Pos.maybefy_positioned
		(Optic.lift_piso (Optic.PartialIso render_from_node parse_into_node))


-- other stuff:

translate_page_name_char :: Base.Char -> Base.Char
translate_page_name_char c = if Base.isAlphaNum c then c else '-'

translate_page_name :: Text -> Text
translate_page_name = map translate_page_name_char

pages :: Tree NodeH -> [NodeH]
pages = toList >>> Base.filter Data.nodeIsSeparatePage

address_of_page :: NodeH -> Text
address_of_page = Optic.to_list Data.texts_in_Node >>> Fold.fold >>> translate_page_name
