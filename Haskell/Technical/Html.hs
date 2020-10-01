module Technical.Html
(
	horizontal_line, with_link_to, classify_into, header, page, 
	page_text,
	redirect_page,
)
where

import Data.Default.Class (Default (..))
import Fana.Prelude

import qualified Fana.Data.HeteroPair as Pair
import qualified Prelude as Base
import qualified Technical.Xml.Data as Xml
import qualified Technical.Xml.Render as Xml
import qualified Text.XML.Light as Xml hiding (node, Content, Element)


type Text = Base.String


horizontal_line :: Xml.Labels -> Xml.ElementL
horizontal_line labels = (Xml.Head "hr" [] labels, [])

with_link_to :: Default e => Text -> [Xml.Content e] -> Xml.Element e
with_link_to target = Pair.after (Xml.Head "a" [("href", target)] def)

classify_into :: [Text] -> [Xml.ContentL] -> Xml.ElementL
classify_into classes = Pair.after (Xml.Head "span" [] (Xml.Labels Nothing classes))

header :: Text -> Text -> Xml.ElementL
header title style_file_path = 
	Xml.tree (Xml.Head "head" [] def)
		[
		Xml.tree (Xml.Head "meta" [("http-equiv", "Content-Type"), ("content", "text/xhtml; charset=UTF-8")] def) [],
		Xml.tree (Xml.Head "link" [("rel", "stylesheet"), ("href", style_file_path)] def) [],
		(Xml.Head "title" [] def, [Xml.text title])
		]

page :: [Text] -> Xml.ElementL -> [Xml.ContentL] -> Xml.ElementL
page classes head content = 
	let
		body :: Xml.ElementL
		body = (Xml.Head "body" [] def, content)
		in (Xml.Head "html" [] (Xml.Labels Nothing classes), map Xml.element_as_content [head, body])

doctype_node :: Text
doctype_node = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

page_text :: Xml.ElementL -> Text
page_text content = 
	doctype_node <>
	(Xml.melt_labels_in_element >>> Xml.render_element >>> Xml.showElement) content

redirect_page :: Text -> Xml.ElementL
redirect_page target = 
	let
		add_html = Xml.tree (Xml.Head "html" [] def)
		add_head = Xml.tree (Xml.Head "head" [] def)
		add_meta = Xml.tree (Xml.Head "meta" [("http-equiv", "refresh"), ("content", "0; url=" <> target)] def)
	in (add_meta >>> pure >>> add_head >>> pure >>> add_html) []
