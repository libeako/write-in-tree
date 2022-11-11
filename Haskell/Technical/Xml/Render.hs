module Technical.Xml.Render
(
	render_content, render_element,
)
where

import Fana.Prelude

import qualified Fana.Data.Tree.Uniform as FanaTree
import qualified Fana.Data.Tree.Discriminating as DTree
import qualified Technical.Xml.Data as Data
import qualified Text.XML.Light as Xml


render_attribute :: Data.Attribute -> Xml.Attr
render_attribute (n, v) = Xml.Attr (Xml.unqual n) v

render_content :: Data.Content () -> Xml.Content
render_content =
	FanaTree.children >>>
	\ case
		DTree.Leaf text -> Xml.Text (Xml.blank_cdata { Xml.cdData = text })
		DTree.Joint head content -> Xml.Elem(render_element (head, content))

render_element :: Data.Element () -> Xml.Element
render_element (head, content) = 
	Xml.blank_element
		{
			Xml.elName = Xml.unqual (Data.head_name head),
			Xml.elAttribs = map render_attribute (Data.head_attributes head),
			Xml.elContent = map render_content content
		}

