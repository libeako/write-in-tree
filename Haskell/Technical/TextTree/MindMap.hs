-- | Parse TextTree data structure from MindMap file.
module Technical.TextTree.MindMap
(
	ParseError, layer,
)
where

import Control.Category
import Data.Tree (Tree)
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude

import qualified Data.List as Base
import qualified Data.Maybe as Base
import qualified Data.Tree as Tree
import qualified Prelude as Base

import qualified Fana.Data.Either as Either
import qualified Fana.Data.Maybe as Maybe
import qualified Fana.Data.Tree.OfBase as Tree
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Technical.Xml as Xml
import qualified Text.XML.Light as Xml


type Text = Base.String


xml_name :: Text -> Xml.QName
xml_name text = Xml.QName text Nothing Nothing

xml_attribute :: Text -> Text -> Xml.Attr
xml_attribute name value = Xml.Attr (xml_name name) value

xml_elem :: Text -> [Xml.Attr] -> [Xml.Element] -> Xml.Element
xml_elem name attributes children = Xml.Element (xml_name name) attributes (map Xml.Elem children) Nothing

attribute_text_name :: Text
attribute_text_name = "TEXT"

node_name :: Text
node_name = "node"

render_elem :: Text -> [Xml.Attr]
render_elem text = [xml_attribute attribute_text_name text]

render_elem_tree :: Tree.Tree Text -> Xml.Element
render_elem_tree tree = 
	xml_elem node_name (render_elem (Tree.rootLabel tree)) 
		(map render_elem_tree (Tree.subForest tree))

render_to_whole_mm_file :: Tree.Forest Text -> Xml.Element
render_to_whole_mm_file = map render_elem_tree >>> xml_elem "map" [xml_attribute "version" "1.0.1"]

data ParseError = XmlParseError

instance Fana.Showable Text ParseError where
	show = \case
		XmlParseError -> "the xml encoding seems invalid"

-- | extracts a text-tree element from the attributes of an xml element
elem_from_attributes :: [Xml.Attr] -> Maybe Text
elem_from_attributes attributes =
	let 
		-- finds the attribute with the given name
		find_attr :: Base.String -> Maybe Xml.Attr
		find_attr s = Base.find ((Base.== s) . Xml.qName . Xml.attrKey) attributes
		from_attributes :: Xml.Attr -> Text
		from_attributes attr_value = Xml.attrVal attr_value
		in
			attribute_text_name -- we are interested in these attributes
			*>>> find_attr -- find them 
			*>>> Base.fmap from_attributes

parse_from_xml_element :: Xml.Element -> Tree.Forest Text
parse_from_xml_element element =
	let
		map_filter_tree :: (ei -> Base.Maybe eo) -> Tree.Tree ei -> [Tree.Tree eo]
		map_filter_tree f = Base.fmap f >>> Tree.filter_shallowly
		-- | whether the xml element is a mindmap node
		xml_elem_is_mm_node :: Xml.ElementHead -> Base.Bool
		xml_elem_is_mm_node = Xml.name >>> (Base.== node_name)
		-- | the tree of the xml elements
		xml_elem_tree :: Tree.Tree Xml.ElementHead
		xml_elem_tree = Base.fmap (Xml.head_of_element) (Xml.element_tree element)
		-- | the xml elements constituting the mindmap tree structure
		mm_node_elems :: Tree.Forest Xml.ElementHead
		mm_node_elems = map_filter_tree (Maybe.keep_iff xml_elem_is_mm_node) xml_elem_tree
		from_mm_to_text_tree :: Tree.Tree Xml.ElementHead -> Tree.Forest Text
		from_mm_to_text_tree = map_filter_tree (Xml.attributes >>> elem_from_attributes)
		in (Base.fmap from_mm_to_text_tree mm_node_elems *>>> Base.concat)


layer_xml :: Optic.PartialIso' () Text Xml.Element
layer_xml = Optic.PartialIso Xml.showTopElement (Xml.parseXMLDoc >>> Either.upgrade_Maybe)

-- | mindmap layer.
layer_mm :: Optic.Iso' Xml.Element [Tree Text]
layer_mm = Optic.Iso render_to_whole_mm_file parse_from_xml_element

layer :: Optic.PartialIso' ParseError Text [Tree Text]
layer =
	Category2.identity
	>**>^ Optic.piso_convert_error (const XmlParseError) layer_xml
	>**>^ layer_mm
