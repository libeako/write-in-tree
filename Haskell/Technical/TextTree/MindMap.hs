-- | Parse TextTree data structure from MindMap file.
module Technical.TextTree.MindMap
(
	ParseError,
	render,	parse, layer,
)
where

import Control.Category
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude

import qualified Data.List as Base
import qualified Data.Maybe as Base
import qualified Data.Tree as Tree
import qualified Prelude as Base

import qualified Fana.Data.Either as Either
import qualified Fana.Data.HomoPair as HoPair
import qualified Fana.Data.Maybe as Maybe
import qualified Fana.Data.Tree.OfBase as Tree
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Text.XML.Light as Xml

import qualified Technical.TextTree.Data as Data
import qualified Technical.Xml as Xml


type Text = Base.String


xml_name :: Text -> Xml.QName
xml_name text = Xml.QName text Nothing Nothing

xml_attribute :: Text -> Text -> Xml.Attr
xml_attribute name value = Xml.Attr (xml_name name) value

xml_elem :: Text -> [Xml.Attr] -> [Xml.Element] -> Xml.Element
xml_elem name attributes children = Xml.Element (xml_name name) attributes (map Xml.Elem children) Nothing


attribute_id_name :: Text
attribute_id_name = "ID"

attribute_text_name :: Text
attribute_text_name = "TEXT"

node_name :: Text
node_name = "node"


render_elem :: Data.ElemT -> [Xml.Attr]
render_elem (Data.Elem iden text) = Base.catMaybes
	[map (xml_attribute attribute_id_name) iden, Just (xml_attribute attribute_text_name text)]

render_elem_tree :: Tree.Tree Data.ElemT -> Xml.Element
render_elem_tree tree = 
	xml_elem node_name (render_elem (Tree.rootLabel tree)) 
		(map render_elem_tree (Tree.subForest tree))

render_to_whole_mm_file :: Tree.Tree Data.ElemT -> Xml.Element
render_to_whole_mm_file = render_elem_tree >>> (: []) >>> xml_elem "map" [xml_attribute "version" "1.0.1"]


data ParseErrorMindMap
	= 
	  PeNonTextNode [Base.String]
	{- ^ | 
		There is a node in the input mindmap without a "TEXT" attribute.
		A frequent cause of such situation is rich formatting.
		The parameter is the list of parent nodes as the path to the erroneous node.
	-}
	| PeNonSingleRoot
	-- ^ The root element either is not alone [has sibling] or does not exist.

instance Fana.Showable Text ParseErrorMindMap where
	show pe = case pe of
		PeNonTextNode path -> 
			Accu.single
				"A node does not have \"ID\" or \"TEXT\" attribute. \
					\It is in parent node at path" <>
			let point_converter p = " \"" <> Accu.single p <> "\" ->"
				in Base.foldMap point_converter path <>
			".\nA frequent possible cause is rich text formatting inside the node, \
				\which can not be parsed by this program."
		PeNonSingleRoot -> "Exactly 1 root element is expected, but input has either 0 or >1."

data ParseError = XmlParseError | MindmapParseError ParseErrorMindMap

instance Fana.Showable Text ParseError where
	show = \case
		XmlParseError -> "the xml encoding seems invalid"
		MindmapParseError details -> "the mindmap encoding seems invalid : " <> Fana.show details


-- | extracts a text-tree element from the attributes of an xml element
elem_from_attributes :: [Xml.Attr] -> Maybe Data.ElemT
elem_from_attributes attributes =
	let 
		-- finds the attribute with the given name
		find_attr :: Base.String -> Maybe Xml.Attr
		find_attr s = Base.find ((Base.== s) . Xml.qName . Xml.attrKey) attributes
		from_attributes :: (Maybe Xml.Attr, Xml.Attr) -> Data.ElemT
		from_attributes (attr_id, attr_value) =
			Data.Elem (map Xml.attrVal attr_id) (Xml.attrVal attr_value)
	in
		(attribute_id_name, attribute_text_name) -- we are interested in these attributes
		*>>> HoPair.map_homo_pair find_attr -- find them 
		*>>> Base.sequenceA -- both of them are needed
		*>>> Base.fmap from_attributes

extract_single_elem :: [a] -> Either ParseErrorMindMap a
extract_single_elem = 
	\case
		[e] -> Right e
		_ -> Left PeNonSingleRoot

parse_from_xml_element :: Xml.Element -> Tree.Forest Data.ElemT
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
		from_mm_to_text_tree :: Tree.Tree Xml.ElementHead -> Tree.Forest Data.ElemT
		from_mm_to_text_tree = map_filter_tree (Xml.attributes >>> elem_from_attributes)
	in
		(Base.fmap from_mm_to_text_tree mm_node_elems *>>> Base.concat) 


layer_xml :: Optic.PartialIso' () Text Xml.Element
layer_xml = Optic.PartialIso Xml.showTopElement (Xml.parseXMLDoc >>> Either.upgrade_Maybe)

-- | mindmap layer.
layer_mm :: Optic.PartialIso' ParseErrorMindMap Xml.Element Data.TreeT
layer_mm = Optic.PartialIso render_to_whole_mm_file (parse_from_xml_element >>> extract_single_elem)

layer :: Optic.PartialIso' ParseError Text Data.TreeT
layer = 
	(Optic.piso_convert_error (const XmlParseError) layer_xml) >**> 
	(Optic.piso_convert_error MindmapParseError layer_mm)


render :: Data.TreeT -> Text
render = Optic.down layer

parse :: Text -> Base.Either ParseError Data.TreeT
parse = Optic.interpret layer
