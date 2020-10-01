module Technical.Xml
(
	ElementHead(..),
	head_of_element, 
	element_tree,
)
where


import qualified Data.Maybe as Base
import qualified Data.Tree as Tree
import qualified Prelude as Base

import qualified Text.XML.Light as Xml


data ElementHead = ElementHead { name :: Base.String, attributes :: [Xml.Attr] }

head_of_element :: Xml.Element -> ElementHead
head_of_element element = 
	ElementHead 
		(Xml.qName (Xml.elName element))
		(Xml.elAttribs element)

-- | Extracts the tree structure of elements.
element_tree :: Xml.Element -> Tree.Tree Xml.Element
element_tree element = 
	let
		element_tree_of_content :: Xml.Content -> Base.Maybe (Tree.Tree Xml.Element)
		element_tree_of_content = 	
			\case
				Xml.Elem e -> Base.Just (element_tree e)
				_ -> Base.Nothing
		children = Base.catMaybes (Base.fmap element_tree_of_content (Xml.elContent element))
	in Tree.Node element children
