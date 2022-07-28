module WriteInTree.Document.Core.ToOutput where

import Fana.Prelude

import qualified Data.Tree as Tree
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as UI
import qualified WriteInTree.Output.Pagination as O

type Text = Base.String


translateInlineVisual :: UI.InlineVisual () Text -> UI.InlineVisual () Text
translateInlineVisual (UI.Text t) = UI.Text t
translateInlineVisual (UI.Image p) = UI.Image p

translateLink :: UI.Link () UI.NodeIdU -> O.Link O.AI UI.NodeIdU
translateLink (UI.LIn ia) = UI.LIn (map Right ia)
translateLink (UI.LEx t) = UI.LEx t

translateInline :: UI.Inline () () UI.NodeIdU Text -> O.Inline O.AI UI.NodeIdU
translateInline i = UI.Inline 
	{ 
		UI.ilVisual = translateInlineVisual (UI.ilVisual i), 
		UI.ilLink = (map >>> map >>> map) translateLink (UI.ilLink i)
	}

translateParagraph :: UI.Paragraph () () UI.NodeIdU Text -> O.Paragraph O.AI UI.NodeIdU
translateParagraph p = map translateInline p

translateStructure :: UI.StructureAsTree () () UI.NodeIdU UI.NodeIdU Text -> O.Structure O.AI UI.NodeIdU
translateStructure (Tree.Node trunk children) = 
	let	
		separate_page = UI.nodeIsSeparatePage trunk
		trunk_node :: O.Node O.AI UI.NodeIdU
		trunk_node = 
			UI.Node (UI.nodeIdAuto trunk) (UI.nodeWitSource trunk) 
				(map translateParagraph (UI.nodeContent trunk)) 
				separate_page
		sub_results = map translateStructure children
		in Tree.Node trunk_node sub_results

translate :: UI.Document () () UI.NodeIdU UI.NodeIdU Text -> O.Site UI.NodeIdU
translate = UI.docTree >>> translateStructure >>> O.compile_site
