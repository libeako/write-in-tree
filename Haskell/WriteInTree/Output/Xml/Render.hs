module WriteInTree.Output.Xml.Render
(
	to_technical,
)
where

import Data.Tree (Tree, rootLabel, subForest)
import Fana.Prelude
import Prelude (String, FilePath)
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Page.Data

import qualified Data.Bifunctor as BiFr
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Tree as Tree
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.Tree.OfBase as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified System.FilePath as Fp

import qualified Technical.Html as Html
import qualified Technical.Xml.Data as Xml
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.ClassPrefix as Class
import qualified WriteInTree.Output.Technical as T


-- | text prefix for html class names - to prevent name collision with
text_class_prefix :: Text
text_class_prefix = Class.class_prefix

-- | html class of some elements of a page if and only if the page is trunk
text_class_trunk_page :: Text
text_class_trunk_page = text_class_prefix <> "trunk-page"

-- | html class marking the core content of sections
text_class_wit_content :: Text
text_class_wit_content = text_class_prefix <> "content"

-- | html class of the core part of the navigation bar [which lists the page references]
text_class_nav_core :: Text
text_class_nav_core = text_class_prefix <> "nav-core"

-- | html class of items in the navigation bar
text_class_nav_item :: Text
text_class_nav_item = text_class_prefix <> "nav-item"

-- | html class of the current page item in the navigation bar
text_class_nav_current_page :: Text
text_class_nav_current_page = text_class_prefix <> "nav-current-page"

-- | html class of the core part of a page, not including the navigation bar
text_class_page_main_part :: Text
text_class_page_main_part = text_class_prefix <> "page_main_part"

-- | html class of the separator [perhaps a horizontal line] 
-- of the navigation bar from the rest of the page
text_class_nav_separator :: Text
text_class_nav_separator = text_class_prefix <> "navigation-separator"

html_classes_of_whether_page_is_trunk :: Bool -> [Text]
html_classes_of_whether_page_is_trunk page_is_trunk =
	if page_is_trunk then [text_class_trunk_page] else []

wrap_by_classes :: [Text] -> Fn.Endo Xml.ElementL
wrap_by_classes classes = Optic.fn_up Xml.lens_classes_of_Element (classes <>)

wrap_subcontent_by_div :: [Xml.ElementL] -> Xml.ElementL
wrap_subcontent_by_div = Xml.tree (Xml.Head "div" [] (Xml.Labels Nothing [text_class_wit_content]))

wrap_by_section :: [Xml.ElementL] -> Xml.ElementL
wrap_by_section = Xml.tree (Xml.Head "section" [] def)

wrap_by_header_content :: Maybe Xml.ElementL -> Fn.Endo [Xml.ElementL]
wrap_by_header_content = 
	\ case
		Nothing -> id
		Just header -> wrap_subcontent_by_div >>> pure >>> (header :)

render_link :: Maybe Link -> Fn.Endo Xml.ContentL
render_link =
	let
		wrap_with_link_to :: String -> Fn.Endo Xml.ContentL
		wrap_with_link_to target = pure >>> Html.with_link_to target >>> Xml.element_as_content
		get_address :: Link -> String
		get_address link = link_to_address link 
		in maybe id (get_address >>> wrap_with_link_to)

render_inline :: Inline -> Xml.ContentL
render_inline il = (render_link (ilLink il)) (Xml.text (ilVisual il))

render_paragraph :: Paragraph -> Xml.ElementL
render_paragraph p = (Xml.Head "p" [] (Xml.Labels Nothing []), [render_inline p])

render_section :: Site -> StructureAsTree -> Xml.ElementL
render_section site node_tree =
	let
		sub_content :: [Xml.ElementL]
		sub_content = map (render_section site) (subForest node_tree)
		from_sub_content :: [Xml.ElementL] -> Xml.ElementL
		from_sub_content =
			let
				trunk_node :: Node
				trunk_node = rootLabel node_tree
				header :: Maybe Xml.ElementL
				header = Just (render_paragraph (Data.nodeContent trunk_node))
				in
					wrap_by_header_content header >>>
					wrap_by_section >>> wrap_by_classes []
		in from_sub_content sub_content

render_navigation_bar_per_element :: Bool -> Page -> Xml.Content Xml.Labels
render_navigation_bar_per_element not_this_page page =
	let
		text = Xml.text (title_of_page page)
		address = page_file_path page
		content :: Xml.ContentL
		content =
			case not_this_page of
				True -> Xml.element_as_content (Html.with_link_to address [text])
				_ -> text
		classes_whether_current_page = if not_this_page then [] else [text_class_nav_current_page]
		classes = [text_class_nav_item] <> classes_whether_current_page
		add_classes :: Fn.Endo Xml.ContentL
		add_classes = (: []) >>> Html.classify_into (classes) >>> Xml.element_as_content
		in add_classes content

render_navigation_bar :: [Page] -> Xml.Element Xml.Labels
render_navigation_bar path_to_site_trunk =
	let
		page_is_trunk = List.null path_to_site_trunk
		list_is_trunk = False : List.repeat True
		list = List.zipWith (render_navigation_bar_per_element) list_is_trunk path_to_site_trunk
		navigation_classes = html_classes_of_whether_page_is_trunk page_is_trunk
		navigation_content_single_line = 
			(Xml.Head "p" [] (Xml.Labels Nothing [text_class_nav_core]), List.reverse list)
		in 
			Xml.tree 
				(Xml.Head "nav" [] (Xml.Labels Nothing navigation_classes)) 
				[navigation_content_single_line]

render_page_body_content :: Site -> ([Page], Page) -> [Xml.ContentL]
render_page_body_content site (path_to_trunk, page) =
	let
		node_forest = snd (snd page)
		page_is_trunk :: Bool
		page_is_trunk = List.null path_to_trunk
		content = 
			let
				core = 
					let
						per_child node_tree =
							Xml.element_as_content (render_section site node_tree)
						in map per_child node_forest
				in [Html.classify_into [text_class_page_main_part] core]
		navigation_bar = render_navigation_bar path_to_trunk
		nav_separator =
			let
				classes_names :: [Text]
				classes_names_trunk_page = if page_is_trunk then [text_class_trunk_page] else []
				classes_names = text_class_nav_separator : classes_names_trunk_page
				in Html.horizontal_line (Xml.Labels Nothing classes_names)
		in map Xml.element_as_content (navigation_bar : nav_separator : content)

render_page :: Site -> ([Page], Page) -> Xml.ElementL
render_page site (path_to_trunk, page) = 
	let
		classes :: [Text]
		classes = html_classes_of_whether_page_is_trunk (List.null path_to_trunk)
		in 
			Html.page classes (Html.header (title_of_page page) "style.css")
				(render_page_body_content site (path_to_trunk, page))

render_page_to_text :: Site -> ([Page], Page) -> String
render_page_to_text site (path_to_trunk, page) =
	Html.page_text (render_page site (path_to_trunk, page))

page_file_name_from_address :: Text -> String
page_file_name_from_address address = Fp.addExtension address "html"

page_file_name :: Page -> String
page_file_name = fst >>> unwrapPageAddress >>> page_file_name_from_address

page_file_path :: Page -> FilePath
page_file_path = page_file_name

link_to_address :: Link -> String
link_to_address = 
	\ case
		LIn address -> page_file_name_from_address address
		LEx a -> a

compile_a_page :: Site -> FilePath -> ([Page], Page) -> T.FileCreation
compile_a_page site output_folder_path (path_to_trunk, page) =
	let 
		page_f_path = page_file_path page
		in
			(
			Fp.joinPath [output_folder_path, page_f_path],
			render_page_to_text site (path_to_trunk, page)
			)

to_technical :: FilePath -> Site -> T.FileOps
to_technical output_folder_path site =
	let
		pages_with_pathes :: Tree ([Page], Page)
		pages_with_pathes = map (BiFr.first (map rootLabel)) (Tree.with_path_to_trunk site)
		main_page = Tree.rootLabel site
		regular_files =
			map
				(compile_a_page site output_folder_path)
				pages_with_pathes
		redirect_file =
			let 
				main_content = Html.redirect_page (page_file_path main_page)
				file_path = Fp.joinPath [output_folder_path, "index.html"]
				full_content = Html.page_text main_content
				in (file_path, full_content)
		in 
			T.FileOps { T.foFileCreations = redirect_file : (Fold.toList regular_files) }
