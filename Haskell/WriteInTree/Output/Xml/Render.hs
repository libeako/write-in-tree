module WriteInTree.Output.Xml.Render
(
	to_technical,
)
where

import Data.Bool (not)
import Fana.Prelude
import Prelude (String, FilePath)

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Data.Map as Map
import qualified Data.Tree as Tree
import qualified Prelude as Base
import qualified System.FilePath as Fp

import qualified Fana.Data.Function as Fn
import qualified Fana.Data.Identified as Identified
import qualified Fana.Optic.Concrete.Prelude as Optic

import qualified Technical.Html as Html
import qualified Technical.Xml.Data as Xml
import qualified WriteInTree.Document.Core.Data as UI
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.ClassPrefix as Class
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Serialize as Label
import qualified WriteInTree.Document.Core.Serial.Page.Tree as OData
import qualified WriteInTree.Output.Technical as T
import qualified WriteInTree.Output.Sentence as Sentence


type Text = Base.String


-- | text prefix for html class names - to prevent name collision with
text_class_prefix :: Text
text_class_prefix = Class.class_prefix

-- | html class of some elements of a page if and only if the page is trunk
text_class_trunk_page :: Text
text_class_trunk_page = text_class_prefix <> "trunk-page"

-- | html class marking the core content of sections
text_class_wit_content :: Text
text_class_wit_content = text_class_prefix <> "content"

-- | html class marking inline text representing sentences
text_class_sentence :: Text
text_class_sentence = text_class_prefix <> "sentence"

-- | html classes, the nodes holding of which are not subject of sentencing
text_classes_non_sentencing :: [Text]
text_classes_non_sentencing = let
	base = ["code", "code-block"]
	in base <> map (text_class_prefix <>) base

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

-- | html class of element that is a link to a sub page
text_class_page_break :: Text
text_class_page_break = text_class_prefix <> "page-break"


html_classes_of_whether_page_is_trunk :: Bool -> [Text]
html_classes_of_whether_page_is_trunk page_is_trunk =
	if page_is_trunk then [text_class_trunk_page] else []

wrap_by_id :: OData.Node UI.NodeIdU -> Fn.Endo Xml.ElementL
wrap_by_id n = 
	case Label.ofElem_id_u_content (UI.nodeWitSource n) of
		Just _ -> Optic.fill Xml.lens_id_of_Element (Just (UI.nodeIdAuto n))
		Nothing -> id

wrap_by_classes :: [Text] -> OData.Node UI.NodeIdU -> Fn.Endo Xml.ElementL
wrap_by_classes additional_classes n = let
	classes_from_node :: [Text]
	classes_from_node = Label.ofElem_class_values (UI.nodeWitSource n)
	all_classes = classes_from_node <> additional_classes
	in Optic.fn_up Xml.lens_classes_of_Element (all_classes <>)

wrap_by_id_and_classes :: [Text] -> OData.Node UI.NodeIdU -> Fn.Endo Xml.ElementL
wrap_by_id_and_classes additional_classes source_node = 
	wrap_by_classes additional_classes source_node >>> wrap_by_id source_node

wrap_subcontent_by_div :: [Xml.ElementL] -> Xml.ElementL
wrap_subcontent_by_div = Xml.tree (Xml.Head "div" [] (Xml.Labels Nothing [text_class_wit_content]))

wrap_by_section :: [Xml.ElementL] -> Xml.ElementL
wrap_by_section = Xml.tree (Xml.Head "section" [] def)

wrap_by_header_content :: Maybe Xml.ElementL -> Fn.Endo [Xml.ElementL]
wrap_by_header_content = 
	\ case
		Nothing -> id
		Just header -> wrap_subcontent_by_div >>> pure >>> (header :)

render_inline_visual :: Text -> Xml.ContentL
render_inline_visual t = Xml.text t

render_link :: Maybe (OData.Link UI.NodeIdU) -> OData.Site UI.NodeIdU -> Fn.Endo Xml.ContentL
render_link =
	let
		wrap_with_link_to :: String -> Fn.Endo Xml.ContentL
		wrap_with_link_to target = pure >>> Html.with_link_to target >>> Xml.element_as_content
		get_address :: OData.Link UI.NodeIdU -> OData.Site UI.NodeIdU -> String
		get_address link site = link_to_address site (OData.siteUserAddressMap site) link 
	in
		\case
			Nothing -> const id
			Just l -> get_address l >>> wrap_with_link_to

render_inline :: OData.Inline UI.NodeIdU -> OData.Site UI.NodeIdU -> Xml.ContentL
render_inline il = flip (render_link (UI.ilLink il)) (render_inline_visual (UI.ilVisual il))

unite_neighboring_texts :: [UI.Inline idts] -> [UI.Inline idts]
unite_neighboring_texts =
	\case
		[] -> []
		[x] -> [x]
		(UI.Inline t1 Nothing : UI.Inline t2 Nothing : rest) ->
			UI.Inline (t1 <> t2) Nothing : unite_neighboring_texts rest
		(to_not_change : rest) -> to_not_change : unite_neighboring_texts rest

render_possibly_sentence :: OData.Site UI.NodeIdU -> OData.Inline UI.NodeIdU -> Xml.ContentL
render_possibly_sentence site inline =
	case inline of
		UI.Inline t Nothing ->
			Xml.element_as_content ((pure >>> Html.classify_into [text_class_sentence]) (render_inline inline site))
		_ -> render_inline inline site

render_paragraph :: 
	Bool -> Bool -> 
	OData.Paragraph UI.NodeIdU -> OData.Site UI.NodeIdU -> Xml.ElementL
render_paragraph is_page_break sentencing p site = 
	let
		content :: [Xml.ContentL]
		content = 
			if not sentencing 
				then [(flip render_inline site) p]
				else
					let
						all_sections :: [OData.Inline UI.NodeIdU]
						all_sections = Sentence.sentences p
						render_possibly_sentence' = 
							flip render_inline site >>> List.singleton >>> Html.classify_into [text_class_sentence] >>> Xml.element_as_content
					in map render_possibly_sentence' all_sections
		classes :: [Text]
		classes = if is_page_break then [text_class_page_break] else []
	in (Xml.Head "p" [] (Xml.Labels Nothing classes), content)

render_section :: 
	Bool -> Bool -> OData.Site UI.NodeIdU -> OData.Structure UI.NodeIdU -> Xml.ElementL
render_section sentencing is_page_root site node_tree =
	let
		sub_content :: [Xml.ElementL]
		sub_content = map (render_section sentencing False site) (Tree.subForest node_tree)
		from_sub_content :: [Xml.ElementL] -> Xml.ElementL
		from_sub_content =
			let
				trunk_node :: OData.Node UI.NodeIdU
				trunk_node = Tree.rootLabel node_tree
				has_class_code :: Bool
				has_class_code =
					let 
						current_classes = (UI.nodeWitSource >>> Label.ofElem_class_values) trunk_node
						exceptional_classes = text_classes_non_sentencing
						in Fold.any (flip Fold.elem exceptional_classes) current_classes
				revised_sentencing = sentencing && not has_class_code
				header :: Maybe Xml.ElementL
				header =
					if is_page_root then Nothing else
						Just (render_paragraph is_page_break revised_sentencing (UI.nodeContent trunk_node) site)
				is_page_break :: Bool
				is_page_break = OData.is_inline_a_page_break (UI.nodeContent trunk_node)
				in
					wrap_by_header_content header >>>
					wrap_by_section >>> wrap_by_id_and_classes [] trunk_node
		in from_sub_content sub_content

render_navigation_bar_per_element :: 
	OData.Site UI.NodeIdU -> 
	Bool -> 
	OData.Node UI.NodeIdU -> 
	Xml.Content Xml.Labels
render_navigation_bar_per_element site not_this_page node = 
	let
		text = Xml.text (OData.title_of_section node)
		address_maybe = (node_address_for_navigation_bar site) node
		content :: Xml.ContentL
		content = 
			case (not_this_page, address_maybe) of
				(True, Just address) -> Xml.element_as_content (Html.with_link_to address [text])
				_ -> text
		classes_whether_current_page = if not_this_page then [] else [text_class_nav_current_page]
		classes = [text_class_nav_item] <> classes_whether_current_page
		add_classes :: Fn.Endo Xml.ContentL
		add_classes = (: []) >>> Html.classify_into (classes) >>> Xml.element_as_content
		in add_classes content

render_navigation_bar :: 
	Bool -> 
	OData.Site UI.NodeIdU -> 
	OData.Node UI.NodeIdU -> 
	[OData.Node UI.NodeIdU] -> 
	Xml.Element Xml.Labels
render_navigation_bar page_is_trunk site trunk_node path_to_site_trunk = 
	let
		list_core = trunk_node : path_to_site_trunk
		list_is_trunk = False : List.repeat True
		list = List.zipWith (render_navigation_bar_per_element site) list_is_trunk list_core 
		navigation_classes = html_classes_of_whether_page_is_trunk page_is_trunk
		navigation_content_single_line = 
			(Xml.Head "p" [] (Xml.Labels Nothing [text_class_nav_core]), List.reverse list)
	in Xml.tree (Xml.Head "nav" [] (Xml.Labels Nothing navigation_classes)) [navigation_content_single_line]

render_page_body_content :: Bool -> OData.Site UI.NodeIdU -> OData.Page UI.NodeIdU -> [Xml.ContentL]
render_page_body_content sentencing site page = 
	let
		node_tree = OData.pageContent page
		trunk_node = Tree.rootLabel node_tree
		path_to_trunk = OData.pagePathToTrunk page
		page_is_trunk :: Bool
		page_is_trunk = List.null path_to_trunk
		content = 
			[Html.classify_into [text_class_page_main_part]
				[Xml.element_as_content (render_section sentencing True site node_tree)]]
		navigation_bar = render_navigation_bar page_is_trunk site trunk_node path_to_trunk
		nav_separator =
			let
				classes_names :: [Text]
				classes_names_trunk_page = if page_is_trunk then [text_class_trunk_page] else []
				classes_names = text_class_nav_separator : classes_names_trunk_page
				in Html.horizontal_line (Xml.Labels Nothing classes_names)
		in map Xml.element_as_content (navigation_bar : nav_separator : content)

render_page :: Bool -> OData.Site UI.NodeIdU -> OData.Page UI.NodeIdU -> Xml.ElementL
render_page sentencing site page = 
	let
		classes :: [Text]
		classes = html_classes_of_whether_page_is_trunk (List.null (OData.pagePathToTrunk page))
		in 
			Html.page classes (Html.header (OData.title_of_page page) "style.css")
				(render_page_body_content sentencing site page)

render_page_to_text :: Bool -> OData.Site UI.NodeIdU -> OData.Page UI.NodeIdU -> String
render_page_to_text sentencing site page = Html.page_text (render_page sentencing site page)

page_file_name_from_id :: Text -> String
page_file_name_from_id page = Fp.addExtension page "html"

page_file_name :: OData.Page u -> String
page_file_name page = page_file_name_from_id (OData.id_of_page page)

page_file_path :: Text -> FilePath
page_file_path page_id = Fp.joinPath ["c", page_file_name_from_id page_id]

link_to_address :: OData.Site UI.NodeIdU -> OData.UserAddressMap UI.NodeIdU -> OData.Link UI.NodeIdU -> String
link_to_address site address_map = 
	\ case
		UI.LIn node_id -> 
			case node_id of
				Left (OData.SubPageTarget _ ida) -> page_file_name_from_id ida
				Right idu ->
					let
						error_message = 
							let idu_text = UI.nidun_u (Identified.cargo idu)
								in 
									"internal error : key " <> idu_text <>
									" is not in address map during link resolution"
						ilt :: OData.CrossLinkTarget
						ilt = Base.fromMaybe (Base.error error_message) (Map.lookup idu address_map)
						in page_file_name (OData.get_CrossLinkTarget_page site ilt)
		UI.LEx a -> a

node_address_for_navigation_bar :: OData.Site UI.NodeIdU -> OData.Node UI.NodeIdU -> Maybe String
node_address_for_navigation_bar site node =
	let 
		address = Map.lookup (UI.nodeIdAuto node) (OData.sitePageMap site)
		in map page_file_name address

compile_a_page :: Bool -> OData.Site UI.NodeIdU -> FilePath -> OData.Page UI.NodeIdU -> T.FileCreation
compile_a_page sentencing site output_folder_path page =
	let 
		page_f_path = page_file_path (OData.id_of_page page)
		in
			(
			Fp.joinPath [output_folder_path, page_f_path],
			render_page_to_text sentencing site page
			)

to_technical :: Bool -> FilePath -> OData.Site UI.NodeIdU -> T.FileOps
to_technical sentencing output_folder_path site = 
	let
		pages = OData.sitePageRelations site
		main_page = OData.get_page_of_Site_at site (Tree.rootLabel pages)
		regular_files =
			map
				(OData.get_page_of_Site_at site >>> compile_a_page sentencing site output_folder_path)
				pages
		redirect_file = 
			let 
				main_content = Html.redirect_page (page_file_path (OData.id_of_page main_page))
				file_path = Fp.joinPath [output_folder_path, "index.html"]
				full_content = Html.page_text main_content
				in (file_path, full_content)
		in 
			T.FileOps { T.foFileCreations = redirect_file : (Fold.toList regular_files) }
