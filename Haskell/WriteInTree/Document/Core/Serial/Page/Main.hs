module WriteInTree.Document.Core.Serial.Page.Main
(
	SubPageTarget (..),
	CrossLinkTarget (..),
	LinkInternalTarget (..),
	Link, Inline, Paragraph, Node, Structure,
	Page (..),
	PageKey, Site (..),
	get_page_of_Site_at, get_CrossLinkTarget_page,

	title_of_section, title_of_page, is_inline_a_page_break, page_addresses_in_site, text_content_in_site,
	node_in_site,

	layer,
)
where

import WriteInTree.Document.Core.Serial.Page.Data
import WriteInTree.Document.Core.Serial.Page.Tree (layer)
