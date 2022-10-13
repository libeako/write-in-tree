module WriteInTree.Document.Core.Serial.RichTextTree.InNodeTextStructure
(
	render_exceptional, 
	layer_escapee,
)
where

import Fana.Prelude
import Prelude (Char)

import qualified Fana.Optic.Concrete.Prelude as Optic


type Text = [Char]

meta_char :: Char
meta_char = '#'

render_exceptional :: Text -> Text
render_exceptional t = (meta_char : ' ' : t)

render_meta :: Text -> Text
render_meta m = meta_char : ' ' : m

parse_meta :: Text -> Maybe Text
parse_meta =
	\case
		c1 : ' ' : rest | c1 == meta_char -> Just rest
		_ -> Nothing

layer_escapee :: Optic.Iso' Text Text
layer_escapee =
	let
		r :: Text -> Text
		r =
			\case
				s@(i : ' ' : rest) | i == meta_char -> i : s
					-- add a meta character to the front
				x -> x
		p :: Text -> Text
		p =
			\case
				i1 : s@(i2 : ' ' : rest) | i1 == meta_char && i2 == meta_char -> s
					-- delete a meta character from the front
				x -> x
		in Optic.Iso r p
