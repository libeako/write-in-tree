module WriteInTree.Document.Core.Serial.InNodeTextStructure
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

layer_escapee :: Optic.Iso' Text Text
layer_escapee =
	let
		r :: Text -> Text
		r =
			\case
				s@(i : ' ' : _) | i == meta_char -> i : s
					-- add a meta character to the front
				x -> x
		p :: Text -> Text
		p =
			\case
				i1 : s@(i2 : ' ' : _) | i1 == meta_char && i2 == meta_char -> s
					-- delete a meta character from the front
				x -> x
		in Optic.Iso r p
