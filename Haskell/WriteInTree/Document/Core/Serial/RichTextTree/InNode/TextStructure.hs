module WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure
(
	Content, Content',
	TextStructureError (..),
	render_exceptional, render, parse,
	layer_escapee, layer, layer_try,
)
where

import Data.Either as Base
import Fana.Prelude
import Prelude (Char)

import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Fana.Serial.Print.Show as Show



type Text = [Char]

meta_char :: Char
meta_char = '#'


type Content exceptional regular = Either exceptional regular
type Content' = Content Text Text

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

render :: Content Text Text -> Text
render = 
	let
		render_regular t = case t of
			mc : _ | mc == meta_char -> meta_char : t
			_ -> t
	in Base.either render_exceptional render_regular

data TextStructureError = TextStructureError { tse_whole_text :: Text }
instance Fana.Showable Text TextStructureError where
	show (TextStructureError text) = 
		mempty
		<> Accu.single "In node content \"" <> Accu.single text
		<> Accu.single "\": text structure error [the meta character '"
		<> Show.from_Char meta_char
		<> 
			"' should be followed by either a space and then the meta text \
			\or by an other instance of the meta character and then a space and then whatever]."

parse :: Text -> Either TextStructureError (Content Text Text)
parse = \case 
	-- meta node
	mc : rest | mc == meta_char -> case rest of
		' ' : rest_of_rest | mc == meta_char -> Right (Left rest_of_rest)
		(text@(mc' : ' ' : _)) | mc' == meta_char -> Right (Right text)
		text -> Left (TextStructureError text)
	-- a simple text node :
	text -> Right (Right text)




layer :: Optic.PartialIso' TextStructureError Text (Content Text Text)
layer = Optic.PartialIso render parse

parse_try :: Text -> Content Text Text
parse_try t = either (const (Right t)) id (parse t)

layer_try :: Optic.Iso' Text (Content Text Text)
layer_try = Optic.Iso render parse_try
