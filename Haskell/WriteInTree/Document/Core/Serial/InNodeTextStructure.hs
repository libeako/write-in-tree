module WriteInTree.Document.Core.Serial.InNodeTextStructure
(
	layer_escapee,
	Structure(..), render, parse, serialize,
)
where

import Fana.Prelude
import Prelude (Char)

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type Text = [Char]

delimit_char :: Char
delimit_char = ' '

norm_char :: Char
norm_char = '*'

meta_char :: Char
meta_char = '#'

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


data Structure = Norm Text | Meta Text
	deriving (Eq, Base.Show)

render :: Structure -> Text
render =
	\case
		Norm t -> norm_char : delimit_char : t
		Meta t -> meta_char : delimit_char : t

parse :: Text -> Either Text Structure
parse =
	\case
		m : d : t | m == meta_char && d == delimit_char -> Right (Meta t)
		n : d : t | n == norm_char && d == delimit_char-> Right (Norm t)
		_ -> Left "node must start with a character that signals the type of the node"

serialize :: Optic.PartialIso' Text Text Structure
serialize = Optic.PartialIso render parse
