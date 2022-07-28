module WriteInTree.Output.Sentence
(
	sentences,
)
where

import Prelude (Char)
import Data.Bool (not)
import Fana.Prelude

import qualified Data.List as List
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as D


type Text = Base.String


split_part :: Maybe (D.Inline al u ia Text) -> Maybe (D.Inline al u ia Text, Maybe (D.Inline al u ia Text))
split_part =
	let
		step :: D.Inline al u ia Text -> (D.Inline al u ia Text, Maybe (D.Inline al u ia Text))
		step inline =
			case D.ilVisual inline of
				D.Text text ->
					let
						is_delimiter :: Char -> Bool
						is_delimiter = (== ';')
						(before_delimiter, rest) = List.break is_delimiter text
						pure_rest = List.dropWhile is_delimiter rest
						current = Optic.fill D.visual_in_Inline (D.Text before_delimiter) inline
						future = 
							case rest of
								[] -> Nothing
								_ -> Just (Optic.fill D.visual_in_Inline (D.Text pure_rest) inline)
						in (current, future)
		in map step

possibly_empty_sentences :: D.Inline () a ia Text -> [D.Inline () a ia Text]
possibly_empty_sentences = Just >>> List.unfoldr split_part

is_sentence_part_empty :: D.Inline () a ia Text -> Bool
is_sentence_part_empty = const False

sentences :: D.Inline () a ia Text -> [D.Inline () a ia Text]
sentences = possibly_empty_sentences >>> List.filter (is_sentence_part_empty >>> not)
