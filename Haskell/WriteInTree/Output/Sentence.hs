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


split_part :: Maybe (D.Inline ia Text) -> Maybe (D.Inline ia Text, Maybe (D.Inline ia Text))
split_part =
	let
		step :: D.Inline ia Text -> (D.Inline ia Text, Maybe (D.Inline ia Text))
		step inline =
			let
				text = D.ilVisual inline
				is_delimiter :: Char -> Bool
				is_delimiter = (== ';')
				(before_delimiter, rest) = List.break is_delimiter text
				pure_rest = List.dropWhile is_delimiter rest
				current = Optic.fill D.visual_in_Inline before_delimiter inline
				future = 
					case rest of
						[] -> Nothing
						_ -> Just (Optic.fill D.visual_in_Inline pure_rest inline)
				in (current, future)
		in map step

possibly_empty_sentences :: D.Inline ia Text -> [D.Inline ia Text]
possibly_empty_sentences = Just >>> List.unfoldr split_part

is_sentence_part_empty :: D.Inline ia Text -> Bool
is_sentence_part_empty = const False

sentences :: D.Inline ia Text -> [D.Inline ia Text]
sentences = possibly_empty_sentences >>> List.filter (is_sentence_part_empty >>> not)
