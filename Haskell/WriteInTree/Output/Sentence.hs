module WriteInTree.Output.Sentence
(
	sentences,
)
where

import Prelude (Char)
import Data.Bool (not)
import Fana.Prelude
import WriteInTree.Document.Core.Data (Inline, ilVisual, visual_in_Inline)

import qualified Data.List as List
import qualified Fana.Optic.Concrete.Prelude as Optic


split_part :: Maybe Inline -> Maybe (Inline, Maybe Inline)
split_part =
	let
		step :: Inline -> (Inline, Maybe Inline)
		step inline =
			let
				text = ilVisual inline
				is_delimiter :: Char -> Bool
				is_delimiter = (== ';')
				(before_delimiter, rest) = List.break is_delimiter text
				pure_rest = List.dropWhile is_delimiter rest
				current = Optic.fill visual_in_Inline before_delimiter inline
				future = 
					case rest of
						[] -> Nothing
						_ -> Just (Optic.fill visual_in_Inline pure_rest inline)
				in (current, future)
		in map step

possibly_empty_sentences :: Inline -> [Inline]
possibly_empty_sentences = Just >>> List.unfoldr split_part

is_sentence_part_empty :: Inline -> Bool
is_sentence_part_empty = const False

sentences :: Inline -> [Inline]
sentences = possibly_empty_sentences >>> List.filter (is_sentence_part_empty >>> not)
