module WriteInTree.Output.Sentence
(
	sentences,
)
where

import Prelude (Char)
import Data.Bool (not)
import Fana.Prelude

import qualified Data.Bifunctor as BiFr
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as D


type Text = Base.String


split_part :: D.Inline al u ia Text -> Maybe (D.Inline al u ia Text, D.Inline al u ia Text)
split_part inline = 
	let 
		is_delimiter :: Char -> Bool
		is_delimiter = (== ';')
	in case D.ilVisual inline of
		D.Image _ -> Nothing
		D.Text text ->
			let 
				(before_delimiter, rest) = List.break is_delimiter text
				pure_rest = List.dropWhile is_delimiter rest
			in case rest of
				[] -> Nothing
				_ -> 
					Just 
						(
							(Optic.fill D.visual_in_Inline (D.Text before_delimiter) inline),
							(Optic.fill D.visual_in_Inline (D.Text pure_rest) inline)
						)

split_parts :: forall a ia . [D.Inline () a ia Text] -> Maybe ([D.Inline () a ia Text], [D.Inline () a ia Text])
split_parts = 
	\case
		[] -> Nothing
		(first_part : rest) ->
			let
				continue_then_attach_to_start :: 
					Maybe ([D.Inline () a ia Text], [D.Inline () a ia Text])
				continue_then_attach_to_start = 
					Base.maybe (Just ([first_part], [])) (BiFr.first (first_part :) >>> Just)
						(split_parts rest)
			in case D.ilVisual first_part of
				D.Image _ -> continue_then_attach_to_start
				D.Text text -> 
					case text of
						[] -> split_parts rest
						_ ->
							case split_part first_part of
								Nothing -> continue_then_attach_to_start
								Just (s, rest_of_first) -> Just ([s], rest_of_first : rest)

possibly_empty_sentences :: [D.Inline () a ia Text] -> [[D.Inline () a ia Text]]
possibly_empty_sentences = List.unfoldr split_parts

is_sentence_part_empty :: D.Inline () a ia Text -> Bool
is_sentence_part_empty = const False

is_sentence_empty :: [D.Inline () a ia Text] -> Bool
is_sentence_empty = Fold.all is_sentence_part_empty

sentences :: [D.Inline () a ia Text] -> [[D.Inline () a ia Text]]
sentences = possibly_empty_sentences >>> List.filter (is_sentence_empty >>> not)
