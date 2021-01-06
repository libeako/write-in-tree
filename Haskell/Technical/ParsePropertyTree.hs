-- | parse high level data from a tree of its fields
module Technical.ParsePropertyTree
(
	ParseError, Parser, 
	FieldOfProduct (..), HiddenFieldOfProduct (..), field_from_optic,
	RecordType,
	parser_of_text,
	parser_of_record, parser_of_list,
)
where

import Fana.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Base
import qualified Fana.Data.Key.LensToMaybeElement as Map
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Data as PropTree
import qualified Prelude as Base


type Char = Base.Char
type Text = [Char]

type Map e = StringyMap.Map Char e


-- * the output :

type ParseError = Accu.Accumulated Text
type ParseResult t = Either ParseError (t -> t)
type Parser t = PropTree.Property -> ParseResult t


data FieldOfProduct f p =
	FieldOfProduct { field_lift :: (f -> f) -> (p -> p), field_parser :: Parser f }

-- | same as 'FieldInProduct', but the field type parameter is existential ["hidden"]
data HiddenFieldOfProduct p = forall f . HiddenFieldOfProduct { deHiddenFieldOfProduct :: FieldOfProduct f p }

-- | represent a high level product data type.
-- its type parameter is the high level data type that it represents;
type RecordType p = Map (HiddenFieldOfProduct p)

field_from_optic :: Optic.IsFnUp o => Optic.Simple o f p -> Parser f -> HiddenFieldOfProduct p
field_from_optic fn_up p = HiddenFieldOfProduct (FieldOfProduct (Optic.fn_up fn_up) p)

compose_modifiers :: [p -> p] -> (p -> p)
compose_modifiers = Base.foldr' (>>>) id 

parser_of_simple :: (Text -> Either ParseError v) -> Parser v
parser_of_simple elem_parser = \case
	PropTree.MakeAtomicProperty e -> map const (elem_parser e)
	PropTree.MakeCompositeProperty _ -> Left "a simple value is to be parsed but instead found a composite value in the input"

parser_of_text :: Parser Base.String
parser_of_text = parser_of_simple pure

parser_of_record :: forall p . RecordType p -> Parser p
parser_of_record record = \case
	PropTree.MakeAtomicProperty e -> Left "a composite value [product] is to be parsed but can not be from a single input property"
	PropTree.MakeCompositeProperty m -> let
		with_field :: HiddenFieldOfProduct p -> Parser p
		with_field (HiddenFieldOfProduct (FieldOfProduct lift parser)) sub_input = map lift (parser sub_input)
		parser_of_field :: Text -> Parser p
		parser_of_field field_name sub_input = let
			when_input_field_name_not_matched :: ParseResult p
			when_input_field_name_not_matched = let
				error_message :: Accu.Accumulated Text
				error_message = "the input field does not have a corresponding field in the data to read into"
				in Left error_message
			internal_parse_result :: ParseResult p
			internal_parse_result = Base.maybe 
				when_input_field_name_not_matched 
				(flip with_field sub_input) 
				(Map.get_at field_name record)
			error_message_prefix :: Accu.Accumulated Text
			error_message_prefix = "at field \"" <> Accu.single field_name <> "\" : "
			in Bifunctor.first (error_message_prefix <>) internal_parse_result
		parse_results :: [ParseResult p]
		parse_results = map (uncurry parser_of_field) m
		in map compose_modifiers (sequenceA parse_results)

parser_of_list :: forall e . e -> Parser e -> Parser [e]
parser_of_list default_elem_value elem_parser = \case
	PropTree.MakeAtomicProperty e -> Left "a composite value [list] is to be parsed but can not be from a single input property"
	PropTree.MakeCompositeProperty m -> let
		per_elem :: (Text, PropTree.Property) -> Either ParseError e
		per_elem = snd >>> elem_parser >>> (map ($ default_elem_value))
		in map const (traverse (per_elem) m)
