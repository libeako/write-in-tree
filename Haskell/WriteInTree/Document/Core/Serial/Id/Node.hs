module WriteInTree.Document.Core.Serial.Id.Node
(
	test,
	render, parse, parse_positioned, serialize,
)
where


import Fana.Develop.Test.Define (Test)
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Position

import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Data.List as List
import qualified WriteInTree.Document.Core.Serial.InNodeTextStructure as InNode

type Error = Text

parse_id_value :: [Text] -> Either Error Address
parse_id_value =
	\case
		[] -> Left "no id value given"
		[i] -> Right (Address i)
		_ -> Left "id value would be only one word"

id_instruction_meta_text :: Text
id_instruction_meta_text = "id"

render_id_instruction_words :: Address -> [Text]
render_id_instruction_words a = [id_instruction_meta_text, unwrapPageAddress a]

parse_id_instruction_words :: [Text] -> Either Error (Maybe Address)
parse_id_instruction_words =
	\case
		[] -> pure Nothing
		h : t ->
			if h == id_instruction_meta_text 
				then map Just (parse_id_value t)
				else pure Nothing

render_id_instruction :: Address -> Text
render_id_instruction = render_id_instruction_words >>> List.intercalate " "

parse_id_instruction :: Text -> Either Error (Maybe Address)
parse_id_instruction = List.words >>> parse_id_instruction_words


type NodeH = Either Address InNode.Structure

render :: NodeH -> InNode.Structure
render = either (render_id_instruction >>> InNode.Meta) id

parse :: InNode.Structure -> Either Error NodeH
parse i =
	case i of
		InNode.Norm _ -> Right (Right i)
		InNode.Meta t ->
			let
				decide :: Maybe Address -> NodeH
				decide = maybe (Right i) Left
				in map decide (parse_id_instruction t)

serialize :: Optic.PartialIso' Text InNode.Structure NodeH
serialize = Optic.PartialIso render parse


parse_positioned :: Positioned InNode.Structure -> Either Error (Either (Positioned Address) (Positioned InNode.Structure))
parse_positioned (Positioned p i) =
	let
		on_success :: NodeH -> Either (Positioned Address) (Positioned InNode.Structure)
		on_success =
			\case
				Left a -> Left (Positioned p a)
				Right e -> Right (Positioned p e)
		in map on_success (parse i)



{- ----------------- TESTS ------------------- -}

test :: Test
test = Test.single "id line" 
	(
		Optic.test_piso' 
			[InNode.Meta "id blabla dfg"] 
			[Left (Address "blabla"), Right (InNode.Meta "idblabla")] 
			serialize
	)
