module WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaStructure
(
	layer_in_node_content,
	layer_in_node_text,
)
where

import Control.Monad ((>=>))
import Fana.Prelude

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaName as Mn


type Text = Base.String


{-|
	.
	
	in case of parse error:
	the node content is treated as ordinary text
-}
layer_in_node_content ::
	forall mn p .
	(Base.Enum mn, Base.Bounded mn) =>
	(p -> Maybe Text) -> (Text -> p) ->
	(mn -> Text) -> Optic.Iso' p (Either mn p)
layer_in_node_content unwrap wrap meta_name_to_text = 
	let
		render :: Either mn p -> p
		render = either (meta_name_to_text >>> Ts.render_exceptional >>> wrap) id
		parse :: p -> Either mn p
		parse p =
			let
				left :: Either l r -> Maybe l
				left = either Just (const Nothing)
				extract_meta_text_from_text :: Text -> Maybe Text
				extract_meta_text_from_text = Ts.parse >>> either (const Nothing) left
				extract_meta_from_meta_text :: Text -> Maybe mn
				extract_meta_from_meta_text = Mn.parse meta_name_to_text >>> left
				extract_meta_name :: p -> Maybe mn
				extract_meta_name = unwrap >=> extract_meta_text_from_text >=> extract_meta_from_meta_text
				in maybe (Right p) Left (extract_meta_name p)
		in Optic.Iso render parse

layer_in_node_text ::
	forall mn .
	(Base.Enum mn, Base.Bounded mn) =>
	(mn -> Text) -> Optic.Iso' Text (Either mn Text)
layer_in_node_text = layer_in_node_content Just id
