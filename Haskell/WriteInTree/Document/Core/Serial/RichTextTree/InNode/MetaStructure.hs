module WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaStructure
(
	lift_iso_to_Left, lift_layer_to_Left,
	put_back_not_used,
	serialize_node_content_without_worry,
	layer_in_node_text, layer_in_node_text',
	forget_about_meta,
	layer_1, layer_not_1,
)
where

import Control.Monad ((>=>))
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Base
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaName as Mn
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String


lift_iso_to_Left :: forall e l r . Optic.Iso' l r -> Optic.Iso' (Either l e) (Either r e)
lift_iso_to_Left = Optic.change_iso_per_component Bifunctor.first Bifunctor.first

sequence_Left :: Applicative a => Either (a l) r -> a (Either l r)
sequence_Left = Base.either (map Left) (Right >>> pure)

lift_layer_to_Left :: forall e l h t. Optic.PartialIso' e l h -> Optic.PartialIso' e (Either l t) (Either h t)
lift_layer_to_Left (Optic.PartialIso render parse) = let
	new_parse :: Either l t -> Either e (Either h t)
	new_parse = Bifunctor.first parse >>> sequence_Left
	in Optic.PartialIso (Bifunctor.first render) new_parse

-- | puts the not recognized meta names back into the lower level text structure content.
put_back_not_used :: Optic.Iso' (Either (Either m t) t) (Either m (Ts.Content t t))
put_back_not_used = 
	Optic.Iso 
		(Base.either (Left <<< Left) (Base.either (Left <<< Right) Right)) 
		(Base.either (Base.either Left (Right <<< Left)) (Right <<< Right))

{-|
	.
	
	"no worry" here means that in case of parse error
	the node content is treated as ordinary text
-}
serialize_node_content_without_worry ::
	forall mn p .
	(Base.Enum mn, Base.Bounded mn) =>
	(p -> Maybe Text) -> (Text -> p) ->
	(mn -> Text) -> Optic.Iso' p (Either mn p)
serialize_node_content_without_worry unwrap wrap meta_name_to_text = 
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
	(mn -> Text) ->
	Optic.Iso' Ts.Content' (Either mn Ts.Content')
layer_in_node_text meta_name_to_text = 
	let
		meta_name_or_not :: Optic.Iso' Text (Either mn Text)
		meta_name_or_not = Mn.iso meta_name_to_text
		in lift_iso_to_Left meta_name_or_not >**> put_back_not_used

layer_in_node_text' ::
	forall mn .
	(Base.Enum mn, Base.Bounded mn) =>
	(mn -> Text) ->
	Optic.Iso' Text (Either mn Text)
layer_in_node_text' = serialize_node_content_without_worry Just id


{-|
	forgets, in parsing, about the possibility of being meta
-}
forget_about_meta :: Optic.Iso' (Either mn p) p
forget_about_meta = Optic.Iso Right (either (Base.error "can not forget existing meta") id)


-- here are 2 versions of the main layer implementation, 
-- the difference between them is their low level data type

-- | a first layer in in-node text processing
layer_1 ::
	forall c pr pp mn .
	(Base.Enum mn, Base.Bounded mn) =>
	Functor pr =>
	(Traversable pp, forall l . Pos.HasPosition (pp l)) =>
	Traversable c =>
	(mn -> Text) ->
	Optic.PartialIso (Pos.Positioned Ts.TextStructureError) 
		(c (pr Text))
		(c (pp Text))
		(c (pr (Either mn Ts.Content')))
		(c (pp (Either mn Ts.Content')))
layer_1 meta_name_to_text = 
	Optic.lift_piso ((Optic.lift_piso >>> Pos.position_error_in_piso)
		(Ts.layer >**> convert_from_describing_class_4 (layer_in_node_text meta_name_to_text)))

-- | a first layer in in-node text processing
layer_1' ::
	forall c pr pp mn .
	(Base.Enum mn, Base.Bounded mn) =>
	Functor pr => Traversable pp => Traversable c =>
	(mn -> Text) ->
	Optic.Iso
		(c (pr Text))
		(c (pp Text))
		(c (pr (Either mn Text)))
		(c (pp (Either mn Text)))
layer_1' meta_name_to_text = (Optic.lift_iso >>> Optic.lift_iso) (layer_in_node_text' meta_name_to_text)

-- | a not first layer in in-node text processing.
-- too simple => should be deleted
layer_not_1 ::
	forall c p mn . (Base.Enum mn, Base.Bounded mn) => (Traversable c, Traversable p) =>
	(mn -> Text) -> Optic.Iso' (c (p Ts.Content')) (c (p (Either mn Ts.Content')))
layer_not_1 meta_name_to_text =
	Optic.lift_iso (Optic.lift_iso (layer_in_node_text meta_name_to_text))

