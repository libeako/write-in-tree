module WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaName
(
	parse, iso,
)
where

import Fana.Prelude
import Prelude (Char)

import qualified Data.Maybe as Base
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Enum as Serial
import qualified Prelude as Base


type Text = [Char]


parse_into_either :: (x -> Maybe v) -> (x -> Either v x)
parse_into_either f text = Base.maybe (Right text) Left (f text)

parse :: forall v . (Base.Enum v, Base.Bounded v) => (v -> Text) -> Text -> Either v Text
parse render = 
	let
		interpret_into_maybe = Optic.piso_interpret (Serial.enum render)
		forget_left = either (const Nothing) Just
		in parse_into_either (interpret_into_maybe >>> forget_left)

iso :: forall v . (Base.Enum v, Base.Bounded v) => (v -> Text) -> Optic.Iso' Text (Either v Text)
iso render = Optic.Iso (Base.either render id) (parse render)
