module WriteInTree.Document.Core.Serial.RichTextTree.InNode.MetaName
(
	iso,
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

iso :: forall v . (Base.Enum v, Base.Bounded v) => (v -> Text) -> Optic.Iso' Text (Either v Text)
iso render = 
	let
		interpret_into_maybe = Optic.piso_interpret (Serial.enum render)
		forget_left = either (const Nothing) Just
		up = parse_into_either (interpret_into_maybe >>> forget_left)
		in Optic.Iso (Base.either render id) up
