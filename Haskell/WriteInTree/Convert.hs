module WriteInTree.Convert
(
	convert,
)
where

import Control.Monad ((>=>))
import Fana.Prelude
import Prelude (IO)
import System.FilePath (FilePath)

import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified System.IO as Base
import qualified WriteInTree.Document.Core.Serial.Layers as Layers
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified WriteInTree.Document.Data as DocData
import qualified WriteInTree.Document.ReadFromFile as DocRead


type Text = Base.String

type Doc = DocRead.DocData''
type DocCore = DocRead.DocCoreData''

-- | test the idempotence of the given layer
test_idempotence :: forall e d . Optic.PartialIso' e Text d -> d -> Bool
test_idempotence layer test_data = 
	let
		render = Optic.down layer
		test_picture :: Text
		test_picture = render test_data
		in Base.either (const False) (render >>> (== test_picture)) (Optic.piso_interpret layer test_picture) 

put :: Optic.PartialIso' e Text d -> d -> IO ()
put layer = Optic.down layer >>> Base.putStrLn

-- | test the idempotence of the given layer
put_with_idempotence_test :: Optic.PartialIso' e Text d -> d -> IO ()
put_with_idempotence_test layer d = 
	if test_idempotence layer d then put layer d
		else Base.hPutStrLn Base.stderr "failed the idempotence test of serialization"

convert :: Bool -> FilePath {- input -} -> IO ()
convert whether_test_idempotence {- ^ whether to test the idempotence of the layer -} = let
	from_doc :: Doc -> IO ()
	from_doc doc = let
		layer :: Optic.PartialIso' (Pos.PositionedMb (Accu.Accumulated Text)) Text DocCore
		layer = Layers.layer_ready (DocData.doc_sep_props doc)
		d :: DocCore
		d = DocData.doc_core doc
		in (if whether_test_idempotence then put_with_idempotence_test else put) layer d
	from_doc_result :: Either DocRead.Error Doc -> IO ()
	from_doc_result = Base.either (Fana.show >>> Accu.extract >>> Base.hPutStrLn Base.stderr) from_doc
	in DocRead.read'' >=> from_doc_result
