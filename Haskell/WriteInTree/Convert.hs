module WriteInTree.Convert
(
	convert,
)
where

import Control.Monad (when)
import Fana.Prelude
import Prelude (IO)
import System.FilePath (FilePath)

import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified System.IO as Base
import qualified WriteInTree.Document.File as File


type Text = Base.String

type Doc = File.DocData''
type DocCore = File.DocCoreData''

tell_read_error :: File.Error -> IO ()
tell_read_error = Fana.show >>> Accu.extract >>> Base.hPutStrLn Base.stderr

write :: Bool -> FilePath -> Doc -> IO ()
write do_readback_test address doc = 
	let
		readback_test :: IO ()
		readback_test =
			let
				inequal_error_message = 
					"Bug! Read-back document does not equal wrote-out document. => Import | Export functionality is not reliable."
				check_readback_doc :: Doc -> IO ()
				check_readback_doc readback_doc = 
					when (readback_doc /= doc) (Base.hPutStrLn Base.stderr inequal_error_message)
				in File.read'' address >>= either tell_read_error check_readback_doc
		in
			do
				File.write'' address doc
				when do_readback_test readback_test

convert :: Bool -> FilePath {- input -} -> FilePath {- output -} -> IO ()
convert do_readback_test input_address output_address = let
	from_doc_result :: Either File.Error Doc -> IO ()
	from_doc_result = Base.either tell_read_error (write do_readback_test output_address)
	in File.read'' input_address >>= from_doc_result
