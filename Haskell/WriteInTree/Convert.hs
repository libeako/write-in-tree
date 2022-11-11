module WriteInTree.Convert
(
	convert,
)
where

import Prelude (IO)
import Control.Monad (when)
import Fana.Prelude
import System.FilePath (FilePath)
import WriteInTree.Document.Main (Document (..))

import qualified Prelude as Base
import qualified System.IO as Base
import qualified WriteInTree.Document.File as File


type Text = Base.String


write :: Bool -> FilePath -> Document -> IO ()
write do_readback_test address doc = 
	let
		readback_test :: IO ()
		readback_test =
			let
				inequal_error_message = 
					"Bug! Read-back document does not equal wrote-out document. => Import | Export functionality is not reliable."
				check_readback_doc :: Document -> IO ()
				check_readback_doc readback_doc = 
					when (readback_doc /= doc) (Base.hPutStrLn Base.stderr inequal_error_message)
				in File.read address >>= check_readback_doc
		in
			do
				File.write address doc
				when do_readback_test readback_test

convert :: Bool -> FilePath {- input -} -> FilePath {- output -} -> IO ()
convert do_readback_test input_address output_address = 
	let
		from_doc_result :: Document -> IO ()
		from_doc_result = write do_readback_test output_address
		in File.read input_address >>= from_doc_result
