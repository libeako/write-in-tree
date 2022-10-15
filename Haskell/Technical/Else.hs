module Technical.Else
(
	FilePath(..),
	tell_error,
)
where

import Prelude (String, IO)
import Fana.Prelude

import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Serial.Print.Show as Fana
import qualified System.IO as Base

newtype FilePath = FilePath { deFilePath :: String }


tell_error :: Fana.Showable String e => e -> IO ()
tell_error = Fana.show >>> Accu.extract >>> Base.hPutStrLn Base.stderr

