module Technical.Else
(
	FilePath(..),
)
where

import Prelude (String)


newtype FilePath = FilePath { deFilePath :: String }
