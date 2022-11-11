module Technical.FolderMember
(
	Member (..),
)
where

import Prelude (IO, String)
import Fana.Prelude

import qualified Prelude as Base

type Path = String
type FolderPath = Path

data Member d =
	Member
	{ memberName :: String
	, memberWriter :: FolderPath -> d -> IO ()
	, memberReader :: FolderPath -> IO (Either String d)
	}

read :: FolderPath -> Member d -> IO (d)
read folder_path (Member name _ reader) = 
	let
		error_result message = Base.error ("could not read '" <> name <> "'\n" <> message)
		in map (either error_result id) (reader folder_path)


