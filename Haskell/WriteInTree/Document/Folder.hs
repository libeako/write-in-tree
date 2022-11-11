module WriteInTree.Document.Folder
(
	write, read,
)
where

import Prelude (IO, String)
import System.FilePath (FilePath, (</>))
import Fana.Prelude

import qualified Prelude as Base

data Member d =
	Member
	{ memberName :: String
	, memberReader :: IO (Either String d)
	}

read_member :: Member d -> IO (d)
read_member (Member name reader) = 
	let
		error_result message = Base.error ("could not read folder entry '" <> name <> "'\n" <> message)
		in map (either error_result id) reader


-- the paths of the files relative to the folder
core_file_name :: String
core_file_name = "tree.mm"

write :: FilePath -> String -> IO ()
write folder_path file_contents =
	let
		write_file :: FilePath -> String -> IO ()
		write_file relative_path = Base.writeFile (folder_path </> relative_path)
		write_files = write_file core_file_name >>> map (const ())
		in write_files file_contents

read :: FilePath -> IO String
read folder_path = ((folder_path </>) >>> Base.readFile) core_file_name
