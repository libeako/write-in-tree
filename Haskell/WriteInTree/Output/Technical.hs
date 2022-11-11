module WriteInTree.Output.Technical where

import Prelude (($!), String, (++), IO, FilePath)

import qualified Data.Either as Ei
import qualified Data.List as List
import qualified Prelude as Base
import qualified System.IO as IO
import qualified System.IO.Error as IOE
import qualified System.Directory as Dir
import qualified System.FilePath as Fp

import qualified Fana.Data.List as FnL
import           Fana.Prelude

type Content = String

-- | Represent a task to create an output file.
type FileCreation = 
	(
	FilePath, 
	-- | Content of the file to write.
	Content
	)

create_file :: FileCreation -> IO ()
create_file (path, content) = let
	file_has_content :: IO Bool
	file_has_content = do
		h <- IO.openFile path IO.ReadMode
		c <- IO.hGetContents h
		uptodate <- return $! (c == content)
		IO.hClose h
		return uptodate
	file_exists_and_has_content :: IO Bool
	file_exists_and_has_content = 
		map (Ei.either (const False) id) (IOE.tryIOError (file_has_content))
	in do
		Dir.createDirectoryIfMissing True (Fp.dropFileName path)
		uptodate <- file_exists_and_has_content
		if uptodate `Base.seq` uptodate
			then return ()
			else Base.writeFile path content

-- | The output in positive case, that is when the things go well.
data FileOps = FileOps
	{
	-- | The output files to create.
	foFileCreations :: [FileCreation]
	}

empty_fileops :: FileOps
empty_fileops = FileOps { foFileCreations = [] }

type Output = 
	(
	-- | The text to be written on to the standard output
	Content, 
	-- | The output files to create.
	FileOps
	)

ensure_new_line_at_end, ensure_new_line_at_end_if_should :: String -> String
ensure_new_line_at_end s = case (FnL.last s) of
	Just e -> if (e == '\n') then s else (s ++ "\n")
	Nothing -> "\n"
ensure_new_line_at_end_if_should s = case s of
	[] -> s
	_ -> ensure_new_line_at_end s

write :: Output -> IO ()
write (so, FileOps { foFileCreations = fcreates }) =
	do
		Base.putStr (ensure_new_line_at_end_if_should so)
		List.foldl (>>) (return ()) (map create_file fcreates)
