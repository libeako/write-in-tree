module WriteInTree.CommandLine where


import Data.Semigroup ((<>))
import Fana.Prelude
import Prelude (pure, FilePath, IO, String)

import qualified Control.Applicative as Base
import qualified Data.Monoid as Base
import qualified Options.Applicative as Parse



data Command = 
	  CTranslate FilePath FilePath Bool -- ^ input and output paths, whether to sentence
	| CListIdUs FilePath -- ^ lists user given node identifiers [params : input path]
	| CShowDefaultDocProps -- ^ shows default document properties
	| CConvert Bool FilePath

matevar_text__input_path :: String
matevar_text__input_path = "INPUT-DOCUMENT-[FOLDER]"

metavar_text__output_filter :: String
metavar_text__output_filter = "OUTPUT-[FOLDER]"

option_input_path :: Parse.Mod Parse.OptionFields String
option_input_path = 
	Parse.short 'i' <>
	Parse.long "input" <>
	Parse.help "input document [folder] path" <>
	Parse.metavar matevar_text__input_path
option_output_path :: Parse.Mod Parse.OptionFields String
option_output_path =
	Parse.short 'o' <>
	Parse.long "output" <>
	Parse.help "output [folder] path" <>
	Parse.metavar metavar_text__output_filter

option_whether_test_idempotence_of_serialization :: Parse.Mod Parse.FlagFields Bool
option_whether_test_idempotence_of_serialization = 
	Parse.long "test-idempotence-of-serialization" <>
	Parse.help "tests this software whether mutliple serialization loops produces the same result, to possibly catch bugs in the serialization of this software"

flag_sentencing :: Parse.Parser Bool
flag_sentencing =
	Parse.switch
		(
			Parse.long "sentencing" <>
			Parse.help 
				"will make the program to detect sentences in text \
				\and annotate them in the output markup text \
				\[inside an HTML element with class 'sentence']; \
				\does not apply for nodes with class 'code' or 'code-block'"
		)

parser_command_translate :: Parse.Mod Parse.CommandFields Command
parser_command_translate = 
	let
		options :: Parse.Parser Command
		options = 
			Base.liftA3 CTranslate
				(Parse.strOption option_input_path)
				(Parse.strOption option_output_path)
				flag_sentencing
		description_text = 
			"Generate HTML website from " <> 
			matevar_text__input_path <>
			" into " <>
			metavar_text__output_filter <>
			"."
	in
		Parse.info options (Parse.progDesc description_text) *>>> Parse.command "translate" 

parser_command_list_idus :: Parse.Mod Parse.CommandFields Command
parser_command_list_idus =
	let
		options :: Parse.Parser Command
		options = Base.liftA CListIdUs (Parse.strOption option_input_path)
		description_text = "List the user given node identifiers with their locations."
	in
		Parse.info options (Parse.progDesc description_text) *>>> Parse.command "list-identifiers" 

parser_command_convert :: Parse.Mod Parse.CommandFields Command
parser_command_convert = 
	let
		options :: Parse.Parser Command
		options = 
			Base.liftA2 CConvert 
				(Parse.switch option_whether_test_idempotence_of_serialization)
				(Parse.strOption option_input_path) 
		description_text = "Convert document " <> matevar_text__input_path <> " to other format or version."
	in
		Parse.info options (Parse.progDesc description_text)
		*>>> Parse.command "convert" 

parser_command_show_default_doc_props :: Parse.Mod Parse.CommandFields Command
parser_command_show_default_doc_props = 
	Parse.command "default-doc-props" 
		(Parse.info (pure CShowDefaultDocProps) (Parse.progDesc "Shows the default document properties."))


parser_commands :: Parse.Parser Command
parser_commands = Parse.hsubparser 
	(
		Base.mempty
		<> parser_command_translate 
		<> parser_command_list_idus 
		<> parser_command_convert 
		<> parser_command_show_default_doc_props
	)

parse_new :: IO (Command)
parse_new = 
	Parse.execParser <<<*
	(
	Parse.info (Parse.helper <*> parser_commands)
		(
		Parse.fullDesc <> 
		Parse.header "This program generates website from MindMap file."
		)
	)
