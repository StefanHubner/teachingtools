module TeachingTools.Settings where
import System.IO.Unsafe

data Settings = Settings { 
	dbfile :: String, 
	tmpdir :: String, 
	pdflatex :: String, 
	pdfreader :: String, 
	printapp :: String, 
	mysqluser :: String,
	mysqlpasswd :: String,
	mysqldb :: String,
	mysqlhost :: String,
	redirect :: Bool } deriving (Show, Read)

settings :: Settings
settings = unsafePerformIO $ readFile "TeachingToolsService.cnf" >>= \s -> return (read s :: Settings)
