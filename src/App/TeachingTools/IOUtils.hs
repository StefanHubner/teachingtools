module TeachingTools.IOUtils where 

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import System.Process
import System.IO

import TeachingTools.Settings

queryDB :: String -> [SqlValue] -> IO [[(String, SqlValue)]]
queryDB queryString queryPars = do
	conn <- connectSqlite3 (dbfile settings)
	stmt <- prepare conn queryString 
	_ <- execute stmt queryPars
	results <- fetchAllRowsAL' stmt
	disconnect conn
	return results

runDB :: String -> [SqlValue] -> IO Integer 
runDB queryString queryPars = do 
	conn <- connectSqlite3 (dbfile settings)
	rc <- run conn queryString queryPars
	commit conn
	disconnect conn
	return rc

compileAndOpenLatexFile :: String -> IO ()
compileAndOpenLatexFile filebasename = do
	devnull <- if redirect settings then openFile "/dev/null" AppendMode else return stdout
	process <- runProcess (pdflatex settings) [filebasename] (Just (tmpdir settings)) Nothing Nothing (Just devnull) Nothing
	_ <- waitForProcess process
	_ <- runCommand $ pdfreader settings ++ " " ++ tmpdir settings ++ filebasename ++ ".pdf"
	return ()

