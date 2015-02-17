module TeachingTools.IOUtils where 

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC.MySQL
import Database.HDBC
import System.Process
import System.IO
import Control.Monad
import Data.Time
import Data.Time.Calendar.WeekDate

import TeachingTools.Settings
import TeachingTools.Utils

queryDB :: String -> [SqlValue] -> IO [[(String, SqlValue)]]
queryDB queryString queryPars = withRTSSignalsBlocked $ do
	--conn <- connectSqlite3 (dbfile settings)
	conn <- connectMySQL (defaultMySQLConnectInfo {mysqlHost = mysqlhost settings, mysqlUser = mysqluser settings, mysqlDatabase = mysqldb settings, mysqlPassword = mysqlpasswd settings})
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

compileLatexFile :: String -> IO String 
compileLatexFile filebasename = let pdffilename = tmpdir settings ++ filebasename ++ ".pdf" in do 
	devnull <- if redirect settings then openFile "/dev/null" AppendMode else return stdout
	process <- runProcess (pdflatex settings) [filebasename] (Just (tmpdir settings)) Nothing Nothing (Just devnull) Nothing
	_ <- waitForProcess process
	return pdffilename

processPdfFile :: String -> String -> IO () 
processPdfFile command file = void $ runCommand (command ++ " " ++ file) 

getStudentList :: Integer -> IO [[String]] 
getStudentList courseId = liftM parseResultAsStringList (queryDB queryString queryParameters)
	where 
		queryString = "select student.name, student.id from student, course where student.courseid = course.id and course.id = ? order by student.name" 
		queryParameters = [toSql courseId]

readAttendance :: Integer -> IO [(Integer, Bool)] 
readAttendance courseId = liftM (map (take 2)) (getStudentList courseId) >>= mapM printAndRead
	where 
		printAndRead x = mapM_ putStrLn [" ", head x] >> liftM (parse ((head . tail) x)) getChar
		parse sid c = (read sid, c `elem` "y1")

createAttendanceList :: Integer -> IO String 
createAttendanceList courseId = getStudentList courseId 
		>>= writeLatexFile (tmpdir settings) filebasename . getStandaloneLatexTable format True
		>>  compileLatexFile filebasename 
	where 
		format = "|l|l|p{.6\\textwidth}|"
		filebasename = "studentlist." ++ show courseId 

writeLatexFile :: String -> String -> String -> IO ()	
writeLatexFile dir filebasename = writeFile $ dir ++ filebasename ++ ".tex" 

queryAndPrintList :: String -> [SqlValue] -> IO()
queryAndPrintList s p = queryDB s p >>= mapM return . unlines . map unwords . parseResultAsStringList >>= putStr

thisWeek :: IO Int
thisWeek = offsetWeek 0

lastWeek :: IO Int
lastWeek = offsetWeek (-1) 

offsetWeek :: Integer -> IO Int
offsetWeek offset = liftM (toWeekDate . addDays (offset*7). utctDay) getCurrentTime >>= (\(_,cw,_) -> return cw)

