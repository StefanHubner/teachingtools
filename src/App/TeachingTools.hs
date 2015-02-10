module TeachingTools where 

import Control.Monad
import Database.HDBC

import TeachingTools.Utils
import TeachingTools.IOUtils
import TeachingTools.Settings


showAttendanceList :: Integer -> IO ()
showAttendanceList courseId = createAttendanceList courseId >>= processPdfFile (pdfreader settings)

printAttendanceList :: Integer -> IO ()
printAttendanceList courseId = createAttendanceList courseId >>= processPdfFile (printapp settings)

newCourse :: String -> String -> Integer -> String -> IO() 		
newCourse code groupid year name = runDB queryString [toSql code, toSql groupid, toSql year, toSql name] >>= putStrLn . formatResult
	where queryString = "insert into course (code, groupid, year, name) values (?, ?, ?, ?)"

reportAttendance :: Integer -> Integer -> IO()
reportAttendance courseId calendarweek = liftM (map fst . filter snd) (readAttendance courseId) 
		>>= mapM (\sid -> runDB queryString [toSql calendarweek, toSql sid]) 
		>>= putStrLn . formatResult . fromIntegral . length
	where 
		queryString = "insert into attendance (week, studentid) values (?, ?)"

addStudentToCourse :: Integer -> Integer -> String -> IO()
addStudentToCourse courseid studentid name = runDB queryString [toSql studentid, toSql name, toSql courseid] >>= putStrLn . formatResult
	where queryString = "insert into student (id, name, courseid) values (?, ?, ?)"

dropStudent :: Integer -> IO ()
dropStudent studentid = runDB "delete from student where id = ?" [toSql studentid]  >>= putStrLn . formatResult

newTeamForCourse :: Integer -> Integer -> IO()
newTeamForCourse courseId teamnr = runDB "insert into team (teamnr, courseid) values (?, ?)" [toSql teamnr, toSql courseId] >>= putStrLn . formatResult 

addMembersToTeam :: Integer -> Integer -> [Integer] -> IO ()
addMembersToTeam courseId teamnr students = mapM (addMember courseId teamnr) students >>= putStrLn . formatResult . fromIntegral . length
	where 
		addMember c t s = runDB queryString  [toSql t, toSql c, toSql s] 		
		queryString = "update student set teamid = (select id from team where teamnr = ? and courseid = ?) where id = ?"

showTeamListByCourse :: Integer -> IO () 
showTeamListByCourse courseId = queryDB queryString [toSql courseId] 
		>>= writeLatexFile (tmpdir settings) filebasename . getStandaloneLatexTable "|l|l|l|" False . parseResultAsStringList
		>> compileLatexFile filebasename >>= processPdfFile (pdfreader settings)
	where 
		filebasename = "teamlist." ++ show courseId
		queryString = unlines [ "select team.id, student.id, student.name from student, team, course ", 
			"where student.teamid = team.id and course.id = student.courseid and student.courseid = ? ",
			"order by team.id, student.name"]

printAttendanceCountByCourse :: Integer -> IO()
printAttendanceCountByCourse courseId = queryAndPrintList queryString [toSql courseId]
	where queryString = unlines [
		"select week, count(*) from attendance, student ",
		"where student.id = attendance.studentid and student.courseid = ? ",
		"group by week"]

