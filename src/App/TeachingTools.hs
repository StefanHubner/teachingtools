module TeachingTools (showAttendanceList, addStudentToCourse, dropStudent, newCourse, newTeamForCourse, addMembersToTeam, printTeamsByCourse) where 

import Database.HDBC

import TeachingTools.Utils
import TeachingTools.IOUtils
import TeachingTools.Settings

showAttendanceList :: Integer -> IO () 
showAttendanceList courseId = queryDB queryString queryParameters 
		>>= writeLatexFile . getStandaloneLatexTable format . parseResultAsStringList 
		>>  compileAndOpenLatexFile filebasename 
	where 
		format = "|l|l|p{.6\\textwidth}|"
		filebasename = "studentlist." ++ show courseId 
		writeLatexFile = writeFile $ tmpdir settings ++ filebasename ++ ".tex"
		queryString = "select student.name, student.id from student, course where student.courseid = course.id and course.id = ? order by student.name" 
		queryParameters = [toSql courseId]

newCourse :: String -> String -> Integer -> String -> IO() 		
newCourse code groupid year name = runDB queryString [toSql code, toSql groupid, toSql year, toSql name] >>= putStrLn . formatResult
	where queryString = "insert into course (code, groupid, year, name) values (?, ?, ?, ?)"

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

printTeamsByCourse :: Integer -> IO () 
printTeamsByCourse courseId = queryDB queryString [toSql courseId] >>= mapM return . unlines . map unwords . parseResultAsStringList >>= putStr
	where queryString = unlines [
						"select team.id, student.name from student, team, course ", 
						"where student.teamid = team.id and course.id = student.courseid and student.courseid = ? ",
						"order by team.id, student.name"]

