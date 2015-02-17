module TeachingTools where 

import Control.Monad
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
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

reportAttendance :: Integer -> IO Int -> IO ()
reportAttendance courseId calendarweek = liftM (map fst . filter snd) (readAttendance courseId) 
		>>= mapM constructQuery 
		>>= putStrLn . formatResult . fromIntegral . length
	where 
		constructQuery sid = calendarweek >>= (\cw -> runDB queryString [toSql cw, toSql sid])
		queryString = "insert into attendance (week, studentid) values (?, ?)"

addStudentToCourse :: Integer -> Integer -> String -> IO()
addStudentToCourse courseid studentid name = runDB queryString [toSql studentid, toSql name, toSql courseid] >>= putStrLn . formatResult
	where queryString = "insert into student (id, name, courseid) values (?, ?, ?)"

dropStudent :: Integer -> IO ()
dropStudent studentid = runDB "delete from student where id = ?" [toSql studentid]  >>= putStrLn . formatResult

newTeamForCourse :: Integer -> Integer -> IO()
newTeamForCourse courseId teamnr = runDB queryString [toSql teamnr, toSql courseId] >>= putStrLn . formatResult 
	where queryString = "insert into team (teamnr, courseid) values (?, ?)"

newAssignment :: Double -> Double -> IO()
newAssignment weight maxpoints = runDB queryString [toSql weight, toSql maxpoints] >>= putStrLn . formatResult 
	where queryString = "insert into assignment (weight, maxpoints) values (?, ?)"

addQuestionsToAssignment :: Integer -> [Double] -> IO ()	
addQuestionsToAssignment assignmentId weights 
		| sum weights == 1.0 = mapM (\w -> runDB queryString [toSql w, toSql assignmentId]) weights >>= format
		| otherwise          = putStrLn "Weights don't add up to 1.0!"
	where 
		queryString = "insert into question (weight, assignmentid) values (?, ?)"
		format = putStrLn . formatResult . fromIntegral . length

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
		queryString = unlines [ "select team.teamnr, student.id, student.name from student, team, course ", 
			"where student.teamid = team.id and course.id = student.courseid and student.courseid = ? ",
			"order by team.id, student.name"]

listAttendanceCountByCourse :: Integer -> IO()
listAttendanceCountByCourse courseId = queryAndPrintList queryString [toSql courseId]
	where queryString = unlines [
		"select week, count(*) from attendance, student ",
		"where student.id = attendance.studentid and student.courseid = ? ",
		"group by week"]

showMyCourses :: IO()
showMyCourses = (\(y,_,_) -> queryAndPrintList queryString [toSql y]) =<< year 
	where 		
		queryString = "select id, name, groupid from course where year = ?"
		year = liftM (toWeekDate . utctDay) getCurrentTime 

listCommands :: IO()
listCommands = readFile "../../util/help.txt" >>= putStrLn

addResult :: Integer -> Integer -> Integer -> Double -> IO ()
addResult studentid assignmentid questionid points = runDB queryString queryPars >>= putStrLn . formatResult
	where 
		queryString = "insert into result (studentid, assignmentid, questionid, points) values (?, ?, ?, ?)"
		queryPars = [toSql studentid, toSql assignmentid, toSql questionid, toSql points]

showTotalResult :: Integer -> IO()
showTotalResult courseId = queryAndPrintList queryString [toSql courseId]
	where queryString = unlines [
		"select student.id, student.name, sum(assignment.weight*(result.points/assignment.maxpoints))*10 ",
		"from result, student, assignment, question ",
		"where student.id = result.studentid and question.assignmentid = assignment.id and result.questionid = question.id ",
		"and student.courseid = ?",
		"group by student.id order by student.name" ]

showAssignmentResult :: Integer -> Integer -> IO ()
showAssignmentResult courseId assignmentid = queryAndPrintList queryString [toSql courseId, toSql assignmentid]
	where queryString = unlines [
		"select student.id, student.name, sum(question.weight*result.points/(assignment.maxpoints*assignment.weight))*10 ", 
		"from result, student, assignment, question ",
		"where student.id = result.studentid and question.assignmentid = assignment.id ",
		"and result.questionid = question.id and result.assignmentid = assignment.id ",
		"and student.courseid = ? and assignment.id = ? ",
		"group by student.id" ]
