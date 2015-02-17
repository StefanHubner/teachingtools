create table course(
	id 			integer primary key,
	code		text,
	groupid		text,
	year		integer,
	name		text
);

create table team(
	id 			integer primary key,
	teamnr		integer,
	courseid 	integer,
	foreign key(courseid) references course(id)
);	

create table student(
	id 			integer primary key,
	name		text,
	courseid	integer,
	teamid 		integer,
	foreign key(courseid) references course(id),
	foreign key(teamid) references team(id)
);	

create table attendance(
	id 			integer primary key,
	week		integer,
	studentid	integer,
	foreign key(studentid) references student(id)
);	

create table assignment(
	id			integer primary key,
	weight		real,	
	maxpoints	real
);

create table question(
	id 				integer primary key,
	weight			real,
	assignmentid	integer,
	foreign key(assignmentid) references assignment(id)
);	

create table result(
	id 				integer primary key,
	studentid		integer,
	assignmentid	integer,
	questionid		integer,
	points			real,
	foreign key(studentid) references student(id),
	foreign key(assignmentid) references assignment(id),
	foreign key(questionid) references question(id)
);	

