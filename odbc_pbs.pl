% file :	db_store.pl
% but :		stocker dans des bases Mysql/Access via ODBC
% date :	2016 juin


% tests sur pb odbc_query  INSERT
/*  suppose you have in Mysql or Access via ODBC a table :
Table: test_table
Columns:	PersonID int(11)
			Name varchar(45)
			Phone int(11)

*/

open_db:- odbc_connect('xyz_sec_db',_Connection,[alias(db)]).

create_table:-
	odbc_query(db,'CREATE TABLE Test_table (PersonID int PRIMARY KEY, Name CHAR(25),Phone INTEGER)',Res).

delete_table :- odbc_query(db,'DROP TABLE Test_tab',Res).

query_db(Row):- odbc_query(db,'SELECT * from test_table',Row).

insert_1(Res):- odbc_query(db,
		'INSERT INTO Test_table (PersonID,Name,Phone) VALUES (1, "jab", 123456)',Res).

insert_2(Res):-	odbc_query(db,
		'INSERT INTO Test_table (PersonID,Name,Phone) VALUES (~w,"jab2", ~w)'
				- [2,6457],Res).
insert_3(Res):- A=3, B="Patricia",C=12156,
		odbc_query(db,
				'INSERT INTO Test_table (PersonID,Name,Phone) VALUES (~w, ~w, ~w)'
				- [A,B,C],Res).

insert_4(A,B,C,Res):- odbc_query(db,
			'INSERT INTO Test_table (PersonID,Name,Phone) VALUES (~w, ~w, ~w)'
			- [A,B,C],Res).



insert_6(A,B,C,Res):-  % converting all atomic entries to quoted as '"Valeur"' does the job but is a bit clumsy
		atom_string(A,AS),term_to_atom(AS,AA),  % at query the integer type is preserved ...
		atom_string(B,S),term_to_atom(S,BB),
		atom_string(C,CS),term_to_atom(CS,CC),
		odbc_query(db,'INSERT INTO Test_table (PersonID,Name,Phone) VALUES (~w, ~w, ~w)'
				- [AA,BB,CC],Res).

insert_8(A,B,C,Res):- odbc_query(db,
			'INSERT INTO Test_table (PersonID,Name,Phone) VALUES (\'~w\', \'~w\', \'~w\')'
			- [A,B,C],Res).
