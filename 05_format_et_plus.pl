% file	: 05_format  & autres ...
% but	: les formats spéciaux

:- ensure_loaded('03_ex_math_et_date').

% definition de formats pour afficher les dates
% Nb : le premier arg est un num placé entre ~ et la lettre du format

% affiche Année à partir de Nb_jours
:- format_predicate(z,affiche_annee(_Arg,_Nbj)).
% affiche_annee(default,Nbj):- number(Nbj),
%	nb_jour(_J,_M,A,Nbj),
%	write(A),!.
affiche_annee(_X,Nbj):- number(Nbj),
	nb_jour(_J,_M,A,Nbj),
	write(A),!.
affiche_annee(_X,A):-
	write(A),!.

% affiche J/M/AA  à partir de Nb_jours
:- format_predicate(x,affiche_jma(_Arg,_Nbj)).
% affiche_jma(default,Nbj):- number(Nbj),
%	nb_jour(J,M,A,Nbj),
%	write(J/M/A),!.
affiche_jma(_X,Nbj):- number(Nbj),
	nb_jour(J,M,A,Nbj),
	write(J/M/A),!.
affiche_jma(_X,In):-
	write(In),!.


% format f acceptant 'n_a' etc
:- format_predicate('F',f_format(_Arg,_NUm_ou_autre)).
f_format(default,Num):- number(Num),  % sans indication de longueur
	format('~f',Num),!.
f_format(default,Non_Num):-
	write(Non_Num),!.
f_format(X,Num):- number(Num),   % avec longueur de X
	string_concat('~',X,F1),
	string_concat(F1,f,Format),
	format(Format,Num),!.
f_format(_X,In):-
	write(In),!.
% usage général : format('pour arrondir utiliser ~2f',123.45678).


% format S pour INSERT Odbc / ajoute de  \'~w\'  autour de ~w
:- format_predicate('S',odbc_format(_Arg,_Nbj)).
odbc_format(_X,String_ou_Atom):- 
	format('\'~w\'',[String_ou_Atom]),!.





% ----------  FIN
