% file :	31_r_edit_symph_DCG_xl.pl
% date :	mars 07
% DCG pour variable_xl

% variable_xl(+Var,-Variable,-Bindings,-Lettres,-Nume):-
variable_xl(Var,Variable,Bindings,Lettres,Nume):-
	atomic(Var),
	atom_chars(Var,Liste_chars),
	phrase(xl_litteral(Var_atom,Lettres,Nume),Liste_chars,[]),
	atom_to_term(Var_atom,Variable,Bindings),
	!.


%-- DCG pour variable xl de type   aj12 ==> AJ12 , AJ ,
xl_litteral(Var_atome,Lettres,Nume) -->			% noms onglets plus complexes ie page(1)
	alpha_num_underscore(F_cars),['!'],xl_variable(V_cars,Nume),
	{atomic_list_concat([F_cars,'_',V_cars],Lettres),
	 atomic_list_concat([F_cars,'_',V_cars,Nume],Var_atome)},!.
xl_litteral(Var_atome,V_cars,Nume) -->
	xl_variable(V_cars,Nume),
	{ atomic_list_concat([V_cars,Nume],Var_atome)},!.

xl_variable(Lettres,Nume) -->
	alpha(Lettres),xl_entier(Nume),!.
xl_variable(Cste_Lettres,Nume) -->               % Cste
	alpha(Lettres),['$'],xl_entier(Nume),
	{atomic_list_concat(['Cste_',Lettres],Cste_Lettres)},!.
xl_variable(Cste_Lettres,Nume) -->               % Cste
	['$'],alpha(Lettres),['$'],xl_entier(Nume),
	{atomic_list_concat(['Cste_',Lettres],Cste_Lettres)},!.
xl_variable(Lettres,Nume) -->
	['$'],alpha(Lettres),xl_entier(Nume),!.



% -----------------------
xl_entier(I) -->
	signe(S),
	digit(D0),
	digits(D),
	{ number_chars(I, [S,D0|D]) },!.

xl_entier(I) -->
	digit(D0),
	digits(D),
	{ number_chars(I, [D0|D]) }.

/* -- Digit(s) defined in : dcg_basics
digits([D|T]) -->
        digit(D), !,
        digits(T).
digits([]) --> [].

digit(D) -->
        [D],
        { code_type(D, digit) }.
--- */

signe(S) --> [S],{memberchk(S,[-,+])}.


% -----------------------
alpha(LETTRES) -->
        lettre(D0),
        lettres(D),
        { atom_chars(Lettres, [D0|D]) ,upcase_atom(Lettres,LETTRES)}.

/*  --- lettre(s) défini dans : proc_controle
lettres([D|T]) -->
        lettre(D), !,
        lettres(T).
lettres([]) --> [].

lettre(D) -->
        [D],
        { code_type(D, alpha) }.
--- */


% ------------------------
alpha_num_underscore(Lettres_chiffres_underscore) -->
        alphanum_underscore(D0),
         alphanum_underscores(D),
        { atom_chars(Csym, [D0|D]) ,upcase_atom(Csym,Lettres_chiffres_underscore)}.

 alphanum_underscores([D|T]) -->
         alphanum_underscore(D), !,
         alphanum_underscores(T).
 alphanum_underscores([]) --> [].

 alphanum_underscore(D) -->
        [D],
        { code_type(D, csym) }.


