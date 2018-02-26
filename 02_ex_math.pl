% file	: 02_ex_math				ex : utilities & interp & utilitair
% date	: 1999 et +


:- ensure_loaded('03_ex_math_et_date').	% passer en use_module(xxx
:- ensure_loaded(emione).

% --- les signaux sonores
signal_sonore(Nom):-
	valeur_locale(fonctionnement,beep,on),
	descripteur_local(fonctionnement,signal_sonore,Nom,Melodie),
	descripteur_local(fonctionnement,volume_sonore,Nom,Volume),
	genere_sons(Volume,Melodie).
signal_sonore(_Nom):-beep,!.  % en attendant de remettre en fonction supra

genere_sons(_V,[]):-!.
genere_sons(V,[X|T]):-
	genere_son(V,X),
	genere_sons(V,T),!.

genere_son(V,[X,Y]):-
	atomic_list_concat(['xset b ',V,' ',X,' ',Y],Com),
	name(Com,_Cmd_1),
	% shell(_Cmd_1),		%   18/04/03
	put(user,7),
	flush_output(user), % doit rester !!!!!
	!.

beep :- put(user,7).
% ------------------------------------
exist_file(Directory,File):-
	system:cd(Old_dir),
	system:cd(Directory),
	( system:ls(File)	-> system:cd(Old_dir)
				 ; system:cd(Old_dir) ,fail),	% avant 15/03/03 : false
	!.

current_dir(D):- working_directory(D,D).

% --- coupe les chiffres apres la virgule
positions_apres_virgule(X,In,Out):-
	Out is integer(In*(10**X))/(10**X),!.

% --- arrondi
arrondi(0,_,0.0):-!.
arrondi(0.0,_,0.0):-!.
arrondi(Number,Nb_ch,Res):-
	Nb is max(integer(Nb_ch),truncate(log10(abs(Number))* -1 )),
	Res is (round(Number * 10 ** Nb) / 10  ** Nb) ,
	!.
arrondi(Autre,_,Autre):- !.




% -- retourne le nom_etendu (avec path) du fichier datastream
get_ds_filename(Name,File_name):-
	descripteur_local(fonctionnement,ds_mail,dir,DS_DIR),
	atomic_list_concat([DS_DIR,'/',Name],File_name),
	!.


% mensualise : mensualiser des donnees trimestrielle par interpollation
mensualise([A],[A]):-!.
mensualise([A,B|Tail],[A,A1,A2|Rtail]):-
	Delta is (B-A)/3,
	A_01 is  A+Delta, arrondi(A_01,3,A1),
	A_02 is A_01+Delta, arrondi(A_02,3,A2),
	mensualise([B|Tail],Rtail).

mensualise_dates([A],[A]):-!.
mensualise_dates([A,B|Tail],[A,A1,A2|Rtail]):-
	Delta is (B-A)/3,
	A_01 is  A+Delta, arrondi(A_01,0,A1),
	A_02 is A_01+Delta, arrondi(A_02,0,A2),
	mensualise_dates([B|Tail],Rtail).

mensualise(dates,Dtes,Dtes_out):-
	mensualise_dates(Dtes,Dtes_out),!.
mensualise(_,V_in,V_out):-
	mensualise(V_in,V_out).

% permet de chopper par backt les valeurs successives d'un att multivalue
valeurs(O,A,V):-valeur(O,A,Vs),
	(Vs =[_|_] -> member(V,Vs) ; V=Vs).



% objets(X,Pere) permet d'obtenir les objets terminaux groupés
% (2013 : pas très juste ...!!)
objets(X,Pere):- atom(Pere),!,
	objets_atom(X,Pere).
objets(X,Peres):- is_list(Peres),
	member(Pere,Peres),
	objets_atom(X,Pere).


objets_atom(X,Pere):-
	pere_de(Pere,Y),
	objets_atom(X,Y).
objets_atom(X,X):-objet(X,_),!.


% -------------------------------------------------
% Les conversions diverses Expr <-->  atom
% a simplifier lors passage à SWI ....

convert_x_to_atom(X,At):-var(At),conv_x_atom(X,At),!.
convert_x_to_atom(X,At):-var(X),conv_atom_x(At,X),!.


conv_x_atom(X,At):-convertir_terme_to_atom(X,At).

conv_atom_x(At,X):- convertir_atom_to_terme(At,X),!.

conv_nb_to_atom(X,At):-var(At),conv_nb_atom(X,At),!.
conv_nb_to_atom(X,At):-var(X),conv_atom_nb(At,X),!.


conv_atom_nb(At,Nb):- term_to_atom(Nb,At).

conv_nb_atom(X,At):- term_to_atom(X,At),!.


% Conversion de Terme to Atome :
convertir_terme_to_atom(Terme,Atome):-term_to_atom(Terme,Atome).

convertir_atom_to_terme(At,Terme):-term_to_atom(Terme,At).


liste_qq_to_liste_atom([],[]):-!.
liste_qq_to_liste_atom([Head|Tail],[Head|N_tail]):-
	atom(Head),
	liste_qq_to_liste_atom(Tail,N_tail),!.
liste_qq_to_liste_atom([Head|Tail],[N_head|N_tail]):-
	convertir_terme_to_atom(Head,N_head),
	liste_qq_to_liste_atom(Tail,N_tail),!.



ajoute_val(O,A,V):-
	valeur_ou_vide(O,A,OV),
	desenclaver_liste([V|OV],Flattened),
	set_val(O,A,Flattened).

append_val(O,A,V):-
	valeur_ou_vide(O,A,OV),
	append(OV,[V],NV),
	set_val(O,A,NV).

append_val_if_not_in(O,A,V):-
	valeur_ou_vide(O,A,OV),
	not(memberchk(V,OV)),
	append(OV,[V],NV),
	set_val(O,A,NV).
append_val_if_not_in(_O,_A,_V):-!.

valeur_ou_vide(O,A,V):- (valeur_locale(O,A,V),!);V=[].

% ---
retire_val(O,A,V):-
	valeur(O,A,OV),OV=[_|_],
	delete(OV,V,NV),
	set_val(O,A,NV),!.
retire_val(O,A,_V):-
	write('tentative de retire_val sur '),write([O,A]),
	write(' dont la valeur n''est pas une liste'),nl,fail.



% ---
nombre_de(Nb,Expr):-
	findall(Expr,Expr,Bag),
	length(Bag,Nb),!.
nombre_de(0,_).


nombre_de(Nb,Tel_que,Expr):-
	findall(Tel_que,Expr,Bag),
	length(Bag,Nb),!.
nombre_de(0,_Tel_que,_Expr).


elem_de_x_mb_de_y(X,Y):- intersection(X,Y,R),R \=[].
intersection_non_vide(X,Y):- intersection(X,Y,R),R \=[].



concat_liste_string(List,String):-
	liste_qq_to_liste_atom(List,Liste_finale),
	atomic_list_concat(Liste_finale,String).

concat_liste_string_with_delimiter(List,String):-
	inserer_delimiteur(List,'_',Nliste),
	liste_qq_to_liste_atom(Nliste,Liste_finale),
	atomic_list_concat(Liste_finale,String).
concat_liste_string_with_delimiter(List,String,Delimiter):-
	inserer_delimiteur(List,Delimiter,Nliste),
	liste_qq_to_liste_atom(Nliste,Liste_finale),
	atomic_list_concat(Liste_finale,String).


%  Min et max de liste
% -- version active (modifié : 2013/11/12)
get_max([Head|Tail],Max):- getmax_1([Head|Tail],Head,Max).
getmax_1([],Max,Max):-!.
getmax_1([Head|Tail],M_sofar,Max):-
	(   Head > M_sofar -> M_new=Head ; M_new =M_sofar),
	getmax_1(Tail,M_new,Max).


% ------------------------------
get_max_position([Head|Tail],Max,Pos):- get_max_1_p([Head|Tail],1,1,Head,Max,Pos).

get_max_1_p([],_Count,Pos_Max,Max,Max,Pos_Max):- !.
get_max_1_p([Head|Tail],Count,Pos_max,Max_sofar,Max,Pos):-
	(Head > Max_sofar
	-> (N_Max=Head      , Npos_max = Count) ;
	   (N_Max=Max_sofar , Npos_max = Pos_max)),
	Ncounter is Count+1,
	get_max_1_p(Tail,Ncounter,Npos_max,N_Max,Max,Pos).

% -------------------------------
get_min([Head|Tail],Min):- getmin_1([Head|Tail],Head,Min).
getmin_1([],Min,Min):-!.
getmin_1([Head|Tail],M_sofar,Min):-
	(   Head < M_sofar -> M_new=Head ; M_new =M_sofar),
	getmin_1(Tail,M_new,Min).


% ------------------------------
get_min_position([Head|Tail],Min,Pos):- get_min_1_p([Head|Tail],1,1,Head,Min,Pos).

get_min_1_p([],_Count,Pos_Min,Min,Min,Pos_Min):- !.
get_min_1_p([Head|Tail],Count,Pos_min,Min_sofar,Min,Pos):-
	(Head < Min_sofar
	-> (N_Min=Head      , Npos_min = Count) ;
	   (N_Min=Min_sofar , Npos_min = Pos_min)),
	Ncounter is Count+1,
	get_min_1_p(Tail,Ncounter,Npos_min,N_Min,Min,Pos).

% ------------------------
get_amplitude(List,Ampl):-
	get_min(List,Min),
	get_max(List,Max),
	Ampl is Max-Min.


% get_min_max_ampl
get_min_max_ampl([Head|Tail],Min,Max,Ampl):-
	get_min_max_1([Head|Tail],Head,Min,Head,Max),
	Ampl is Max - Min.

% cette version est plus rapide
get_min_max_1([Head|Tail],In_min,Min,In_max,Max):-
	In_min =< Head, Head =< In_max,
	get_min_max_1(Tail,In_min,Min,In_max,Max),!.
get_min_max_1([Head|Tail],In_min,Min,In_max,Max):-
	(Head > In_max -> (Out_max=Head,Out_min=In_min)  ; (Out_max=In_max,Out_min=Head)  ) ,
	get_min_max_1(Tail,Out_min,Min,Out_max,Max),!.

get_min_max_1([],Min,Min,Max,Max):-!.

/* -- version moins rapide
get_minmax_ampl([Head|Tail],Min,Max,Ampl):-
	get_minmax_1([Head|Tail],Head,Min,Head,Max),
	Ampl is Max - Min.

get_minmax_1([Head|Tail],In_min,Min,In_max,Max):-
	(   Head > In_max -> Out_max=Head ;Out_max = In_max),
	(   Head < In_min -> Out_min=Head ;Out_min = In_min  ) ,
	get_minmax_1(Tail,Out_min,Min,Out_max,Max).

get_minmax_1([],Min,Min,Max,Max):-!.
--------------------------------------*/



% ---- OPERATIONS VECTORIELLES ---------
% le resultat est de la longueur du vecteur le plus court
%
% op_vecto(pch({rsi_eur,serie},50),R) .
%
% pour formule = ....	5/05/06
op_vecto(pch(Expr,Lg),R):- op_vecto(Expr,Res), percentage_change(Res,Lg,R),!.
op_vecto(mm(Expr,Lg),R):- op_vecto(Expr,Res),!, mav(Res,Lg,R),!.    % ex mm(...) A supp (cf mav)
op_vecto(mav(Expr,Lg),R):- op_vecto(Expr,Res),!, mav(Res,Lg,R),!.
op_vecto(pente(Expr,Lg),R):- op_vecto(Expr,Res), pentes(Res,Lg,R),!.
op_vecto(rsi(Expr,Lg),R):- op_vecto(Expr,Res),calc_rsi(Res,Lg,R),!.

op_vecto(A + B ,R):- op_vecto(A,R_a),op_vecto(B,R_b),map_op(+,R_a,R_b,R),!.
op_vecto(A * B ,R):- op_vecto(A,R_a),op_vecto(B,R_b),map_op(*,R_a,R_b,R),!.
op_vecto(A / B ,R):- op_vecto(A,R_a),op_vecto(B,R_b),map_op(/,R_a,R_b,R),!.
op_vecto(A - B ,R):- op_vecto(A,R_a),op_vecto(B,R_b),map_op(-,R_a,R_b,R),!.

op_vecto({Obj,Att},Val):- valeur(Obj,Att,Val) ,!.
op_vecto(A  ,A):-!.

map_op(+,A,B,C):-number(B),!,map_vect_number_plus(A,B,C).
map_op(+,A,B,C):-number(A),!,map_vect_number_plus(B,A,C).
map_op(+,A,B,C):-map_plus(A,B,C).
map_op(-,A,B,C):-number(B),!,map_vect_number_moins(A,B,C).
map_op(-,A,B,C):-number(A),!,map_number_vect_moins(A,B,C).
map_op(-,A,B,C):-map_moins(A,B,C).
map_op(*,A,B,C):-number(B),!,map_vect_number_multiplie(A,B,C).
map_op(*,A,B,C):-number(A),!,map_vect_number_multiplie(B,A,C).
map_op(*,A,B,C):-map_multiplie(A,B,C).
map_op(/,A,B,C):-number(A),!,map_number_vect_divise(A,B,C).
map_op(/,A,B,C):-number(B),!,map_vect_number_divise(A,B,C).
map_op(/,A,B,C):-map_divise(A,B,C).
map_op(log,A,Res):-map_log(A,Res).       % ne peut pas  fonctionner ?

map_log([],[]):-!.
map_log([A|R_a],[C|R_c]):-
	C is log(A),
	map_log(R_a,R_c).


map_plus([],_,[]):-!.
map_plus(_,[],[]):-!.
map_plus([A|R_a],[B|R_b],[C|R_c]):-
	% C is A+B,
	C is (round((A+B)*10**8)/10**8),  % rounding to 8 positions
	map_plus(R_a,R_b,R_c).

map_moins([],_,[]):-!.
map_moins(_,[],[]):-!.
map_moins([A|R_a],[B|R_b],[C|R_c]):-
	% C is A-B,
	C is (round((A-B)*10**8)/10**8),  % rounding to 8 positions
	map_moins(R_a,R_b,R_c).

map_multiplie([],_,[]):-!.
map_multiplie(_,[],[]):-!.
map_multiplie([A|R_a],[B|R_b],[C|R_c]):-
	% C is A*B,
	C is (round((A*B)*10**8)/10**8),  % rounding to 8 positions
	map_multiplie(R_a,R_b,R_c).

map_divise([],_,[]):-!.
map_divise(_,[],[]):-!.
map_divise([A|R_a],[B|R_b],[C|R_c]):-
	% C is A/B,
	C is (round((A/B)*10**8)/10**8),  % rounding to 8 positions
	map_divise(R_a,R_b,R_c).

map_vect_number_plus([],_,[]):-!.
map_vect_number_plus([A|R_a],B,[C|R_c]):-
	% C is A+B,
	C is (round((A+B)*10**8)/10**8),  % rounding to 8 positions
	map_vect_number_plus(R_a,B,R_c),!.

map_vect_number_moins([],_,[]):-!.
map_vect_number_moins([A|R_a],B,[C|R_c]):-
	C is A-B,
	map_vect_number_moins(R_a,B,R_c).

map_number_vect_moins([],_,[]):-!.
map_number_vect_moins(A,[B|R_b],[C|R_c]):-
	C is A-B,
	map_number_vect_moins(A,R_b,R_c).

map_vect_number_multiplie([],_,[]):-!.
map_vect_number_multiplie([A|R_a],B,[C|R_c]):-
	% C is A*B,
	C is (round((A*B)*10**8)/10**8),  % rounding to 8 positions
	map_vect_number_multiplie(R_a,B,R_c),!.

map_number_vect_divise(_A,[],[]):-!.
map_number_vect_divise(A,[B|R_b],[C|R_c]):-
	% C is A/B,
	C is (round((A/B)*10**8)/10**8),  % rounding to 8 positions
	map_number_vect_divise(A,R_b,R_c),!.

map_vect_number_divise([],_B,[]):-!.
map_vect_number_divise([A|R_a],B,[C|R_c]):-
	% C is A/B,
	C is (round((A/B)*10**8)/10**8),  % rounding to 8 positions
	map_vect_number_divise(R_a,B,R_c),!.


% -- Variantes ou le resultat est de la longeur de la premiere serie
%    et la deuxieme serie est reprise cycliquement
map_op_rotatif(*,Serie,Serie_2,Res):-
	map_op_rot_multiplie(Serie,Serie_2,Serie_2,Res),!.


map_op_rot_multiplie([],_Serie_2,_Memoire,[]):-!.
map_op_rot_multiplie([A|R_a],[],[Mem_1|Mem_r],[Res|Res_tail]):-!,
	Res is A*Mem_1,
	map_op_rot_multiplie(R_a,Mem_r,[Mem_1|Mem_r],Res_tail),!.
map_op_rot_multiplie([A|R_a],[B|R_b],Memoire,[Res|Res_tail]):-!,
	Res is A*B,
	map_op_rot_multiplie(R_a,R_b,Memoire,Res_tail),!.


% multlist(Liste_nbs,Res_multiplication_des_éléments)
%            multlist([1,2,3], 6).
multlist(Liste_nbs,Res):-
	multlist(Liste_nbs,1,Res).

multlist([],R,R).
multlist([H|T],In,R):-
	Out is H*In,
	multlist(T,Out,R).



% -------------
%  sum_up([[a:2,b:3],[a:1],[b:2,c:3]],R).   ==> R = [c:3, a:3, b:5]
%  ? par qui est-ce utilisé ?
sum_up([],[]):-!.
sum_up([A],A):-!.
sum_up([A,B|Tail],Res):-
	sommation1(A,B,R1),
	sommation2(B,R1,R2),
	sum_up([R2|Tail],Res).


% ------------
sommation1([],_,[]):-!.
sommation1([X:Qt|Tail],B,[X:R_Qt|R_tail]):-
	member(X:Qt2,B),
	R_Qt is Qt+Qt2,
	sommation1(Tail,B,R_tail),!.
sommation1([X:Qt|Tail],B,[X:Qt|R_tail]):-
	sommation1(Tail,B,R_tail).

sommation2([],R_sofar,R_sofar):-!.
sommation2([X:_|Tail],R_sofar,R_tail):-
	member(X:_,R_sofar),
	sommation2(Tail,R_sofar,R_tail),!.
sommation2([X:Qt|Tail],R_sofar,R_tail):-
	sommation2(Tail,[X:Qt|R_sofar],R_tail).


sumlist_avec_coef(Liste,from(Coef_seed,Inc),R):- number(Coef_seed),
	sumlist_coef(Liste,0,Coef_seed,Inc,R).
sumlist_avec_coef(Liste,Coef_seed,R):- number(Coef_seed),
	sumlist_coef(Liste,0,Coef_seed,Coef_seed,R).

sumlist_coef([],R,_,_,R):-!.
sumlist_coef([A|Tail],R_so_far,Coef,Coef_inc,R):-
	N_R_so_far is (A*Coef)+R_so_far,
	N_Coef is Coef+Coef_inc,
	sumlist_coef(Tail,N_R_so_far,N_Coef,Coef_inc,R).

% ---
perf(X):-time(X),nl,!.



/* UTILITAIRES POUR LE DEVELOPPEMENT */



% date_qp : donne la date system sous format JJ/MM/AAAA
date_qp(D):-get_time(Time),convert_time(Time,Y,M,J,_,_,_,_) ,D = J/M/Y .


% printlist( + Liste)
printlist([]):-!.
printlist([A,B,C|T]):- write(A),write(B),write(C),printlist(T),!. % unrolled for performance
printlist([H|T]):- write(H),printlist(T),!.

% printlist_with_spaces( + Liste)	unrolled for performance
printlist_with_spaces([])	:- write(' '),!.
printlist_with_spaces([A,B,C|T]):- write(A),write(' '),
				   write(B),write(' '),
				   write(C),write(' '),
				   printlist_with_spaces(T),!.
printlist_with_spaces([H|T])	:- write(H),write(' '),printlist_with_spaces(T).

% printlist_with( Separator,+ Liste)
printlist_with(_Separateur,[A])	:- write(A),!.
printlist_with(Separateur,[H|T]):- write(H),Separateur,printlist_with(Separateur,T).


% objet_racine(X) : unifie X avec les noms des objet racine du systeme i.e. fils de []
objet_racine(X):-objet_qq(X),\+ pere_de(_Y,X).

objets_racine:-objet_qq(X),\+ pere_de(_Y,X),
		write(X),nl,fail.



% -------------------------------------------
% file : interp
% but  : un top-level interpreteur PROLOG

emicat_prolog :-
     write('*******************************************************'),
     nl,
     write('* PROLOG-EMICAT        ( type : exit.  to exit )      *'),
     nl,
     write('*******************************************************'),
     nl,nl,
     interprete_prolog,
     nl,
     write('** RETOUR APPLICATION  **'),
     nl.

interprete_prolog :-  % version bktkante pour eviter d'empiler dans le global_stack
     repeat,
     write('> ?- '),
     read(Y),
     (Y = exit -> true
		;
		(call(Y) -> nl,write('yes'),nl
		; nl,write('no'),nl
		),fail		% pour provoquer bktking
     ),
     !.

user_callable(X):-
	write(user_callable-X),nl,!.

% ----------------------------------------------------------------
% ex-file : utilitaire
% compresse :   suppression de donnees qui peuvent etre rededuites
%               ou recalculees. Permet d'economiser la place de la sauvegarde ..
compresse_symphony:-
        objet(X,s_indic),          % pour Symphony
        write('Compression '-X),
        once(reset_deductions(X)),
        % once(enlever_correlations(X)),
        write('		Done'),nl,
        fail.
compresse_symphony:-
	console(['Fin de la suprression de donnees re-deductibles Symphony']).

compresse_anie:-
	objet(X,indicateur),       % pour ANIE
        console(['Compression :',X]),
        once(reset_deductions(X)),
        once(reset_calculated_by(X)),
        write('		Done'),nl,
        fail.

compresse_anie:-
	console(['Fin de la suprression de donnees re-deductibles ANIE']).
