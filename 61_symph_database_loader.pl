% file	: 61-symph_database_loader.pl
% AUTHOR :
% VERSION : 0.93 for SWI-PL
% UPDATE : 12/04/2006
% PURPOSE: Chargement de données via liens ODBC
%
% Outil de chargement de données (relativement) indépendant de l'application exploitant les données
% Principe : on charge les données soit dans emione soit dans des prédicats prolog
% Interface XPCE pour guider l'utilisateur à creer

:- ensure_loaded(emione).		% passer en use_module( emione)
:- ensure_loaded('00_ex_util').	% passer en use_module(
:- ensure_loaded('02_ex_math').	% passer en use_module(
:- ensure_loaded('03_ex_math_et_date').	% passer en use_module(

:- set_prolog_flag(float_format,'%.3f').

%-----------------------------------------------------
% Liste des bases accessibles
% !! à compléter car en cas d'échec une base reste ouverte ==> pb
data_sources(Liste):-
	findall(Source-Desc-Tables,(odbc_data_source(Source,Desc),data_source(Source,Desc,Tables)),Liste).

data_source(Source,Desc,Tables):-
	odbc_data_source(Source,Desc),
	% si ERROR ms ==> fail
	catch(odbc_connect(Source,Connection,[]) ,error(odbc(_State,_Native,_Message), _),fail),
	liste_des_tables(Connection,Tables),
	odbc_disconnect(Connection),
	!.


% ----------------------------
% connection to database
connecte :- odbc_connect(dynamique,_,[alias(db)]).	% Base Access servant d'accès aux données
connecte(Base):- odbc_connect(Base,_,[alias(db)]).


% ----------------------------
liste_des_tables(Base,Liste):-
	bagof(Table,A^Base^(odbc_current_table(Base,Table,type(A)),not(A='SYSTEM TABLE')),Liste).

tables_de(Base,Table):-odbc_current_table(Base,Table,type(A)),not(A='SYSTEM TABLE').

% ----------------------------
liste_des_attributs(Base,Table,L_Atts):-
	bagof(Att,Att^Base^(odbc_table_column(Base,Table,Att)),L_Atts).

attribut_de(Base,Table,Att):- odbc_table_column(Base,Table,Att).


% ----------------------------------
load_table_id_dyn(Table,Id_dyn,Frequence_maj,D_Update):-
	liste_des_attributs(db,Table,Attributs),
	length(Attributs,Nb_att),
	load_data(Nb_att,Table,Valeurs),
	stocker_valeurs(Id_dyn,Attributs,Valeurs,Frequence_maj,D_Update),
	!.


load_table(Base,Table):-
	connecte(Base),
	load_table(Table),
	odbc_disconnect(db).

load_table(Table):-
	liste_des_attributs(db,Table,Attributs),
	length(Attributs,Nb_att),
	load_data(Nb_att,Table,Valeurs),
	stocker_valeurs(Table,Attributs,Valeurs),
	!.


load_data(2,Table,[Ds,Vals]):-
	odbc_query(db, 'select * FROM ~w Order by 1'-[Table],  % order by
		Champs,	[findall((Date,Serie),row(Date,Serie)) ]),
	check_and_split_2(Table,Champs,0,Ds,Vals),
	!.

load_data(3,Table,[Ds,Vals,Vals_2]):-
	odbc_query(db, 'select * FROM ~w Order by 1'-[Table],
		Champs,	[findall((Date,Serie,S2),row(Date,Serie,S2)) ]),
	check_and_split_3(Table,Champs,0,0,Ds,Vals,Vals_2),
	!.

% ------------------------------------------
% SI LA DERNIERE VALEUR est '$null$' ON A UN PETIT Pb ....!! (modifié
check_and_split_2(_Obj,[],_,[],[]):-!.
check_and_split_2(_Obj,[(timestamp(A,_M,_J,_,_,_,_),_V)|_Tail],_,[],[]):-A =<1900,!.
check_and_split_2(_Obj,[(timestamp(_A,_M,_J,_,_,_,_),'$null$')],_,[],[]):-!.
	% dernière valeur est '$null$'
check_and_split_2(_Obj,[(_,'$null$'),(_,'$null$')],_V_prec,[],[]):-!.
	% 2 dernières valeur sont '$null$'
check_and_split_2(_Obj,[(_,'$null$'),(_,'$null$'),(_,'$null$')],_V_prec,[],[]):-!.
	% 3 dernières valeur sont '$null$'  % Pas terrible comme implémentation 2-3 vides ? 4 5
check_and_split_2(_Obj,[('$null$',_V)|_Tail],_,[],[]):- !.
check_and_split_2(Obj,[(timestamp(A,M,J,_,_,_,_),V)|Tail],V_prec,[Nb_jour|Nb_j_tail],[Val|V_tail]):-
	A> 1900 ,
	nb_jour(J,M,A,Nb_jour),
	(V = '$null$' -> Val=V_prec;Val=V),
	check_and_split_2(Obj,Tail,Val,Nb_j_tail,V_tail).

check_and_split_3(_Obj,[],_,_,[],[],[]):-!.
check_and_split_3(_Obj,[(timestamp(A,_M,_J,_,_,_,_),_V,_S)|_Tail],_,_,[],[],[]):-A =<1900,!.
 % check_and_split_3(Obj,[(timestamp(_A,_M,_J,_,_,_,_),V,S)],_,_,[],[],[]):-(V ='$null$' ; S = '$null$'),write('Dernier est nul : '-Obj),nl,!.
check_and_split_3(_Obj,[('$null$',_V,_S)|_Tail],_,_,[],[],[]):- !.
check_and_split_3(Obj,[(timestamp(A,M,J,_,_,_,_),V,S)|Tail],V_prec,S_prec,[Nb_jour|Nb_j_tail],[Val|V_tail],[Ser|S_tail]):-
	A> 1900 ,
	nb_jour(J,M,A,Nb_jour),
	(V = '$null$' -> Val=V_prec;Val=V),
	(S = '$null$' -> Ser=S_prec; Ser=S),
	check_and_split_3(Obj,Tail,Val,Ser,Nb_j_tail,V_tail,S_tail).

% -----------------------------------------
%  stocker les données sur les indicateurs ayant  (Obj,id_dyn,Indic)
stocker_valeurs(Indic,Attributs,Valeurs,_Frequence_maj,D_Update):-
	forall( valeur_locale(Obj,id_dyn,Indic),
		(   (valeur_locale(Obj,periodicite_maj,Per) -> true ;
	                determine_periodicite_maj(Obj,Attributs,Valeurs,Per)),
		   stocker_val(Per,Obj,Attributs,Valeurs,D_Update)
	 %    ,console(['       Objet :',Obj,'  Periodicite_maj',Per])
		% ajouté suite à set_start_dataset -> purger Indic,last_loaded
		  ,reset_val(Obj,last_loaded(Attributs))
		)
	      ),
	!.
stocker_valeurs(Indic,Attributs,Valeurs,_Frequence_maj,D_Update):-
	(objet_qq(Indic) -> true ; creer_obj(Indic,[s_serie_raw]) ),
	(valeur_locale(Indic,periodicite_maj,Per)
	     -> true ;
	     determine_periodicite_maj(Indic,Attributs,Valeurs,Per)),
	stocker_val(Per,Indic,Attributs,Valeurs,D_Update).

stocker_valeurs(Table,Attributs,Valeurs):-
	console([stocker_valeurs,Table,Attributs,Valeurs,nl,
		 'à finaliser si nécessaire']).


% ajouter ici le stockage de la date_dstr sur Indic
stocker_val(Per,Indic,Attributs,Valeurs,D_Update):-
	set_val(Indic,date_dataset,D_Update),
	% récuperer les valeurs avant
	stocker_val(Per,Indic,Attributs,Valeurs),
	% comparer valeurs Avant vs Valeurs_stockées
	% et mettre à jour att: date_events selon modèle ci-dessous
	!.

/*-----

initial |	date_dataset=16050	| dates	[16040, 16010, ...]
				| series	[ 1.60 , 1.65 , ...]

	date_dataset=16055	| dates	[16040, 16010, ...]
				| series	[ 1.60 , 1.68 , ...]
	===>  date_events:  16055-revised(16010,1.65,1.68)

	date_dataset=16060	| dates	[16040, 16010, ...]
				| series	[ 1.60 , 1.68 , ...]
	===>  date_events:  16060-unchanged
		[16060-unchaged,16055-revised(16010,1.65,1.68), ...

	date_dataset=16080	| dates	[16070,16040, 16010, ...]
				| series	[ 1.65,  1.65 , 1.68 , ...]
	===>  date_events:  16080-new(16070,1.65)  revised(16040,1.60,1.65)
		[16080-new(16070,1.65)  revised(16040,1.60,1.65),16060-unchaged,
		16055-revised(16010,1.65,1.68), ...
	structure de données à finaliser

---- */

stocker_val(_Per,_Obj,[],[]):-!.
stocker_val(q,Obj,[A|A_tail],[V|V_tail]):-
	rev(V,Rev_V),
	mensualise(A,Rev_V,Values),
	set_val(Obj,A,Values),
	stocker_val(q,Obj,A_tail,V_tail).
stocker_val(Per,Obj,[A|A_tail],[V|V_tail]):-
	rev(V,Rev_V),
	set_val(Obj,A,Rev_V),
	stocker_val(Per,Obj,A_tail,V_tail).


% -------------------------
% determine_periodicite_maj(Indic,Attributs,Valeurs,Per)
determine_periodicite_maj(Indic,Attributs,L_Valeurs,Per):-
	det_period_maj(Attributs,L_Valeurs,Per),
	set_val(Indic,periodicite_maj,Per),
	console([determine_periodicite_maj_apres_set_val,Indic,Per]),
	!.

det_period_maj([],[],unk).
det_period_maj([dates|_],[[D1,D2,D3,D4|_]|_],Periodicite):-
	X is D2-D1, days_periode(X,Periodicite),
	Y is D3-D2, days_periode(Y,Periodicite),
	Z is D4-D3, days_periode(Z,Periodicite),
	!.
det_period_maj([_|T_Atts],[_|T_vals],Per):-
	det_period_maj(T_Atts,T_vals,Per).


% ------------------------------------
split_liste_paire([],[],[]).
split_liste_paire([P1-P2|T],[P1|R_T1],[P2|R_T2]):-
	split_liste_paire(T,R_T1,R_T2).

split_liste_triplet([],[],[],[]).
split_liste_triplet([P1-P2-P3|T],[P1|R_T1],[P2|R_T2],[P3|R_T3]):-
	split_liste_triplet(T,R_T1,R_T2,R_T3).

% -------  FIN --------
