/* file :	17_ex_langage   csv_loader */

% Langage d'interrogation
% PP proposition principale
% GR groupe relatif  (supprimÅs dans cette version)
% Code_exe : code executable de la requete
% Expr : Expression des conditions
% Expr_1 : expression atomique expl: nom=cpi
%
% utilisation : 'indicateur dont pays = usa et nom = Nom'
%		--> Nom est instancie au differentes valeurs par bkt
%		indicateur dont pays = usa et nom = Nom et libelle de nom =Lib (acces au descripteur)


% Juin  98 : Les traitements des groupes relatifs de la version precedente ont ete supprimes.
% Les possibilites de composition sont reduites a :  Descripteur de Attribut ....
% Dans l'appel final on procede a l'interversion 'Desc de Att'  en xx(Obj,Att,Des) afin de simplifier
% la lecture des objets sous emicat
%
% NB : le source d'origine se trouve dans le fichier 'naviguer'

% expl : s_indicateur dont obj_name=a_dmi_usa et valeur de serie entre(t=25/09/2000 ,t= 6/10/2000) =V.

% Pour tester les traductions obtenues sans exexuter le code resultant
translate(P):-proposition_principale(P,Code_exe,O1),
	write('traduction :' ),write(Code_exe),nl,
	write('Objets : '),write(O1),nl.



dont(X ,Y ):- qualification(X dont Y,_O1).

et(X ,Y ):- qualification(X et Y,_O1).

ou(X ,Y ):- qualification(X ou Y,_O1).


% Cet appel sert pour les traducteur de regle ....
phrases(Phrase,Code_executable,Obj1):-
	proposition_principale(Phrase,Code_executable,Obj1),!.

% Execute la phrase  i.e. instancie les variables ...
qualification(Proposition ,O1):-
	proposition_principale(Proposition,Code_exe,O1),
	!,
	call(Code_exe).



proposition_principale(Type dont Expr,Code_pp,Obj_pp):-
	!,analyser_expr(Type,Expr,Code_pp,Obj_pp).
proposition_principale(Expr,Code_pp,Obj_pp):-
	\+ atom(Expr), \+ var(Expr),!,
	analyser_expr(Expr,Code_pp,Obj_pp).
proposition_principale(Expr,true,Expr):-
	atom(Expr),!,
	objet_qq(Expr).
proposition_principale(Expr,true,Expr):- % INTEGRER  AVEC  -> et ;
	var(Expr).

analyser_expr(Expr,Code_exe,Obj_pp):-
	analyser_expr2(Expr,_,Code_exe,Obj_pp).
analyser_expr(Type,Expr,Code_exe,Obj_pp) :-
	\+ var(Type),!,
	analyser_expr2(Expr  ,Type,Code,Obj_pp),
	find_key(Type,Code,Obj_pp,Code_exe),!.
analyser_expr(Type,Expr,(Code_exe,objet(Obj_pp,Type)),Obj_pp):-
	analyser_expr2(Expr,Type,Code_exe,Obj_pp).


find_key(Type,Code,Obj,Code_out):-
	descripteur(Type,nom,key,Seed-K_a),
	extraire_clef(K_a,[],[],Code,Atts,Vals,Code_out),   % code permettant d'accelerer les recherches
	Atts=K_a,
	concat_liste_string_with_delimiter(Vals,Obj_1),
	concat_liste_string([Seed,Obj_1],Obj),
	objet(Obj,Type),!.



find_key(Type,Code,Obj,Code):-
	descripteur(Type,nom,key,K_a),
	bagof(X-V,Obj^X^(member(X,K_a),element_de(valeur(Obj,X,V),Code,(_,_),droite), \+ var(V)),Bag),
	length(K_a,Taille_K_a),length(Bag,Taille_K_a),
	bagof(V,X^(member(X,K_a),member(X-V,Bag)),Bag_final),  % a cause de element_de
	concat_liste_string_with_delimiter(Bag_final,Obj),
	objet(Obj,Type),!.
find_key(Type,Code,Obj,(objet(Obj,Type),Code)).

% complément du 20/06/98

extraire_clef([],Att,Val,Code,Att,Val,Code):-!.
extraire_clef([Head|Tail],Att_so_far,Val_so_far,Code_in,Att_final,Val_final,Code_final):-
	cherche_et_remplace_clef(Code_in,true,Code_transf,Head,Att,Val),
	append(Att_so_far,Att,New_att),
	append(Val_so_far,Val,New_val),
	extraire_clef(Tail,New_att,New_val,Code_transf,Att_final,Val_final,Code_final).

% ce petit morceau de code est un GROS MORCEAU

 cherche_et_remplace_clef(valeur(O,A,V),(Code_so_far),N_code,A,[A],[V]):-  % trouvé sur le dernier
	\+ var(V),
	Code_so_far = (true,Code_utile),
	concat(Code_utile,valeur_locale(O,A,V),N_code, (_,_),droite),!.
 cherche_et_remplace_clef(valeur(O,A,V),(Code_so_far),N_code,_,[],[]):-  % pas trouvé
	Code_so_far = (true,Code_utile),
	concat(Code_utile,valeur(O,A,V),N_code, (_,_),droite),!.

 cherche_et_remplace_clef((valeur(O,A,V),Tail),true ,N_code,A,[A],[V]):-  % trouvé sur le premier
	\+ var(V),
	N_code= (valeur_locale(O,A,V),Tail),
	!.

 cherche_et_remplace_clef((valeur(O,A,V),Tail),(Code_so_far),Res,A,[A],[V]):-
	\+ var(V),
	Code_so_far = (true,Code_utile),
	concat(Code_utile,valeur_locale(O,A,V),N_code, (_,_),droite),
	concat(N_code,Tail,Res, (_,_),droite),
	!.

 cherche_et_remplace_clef((Head,Tail),(Code_so_far),Res,X,Att,Val):-
	concat(Code_so_far,Head,N_code, (_,_),droite),
	cherche_et_remplace_clef(Tail,N_code,Res,X,Att,Val),!.

% fin complément du 20/6/98



analyser_expr2(Head et Suite,Type,(Head_exe,Suite_exe),Objet):-
	analyser_expr3(Head,Head_exe,Objet),
	analyser_expr2(Suite,Type,Suite_exe,Objet),!.
analyser_expr2(Head ou Suite,Type,(Head_exe;Suite_exe),Objet):-
	analyser_expr2(Head,Type,Head_exe,Objet),
	analyser_expr2(Suite,Type,Suite_exe,Objet),!.
analyser_expr2(Head,_Type,Head_exe,Objet):-   % pas de recur terminale !!
	analyser_expr3(Head,Head_exe,Objet).


analyser_expr3(obj_name = Objet,true,Objet):- !.
analyser_expr3(A = V,valeur(Objet,A,V),Objet):- atomic(A),!.
analyser_expr3( A en Time = V,Code, Obj):-
	analyser_time(Time,Temps),
	analyser_expr_4_3_en(A,Temps,V,Code,Obj),!.
analyser_expr3( A entre Time = V ,Code, Obj):-
	analyser_expr_4_3_entre(A,Time,V,Code,Obj),!.
% analyser_expr3(D_M de A de Serie  = V ,Code, Obj):-		% cette clause ne devrait plus etre utilisÎe
%	analyser_expr_4_3(D_M de A de Serie,V,Code,Obj),!.	% dans laversion EASy 1.0

analyser_expr3(D_M de A = V,(descripteur_local(Obj,A,D_M,V)->true;Code), Obj):-	% ne vaut-il pas mieux passer
	analyser_expr_4_3(D_M de A ,V,Code,Obj),!.				% toujour par activer_des(.. ?
	% On essaye de trouver un descripteur local sinon on active la methode
	% Ceci permet de stocker en local une valeur et d'y acceder tres vite tout en preservant
	% l'acces Ì la methode  (Bonne solution !?)
analyser_expr3(un_des(Exp) = V,(Code,member(V,Vals)),Objet):-
	analyser_expr3(Exp= Vals,Code,Objet),!.
analyser_expr3(local(A = V),valeur_locale(Objet,A,V),Objet):-!.
analyser_expr3(Clause,Clause,_):-!.



% le niveau inférieur de l'analyse de l'expression
analyser_expr_4_3_en(Exp,t_variable(T),V,(valeur(O,serie,Vs),egrener(Vs,T),
				activer_des(O,Att,D_M,parm(O,Att,[T],V)) ),O):-
	var(T),
	get_att_dm(Exp,Att,D_M,_Parm),!.
analyser_expr_4_3_en(Exp,expression(Exp_T,T) ,V,
			(Exp_T,activer_des(Obj,Att,D_M,parm(Obj,Att,[T],V)) ) ,Obj):-
	get_att_dm(Exp,Att,D_M,_Parm),!.
analyser_expr_4_3_en(Exp,atom_ou_var(T),V,(activer_des(Obj,Att,D_M,parm(Obj,Att,[T],V)) ),Obj):-
	get_att_dm(Exp,Att,D_M,_Parm),!.
analyser_expr_4_3_en(Exp,date(T,N_jour),V,(activer_des(Obj,Att,D_M,parm(Obj,Att,[date(T,N_jour)],V)) ),Obj):-
	get_att_dm(Exp,Att,D_M,_Parm),!.
analyser_expr_4_3_en(Exp,date_var(T,N_jour),V,(activer_des(Obj,Att,D_M,parm(Obj,Att,[date_var(T,N_jour)],V)) ),Obj):-
	get_att_dm(Exp,Att,D_M,_Parm),!.



analyser_expr_4_3_entre(Exp,Time,V,(Code_time,activer_des(Obj,Att,D_M,parm(Obj,Att,[T1,T2],V)) ),Obj):-
	analyser_time_entre(Time ,instanciation(Code_time,T1,T2)),
	get_att_dm(Exp,Att,D_M,_Parm),!.
analyser_expr_4_3_entre(Exp,Time,V,(activer_des(Obj,Att,D_M,parm(Obj,Att,[T1,T2],V)) ),Obj):-
	analyser_time_entre(Time ,[T1,T2]),
	get_att_dm(Exp,Att,D_M,_Parm),!.


%  modifié 2014_10_02 (pour ind_ref_evaluateur : bench_evaluation et  ref_evaluation)
analyser_expr_4_3(Exp,V,(activer_des(Obj,Att,D_M,parm(Obj,Att,D_M,V)) ),Obj):-
	get_att_dm(Exp,Att,D_M,_Parm),!.
/* version avant 10_2014 :
analyser_expr_4_3(Exp,V,(activer_des(Obj,Att,D_M,parm(Obj,Att,[],V)) ),Obj):-
	get_att_dm(Exp,Att,D_M,_Parm),!.
*/


%
%get_att_dm(D_M de Att de X de Y,D_M,Att,[X,Y]):-write('Syntaxe proscrite :'-D_M de Att de X de Y),nl, !.
% get_att_dm(D_M de Att de X ,D_M,Att,[X]):- write('Syntaxe proscrite :'-D_M de Att de X ),nl, !.
get_att_dm(D_M de Att ,Att,D_M,[]):-!.
get_att_dm(D_M	      ,D_M,D_M,[]):-!.


% Analyse des espaces temps  entre(t-X,t-Y)
analyser_time_entre((t - X,t-Y) , [Xr, Zr]):-
	\+ var(X),\+ var(Y),
	Z1 is X - Y,
	(Z1 <0 -> ( Xr=X, Zr is Z1-1);(Xr = Y, Zr is (Z1* (-1))-1)).
analyser_time_entre((t - X,t-Y) ,
		instanciation( (AX is abs(X),AY is abs(Y),Z1 is AX - AY,
		(Z1 <0 -> ( Xr=AX, Zr is Z1-1);(Xr = AY, Zr is (Z1* (-1))-1))),Xr, Zr) ):-
	(\+ground(X);\+ground(Y)),!.

analyser_time_entre((t=Date_1,t=Date_2) , [ date(Date_1,N_jour_1), date(Date_2,N_jour_2) ]):-      % 09/2006
	Date_1=J/M/A ,nonvar(J),nonvar(M),nonvar(A),nb_jour(J,M,A,N_jour_1),
	Date_2=J_2/M_2/A_2 ,nonvar(J_2),nonvar(M_2),nonvar(A_2),nb_jour(J_2,M_2,A_2,N_jour_2),
	!.
analyser_time_entre((t=Date,t-Y) , [ date(Date,N_jour), Zr]):-				% 03/2004
	Date=J/M/A ,nonvar(J),nonvar(M),nonvar(A),nb_jour(J,M,A,N_jour),
	Zr =Y ,	% tmp
	!.
analyser_time_entre((Date_1,Date_2) , [Date_1,Date_2]):- !. % pas de test pour le moment

% -- analyser_time (t=X)  ou (t-Y)------------------------------------------------
analyser_time(t_variable(X) ,t_variable(X) ):-!.
analyser_time(t - X , atom_ou_var(X)):-atomic(X);var(X),!.
analyser_time(t = Date , date(Date,N_jour)):- Date=J/M/A ,nonvar(J),nonvar(M),nonvar(A),nb_jour(J,M,A,N_jour),!.
analyser_time(t = Date , date_var(Date,_N_jour)):- Date=_J/_M/_A ,!.		% var(J),var(M),var(A), !.
analyser_time(t- X ,expression( T is X, T)):-!.	% expression doit etre entre ()


egrener(Val,T):- egrener_temps(Val,0,T).
egrener_temps([_H],T,T):-!.
egrener_temps(_,T,T).
egrener_temps([_H|Tail],In,Out):-
	N_In is In -1,
	egrener_temps(Tail,N_In,Out).


% ----------------------------------
tic(From,To,Inc,Tic):-
	float(From),
	N_to is float(To),
	N_inc is float(Inc),
	test_tic(From,N_to,N_inc,Code,Tic),!,
	call(Code).
tic(From,To,Inc,Tic):-
	test_tic(From,To,Inc,Code,Tic),!,
	call(Code).



test_tic(From,To,Inc,Code,Tic):-
	From =< To,!,
	(var(Inc) -> Inc = 1 ; Inc >0),
	Code = up_to_avec_test(From,To,Inc,From,Tic).

test_tic(From,To,Inc,Code,Tic):-
	From > To,!,
	(var(Inc) -> Inc = -1 ; Inc < 0),
	Code = down_to_avec_test(From,To,Inc,From,Tic) .


up_to_avec_test(_From,To,Inc,Current,Current):-Next is (Current+Inc) , Next > To,!.
up_to_avec_test(_From,_To,_Inc,Current,Current).
up_to_avec_test(From,To,Inc,Current,Tic):-
	N_current is  Inc + Current ,
	up_to_avec_test(From,To,Inc,N_current,Tic).

down_to_avec_test(_From,To,Inc,Current,Current):- Next is Current + Inc ,Next < To,!.
down_to_avec_test(_From,_To,_Inc,Current,Current).
down_to_avec_test(From,To,Inc,Current,Tic):-
	N_current is  (Current+Inc) ,
	down_to_avec_test(From,To,Inc,N_current,Tic).


% ATTENTION A Time positif analyser_time(t + X , X). analyser_time(+ X , X).



% collecter ...
pour(Valeur dans Table collecter Resultat):-
	collecter(Resultat,Valeur,Table).
pour(Valeur dans Table collecter_all Resultat):-
	collecter_all(Resultat,Valeur,Table).


% collecte Resultat pour Liste_de_Valeur dans Table
% permet de collecter tous les Resultats pour lesquels une Valeur matche avec
% une structure de type : [ Valeur_1 & Code_annexe_1 then Resultat_1 ,
%			    Valeur_2 & Code_annexe_2 then Resultat_2 | _]
%
% l'appel via : 'pour Valeur dans Table collecter Res' est decrit plus haut dans pour(X)
collecter(Resultats,Liste_de_Valeur,Table):-
	Liste_de_Valeur = [_|_],!,
	bagof(Result,Valeur^Liste_de_Valeur^(member(Valeur,Liste_de_Valeur),
			collect_for_one(Result,Valeur,Table) ) ,Results),
	desenclaver_liste(Results,Resultat_inter),
	setof(Elem,member(Elem,Resultat_inter),Resultats).
collecter(Resultat,Valeur,Table):-
	collect_for_one(Resultat_inter,Valeur,Table),
	setof(Elem,member(Elem,Resultat_inter),Resultat),
	!.

collecter_all(Resultats,Liste_de_Valeur,Table):-
	Liste_de_Valeur = [_|_],!,
	bagof(Result,Valeur^Liste_de_Valeur^(member(Valeur,Liste_de_Valeur),
			collect_for_one(Result,Valeur,Table) ) ,Results),
	desenclaver_liste(Results,Resultats)	.
collecter_all(Resultat,Valeur,Table):-
	collect_for_one(Resultat,Valeur,Table),!.

collect_for_one(Resultat,Valeur,Table):-
	bagof(Result,Cond^Table^(member(Cond then Result,Table),
			check_cond_val(Cond,Valeur) ) ,Resultat).

check_cond_val(Valeur & Cond,Valeur):-
	call(Cond),!.
check_cond_val(Valeur ,Valeur).


% REPONSES MULTIPLES
% liste_de(Obj_ou_atts, Repondant_au_query, Liste_resultat).
% !!! si on veut plusieurs atts....
%  expl : liste_de( X, indicateur dont pays=usa et nom=X, Res)
liste_de(Obj_ou_atts,Query,Liste):-
	bagof(Obj_ou_atts,Obj_ou_atts^(Query),Liste).


/* le 20/06/1998  */
naviguate:-
time((indicateur dont nom=cpi et date_maj=D et pays=usa,
indicateur dont nom=cpi et date_maj=D et pays=usa,indicateur dont nom=cpi et date_maj=D et pays=usa,
indicateur dont nom=cpi et date_maj=D et pays=usa,indicateur dont nom=cpi et date_maj=D et pays=usa,
indicateur dont nom=cpi et date_maj=D et pays=usa,indicateur dont nom=cpi et date_maj=D et pays=usa,
indicateur dont nom=cpi et date_maj=D et pays=usa,indicateur dont nom=cpi et date_maj=D et pays=usa,
indicateur dont nom=cpi et date_maj=D et pays=usa,indicateur dont nom=cpi et date_maj=D et pays=usa,
indicateur dont nom=cpi et date_maj=D et pays=usa,indicateur dont nom=cpi et date_maj=D et pays=usa,
indicateur dont nom=cpi et date_maj=D et pays=usa,indicateur dont nom=cpi et date_maj=D et pays=usa,
indicateur dont nom=cpi et date_maj=D et pays=usa,indicateur dont nom=cpi et date_maj=D et pays=usa,
indicateur dont nom=cpi et date_maj=D et pays=usa)).

naviguate_1:-
	time((_A dont nom=cpi et date_maj=D et pays=usa,
	_X dont nom=cpi et date_maj=D et pays=usa, _Y dont nom=cpi et date_maj=D et pays=usa)).
% Cpu used 0.42 sec. dans demo.sav
%
% on peut écrire  indicateur dont obj_name=cpi_usa et valeur en (t=J/M/A) = V .
% pour obtenir la date et la valeur de la dernière observation !!!!
