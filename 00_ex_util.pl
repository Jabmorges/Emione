% file : aa_ut_basic             ex : aa_operat & liste & montrer & tuer
% but  : fichier des operateurs et des utilitaires generaux (prolog-quintus-etc..)
% il porte le nom aa_ut_basic car il est le premier a etre charge (orde alpha)

 :- set_prolog_flag(optimise,true).


% Les operateurs   !! certains seront purgés après re-ecriture de regles
:-op(975,fx,if).
:-op(972,xfy,then).
:-op(970,xfy,else).

:-op(901,xfy,sinon).
:-op(899,fx,si).
:-op(898,xfy,alors).
:-op(896,xfy,#).
:-op(895,xfy,&).

:-op(855,xfy,ou).
:-op(854,xfy,et).

:-op(700,xfx,est).

% entre 600 et 700 pour etre sous precedence de '='
:-op(640,yfx,le).	% le JJ-MM-AA
:-op(640,yfx,en).
:-op(640,yfx,entre).  % tendance entre (t-1,t-2)
:-op(630,xfy,du).
:-op(630,xfy,de).
:-op(630,xf,semaines).
:-op(630,xf,mois).

:-op(860,xfx,dont).
:-op(865,xfy,qui).
:-op(865,xfy,lequel).

:-op(845,fx,est_en_relation_avec).

:-op(870,fx,pour).
:-op(869,yfx,faire).
:-op(868,yfx,affecter).

:-op(830,fx,augmenter).
:-op(830,fx,diminuer).
:-op(840,xfx,au_profit_de).
:-op(840,xfx,au_detriment_de).

:-op(854,yfx,dans).
:-op(855,yfx,collecter).
:-op(855,yfx,collecter_all).

:- op(980,xfx,(::)).
:- op(900,fy,not) .

% ---
%  X dans Y (i.e. le member plus convivial?)
X dans Y :- member(X,Y).

% X est +Valeur  , une utilisation de l'unification pour faire de l'affectation
est(X,X).

% ---
% X & Y : redefinition du 'et'
X & Y :- call(X),call(Y).

% X # Y : redefinition du 'ou'
X # Y :- (call(X);call(Y)),!.


if X then Y else Z   :- !,
	(call(X) -> call(Y) ; call(Z)) .

if X then Y  :-
	(call(X) -> call(Y) ; true ).


% Les predicats utilitaires de base
% beaucoup de prédicats de traitement de liste sont dans SWI module list

% nmember(Elem, List, Index)
nmember(Element, List, Index) :-
	nth1(Index, List, Element).

%   nth0v: find the Index of an Element in the given List.
%   The Element might occur more than once, find each place.
nth0v([Element|_], Element, Index, Index).
nth0v([_|Tail], Element, M, Index) :-
	N is M+1,
	nth0v(Tail, Element, N, Index).

%   rev(+List, ?Reversed)
%   is a version of reverse/2 which only works one way around.
%   Its List argument must be a proper list whatever Reversed is.
%   You should use reverse/2 in new programs, though rev/2 is
%   faster when it is safe to use it.

rev(List, Reversed) :-
	rev(List, [], Reversed).

rev([], Reversed, Reversed).
rev([Head|Tail], Sofar, Reversed) :-
	rev(Tail, [Head|Sofar], Reversed).


% special for quarterly series interpolated to monthly
sumlist_q([], Total, Total).
sumlist_q([Head,_Month_2], Sofar, Total) :-
	Total is Sofar+Head.
sumlist_q([Head], Sofar, Total) :-
	Total is Sofar+Head.
sumlist_q([Head,_Month_2,_Month_3|Tail], Sofar, Total) :-
	Next is Sofar+Head,
	sumlist_q(Tail, Next, Total).


%   add_element(+Element, +Set1, ?Set2)
%   is true when Set1 and Set2 are sets represented as unordered lists,
%   and Set2 = Set1 U {Element}.  It may only be used to calculate Set2
%   given Element and Set1.  However, if Set1 is a partial list, there
%   is an unpleasant hack using add_element(Element, Set1, Set1) which
%   adds new Elements at the end of Set1.
add_element(Element, Set1, Set2) :-
	memberchk(Element, Set1),
	!,
	Set2 = Set1.
add_element(Element, Set1, [Element|Set1]).



%   del_element(+Element, +Set1, ?Set2)
%   is true when Set1 and Set2 are sets represented as unordered lists,
%   and Set2 = Set1 \ {Element}.  It may only be used to calculate Set2
%   given Element and Set1.  If Set1 does not contain Element, Set2 will
%   be identical to Set1 (the old version made a new copy of Set1).  If
%   Set1 is not an unordered set, but contains more than one copy of
%   Element, only the first will be removed.  If you want to delete all
%   copies of a given element, use delete/3 from library(lists).  For a
%   version which fails if Element is not in Set1, use selectchk/3.
del_element(Element, Set1, Set2) :-
	selectchk(Element, Set1, Result),
	!,
	Set2 = Result.
del_element(_, Set1, Set1).



%   selectchk(+Element, +Set, ?Residue)
%   is to select/3 what memberchk/2 is to member/2.  That is, it locates
%   the first occurrence of Element in Set, and deletes it, giving Residue.
%   It is steadfast in Residue.
selectchk(X, [X|R],     Residue) :- !, Residue = R.
selectchk(X, [A,X|R],   Residue) :- !, Residue = [A|R].
selectchk(X, [A,B,X|R], Residue) :- !, Residue = [A,B|R].
selectchk(X, [A,B,C|L], [A,B,C|R]) :-
	selectchk(X, L, R).


% insert_x_before_y(X,Y,List,New_list)
% insert element X before element Y in List giving New_list

insert_x_before_y(_X,_Y,[],[]):-!.
insert_x_before_y(X,Y,[Y|Tail],[X,Y|Tail]):-!.
insert_x_before_y(X,Y,[Z|Tail],[Z|Res]):-
	insert_x_before_y(X,Y,Tail,Res),
	!.


%
%  Partie 2  (ex-fichier : liste)
%
% Predicat de traitement de listes plus spÎcifique Ì EASy


sublist_from_to(From,To,List,Result):-
	(var(From) -> From_abs=0 ; From_abs is abs(From)) ,
	From =< To,
	get_sublist(From_abs,List,_,Sublist1),
	Duree is To - From, A_duree is abs(Duree),
	get_sublist(A_duree,Sublist1,Result,_).

get_sublist_from_start_point(List,Start,Duree,From,To,Sublist):-
	Duree < 0,
	R_sta is abs(Start),
	E1 is (Duree - R_sta) ,
	R_end is abs(E1),
	(To is R_end-1,From=R_sta),
	get_sublist(From,List,_,Sublist1),
	A_duree is abs(Duree),
	get_sublist(A_duree,Sublist1,Sublist,_).

get_sublist_from_start_point(List,Start,Duree,From,To,Sublist):-
	Duree >= 0,
	R_sta is abs(Start),
	E1 is ( R_sta -Duree) ,
	E1 >= -1 ,	% sinon on est hors de la liste
	(From is E1+1,To=R_sta) ,
	get_sublist(From,List,_,Sublist1),
	get_sublist(Duree,Sublist1,Sublist,_).

sublist_from(From,List,Sublist):-
	(var(From) -> From_abs =0; From_abs is abs(From)),
	get_sublist(From_abs,List,_Head,Sublist).



% version plus rapide sur grande liste
get_sublist(0,[_129|Residu],[],[_129|Residu],nil):-!.
get_sublist(1,[A|Residu],[A],Residu,A):-!.
get_sublist(2,[A,B|Residu],[A,B],Residu,B):-!.
get_sublist(3,[A,B,C|Residu],[A,B,C],Residu,C):-!.
get_sublist(4,[A,B,C,D|Residu],[A,B,C,D],Residu,D):-!.
get_sublist(5,[A,B,C,D,E|Residu],[A,B,C,D,E],Residu,E):-!.
get_sublist(6,[A,B,C,D,E,F|Residu],[A,B,C,D,E,F],Residu,F):-!.
get_sublist(7,[A,B,C,D,E,F,G|Residu],[A,B,C,D,E,F,G],Residu,G):-!.
get_sublist(8,[A,B,C,D,E,F,G,H|Residu],[A,B,C,D,E,F,G,H],Residu,H):-!.
get_sublist(9,[A,B,C,D,E,F,G,H,I|Residu],[A,B,C,D,E,F,G,H,I],Residu,I):-!.
get_sublist(10,[A,B,C,D,E,F,G,H,I,J|Residu],[A,B,C,D,E,F,G,H,I,J],Residu,J):-!.
get_sublist(X, [A,B,C,D,E,F,G,H,I,J|Suite],[A,B,C,D,E,F,G,H,I,J|Extrait],Residu,Last):-
	N is X-10,
	N>=0,
	get_sublist(N,Suite,Extrait,Residu,Last).

get_sublist(N,Liste,L_Head,L_tail):-
	get_sublist(N,Liste,L_Head,L_tail,_Head_last).

get_sublist_first_and_last(N,[First|Liste],L_Head,First,H_last,L_Tail):-
	get_sublist(N,[First|Liste],L_Head,L_Tail,H_last).


% --------------------------------------------------------
% get_sub_dlist : produit difference_liste de tête
% get_sub_dlist(0,[_129|Residu],[]-[],[_129|Residu]):-!.
get_sub_dlist(0,[_129|Residu],Z-Z,[_129|Residu],nil):-!.
get_sub_dlist(1,[A|Residu]   ,[A|Z]-Z,Residu,A):-!.
get_sub_dlist(2,[A,B|Residu],[A,B|Z]-Z,Residu,B):-!.
get_sub_dlist(3,[A,B,C|Residu],[A,B,C|Z]-Z,Residu,C):-!.
get_sub_dlist(4,[A,B,C,D|Residu],[A,B,C,D|Z]-Z,Residu,D):-!.
get_sub_dlist(5,[A,B,C,D,E|Residu],[A,B,C,D,E|Z]-Z,Residu,E):-!.
get_sub_dlist(6,[A,B,C,D,E,F|Residu],[A,B,C,D,E,F|Z]-Z,Residu,F):-!.
get_sub_dlist(7,[A,B,C,D,E,F,G|Residu],[A,B,C,D,E,F,G|Z]-Z,Residu,G):-!.
get_sub_dlist(8,[A,B,C,D,E,F,G,H|Residu],[A,B,C,D,E,F,G,H|Z]-Z,Residu,H):-!.
get_sub_dlist(9,[A,B,C,D,E,F,G,H,I|Residu],[A,B,C,D,E,F,G,H,I|Z]-Z,Residu,I):-!.
get_sub_dlist(10,[A,B,C,D,E,F,G,H,I,J|Residu],[A,B,C,D,E,F,G,H,I,J|Z]-Z,Residu,J):-!.
get_sub_dlist(X, [A,B,C,D,E,F,G,H,I,J|Suite], [A,B,C,D,E,F,G,H,I,J|Extrait]-Z,Residu,Dlist_last):-
	N is X-10,
	N>=0,
	get_sub_dlist(N,Suite,Extrait-Z,Residu,Dlist_last).

get_sub_dlist(N,Liste,Dlist_H,L_tail):-
	get_sub_dlist(N,Liste,Dlist_H,L_tail,_Dlist_H_last).

get_sub_dlist_first_and_last(N,[First|Liste],Dlist_H,First,Dlist_H_last,L_tail):-
	get_sub_dlist(N,[First|Liste],Dlist_H,L_tail,Dlist_H_last).


%
% dlist2list(Liste-X,Liste). sans instancier la queue de Dliste
dlist2list(DListe,Liste):-copy_term(DListe,Liste-[]).

% complétude à vérifier
list2dlist([],[]-[]).
list2dlist(Liste,D_liste-T):-
	append(Liste,T,D_liste),!.


% ----------------------------------------------------------
sous_liste(Lgt,Liste,Sous_liste,Residu):-
	get_sublist(Lgt,Liste,Sous_liste,Residu),!.
sous_liste(_Lgt,Liste,Liste,[]):-!.  % si la liste est plus courte que Lgt

% desenclaver : produit a partir d'une [[_|_]|[_|_]] une [_|_] ?? ne sup qu'un niveau
desenclaver_liste(In_list,Out_list):-
	desenclaver(In_list,[],Out_list).
desenclaver([],In,In):-!.
desenclaver([Head|Tail],In,Out):-
	Head = [_|_],
	append(In,Head,Nin),
	desenclaver(Tail,Nin,Out),!.
desenclaver([Head|Tail],In,[Head|Out]):-
	desenclaver(Tail,In,Out),!.


% inserer_delimiteur([X|Tail],Delimiteur,[X,Delimiteur|Tail])
% insere entre chaque element de Liste un Delimteur
inserer_delimiteur([X],_,[X]):-!.
inserer_delimiteur([X|Tail],Delimiteur,[X,Delimiteur|RTail]):-
	inserer_delimiteur(Tail,Delimiteur,RTail).

merge_lists([], [], []).
merge_lists( [Key|Keys], [Value|Values],[Key-Value|Pairs]) :-
	merge_lists( Keys, Values,Pairs).

remove_dup(List, Pruned) :-  % utilise le sort standard PROLOG
	list_to_set(List, Pruned).



% substitue(X,Y,L,New_L) substitue un elem X dans L par Y dans New_L
substitue(_X,_Y,[],[]):-!.
substitue(X,Y,[X|Tail],[Y|RTail]):-
	substitue(X,Y,Tail,RTail),!.
substitue(X,Y,[Z|Tail],[Z|RTail]):-
	substitue(X,Y,Tail,RTail).

% substitue_ou_ajoute(X,Y,L,New_L) substitue un elem X dans L par Y dans New_L
%				   si le pattern X n'existe pas alors ajoute Y a L
substitue_ou_ajoute(X,Y,L,New_L):-
	memberchk(X,L),substitue(X,Y,L,New_L),!.
substitue_ou_ajoute(_X,Y,L,[Y|L]):-!.

% search_replace(Searched-Replacement,In_list,Res_list)
search_replace(_,[],[]):-!.
search_replace(S_R,[X|T],[R|N_T]):-
	(memberchk(X-R,S_R);R=X ),
	search_replace(S_R,T,N_T).




% *******************************************************
% produit_cartesion de Lofl ==> combinaison des Lofl

produit_cartesien(LofL_valeurs,Lofl_Csts):-
	findall(Combinaison,prod_cartesien(LofL_valeurs, Combinaison),Lofl_Csts).

% -----------
% prod_cartesien(LofL,Combinaison_L) : partie backtrackante
%       ([[1,2],[a,b],[x,y,z]],[1,a,x] + bktk).
% Gustavo Brown in stackoverflow.com très élégante et rapide
%
prod_cartesien([], []).
prod_cartesien([[Head|_]|Lists], [Head|L]):-
	prod_cartesien(Lists, L).
prod_cartesien([[_,Head|Tail]|Lists], L):-
	prod_cartesien([[Head|Tail]|Lists], L).

% /* --- version alternative pour cartesien_lofl(LofL,El1_of_prod_cartesien)
%  bart Daemon
%
cart_prod([],[[]]).
cart_prod([L|Ls],Out) :-
        findall([X|R],(cart_prod(Ls,O), member(X,L), member(R,O)),Out).
% ----
prod_cart(Ls, P) :-
     findall(Xs, members(Xs, Ls), P).
members([], []).
members([E|Es], [L|Ls]) :-
     member(E, L),
     members(Es, Ls).

% */




% -------------------------------------------------------------------------
% OPERATION SUR LES STRINGS

conv_to_lower(X,Lower):-downcase_atom(X,Lower).

conv_to_upper(X,Upper):-upcase_atom(X,Upper).


% A remplacer par sub_string (SWI-Built_in)
substring(Part_str,Whole_str,Position):-
	name(Part_str,Part),
	name(Whole_str,Whole),
	length(Part,Lgt),
	substr(Whole,Part,Lgt,1,Position).

	substr(Whole,_Part,Lgt,1,_Position):-
		length(Whole,Lgt_w),
		Lgt_w < Lgt ,!,fail.
	substr(Whole,Part,Lgt,Position,Position):-
		get_sublist(Lgt,Whole,Part,_).
	substr([_Whole_head|Whole_tail],Part,Lgt,Cpteur,Position):-
		Ncompteur is Cpteur+1,
		substr(Whole_tail,Part,Lgt,Ncompteur,Position).


substitue_string(Search_str,Replace_str,Whole_str,Res):-
	name(Search_str,Search),
	name(Replace_str,Replace),
	name(Whole_str,Whole),
	length(Search,Lgt),
	substit_str(Whole,Search,Replace,Lgt,Res_str),
	name(Res,Res_str).

	substit_str([],_,_,_,[]):-
		!.
		substit_str(Whole,Search,Replace,Lgt,New_str):-
		get_sublist(Lgt,Whole,Search,Tail),
		substit_str(Tail,Search,Replace,Lgt,Res),
		append(Replace,Res,New_str),!.
	substit_str([W_head|W_tail],Search,Replace,Lgt,[W_head|Res]):-
		substit_str(W_tail,Search,Replace,Lgt,Res).


% --- trim(Raw_atom,Cleaned_atom)  % trim les blancs a double dans une phrase
trim_espaces(Raw,Cleaned):-
	name(Raw,Chars),
	trim_blanks(Chars,C_chars),
	name(Cleaned,C_chars).

trim_blanks([],[]):-!.
trim_blanks([Char|Chars],[Char|Cleaned]):-
	is_char(Char),
	!,
	trim_blanks_rest_word(Chars,Cleaned).
trim_blanks([_|Chars],Cleaned):-
	trim_blanks(Chars,Cleaned).

trim_blanks_rest_word([],[]):-!.
trim_blanks_rest_word([Char|Chars],[Char|Cleaned]):-
	is_char(Char),
	!,
	trim_blanks_rest_word(Chars,Cleaned).
trim_blanks_rest_word([_|Chars],Cleaned):-
	trim_blanks_next_word(Chars,Cleaned).

trim_blanks_next_word([],[]):-!.
trim_blanks_next_word([Char|Chars],[Space,Char|Cleaned]):-
	is_char(Char),
	!,
	Space is " ",
	trim_blanks_rest_word(Chars,Cleaned).
trim_blanks_next_word([_|Chars],Cleaned):-
	trim_blanks_next_word(Chars,Cleaned).


is_char(D):-
	D  > 32,
	D  < 127 .

trim(Raw,Cleaned):-
	name(Raw,Chars),
	trimeur_blanks(Chars,C_chars),
	name(Cleaned,C_chars).
trimeur_blanks([],[]):-!.
trimeur_blanks([Char|Chars],[Char|Cleaned]):-
	is_char(Char),
	trimeur_blanks(Chars,Cleaned),!.
trimeur_blanks([_|Chars],Cleaned):-
	trimeur_blanks(Chars,Cleaned).



% -------  !! Mettre les prédicats dépendant de Emione dans un autre fichier !!!

% ------- Predicats appelables ------
% tuer(Objet) : detruire un objet avec validation avant l'operation
% kill([Liste_objets) : detruire une liste d'objet sans validation avant operation
% kill(Objet) : detruire un objet sans validation avant
% vider_obj(Objet) : detruire les att,des et mar d'un objet

% version avec validation
tuer(Obj):-	montrer(Obj),
		format('Tuer : ~w	et ses instances eventuelles <oui/non> ? ',Obj),
		read(X), X=oui,
		forall(pere_de(Obj,Fils),(vider_obj(Fils),tuer_obj(Fils))),
		vider_obj(Obj),
		tuer_obj(Obj).

% version sans validation
kill(Obj_list):- is_list(Obj_list),member(Obj,Obj_list),kill_one(Obj),fail.
kill(Obj) :- atom(Obj),kill_one(Obj),!.
kill(_).

kill_one(Obj):-	console(['Destruction de :',Obj]),
		forall(pere_de(Obj,Fils),(kill(Fils))),
		vider_obj(Obj),
		tuer_obj(Obj).

vider_obj(Obj) :-
	descripteur_local(Obj,A,D,_),reset_des(Obj,A,D),fail
	;
	marqueur(Obj,A,M,_),reset_mar(Obj,A,M),fail
	;
	valeur_locale(Obj,A,_),reset_val(Obj,A),fail
	;
	true.

% vider les descripteurs de O,A
vider_desc(Obj,A):-
	descripteur_local(Obj,A,D,_),reset_des(Obj,A,D),fail.
vider_desc(_Obj,_Att).

tuer_fils(Obj):-
	objet(X,Obj),
	tuer(X),fail.
tuer_fils(_):-!.

kill_fils(Obj):-
	objet(X,Obj),
	kill(X),fail.
kill_fils(_):-!.



% supprimer_descripteur(Obj,Desc)  pour un Obj donne pour tous les Atts portant ce Desc
supprimer_descripteur(Obj,Desc):-
	not(var(Obj)),not( var(Desc)),
	descripteur_local(Obj,Att,Desc,_),
	reset_des(Obj,Att,Desc),fail.
supprimer_descripteur(_Obj,_Desc):-!.


% --- Mettre de cote le contenu d'un objet pour faire une simulation
met_de_cote(Obj):-
	valeur_locale(objet_temporaire,vides,L_vide),
	member(Sp_obj,L_vide),
	vider_obj(Sp_obj),
	copier_obj(Obj,Sp_obj),
	del_element(Sp_obj,L_vide,N_L_vide),
	set_val(objet_temporaire,vides,N_L_vide),
	set_des(objet_temporaire,Sp_obj,obj_name,Obj),
	!.

met_de_cote(Obj):-
	console(['met_de_cote(',Obj,') - Tous les objets spare_x sont occupes - echec']),
	!.


% --- Restituer le contenu d'un objet apres une simulation
restitue(Obj):-
	descripteur_local(objet_temporaire,Sp_obj,obj_name,Obj),
	vider_obj(Obj),
	copier_obj(Sp_obj,Obj),
	valeur_locale(objet_temporaire,vides,L_vide),
	add_element(Sp_obj,L_vide,N_L_vide),
	set_val(objet_temporaire,vides,N_L_vide),
	!.
restitue(Obj):-
	console(['restitue(',Obj,') Pas trouve de spare pour :',Obj]),
	!.

% --- libere_spare(Obj) : libere l'objet spare_x qui contient une copie de Obj
libere_spare(Obj):-
	descripteur_local(objet_temporaire,Sp_obj,obj_name,Obj),
	vider_obj(Sp_obj),
	valeur_locale(objet_temporaire,vides,L_vide),
	add_element(Sp_obj,L_vide,N_L_vide),
	set_val(objet_temporaire,vides,N_L_vide),
	reset_des(objet_temporaire,Sp_obj,obj_name),
	!.
libere_spare(_Obj):-!.

% -- copier le contenu d'un objet_orig sur un autre obj_dest
copier_obj(Obj_orig,Obj_dest):-
	forall(valeur_locale(Obj_orig,A,V),set_val(Obj_dest,A,V)),
	forall(descripteur_local(Obj_orig,A,D,V),set_des(Obj_dest,A,D,V)),
	forall(   marqueur(Obj_orig,A,M,V),set_mar(Obj_dest,A,M,V)),
	!.
copier_obj(_Obj_orig,_Obj_dest):-
	console(['copier_obj : failed...']),!.


% forall(Generator, Goal)
% succeeds when Goal is provable for each true instance of Generator.
% Note that there is a sort of double negation going on in here (it
% is in effect a nested pair of failure-driven loops), so it will
% never bind any of the variables which occur in it.
% forall(Generator, Goal) :-
%	( call(Generator), ( call(Goal) -> fail ; true ) -> fail ; true ).


% doforall an other version of forall which executes Goal for each instance of Generator
%				even if Goal does not succed (here is the difference)

doforall(Generator, Goal) :-
	( call(Generator), ( call(Goal) -> fail ; fail ) -> fail ; true ).

% ------------------
% imprime(X) CAPTURE tous les write fait par le predicat X dans un fichier et l'imprime
imprime(liste_des_indicateurs):-
	set_des(fonctionnement,textes,orientation,'-olandscape -otl45'),fail.
imprime(X):-
	set_val(etat_msg,titre,'preparing print'),
	valeur_locale(fonctionnement,textes,File_name),
	put_on_file(File_name,X),
	traiter_fichier_sauve(textes),
	!.

imprime(X,landscape):-
	set_des(fonctionnement,textes,orientation,'-olandscape -otl45'),
	imprime(X),!.

% -- egalement utilise par export_infos
put_on_file(File,Callable):-
	absolute_file_name(File,File_abs),
	open(File_abs,write,Flot),
	set_output(Flot),
	(call(Callable);true),
	set_output(user),
	close(Flot),
	!.

append_to_file(File,Callable):-
	open(File,append,Flot),
	set_output(Flot),
	(call(Callable);true),
	set_output(user),
	close(Flot),
	!.


% --- TRAITEMENT DES FICHIERS CAPTURES (Impression ou sauve)
traiter_fichier_sauve(Type):-
	descripteur_local(fonctionnement,Type,traitement,T),	% T in save_file(Type) / print_file(Type)
	call(T),!.

% ---
print_file(textes):-
	valeur(fonctionnement,textes,File),
	descripteur_local(fonctionnement,textes,orientation,Orientation),
	descripteur_local(fonctionnement,textes,fonte,Fonte),
	system_command(['lp -onb ',Orientation,' ',Fonte,' ',File]),
	descripteur_local(fonctionnement,textes,default_orientation,Def_o),
	set_des(fonctionnement,textes,orientation,Def_o),
	write('Impression terminee'),
	!.
print_file(textes):-
	write('Print command failed'),nl,!.

% ---
save_file(Type):-
	console([' save_file ',Type,'  implantation SWI a faire - 00_ex_util.pl']),
	% ask_near_bt(save_bt,Type,'SAVE AS (Filename) :'),  % version EMICAT
	% affiche une fenetre de saisie qui appelera save_file(Type,Filemane)
	!.


% --- appelÎ par trigger sur print_bt-Type
save_file(Type,File_name):-
	descripteur_local(fonctionnement,Type,save_ext,Ext),
	concat_atom(['SAVING FILE : ',File_name,Ext],MSG),
	set_val(etat_msg,titre,MSG),
	descripteur_local(fonctionnement,graphiques,dir,Path),
	valeur_locale(fonctionnement,Type,Source_f),
	system_command(['cp ',Source_f,' ',Path,'/',File_name,Ext]),
	!.
save_file(_Type,File_name):-
	write('Save command failed for file :'),write(File_name),nl,!.

% --- Affiche la fenetre de saisie pour filename
ask_near_bt(Bouton,Att,_Text):-
	reset_val(Bouton,Att),
	% set_up_saisie(Bouton,Bouton-Att,Text),
	!.



% -----------------------
% --- PURGER LES FICHIERS stockes dans le directory mentionnÎ dans fonctionnement,graphiques,GR
purge_graphique_perime:-
	valeur_locale(fonctionnement,user,_User),
	descripteur_local(fonctionnement,graphiques,dir,Gr_dir),
	descripteur_local(fonctionnement,graphiques,duree_retention,Duree),
	system:cd(Old_dir),
	system:cd(Gr_dir),
	remove_file_older_than(Duree),
	system:cd(Old_dir),
	!.

% - ATTENTION au directory courant !!!
remove_file_older_than(Duree):-
	write('Removing files older than '),write(Duree),write(' Days'),nl,
	date_qp(Cur_date),nb_jour(Cur_date,Num_jour),
	system:ls(File_name),   % a corriger ...pas très opérationel
	not(name(File_name,[46|_])),	% ne pas supprimer les fichiers '.' , '..' et '.xxx'
	time_file(File_name,Num_j_file),
	Delta_j is Num_jour-Num_j_file,
	Delta_j >= Duree,
	write('Removing : '), write(File_name),nl,
	system:rm(File_name) ,
	fail.
remove_file_older_than(_Duree):-
	nl,write('Cleaning Done...'),nl,!.


% CODE PERMETTANT DE POSITIONNER LE CURSEUR SUR LE CURRENT-OUTPUT-STREAM
positionne_a(X):-
        integer(X),
        current_output(Flot),
        line_position(Flot,P),
        (X > P -> Tab is X-P; Tab=0),
        tab(Flot,Tab),!.
positionne_a(X):-
        Y is X, % assume que X est une expression arithmétique
        positionne_a(Y).

% ************************************************************************
% Sauve le contenu d'un module sous forme d'un fichier
sauve_module_fichier(Module,File):-
	% sauvegarde brutale à compléter par en-tête de module etc...
	put_on_file(File,( write('% fichier de module géneré par sauve_module_fichier'),nl,nl,
			 write(':- module('),write(Module),write(',[]).'),nl,nl,
			 Module:listing)),
	!.


% ************************************************************************
% liste les directories depuis le Working_dir
directories(Directories):-
	expand_file_name('*',L),
	findall(Dir,(member(Dir,L),exists_directory(Dir)),Directories).

%----------------------------------------------------------------------
%console : remplace le write(X) , à utiliser lors de dev. html
%
console(Liste):-
	log_file(_File,Flot),
	console(Flot,Liste),
	!.
console(Liste):-
	log_file(File),
	open(File,append,Flot),
	console(Flot,Liste),
	close(Flot),
	!.
console(Liste):-
	console(user,Liste),!.

console(Stream,[]):-nl(Stream),!.
console(_,[X]):-nonvar(X),X=no_nl,!.
console(Stream,[X|Tail]):-nonvar(X),X=nl,nl(Stream),console(Stream,Tail),!.
console(Stream,[Head|Tail]):- write(Stream,Head),write(Stream,' '),console(Stream,Tail),!.

% log_directory  : le repertoire des log_files
%
% log_directory('../SymphAnie_logs/').



:- dynamic log_file/1.
:- dynamic log_file/2.

activer_log_fixe:-
	retractall(log_file(_,_)),
	log_directory(Log_dir),
	atom_concat(Log_dir,'Log_sessions.txt',File),
	open(File,append,Flot),
	assert(log_file(File,Flot)).
desactiver_log_fixe:-
	log_file(_,Flot),
	close(Flot),
	retractall(log_file(_,_)).

activer_log:-
	retractall(log_file(_)),
	log_directory(Log_dir),
	atom_concat(Log_dir,'Log_sessions.txt',File),
	assert(log_file(File)).
activer_log(File):-
	retractall(log_file(_)),
	log_directory(Log_dir),
	atom_concat(Log_dir,File,Fichier),
	assert(log_file(Fichier)).
desactiver_log:-
	retractall(log_file(_)).


activer_log_on(Host):-
	gethostname(Host),
	get_time(Ts),
	stamp_date_time(Ts,Dt,local),
	date_time_value(date,Dt,date(Y,M,D)),
	atomic_list_concat([Host,log,Y,M,D],'_',Fi),
	atom_concat(Fi,'.txt',File),
	activer_log(File),
	get_time(Time),
	format_time(atom(Start_time),'Started : %T',Time),
	console([Start_time]),
	!.
activer_log_on(_):- desactiver_log.

% --------------------------------------------------------
% evaluer les perfs
%
many(N,Call):- time(lots(N,Call)),!.
many(Call):- time(lots(1000,Call)),!.
manymany(Call):- time(lots(10000,Call)),!.
manymanymany(Call):- time(lots(1000000,Call)),!.

lots(0,_):-!.
lots(C,Call):-
	NC is C-1,
	catch(Call,_,true),
	lots(NC,Call).

% ------------------------------------------------------------------

fail_free_system_command(Commande_L):-
	atomic_list_concat(Commande_L,Commande),
	console([' fail_free_system_command ',Commande,'  implantation a finir - 00_ex_util.pl']),
	!.

system_command(Commande_L):-
	atomic_list_concat(Commande_L,Commande),
	console([' system_command ',Commande,'  implantation a finir - 00_ex_util.pl']),
	!.
% ---------
%  nrev for test

nrev([],[]).
   nrev([H|T],R):-  nrev(T,RevT),  append(RevT,[H],R).

long_period:- forall(between(0,1000000,_I),nrev([1,2,3,4,5,6,7],_Rev)).



% ---------  Fin de fichier : 00_ex_util.pl

