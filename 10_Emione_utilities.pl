% file	: 10_Emione_utilities.pl
% date	: 01/04/2007
% Utilitaires liés à Emione

is_terminal(Obj):-    % not Var Obj
	m_terminal:terminal(_,Obj),!.

% ----------------------------------
% descendance(Obj, [(Obj,Niveau,NodeOrLeaf)|(Obj2,Niv,NodeorLeaf)]
% pas très joliement implanté ....
%
descendance(Obj,[(Obj,0,leaf)]):-
	is_terminal(Obj),!.

descendance(Obj,Liste_Desc_Niv_NodeLeaf):-
	descendance(Obj,0,L_Desc_Niv_NodeLeaf),
	flatten(L_Desc_Niv_NodeLeaf,Liste_Desc_Niv_NodeLeaf).

descendance(Obj,N_in,(Obj,N_in,leaf)):-
	is_terminal(Obj),!.

descendance(Obj,N_in,[(Obj,N_in,node)|Suivants]):-
	Niveau is N_in+1,
	peres_de(Obj,Fils),
	findall(L_Desc_Niv_NodeLeaf,
		(   member(F,Fils),
		    descendance(F,Niveau,L_Desc_Niv_NodeLeaf)),
		Suivants).



% Sec et Sec_rgls ----------------------------------------------------------------
% Liste des repertoires contenant des SEC (Sous-ensemble de Connaissance)
%
sec_directories(L_sec_dir):-
	directories(Dir),	% voir 00_ex_util.pl
	findall(Sec_dir,(member(Sec_dir,Dir),check_dir(sec,Sec_dir)),
		L_sec_dir),
	!.

sec_directory(Sec_dir):-
	sec_directories(L_sec_dir),
	member(Sec_dir,L_sec_dir).

check_dir(Type,Directory):-
	dir_must_contain(Type,Files),
	forall(member(File,Files),
	       (absolute_file_name(File,[relative_to(Directory)],Abs_file_name),
		exists_file(Abs_file_name))),
	!.

dir_must_contain(sec,['comment','peres.pl','oper.pl','contenu.pl']).


% sec_save(Sec_name doit etre un Objet) ---------------------
%
sec_save(Sec_name):-
	valeur_locale(Sec_name,isa,sec),
	atomic_list_concat(['sec_',Sec_name],Sec_complete_name),
	(valeur_locale(Sec_name,objets_et_heritiers,Objs_h)-> true;Objs_h=[]),
	(valeur_locale(Sec_name,objets,Objs)-> true;Objs=[]),
	(valeur_locale(Sec_name,sec_comment,Rem)->true;Rem='No comment'),
	sauver_sec(sec(X::(((Y=Sec_name;member(Y,Objs_h)),(X=Y;herite_de(X,Y)));
			    member(X,Objs))),
		   Sec_complete_name,Rem),
	save_rules_for_sec(Sec_name),
	!.

sec_save(Sec_name):-
	console(['Le sec :',Sec_name,' n''est pas chargé']),
	console(['         ou un des attributs [sec_name,...] est absent']).


sec_save(Sec_name,Verbose_Silent):-
	valeur_locale(Sec_name,isa,sec),
	atomic_list_concat(['sec_',Sec_name],Sec_complete_name),
	(valeur_locale(Sec_name,objets_et_heritiers,Objs_h)-> true;Objs_h=[]),
	(valeur_locale(Sec_name,objets,Objs)-> true;Objs=[]),
	(valeur_locale(Sec_name,sec_comment,Rem)->true;Rem='No comment'),
	sauver_sec(sec(X::(((Y=Sec_name;member(Y,Objs_h)),(X=Y;herite_de(X,Y)));
			    member(X,Objs))),
		   Sec_complete_name,Rem,Verbose_Silent),
	save_rules_for_sec(Sec_name),
	!.

sec_save(Sec_name,_Verbose_Silent):-
	console(['Le sec :',Sec_name,' n''est pas chargé']),
	console(['         ou un des attributs [sec_name,...] est absent']).

% ------------------------
save_rules_for_sec(Sec_name):-
	valeur_locale(Sec_name,regles,L_head_hierarchies),
	valeur_locale(Sec_name,isa,sec),
	atomic_list_concat(['sec_',Sec_name],Sec_dir),
	working_directory(Old_D,Sec_dir),
	forall(member(Head,L_head_hierarchies),save_rule_hierarchies(Head)),
	working_directory(_,Old_D),
	!.
save_rules_for_sec(_Sec_name):-!.


save_rule_hierarchies(Head_obj):-
	valeur_locale(Head_obj,modules_regles,L_modules),
	forall(member(R_module,L_modules),
	       sauver_module_regles_de_sec(R_module)),
	!.
save_rule_hierarchies(_):-!.


/* ---------------------------------------------
<symphony>     ^ : []     ^^ : []     fils : []
   - sec_comment : Symphony SEC post_2013_09_11
   - isa : sec
   - objets : [r_dynamique]
   - objets_et_heritiers : [symphony_obj]
   - regles : [r_dynamique]
   - directives : <code prolog à execter après chargement>
------------------------------------------------
% exemple de SEC
sec(X::(objet_qq(Y),\+fils_de(Y,_),
       (X=Y;(herite_de(X,Y),not(objet(X,indic)),
	not(objet(X,diapositives)),not(objet(X,maps)) )) )).
------------------------------------------------ */

sec_load(Sec_name):-
	atomic_list_concat(['sec_',Sec_name],Sec_dir),
	inclure_sec(Sec_dir),
	load_rules_for_sec(Sec_name),
	% redo_filiation,    % filiation de tous les objets présents :pb
	!.


load_rules_for_sec(Sec_name):-
	valeur_locale(Sec_name,regles,L_Rule_sets),
	valeur_locale(Sec_name,isa,sec),
	atomic_list_concat(['sec_',Sec_name],Sec_dir),
	working_directory(Old_D,Sec_dir),
	forall(member(Head,L_Rule_sets),load_rule_sets(Head)),
	working_directory(_,Old_D),
	!.
load_rules_for_sec(_Sec_name):-!.


load_rule_sets(Head_obj):-
	valeur_locale(Head_obj,modules_regles,L_modules),
	forall(member(R_module,L_modules),
	       (   charger_module_regles_de_sec(R_module),
		   connecter_module_regles(R_module))),

	!.
load_rule_sets(_):-!.

% -----------------------
%
reset_val_demande(O,A,V):-
	console(['reset_val_demande :',O,A,' ancienne valeur :',V]),
	% code ici
	true.
% ---------- FIN --------
