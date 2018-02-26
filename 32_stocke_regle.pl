% file	:	32_stocke_regle
% date	:	fevrier 2007 - juin 2012
% but:		stockage des règles compilées pour symphony

:- module(stocker_regle,[
			 stocke_regle/3,
			 stocke_regle_recur/3,
			 retracte_regle/2,
			 connecte_code/3,
			 delete_rule/1,
			 delete_rule/3,
			 sauver_module_regles/1,
	                 sauver_module_regles_de_sec/1,
			 charger_module_regles/1,
	                 charger_module_regles_de_sec/1,
			 connecter_module_regles/1,
			 purger_module/1,

			 connecte_code_m/3   % est il utilisé ? oui!
			 ]).


stocke_regle(Module,Head,Body):-
	Rule =(Head :- Body),
	Module:retractall(Head),	% pour ne pas doublonner
	Module:assert(Rule),
	Module:export(Head),
	% user:import(Module:Head),  % Pas import dans user : conflits entre modules multiples
	!.

stocke_regle_recur(Module,Head,Body):-
	Rule =(Head :- Body),
	Module:assert(Rule),
	!.

retracte_regle(Module,Head):-
	Module:retractall(Head),!.



%  connecte la règle à un Obj,Att
%
connecte_code(_,_,[]):- !.
connecte_code(Module,Nom_regle,(Obj,Att)):-
	atomic_list_concat([Nom_regle,'_regle'],Rule_name),
	Regle =..[Rule_name,Val],
	set_des(Obj,Att,si_besoin,(parm(Obj,Att,Val),Module:Regle,set_val(Obj,Att,Val))),
	reset_val(Obj,Att),
	set_des(Obj,Att,rule,Rule_name),
	!.

connecte_code_m(Module,Nom_regle,Obj_Att):-  	% pour traiter
     console(['connecte_code_m : Obj_Att=',Obj_Att]),
	atomic_list_concat([Obj,Att],' - ',Obj_Att),   	% concat decomposant !!
	connecte_code(Module,Nom_regle,(Obj,Att)),
     console(['connecte_code_m   Regle:',Module,Nom_regle,'  Connectée à ',Obj_Att]),
	!.


% -------------------------------
%  delete_rule : efface la règle Rule_name connectée à un Obj,Att
delete_rule(Rule_name):-
	valeur(Rule_name,rule_module,Module),
	delete_rule(Module,Rule_name),
	!.

delete_rule(Module,Rule_name):-
	valeur_locale(Rule_name,connect_to,Connection),
	confirmer(['Effacer la règle',Rule_name,' Module :',Module,Connection],yes),
	Connection=(Obj,Att),
	% effacer les données sur Obj,Att
	reset_des(Obj,Att,rule),
	reset_des(Obj,Att,si_besoin),
	reset_val(Obj,Att),
	% effacer objet : Rule_name
	kill(Rule_name),
	% effacer les données dans le Module
	Module:retractall(lien_regle(Module,Rule_name,Connection)),
	% effacer la regle dans le module
	atomic_list_concat([Rule_name,'_regle'],Rule_funct),
	Rule_Head =..[Rule_funct,_Val],
	Module:retractall(Rule_Head),
	% effacer l'appel recursif de la regle
	atomic_list_concat([Rule_name,'_recur'],Recur_funct),
	Module:current_predicate(Recur_funct,Recur_Head),
	Module:retractall(Recur_Head),


	console(['  Regle:',Rule_name,' effacée']),
	console(['  Objet,Att:',Connection,'  données effacées']),
	console(['Pour VALIDER complètement faire : sauver_regles.',nl]),
	!.
delete_rule(Module,Rule_name):-
	console(['[CANCELED] Delete_rule :',Rule_name,'  Mod:',Module]).

delete_rule(Module,Rule_name,_Connection):-   % turn around bad start coding xpce
	delete_rule(Module,Rule_name).

% -------------------------------
sauver_module_regles(Module):-
	valeur_locale(Obj_origine,rule_module,Module),
	valeur(Obj_origine,sec,Sec),
	atomic_concat('sec_',Sec,Sec_dir),
	working_directory(Cur_dir,Sec_dir),
	sauver_module_regles_de_sec(Module),
	working_directory(_,Cur_dir),!.


sauver_module_regles_de_sec(Module):-
	valeur_locale(Obj_origine,rule_module,Module),
	atomic_concat(Obj_origine,'.rgl',Rule_Sec),
	sauver_sec(sec(X::(X=Obj_origine;herite_de(X,Obj_origine))),
		   Rule_Sec,Rule_Sec,silent),
	atomic_list_concat(['./',Rule_Sec,'/',Module,'.pl'],Fichier),
	sauve_module_fichier(Module,Fichier),        % source dans 00_util [à compléter]
	pere_de(Obj_pere,Obj_origine),
	(   valeur(Obj_pere,modules_regles,L_modules)-> true ; L_modules =[]),
	ord_add_element(L_modules,Obj_origine,N_modules),
	set_val(Obj_pere,modules_regles,N_modules),
       console(['Fin de sauver_module_regles ']),
	!.


% redefinir le principe de charger_module_regles selon le besoin
%
charger_module_regles(Regle):-
	valeur(Rule_root,modules_regles,Regles),
	member(Regle,Regles),!,
	valeur(Rule_root,sec,Sec),
	atomic_concat('sec_',Sec,Sec_dir),
	working_directory(Cur_dir,Sec_dir),
	charger_module_regles_de_sec(Regle),
	working_directory(_,Cur_dir),
	!.

charger_module_regles(Regle):-
	objet_qq(Regle),
	write('Allready loaded : '),write(Regle),nl.


charger_module_regles_de_sec(Regle):-
	% not(objet_qq(Regle)),  % évite les chargements successifs
	atomic_concat(Regle,'.rgl',Rule_sec_name),
	inclure_sec(Rule_sec_name),
	valeur(Regle,rule_module,Module),
	atomic_list_concat(['./',Rule_sec_name,'/',Module,'.pl'],Fichier),
	consult(Fichier),    % utiliser load_files(Files, <Options>) si besoin
	pere_de(Obj_pere,Regle),
	(   valeur(Obj_pere,modules_regles,L_modules)-> true ; L_modules =[]),
	ord_add_element(L_modules,Regle,N_modules),
	set_val(Obj_pere,modules_regles,N_modules),
	write('Fin de Charger_module_regles '),nl,
	!.
charger_module_regles_de_sec(_Regle):-!. % true in fine


connecter_module_regles(Objet):-
	Objet=Module ,                 % sert de controle obj_Regle = Module_name
	valeur_locale(Objet,rule_module,Module),
	forall( ( pere_de(Objet,Rule_name) ,
		  valeur_locale(Rule_name,connect_to,Obj_att)),
		connecte_code(Objet,Rule_name,Obj_att) ),
	% ? utilisation de  Module:lien_regle(Module,Regle, Obj_att)
	% forall( Module:lien_regle(Module,Regle, Obj_att),
	%	connecte_code(Module,Regle,Obj_att)  ),
	!.


% ------------------------------------------------------------------------
% purge_module(Module) % A placer peut être dans fichier séparé [00_util] ?
purger_module(Module):-
	current_predicate(_,Module:Pred),
	\+ predicate_property(Module:Pred, imported_from(_)),
	\+ predicate_property(Module:Pred, built_in),
	retractall(Module:Pred),
	fail.
purger_module(Module):-
	console(['Module :',Module,'purgé']).


% ------- FIN -------------

