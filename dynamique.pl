% FILE   : dynamique.pl
% AUTHOR :
% VERSION: 0.91 for SWI-PL
% UPDATE : 04/02/2005
% PURPOSE: Outil d'analyse pour series chronologiques


:- ensure_loaded(emione).
:- set_prolog_flag(float_format,'%.2f').

go_produits :-
	load_base_dstr(symphony_bonds),
	load_base_dstr(produits),
	load_base_dstr(analyses),
	console(['go_produits : done']),
	!.

go_usa :-load_base_dstr(symphony_usa),
	console(['go_usa : done']).

go_anima :-
	load_base_dstr(anima_eur),
	load_base_dstr(anima_2_eur),
	console(['Anima_eur chargée']).

go_anie :-
	load_base_dstr(bonds_usa),
	load_base_dstr(anie_usa),
	load_base_dstr(anie_ch),
	load_base_dstr(anie_eur),
	load_base_dstr(anie_de),
	load_base_dstr(anie_fr),
	load_base_dstr(anie_uk),
	load_base_dstr(anie_jp),
	load_base_dstr(anie_cpr),
	% load_base_dstr(anie_cn),
	console(['Anie databases loaded']).

go_symphony :-
	go_produits,
	go_usa,
	go_anima,
	load_base_dstr(symphony_eur),
	console(['go_symphony : done']).

go_vh :-
	load_base_dstr(vh_uk),
	load_base_dstr(vh_ch),
	load_base_dstr(vh_em),
	load_base_dstr(vh_jpn),
	console(['VH data for uk, jpn, uk and ch loaded']).

% ------------- les sauvegardes des SEC
sauver_emione:- sec_save(emione).
save_emione:- sec_save(emione,silent).

sauver_symphony:-sec_save(symphony).
save_symphony:-sec_save(symphony,silent).

sauver_anie:- sec_save(anie).
save_anie:- sec_save(anie,silent).

% ---- anciennes versions pour sauver qqs SEC
sauver_tout:-
	today(date(A,M,J),_),
	atomic_list_concat(['save_complet_',A,'_',M,'_',J,'.sec'],Sec_name),
	sauver_sec(sec(X::(objet_qq(Y), \+fils_de(Y,_),(X=Y;herite_de(X,Y)))),
		   Sec_name,'Ensemble complet Emione & symphony').


sauver_regles:- sauver_module_regles(rgls_symph).

% fin : sortie avec sauvegarde de symphony et du module de règles
fin :-  sauver_symphony,
	halt.


% ---------------------------------------------------------
% demo initiale
demo(Obj):- demo(Obj,d_signal).
demo_new(Obj):- objet(Obj,symphony_obj),  % pour les objets symphony(varihedge)
	demo(Obj,s_signal),!.
demo_new(Obj):- objet(Obj,indic),         % pour les objets Anie
	demo_anie(Obj,signal),
	!.

% --- graphiques pour ANIE
demo_anie(Obj,signal):-
	(   descripteur(Obj,historique_signal,ref_id,Obj_ref-Att_ref);
	    valeur(Obj,explains,Obj_ref-Att_ref) ),
	plot_indic(Obj,historique_signal,p1),

	plot_indic(Obj,historique_signal,p2),  % historique_signal en fond
	plot_complete(Obj_ref,Att_ref,p2),
	console(['demo_anie for Anie object :',Obj]),
	!.
demo_anie(Obj,signal):-
	plot_indic(Obj,historique_signal,p1),
	!.
demo_anie(Obj,Att):-
	plot_indic(Obj,Att,p1),
	!.

% --- graphiques pour VariHedge
demo(Obj,Signal) :-
	s_indic dont obj_name=Obj et Signal=S et evaluation de Signal = Eval ,
	plot_indic(Obj,Signal,p1),
	plot_complete(Obj,serie,p1),
	S=[S1|_],
	Eval=[E1|_],
        console(['Indicateur :	',Obj,'      Signal= ',S1,'   Eval= ',E1]),
	plot_indic(Obj,Signal,evaluation,p2),
	plot_add(Obj,Signal,ref_evaluation,p2),
	plot_add(Obj,Signal,bench_evaluation,p2,color:orangered),
	affiche_signal(Obj,Signal),
	!.

% plot_type
%
plot_type(eval_signal,Obj,Att_signal,Emi_Plotter):-
	plot_eval_signal(Obj,Att_signal,Emi_Plotter).

plot_type(signal_serie,Obj,Att_signal,Emi_Plotter):-
	plot_serie_signal(Obj,Att_signal,Emi_Plotter).

% plot_eval_signal :
%
plot_eval_signal(Obj,Att_signal,Emi_Plotter):-
      % console(['plot_eval_signal ENTREE :',Obj,Att_signal,Emi_Plotter]),
	% valeur_locale(Emi_plotter,id,Plotter),
	plot_indic(Obj,Att_signal,evaluation,Emi_Plotter),
	plot_add(Obj,Att_signal,ref_evaluation,Emi_Plotter,color:orangered),
	% sur indic de base : pas de bench, id_ref sur s_signal en fait office
       console(['plot_eval_signal SORTIE :',Obj,Att_signal,Emi_Plotter]),
	!.

plot_serie_signal(Obj,Att_signal,Emi_Plotter):-
      % console(['plot_serie_signal ENTREE :',Obj,Att_signal,Emi_Plotter]),
	plot_indic(Obj,Att_signal,Emi_Plotter),
	plot_complete(Obj,serie,Emi_Plotter),
       console(['plot_serie_signal OUT :',Obj,Att_signal,Emi_Plotter]),
	!.

% plot_eval_produit :
%
plot_eval_produit(Obj,Att_signal,Emi_Plotter):-
      % console(['plot_eval_produit ENTREE :',Obj,Att_signal,Emi_Plotter]),
	% valeur_locale(Emi_plotter,id,Plotter),
	plot_indic(Obj,Att_signal,evaluation,Emi_Plotter,color:blue2),
	plot_add(Obj,Att_signal,ref_evaluation,Emi_Plotter),
	plot_add(Obj,Att_signal,bench_evaluation,Emi_Plotter,color:orangered),
       console(['plot_eval_produit SORTIE :',Obj,Att_signal,Emi_Plotter]),
	!.


% plot evaluation de ObjAtt
%
plot_eval_objatt(Obj,Att) :-
	plot_indic(Obj,Att,p1),
	plot_complete(Obj,serie,p1),
	plot_indic(Obj,Att,evaluation,p2),
	plot_add(Obj,Att,ref_evaluation,p2),
	affiche_signal(Obj,Att),
	!.


% affiche_signal ------------------------------------------
%
affiche_signal(Obj,Signal):-
	obj_name=Obj et Signal en (t=D) = _S,
	genere_liste_de_dates(D,21,L_Dates),
	send(@t,value,Obj),
	send(@signaux,clear),
	send(@signaux,append,
	     dict_item(test_1,string('%s\t%s\t%s','Date','Signal','Valeur'),arial)) ,
	findall(	Dt-Sig-Val ,
		(member(Dt,L_Dates),
		obj_name=Obj et Signal en (t=Dt) = Sig et valeur de serie en (t=Dt)=Val ,
		date_imprimable(Dt,Date),
		send(@signaux,append,
		     dict_item(test_1,string('%s\t%s\t%.3f',Date,Sig,Val),arial)) ),
		_Bag) ,
      % console([' affiche_signal Bag:',Bag]),
	!.

genere_liste_de_dates(D_from,Nb,L_Dates):-
	nb_jour(D_from,Nb_j_from),
	Nb_j_to is Nb_j_from - Nb+1,
	findall(	Dt,(	tic(Nb_j_from,Nb_j_to,-1,Nb_j),
			nb_jour(Dt,Nb_j)	),
		L_Dates).

% ----------------------------

load_base_dstr(Dstr_DB):-
	connecte(Dstr_DB),
	load_base,
	odbc_disconnect(db),
	console(['load_base_dstr : ',Dstr_DB,' done']),
	!.


load_base :-
	odbc_query(db,'select Mnemo,Datatype,table_dest,pays,frequence,
	               update_status,LAST_UPDATE from 0_DS_INFO',
		   row(Mnemo,Datatype,Table,Pays,Freq,succes,D_Update)),
	atomic_list_concat([Table,'_',Pays],Id_dyn_pays),
	check_object(Id_dyn_pays,Table,Mnemo,Datatype,Pays,Freq),
	D_Update= timestamp(AA, MM,JJ,_,_,_,_),
	nb_jour(JJ,MM,AA,Nbj_Update),
	load_table_id_dyn(Table,Id_dyn_pays,Freq,Nbj_Update),
	% console(['Loaded : ',Id_dyn_pays]),
	console(['.',no_nl]),
	fail.
load_base:-
	console([' Fin']),
	!.



check_object(Id_dyn_pays,_Table,_Mnemo,_DataType,_Pays,_Freq):-
	valeur_locale(_Obj,id_dyn,Id_dyn_pays),
	% stocker_memo(Obj,Mnemo,Pays,Freq), % ? utilité +-
	% si id_dyn existe ==> l'Objet a été créé avec les attributs :
	%  id_dyn / memo_ds / frequence / pays
	!.

check_object(Id_dyn_pays,_Table,Datatype,Mnemo,Pays,Freq):-  % pour Indic d Anie
	(   valeur_locale(Obj_name,memonique,Mnemo);
	    valeur_locale(Obj_name,memo_ds,Mnemo)),
	valeur_locale(Obj_name,datatype_ds,Datatype),
	stocker_memo(Obj_name,Id_dyn_pays,Mnemo,Datatype,Pays,Freq),
	!.

% temporaire pour pays = cpr, ger,fr
check_object(Id_dyn_pays,Table_evt_Maj,Mnemo,Datatype,Pays,Freq):-
	( Pays=cpr ; Pays=ger ; Pays= fr),!,
	downcase_atom(Table_evt_Maj,Table),
	concat_atom([Table,'_',Pays],Obj_name),
	creer_obj(Obj_name,[indicateur]),
	stocker_memo(Obj_name,Id_dyn_pays,Mnemo,Datatype,Pays,Freq),
	!.


check_object(Id_dyn_pays,Table_evt_Maj,Mnemo,Datatype,Pays,Freq):-
	downcase_atom(Table_evt_Maj,Table),
	concat_atom([Table,'_',Pays],Obj_name),
	creer_obj(Obj_name,[s_serie_raw]),
	stocker_memo(Obj_name,Id_dyn_pays,Mnemo,Datatype,Pays,Freq),
	!.


stocker_memo(Obj,Id_dyn_pays,Mnemo,Datatype,Pays,Freq):-
	set_val(Obj,id_dyn,Id_dyn_pays),
	set_val(Obj,memo_ds,Mnemo),
	set_val(Obj,datatype_ds,Datatype),
	set_val(Obj,pays,Pays),
	ds_freq(Freq,Frequence),
	set_val(Obj,frequence,Frequence),
	!.

ds_freq('D',daily).
ds_freq('W',weekly).
ds_freq('M',monthly).
ds_freq('Q',quarterly).
% Anie
ds_freq(d,daily).
ds_freq(w,weekly).
ds_freq(m,monthly).
ds_freq(q,quarterly).

% ----------------------------------------------------------
test :- test(s_signal).

test(Signal) :-	valeur_locale(Obj,regle_dyn,_Rule),
     %console(['Indicateur :',Obj]),
	demo(Obj,Signal),
	atomic_list_concat(['documents/',Signal,'_',Obj,'.',emf],File),
	sauver_graphique(f1,File),
	fail.
test(_):- !.	% true in fine.


test_s_signal:-
	L=[sentiment_eur,technique_eur,couverture_eur],
	forall(member(Obj,L),
	       test_s_signal(Obj)).

test_s_signal(Obj):-
	valeur(Obj,composants,Composants),
	forall(member(X,Composants),
	       test_s_signal(X)),
	test_s_sig(Obj),!.
test_s_signal(Obj):-
	test_s_sig(Obj).

test_s_sig(Obj) :-
	console(['testing ',Obj,no_nl]),
	demo(Obj,s_signal),
	console(['   End of testing : ',Obj]),
	atomic_list_concat(['documents/',s_signal,'_',Obj,'.',emf],File),
	sauver_graphique(f1,File),
	!.



% -------------------------------------
% lancement d'access
% la base : analys.mdb  contient la macro et la fonction permettant de controler la maj de la base
% "C:\Program Files\microsoft office\office10\msaccess.exe" "U:\workarea\Mandat Dynamique\Bases_DSTR\analys.mdb" /Cmd maj
%  shell('"C:/Program Files/microsoft office/office10/msaccess.exe" "//GVAS000RES05.CH.NET.INTRA/abej01a$/WORKAREA/Mandat Dynamique/Bases_DSTR/analys.mdb" /x maj_and_quit',R).
%  win_exec('"C:/Program Files/microsoft office/office10/msaccess.exe" "//GVAS000RES05.CH.NET.INTRA/abej01a$/WORKAREA/Mandat Dynamique/Bases_DSTR/analys.mdb" /x maj_and_quit', show).
%



