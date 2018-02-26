% file	:  51b_play_with_pip
% AUTHOR :
% VERSION : 0.93 for SWI-PL
% UPDATE : 28/10/2015
% ---  prepare une serie pour tester les calculs de PiP
%	serie : a_trend
%	longeur de la serie : Longueur à partir de Date

set_up_pip_test:-
	get_date_serie(djstoxx_eur,serie,D,S),
	set_a_trend_val(djstoxx_eur-serie,D,S).

set_up_pip_test(Longueur):-
	integer(Longueur),
	get_date_serie(djstoxx_eur,serie,D,S),
	get_sublist(Longueur,D,Dates,_),
	get_sublist(Longueur,S,Serie,_),
	set_a_trend_val(djstoxx_eur-serie,Dates,Serie),
	!.
set_up_pip_test(Obj):-
	get_date_serie(Obj,serie,D,S),
	set_a_trend_val(Obj-serie,D,S),
	!.

set_up_pip_test(From,To):-
	get_date_serie(djstoxx_eur,serie,D,S),
	extract_from_to(D,S,From,To,Dates,Serie),
	set_a_trend_val(djstoxx_eur-serie,Dates,Serie),
	!.

set_up_pip_test(Obj,From,To):-
	get_date_serie(Obj,serie,D,S),
	extract_from_to(D,S,From,To,Dates,Serie),
	set_a_trend_val(Obj-serie,Dates,Serie),
	!.

%-----------------------------
%
get_date_serie(Obj,_Att,D,S):-
	valeur(Obj,serie,S),
	valeur(Obj,dates,D).
get_date_serie(Obj,Att,Dates,Serie,D_S):-
	valeur(Obj,Att,Serie),
	valeur(Obj,dates,Dates),
	pairs_keys_values(D_S,Dates,Serie).


set_a_trend_val(O-A,D,S):-
	set_val(a_trend,serie,S),
	set_val(a_trend,dates,D),
	set_val(a_trend,source,O-A),
	reset_pips(a_trend).

reset_pips(Obj):-
	reset_val(Obj,cur_pip(_,_)),
	reset_val(Obj,pip_histo(_)),
	!.

% ---
%
pip_silent:-
	set_pip_plotter(no_plot),
	pip_report_store(no),
	nl.
pip_verbose:-
	set_pip_plotter(p1),
	pip_report_store([pip,histo]),
	nl.


/* ----------------------------------------------------------
split_H_T(DS,Nbj_depart,Longueur,Head,Tail):-
		get_sublist(Nbj_depart,DS     ,Head,DS_temp),
		get_sublist(Longueur  ,DS_temp,Tail,_).
------------------------------------------------------------- */

% --- extrait Dates et Serie entre deux dates
%
extract_from_to(D,S,From,To,Dates,Serie):-  % D is decreasing
	(integer(From)-> Nb_from is From ;nb_jour(From,Nb_from)),
	(integer(To)-> Nb_to is To ;nb_jour(To,Nb_to)),
	Max is max(Nb_from,Nb_to),
	Min is min(Nb_from,Nb_to),
	extract_skip_until_max(D,S,Max,T_D,T_S),
	extract_until_min(T_D,T_S,Min,Dates,Serie).

% coupe la fin des séries D et S [D,D-1,..,Min]|skipped]
extract_until_min([],_,_,[],[]):-!.
extract_until_min([Dtn|_],_,Min,[],[]):- Dtn < Min,!.
extract_until_min([Dtn|D_S],[S|T_S],Min,[Dtn|D_Sn],[S|T_Sn]):-
	extract_until_min(D_S,T_S,Min,D_Sn,T_Sn).

% skip le début des series D et S [.skipped|[Max,Max-x,..,Last]
extract_skip_until_max([],_,_,[],[]):-!.
extract_skip_until_max([D|D_T],S,Max,[D|D_T],S):- D =< Max,!.
extract_skip_until_max([D|D_T],[_|S_T],Max,Dates,Serie):-
	D > Max,
	extract_skip_until_max(D_T,S_T,Max,Dates,Serie).

%------------------
%
first_ip(O,A,I_Points):-
	% calcule le premier Important Point d'une serie
	% sera le point de départ des calculs subséquents
	true.




% --- try_pip -----
%
try_pip(Date,Pips):-    % new
	Obj=a_trend,
	Att=serie,
	pip_plotter(Plotter),
	(integer(Date)-> Nb_from is Date ;nb_jour(Date,Nb_from)),
	get_date_serie(Obj,Att,D,S),
	memberchk(Nb_from,D),  % fail sinon
	( Plotter = no_plot
	-> true;
	   plot_indic(Obj,Att,Plotter,color:gray50)),
	% get_date_serie(Obj,Att,D,S),
	% (integer(Date)-> Nb_from is Date ;nb_jour(Date,Nb_from)),
	extract_skip_until_max(D,S,Nb_from,Dates,Serie),
	pairs_keys_values(D_S,Dates,Serie),
	D_S=[Date_ref-_|_],
	calcule_pips(D_S,Pips),
	traite_pips(Obj,Att,Pips),

	store_pips(Obj,Att,Date_ref,Pips,
		   [Dt,Dur,Bps,Corr]:cur_pip(Dt,Dur,Bps,Corr), % extracteur
		   true,       % code a executer (ie Corr > 0.9)
		   cur_pip),
	store_pips_histo(Obj,Att,Date_ref,Pips),
	!.
try_pip(_Date,[]).



explore_pip(From,To):-   % calcul pips sur une plage de dates
	Frequence=1,	 % incrément en jours (calendaires)
	reset_pips(a_trend),
	reset_allready_calculated,
	nb_jour(To,Nb_to),
	% pip_report_store(no),	  %reporting de calcul yes/no/pip/histo
	nb_jour(From,Nbj_date_etude),
	To_tech is round((Nb_to-Nbj_date_etude)/Frequence),
	between(0,To_tech,I),
	N_nbj_date_etude is Nbj_date_etude+(I*Frequence),
	% console([i, N_nbj_date_etude]),
	try_pip(N_nbj_date_etude,_Pips).

explore_pips:-
	set_up_pip_test,
	reset_allready_calculated,
	% pip_report_store([histo,pip]),
	% set_pip_plotter(p1),
	valeur(a_trend,dates,[Date_last|Dates]),
	sort(Dates,[D_oldest|_]), % manière rapide de trouver la date la plus ancienne
	From_date is D_oldest +10,
	nb_jour(From,From_date),
	nb_jour(To,Date_last),
	console(['calcule les pips sur la période :',From,'à',To]),
	!,
	time((explore_pip(From,To),fail)).



% ------------------------------------------
% pip_histo_report(pip(D,Dur,Bps,Co),(Dur<5230,Dur>11,X is Dur*Bps,X>2000),3).
% time(pip_histo_report(pip(D,Dur,Bps,Co),(Dur<130,Dur>61),3)).
%
pip_histo_report(Template,Code):-
	pip_histo_report(Template,Code,1).
pip_histo_report(Template,Code,Key):-
	pip_histo_report(Template,Code,Key,SBag),
	console(['FOUND :'|SBag]).
pip_histo_report(Template,Code,Key,SBag):-
	Template=pip(D,Dur,Bps,Cor),
	valeur(a_trend,pip_histo(serie),V),
	length(V,L),
	% console(['pip_histo lenght:',L,'       Code:',Code]),
	findall(pip(D,Dur,Bps,Cor),(member(pip(D,Dur,Bps,Cor),V),Code),Bag),
	sort(Key,@=<,Bag,SBag), % sort(Bag,SBag),  %en 6.4
	% length(Bag,LBag),
	% console(['nb_elemnts correspondant dans pip_histo:',LBag]),
	!.





% ---------------------------------------------
% analyse_pips(Date,Pips).   % en fait : pips_similaires
% recherche et affiche pips similaire
%
analyse_pips(Date):-analyse_pips(Date,_Pips). %
analyse_pips(Date,Pips):-
	Plotter=p2,
	Obj=a_trend,Att=serie,
	get_date_serie(Obj,Att,_D,_S,D_S),

	try_pip(Date,Pips),
	plot_indic(Obj,Att,Plotter,color:skyblue3),
	sort(2,@=<,Pips,SPips),
	member(Pip,SPips),
	(Pip=cur_pip(_Dtp,Durp,Bpsp,_Corp)), % ;Pip= pip(Dtp,Durp,Bpsp,Corp)
	console([nl,'SEARCHED :' ,Pip]),
	(   Durp=1,abs(Bpsp)<100 -> fail;true),
	MaxDur is Durp+abs(Durp*0.15), MinDur is Durp - abs(Durp*0.15),
	MaxBps is Bpsp+abs(Bpsp*0.15), MinBps is Bpsp - abs(Bpsp*0.15),

	pip_histo_report(pip(_Dt,Dur,Bps,_Cor),
			 (MinDur<Dur,Dur<MaxDur,MinBps<Bps,Bps<MaxBps),2,Pips_histo),
	affiche_pips(Obj,Att,D_S,Pips_histo,Plotter),
	% Pip sera remplacé par Pips (lorsque findall)
	pips_similaires(Obj,Att,D_S,Pip,Pips_histo,Plotter),
	true.

analyse_pips_v2(Date):-analyse_pips_v2(Date,_Pips). %
analyse_pips_v2(Date,Pips):-
	Plotter=p2,
	Obj=a_trend,Att=serie,
	get_date_serie(Obj,Att,_D,_S,D_S),
	try_pip(Date,Pips),
	nb_jour(Date,Cur_nbj),
	plot_indic(Obj,Att,Plotter,color:skyblue3),
%
	findall(Pips_histo,
		(   member(Pip,Pips),
		    Pip=cur_pip(_Dtp,Durp,Bpsp,_Corp),
		    (   Durp=1,abs(Bpsp)<100 -> fail;true),
		    MaxDur is Durp+abs(Durp*0.15),
		    MinDur is Durp - abs(Durp*0.15),
		    MaxBps is Bpsp+abs(Bpsp*0.15),
		    MinBps is Bpsp - abs(Bpsp*0.15),
		    pip_histo_report(pip(_Dt,Dur,Bps,_Cor),
				( MinDur<Dur,Dur<MaxDur,MinBps<Bps,Bps<MaxBps),
				  2,Pips_histo)),
		Bag_pips_histo),
	flatten(Bag_pips_histo,F_Bag),
	% console([bag_pips_histo,F_Bag]),
	sort( 1,@=<,F_Bag,S_Bag_pips_histo),
	length(S_Bag_pips_histo,Lg_s_bag),
	% console([nl,Lg_s_bag,'   S_bag_pips_histo : ',S_Bag_pips_histo]),
	double_pip_histo(S_Bag_pips_histo,Gr_pips_histo),
	length(Gr_pips_histo,Lg_r),
	console([Lg_r,'   RRRRR : ',Gr_pips_histo]),

	pips_similaires_v2(Obj,Att,D_S,Cur_nbj,Gr_pips_histo,Plotter),
	true.

double_pip_histo(S_Bag_pips_histo,Grouped_R):-
	findall(Date-[Dur,Bps,Cor],
		(   member(pip(D,Dur,Bps,Cor),S_Bag_pips_histo),
		    Date is D+Dur),
		R),
	group_pairs_by_key(R,Grouped_R).

% --------------------------------------
%
affiche_pips(_Obj,_Att,D_S,Pips,Plotter):-  % pour pip_histo et cur_pip
	length(Pips,Lg),
	Pip_level is Lg mod 9, % tempo pour calculer la couleur
	% console(['FOUND :',Lg,'  in Pips (histo) : ',Pips]),
	affiche_pips(_Obj,_Att,D_S,Pips,Plotter,Pip_level).

affiche_pips(_Obj,_Att,D_S,Pips,Plotter,Color_pip_level):-  % pip_histo ; cur_pip
	% Pips =[pip(Dt,Dur,Bps,Cor)|cur_pip(Dt,Dur,Bps,Cor)]
	% deroule les Pip de Pips
	forall(( member(Pip,Pips),
		(Pip=pip(Dt,Dur,Bps,Cor) ; Pip=cur_pip(Dt,Dur,Bps,Cor)),
		% recherche valeur(serie) Y sur D_S
		memberchk(Dt-Y,D_S),
		Dt_fin is Dt+Dur,
		Y_fin is (Y*(1+(Bps/10000))**Dur)),
	       affiche_pip(Color_pip_level,Plotter,[Dt,Dt_fin],[Y,Y_fin])),
	flush_pip(Plotter).

%----------------------------------------
% recherche et affiche l'extension des pips_similaires trouvés
%
pips_similaires(_Obj,_Att,_D_S,_Pip,[],_Plotter):-!.
pips_similaires(Obj,Att,D_S,Pip,Pips_histo_curr,Plotter):-
	% console([pips_similaires,Pip,' \t Pip_histo :',Pips_histo]),
	Pip=cur_pip(Dtp,Durp,_Bpsp,Corp),
	Corp<0.10 ,  % si Coeff explication < X ne pas chercher les similaires
	console(['      pips_similaires pas assez explicatif :',Pip]),
	!.
pips_similaires(Obj,Att,D_S,Pip,Pips_histo_curr,Plotter):-
	% console([pips_similaires,Pip,' \t Pip_histo :',Pips_histo]),
	Pip=cur_pip(Dtp,Durp,_Bpsp,_Corp),
	Dtn is Dtp+Durp,
	valeur(Obj,pip_histo(Att),All_pips_histo),

/*	findall(pip(Dtn,DurX,BpsX,CorX),
		(   member(pip(Dth,Durh,_Bpsh,_Corh),Pips_histo_curr),
		    DtX is Dth+Durh
	%	    ,show_pip_route(Dtn,DtX,D_S,All_pips_histo)
		)
		,_),
*/
	findall(pip(Dtn,DurX,BpsX,CorX),
		(   member(pip(Dth,Durh,_Bpsh,_Corh),Pips_histo_curr),
		    DtX is Dth+Durh,
		    member(pip(DtX,DurX,BpsX,CorX),All_pips_histo)
		)
		,Pips_next_bag),
	length(Pips_next_bag,Lg_color),
	console([Lg_color,' NEXT pips \t:',Pips_next_bag]),

	affiche_pips(Obj,Att,D_S,Pips_next_bag,Plotter,Lg_color),
	!.

show_pip_route(Cur_date,Search_date,D_S,All_pips_histo):-
	memberchk(Cur_date-Val,D_S),
	findall(Date-Valeur,
		( member(pip(Search_date,DurX,BpsX,_CorX),All_pips_histo),
		  Date is Cur_date+DurX,
		  Valeur is Val*((1+(BpsX/10000))**DurX)),
		Bag),
	pairs_keys_values(Bag,Dates_next,S_next),
	length(Dates_next,Lg),
	affiche_pip(Lg,p2,[Cur_date|Dates_next],[Val|S_next]),

	!.

pips_similaires_v2(Obj,Att,D_S,Date_cur,Pips_histo_cur,Plotter):-
	% Pips_histo_curr=[D-[[Dur,Bps,Cor]|..]
	% console([pips_similaires_v2,' \t Pip_histo :',Pips_histo_cur]),
	Dtn is Date_cur,
	valeur(Obj,pip_histo(Att),All_pips_histo),

	findall(pip(Dtn,DurX,BpsX,CorX),
		(   member(Dth-Pips_histo_elmnt,Pips_histo_cur),

		    member([Durh,_Bpsh,_Corh],Pips_histo_elmnt),
		    DtX is Dth+Durh,
		    member(pip(DtX,DurX,BpsX,CorX),All_pips_histo)
			,console([DtX,DurX,BpsX,CorX])
		)
		,Pips_next_bag),
	length(Pips_next_bag,Lg_color),
	console(['Next Pipsv2 Lg:',Lg_color,nl,next_pips,Pips_next_bag]),

	affiche_pips(Obj,Att,D_S,Pips_next_bag,Plotter,Lg_color),
	!.

/* ----------------------------------------------------------------------------
%  commandes pour les test
set_up_pip_test.
pip_silent.
explore_pip(1/10/01,1/10/02),fail.  % se termine par fail (normal)
pip_verbose.
analyse_pips(2/10/02).
analyse_pips(2/10/03).
valeur(a_trend,pip_histo(serie),V),length(V,Lg).

------------------------------- */
pip1:-   console(['set_up_pip_test(1/1/00,31/12/06)  env. 50 secs.']),
	set_up_pip_test(1/1/00,31/12/06),
	pip_silent,
	(  ( explore_pip(1/10/01,1/04/06),fail);true),
	valeur(a_trend,pip_histo(serie),S),length(S,L),
	pip_verbose,
	console([pip_histo_length,L]),
	analyse_pips(3/04/06).

pip2:-   console(['set_up_pip_test(1/9/00,31/12/07)  env. 50 secs.']),
	set_up_pip_test(1/9/00,31/12/07),
	pip_silent,
	(  ( explore_pip(1/10/01,1/02/07),fail);true),
	valeur(a_trend,pip_histo(serie),S),length(S,L),
	pip_verbose,
	console([pip_histo_length,L]),
	analyse_pips(5/02/07).

pip3:-   console(['set_up_pip_test(1/9/00,31/12/10)  env. 70 secs.']),
	set_up_pip_test(1/9/00,31/12/10),
	pip_silent,
	(  ( explore_pip(1/10/01,1/02/08),fail);true),
	valeur(a_trend,pip_histo(serie),S),length(S,L),
	pip_verbose,
	console([pip_histo_length,L]),
	analyse_pips(5/02/08).

pip4:-   console(['set_up_pip_test  env. 4 min.']),
	set_up_pip_test,
	pip_silent,
	(  ( explore_pip(1/10/01,1/11/15),fail);true),
	valeur(a_trend,pip_histo(serie),S),length(S,L),
	pip_verbose,
	console([pip_histo_length,L]),
	analyse_pips(2/11/15).

pip5:-   console(['set_up_pip_test  env. 2 min.']),
	set_up_pip_test(1/07/07,31/12/15),
	pip_silent,
	(  ( explore_pip(13/07/07,1/2/15),fail);true),
	valeur(a_trend,pip_histo(serie),S),length(S,L),
	pip_verbose,
	console([pip_histo_length,L]),
	analyse_pips(2/02/15).

% --- END
