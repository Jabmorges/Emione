% file	:	51_pipts.pl
% AUTHOR :
% VERSION :	0.93 for SWI-PL
% UPDATE :	10/02/2013  & 29/10/2015
% PURPOSE:	Perceptually important Points

% L'idée est de faire un outil d'analyse technique basé sur des patterns
% de mouvement soit à long terme, moyen ou court terme.
% Il faudrait avoir une "théorie" ou des règles d'expertise décrivant
% des enchainements de patterns (prologement ou renversement de trends)
% cet outillage permettrait aussi développer l'analyse basée sur le
% forces d'inertie, les notions de tension en utilisant les points
% d'inflexion.
% Il y a également les enchainements de pattern (la balle qui tombe
% dans les escaliers ...)
%
% Bref il y a de la matière à creuser....

:- ensure_loaded('51b_play_with_pip').

:- ensure_loaded('50_geometrie_plane').
:- ensure_loaded(emione).

 :-set_prolog_flag(optimise,true).

% -- parametres
couleurs([red, dark_blue,dark_orange, dark_green,red,
	  dark_blue,orange,green,tomato,pink3,skyblue4,cyan4]).
pip_depth(30).  % profondeur max du calcul des Pips (6-10 / 30)

% -- optimisation calcul
:- dynamic pip_allready_calculated/1.
reset_allready_calculated:-
	retractall(pip_allready_calculated(_)).


% plotter contrl
:- dynamic pip_plotter/1.
pip_plotter(p1).   % no_plot
set_pip_plotter(Plotter):-  % Plotter = [p1,p2,no_plot]
	retractall(pip_plotter(_)),
	assert(pip_plotter(Plotter)).

% pilote le reporting du calcul des pips store_pips
%
:- dynamic pip_rep_store/1.
pip_rep_store(yes).
pip_report_store(Reports):- % Reports = [yes;histo;pip;Var] --> affichage
	is_list(Reports),
	retractall(pip_rep_store(_)),
	forall(member(X,Reports),asserta(pip_rep_store(X))),!.
pip_report_store(YesNo):- % YesNo = yes;histo;pip;Var --> affichage
	retractall(pip_rep_store(_)),
	asserta(pip_rep_store(YesNo)),!.
% --------------------------
%
test_pip(O,Depth):-  % plus utilisé ?
	A =serie,
	plot_indic(O,A,p2),
     console(['... test_pip(',O,',',Depth,')']),
	pip(O,A,Depth,p2,Date_ref,Pips),
	length(Pips,Lg_res),
	% msort(Pips,_Pips_sorted_1),
	console(['Objet:',O,'  qte:',Lg_res,nl]),
	traite_pips(O,A,Pips),
	store_pips(O,A,Date_ref,Pips,
		   [D,Lg,Bps,Coef]:cur_pip(D,Lg,Bps,Coef),
		   true,cur_pip),
	store_pips_histo(O,A,Date_ref,Pips),
	!.


% -------------------------------------------------------
%
pip(O,A,Pips):-
	plot_indic(O,A,p1),
	pip(O,A,6,p1,_Date_ref,Pips),!.

pip(O,A,Depth,Plotter,Date_ref,[ResH,ResT|Pip_res_T]):-
	valeur(O,A,Serie),
	valeur(O,dates,Dates),
	pairs_keys_values(D_S,Dates,Serie),
	D_S=[Date_ref-_|_],
	pip_calc(1,Plotter,D_S,Date_ref,D_S_Head,D_S_Tail,
		 _X2-_Y2,_Xm-_Ym,_X1-_Y1,ResH,ResT),

     % console(['MinMax_d :',MinMax_d,' coef_ajust :',Coef_ajust,
     %	 '  coord :(',X,',',Y,')  at :',Pos]),
	pip_to_level(2,Depth,Plotter,D_S_Head,Date_ref,[],Pip_res_H),
	pip_to_level(2,Depth,Plotter,D_S_Tail,Date_ref,Pip_res_H,Pip_res_T),
	!.


calcule_pips(D_S,Pips):-
	pip_depth(Dpth),pip_plotter(Plot),
	D_S=[D_ref-_|_],

	pip_calc(1,Plot,D_S,D_ref,D_S_Head,D_S_Tail,_XY2,_XYm,_XY1,Res1,Res2),
	pip_to_level(2,Dpth,Plot,D_S_Head,D_ref,[],Pip_res_H),
	pip_to_level(2,Dpth,Plot,D_S_Tail,D_ref,Pip_res_H,Pip_res_HT),
	% exclussif l'un de l'autre
	% pip_to_level_head(2,Dpth,Plot,D_S_Head,D_ref,[],Pip_res_H),
	% pip_to_level_head(2,Dpth,Plot,D_S_Tail,D_ref,Pip_res_H,Pip_res_HT),
	Pips=[Res1,Res2|Pip_res_HT],
	flush_pip(Plot),
	!.

calcule_pips_head(D_S,Pips):-  % calcul uniquement sur Head (Liste_gauche)
	pip_depth(Dpth),
	pip_plotter(Plot),
	D_S=[D_ref-_|_],
	pip_calc(1,Plot,D_S,D_ref,D_S_Head,_D_S_Tail,_XY2,_XYm,_XY1,Res1,Res2),
	pip_to_level_head(2,Dpth,Plot,D_S_Head,D_ref,[],Pip_res_H),
	Pips=[Res1,Res2|Pip_res_H],
	flush_pip(Plot),
	!.


pip_to_level(_Level,_Depth,_Plotter,[],_,Res,Res):-!.
pip_to_level(_Level,_Depth,_Plotter,[_],_,Res,Res):-!.
pip_to_level(_Level,_Depth,_Plotter,[_,_],_,Res,Res):-!.
pip_to_level(_Level,_Depth,_Plotter,[_,_,_],_,Res,Res):-!.   % ? garder ?enlever
pip_to_level(_Level,_Depth,_Ploter,D_S,_,Res,Res):- % allready done ?
	pip_allready_calculated(D_S),  % Res
	% faire affichage ici mais il faut stocker les Resultats
	!.
pip_to_level(Level,Depth,_Plotter,_,_,Res,Res):-Level > Depth,!.
pip_to_level(Level,Depth,Ploter,D_S,Date_ref,Res_so_far,Res):-
      % length(D_S,Lg),
      % console([entree_pip_to_level,Level,Split_lg,Lg]),
	pip_calc(Level,Ploter,D_S,Date_ref,D_S_Head,D_S_Tail,_X2-_Y2,_Xm-_Ym,_X1-_Y1,ResH,ResT),
	NLevel is Level +1,
	pip_to_level(NLevel,Depth,Ploter,D_S_Head,Date_ref,[ResH,ResT|Res_so_far],Res_h),
	pip_to_level(NLevel,Depth,Ploter,D_S_Tail,Date_ref,Res_h,Res),
	assert(pip_allready_calculated(D_S_Tail)),  % casse recur_terminale !!
	!.

% ---  n'explore que la partie Head
pip_to_level_head(_Level,_Depth,_Plotter,[],_,Res,Res):-!.
pip_to_level_head(_Level,_Depth,_Plotter,[_],_,Res,Res):-!.
pip_to_level_head(_Level,_Depth,_Plotter,[_,_],_,Res,Res):-!.
pip_to_level_head(_Level,_Depth,_Plotter,[_,_,_],_,Res,Res):-!.   % ? garder ?enlever
pip_to_level_head(Level,Depth,_Plotter,_,_,Res,Res):-Level > Depth,!.
pip_to_level_head(Level,Depth,Ploter,D_S,Date_ref,Res_so_far,Res):-
      % length(D_S,Lg),
      % console([pip_to_level_head,Level,Split_lg,Lg]),
	pip_calc(Level,Ploter,D_S,Date_ref,D_S_Head,_D_S_Tail,_X2-_Y2,_Xm-_Ym,_X1-_Y1,ResH,ResT),
	NLevel is Level +1,
	pip_to_level_head(NLevel,Depth,Ploter,D_S_Head,Date_ref,[ResH,ResT|Res_so_far],Res),
	!.


% ---------------------------------
%
pip_res(X1,Y1,Xm,Ym,Coef,Res1):-
	Lg is Xm-X1,
	Bps is round((((Ym/Y1)^(1/Lg))-1)*100000)/10,
	Res1=pip(X1,Lg,Bps,Coef),
	!.

pip_res_ref(X2,Xm,Ym,X2,Y2,Coef_2,Res2):-  % si Dref=X2
	Lg is X2-Xm,
	Bps is round((((Y2/Ym)^(1/Lg))-1)*100000)/10,
	Res2=cur_pip(Xm,Lg,Bps,Coef_2),
	!.
pip_res_ref(_DRef,Xm,Ym,X2,Y2,Coef_2,Res2):-  % si Dref =/= X2
	Lg is X2-Xm,
	Bps is round((((Y2/Ym)^(1/Lg))-1)*100000)/10,
	Res2=pip(Xm,Lg,Bps,Coef_2),
	!.


% --------------------------------------------------------------
% calcule le PIP (Perceptually Important Point) d'une serie D_S
% test :
% D_S=[5-6,4-5,3-5,1-2],pip_calc(1,p1,D_S,Pos,MinMax_d,Coef_ajust,X-Y).
%
% En sortie :
%    données sur nouveau point : [Max_d , Pos_m(sur D_S), Xm-Ym ]
%    en X2  : [Pte_X2, Lg_seg_X1_Xm, X2-Y2, Xm-Ym]
%    en Xm  : [Pte_Xm, Lg_seg_Xm_X2, Xm-Ym, X1-Y1]
%    en X2_X1 : Pte_X2_X1, length(D_S,Lg_X2_X1) , X2-Y2, X1-Y1]
%    les trois points : [X2-Y2 , Xm-Ym, X1-Y1]
%    Droite X2-Y2,X1-Y1  : [ Ax,B,Correl_D_S_avec droite AX+b]
%
%    En lieu et place de la correlation on pourrait utiliser un rapport
%    entre les pentes
%

% pip_calc(Level,Ploter,D_S,Date_ref,D_S_Head,D_S_Tail,X2-Y2,Xm-Ym,X1-Y1,Res1,Res2):-

pip_calc(Level,Ploter,D_S,Date_ref,D_S_Head,D_S_Tail,
	 X2-Y2,Xm-Ym,X1-Y1,Res1,Res2):-
	pip_find(D_S,D_S_Head,D_S_Tail,X2-Y2,Xm-Ym,X1-Y1,Coef_1,Coef_2),
	pip_res(X1,Y1,Xm,Ym,Coef_1,Res1),
	pip_res_ref(Date_ref,Xm,Ym,X2,Y2,Coef_2,Res2),

   % console(['Level:',Level,'  x1-y1:',X1-Y1,'  Pip:',Xm-Ym,' Res_1:',Res1,nl,
   %	     '           x2-y2:',X2-Y2,' Res2:',Res2,'  c_1-c_2:',Coef_1,Coef_2]),

	% juste pour les tests : affichage des points trouvés
	affiche_pip(Level,Ploter,[X2,Xm,X1],[Y2,Ym,Y1]),
	!.

% a revoir
pip_calc(_,_,D_S,_Date_ref,[X2-Y2],[X2-Y2],X2-Y2,Xm-Ym,_X1-_Y1,0,0):-
	keysort(D_S,DS_asc),
	rev(DS_asc,[X2-Y2|_]),
	Xm-Ym=X2-Y2,
	trace,
	console(['.......................................... ',D_S,X2-Y2]),
	!.




% affiche pip  -----------------------------------
%
affiche_pip(_,no_plot_,_,_):-!. % pas d'affichage
affiche_pip(Pip_level,Ploter,Xs,Ys):-
	integer(Pip_level),
	Col_pos is Pip_level mod 12,
	couleurs(Colors),
	nth0(Col_pos,Colors,Couleur),
	plot_serie(Ploter,Couleur,Xs,Ys),
	!.
affiche_pip(Couleur,Ploter,Xs,Ys):-
	atom(Couleur),
	plot_serie(Ploter,Couleur,Xs,Ys),
	!.
affiche_pip(_,_,_,_). % true in fine

affiche_pip(no_plot,_Xs,_Ys):-!.
affiche_pip(Ploter,Xs,Ys):-
	plot_serie(Ploter,gray50,Xs,Ys),
	!.
affiche_pip(_,_,_). % true in fine

flush_pip(Plotter):-
	(   valeur_locale(Plotter,id,Id)->send(Id,flush);true).


% -------------------------------------------------------
% Extraire Min_xy,Max_xy d'une serie Date_x-Serie_y
%        assomption : D_S est en ordre décroissant de Date_x-Serie_y
min_max_ds(D_S,Min,Max):-
	D_S=[First|_],
	keysort(D_S,Sorted_D_S),
	Sorted_D_S=[Min|_],
	% test si First = Min ==> chercher last(Sorted_D_S,Max)
	(First = Min -> last(Sorted_D_S,Max);Max =First),
	!.



%-------------------------------------------------------------------
% PIP calculation :
% get the point from DS with max v_distance to A(x1-y1) B(x2-y2)
%
pip_find(D_S,D_S_Head,D_S_Tail,X2-Y2,Xm-Ym,X1-Y1,Coef_1,Coef_2):-
	% Assomption : D_S ordre décroissant Date
	D_S=[X2-Y2|T_DS],
	keysort(T_DS,[X1-Y1|_L_XY]),  % at least 2 elmnts
	X2 >= X1,
    % console([nl,'pip_find : x1-y1 =',X1-Y1,'  x2-y2=',X2-Y2]),
	distance_max(T_DS,X2-Y2,X1-Y1,Xm-Ym,_Dm,Pos_m),
	% split DS et calcul coefs
	pip_find_final(Pos_m,D_S,D_S_Head,D_S_Tail,X2-Y2,Xm-Ym,X1-Y1,Coef_1,Coef_2),
	!.
pip_find(D_S,D_S_Head,D_S_Tail,X2-Y2,Xm-Ym,X1-Y1,Coef_1,Coef_2):-
	% D_S dans un ordre quelconque et au moins 2 elmnts
	keysort(D_S,[X1-Y1,E2|T_asc_DS]) ,
	rev([X1-Y1,E2|T_asc_DS],[X2-Y2|T_DS]),
	console([pip_find_2,X1-Y1,X2-Y2,' : ne devrait pas etre utilisé']),
	distance_max(T_DS,X2-Y2,X1-Y1,Xm-Ym,_Dm,Pos_m),
	% split DS et calcul coefs
	pip_find_final(Pos_m,D_S,D_S_Head,D_S_Tail,X2-Y2,Xm-Ym,X1-Y1,Coef_1,Coef_2),
	!.


% si il n'y a pas de vrai pip (ie serie de 2 ou 3 points ou ajustement parfait)
pip_find_final(0,[_-_,Xm-Ym|_],D_S_Head,D_S_Tail,X2-Y2,Xm-Ym,X1-Y1,Coef_1,Coef_2):-
	D_S_Head=[X2-Y2,Xm-Ym],
	D_S_Tail=[Xm-Ym,X1-Y1],
	% console([ajustement_parfait,X2-Y2,Xm-Ym,X1-Y1]),
	Coef_1=1,Coef_2=1,
	!.
pip_find_final(Pos_m,D_S,D_S_Head,D_S_Tail,X2-Y2,Xm-Ym,X1-Y1,Coef_1,Coef_2):-
	get_sublist(Pos_m,D_S,D_S_Head,Tail),
	D_S_Tail=[Xm-Ym|Tail],
	% calcul coeff_ajustement (ie correl DS vs droite)
	ctl_ajustement(D_S_Head,X2-Y2,Xm-Ym,Coef_2),
	ctl_ajustement(D_S_Tail,Xm-Ym,X1-Y1,Coef_1),
	!.


% -------------------------------------------------------------------
% Liste_C contient X1-Y1 (as last) mais pas X2-Y2 ?
%
distance_max(Liste_C,X2-Y2,X1-Y1,Xm-Ym,Dm,Mpos):-
	ax_plus_b(X2-Y2,X1-Y1,Ax,B),
	distance_max_l(Liste_C,Ax,B,X1-Y1,1,
		       [0,0,_X2,_Y2],           % [0,1,X2,Y2],
			[Dm,Mpos,Xm,Ym]),
	!.


distance_max_l([] ,_Ax,_B,_X1-_Y1,_N,[Dm,Pos_m,Xm,Ym],[Dm,Pos_m,Xm,Ym]):-!.

distance_max_l([Xc-Yc|T_Xy],Ax,B,X1-Y1,N,
		 [Dist_sf,Mpos_sf,Mx_sf,My_sf],
		 [Md_F   ,Mpos_F ,Mx_F ,My_F]):-
	Dist is (round((Yc-(Ax*Xc+B)) * 10**3) / 10**3),  % vertical_dist
	% Dist is round((-(Ax*Xc-Yc+B)/sqrt(Ax^2+1))*10**3)/10**3, % perp_dist
	Pos is N+1,
	( abs(Dist) > abs(Dist_sf)         % abs(Dist) >= abs(Dist_sf)
	-> (Mdist = Dist ,   Mpos=Pos, Mx =Xc,  My=Yc ) ;
	   (Mdist = Dist_sf, Mpos=Mpos_sf, Mx=Mx_sf,My=My_sf)
	),
	distance_max_l(T_Xy,Ax,B,X1-Y1,Pos,
		       [Mdist,Mpos,Mx,My],
		       [Md_F,Mpos_F,Mx_F,My_F]).


% -------------------------------------------------------------------
% A faire eventuellement : version retournant Max et Min
% PIP : get the points with max & min distance to A(x1-y1) B(x2-y2) :
% à faire sur la base supra avec Max et Min
%

ctl_ajustement([],_X2-_Y2,_X1-_Y1,1):-!.
ctl_ajustement(D_S,X2-Y2,X1-Y1,Coef):-
	pairs_keys_values(D_S,Dates,Serie),
	ax_plus_b(X2-Y2,X1-Y1,Ax,B),
	droite(Dates,Ax,B,Droite),
	correlation(Serie,Droite,Coef_ajust),
	Coef is (round(Coef_ajust * 10**3) / 10**3),
	!.


droite([],_Ax,_B,[]):-!.
droite([X|T_x],Ax,B,[Y|T_Res]):-
	Y is Ax*X+B,
	droite(T_x,Ax,B,T_Res).

% --------------------------------------------------------

traite_pips(O,A,Pips_unsorted):-
	msort(Pips_unsorted,Pips),
	% console([traite_pips,Pips,nl]),
	% historise_pips(O,A,long,Pips,cur_pip(D,Lg,Bps,Cor), Lg>100),
	historise_pips(O,A,short,Pips,cur_pip(D,Lg,Bps,Cor), Lg<3),
	historise_pips(O,A,high_bps,Pips,cur_pip(D,Lg,Bps,Cor), Bps>50),
	% historise_pips(O,A,danger,Pips,pip(D,Lg,Bps,Cor), (Bps>10,Lg>15)),
	historise_pips(O,A,danger_cur,Pips,cur_pip(D,Lg,Bps,Cor),((Bps>20,Lg>10);(Bps>10,Lg>100))),
	% historise_pips(O,A,current,Pips,cur_pip(D,Lg,Bps,Cor), true),
	% pip_pattern(a,Pips,A_pattern),
	% pip_pattern_w(Pips,W_pattern),

	!.


historise_pips(_O,_A,Func_Att_h,Pips,Selector,Code):-
	% Selector : pip(D,Lg,Bps,Cor)
	% Code : Lg>100
	forall((member(Selector,Pips),Code),
	       (   (Selector= pip(D,Lg,Bps,Cor);Selector= cur_pip(D,Lg,Bps,Cor)),
		   nb_jour(Dt1,D),
		   D2 is D + Lg,
		   nb_jour(Dt2,D2),
		   (pip_rep_store(Rep),(Rep=yes;Rep=historise) ->
		    console([Func_Att_h,';\tCode ;',Code,';',Dt1,';\t',Selector,';\t',Dt2]);
		    true)
	       )
	      ),
	!.


% --- recherche de pattern  / à faire évoluer
%
pip_pattern(a,[],_A_pattern):-!.
pip_pattern(a,[A_debut|Tail],A_pattern):-
	A_debut=pip(D,Lg,Bps,_Cor),
	Bps >0,
	Dte_fin is D+Lg,
	memberchk(pip(Dte_fin,Lg_f,Bps_f,Cor_f),Tail),
	Bps_f <0,
	console([a_pattern,debut,A_debut,fin,pip(Dte_fin,Lg_f,Bps_f,Cor_f)]),

	pip_pattern(a,Tail,A_pattern),!.

pip_pattern(a,[_A_debut|Tail],A_pattern):-
	pip_pattern(a,Tail,A_pattern),!.

pip_pattern_w([],_W_pattern):-!.
pip_pattern_w([W_debut|Tail],W_pattern):-
	W_debut=pip(D,Bps,Lg,_Cor),
	Bps >0,
	Dte_w1 is D+Lg,
	memberchk(pip(Dte_w1,Lg_w1,Bps_w1,Cor_w1),Tail),
	Bps_w1 <0,
	Dte_w2 is Dte_w1+Lg_w1,
	memberchk(pip(Dte_w2,_Lg_w2,Bps_w2,_Cor_w2),Tail),
	console([w_pattern,W_debut,pip(Dte_w1,Lg_w1,Bps_w1,Cor_w1),pip(Dte_w2,Bps_w2)]),

	pip_pattern_w(Tail,W_pattern),!.

pip_pattern_w([_A_debut|Tail],W_pattern):-
	pip_pattern_w(Tail,W_pattern),!.

% -----------------------------------------------
%
store_pips(O,A,Date_ref,Pips,Vars:Selector,Code,Pip_att):-
	findall(Vars,(member(Selector,Pips),Code),Bag),
	sort(Bag,Sorted),
	Attribut=..[Pip_att,A,Date_ref],
	set_val(O,Attribut,Sorted),

	nb_jour(Date,Date_ref),
	(pip_rep_store(Rep),(Rep=yes;Rep=pip)
	     -> console([Pip_att,';',Date,';',Sorted]) ; true),

	!.

store_pips_histo(O,A,Date_ref,Pips):-
	( valeur(O,pip_histo(A),H_pips);H_pips=[]),
	findall(Pip,(member(Pip,Pips),
		      \+Pip=cur_pip(_,_,_,_),
		      \+ memberchk(Pip,H_pips)),
		Bag),
	append(H_pips,Bag,N_pips_unsorted),
	sort(N_pips_unsorted,N_pips),
	set_val(O,pip_histo(A),N_pips),

	nb_jour(Date,Date_ref),
	length(Bag,Lg),
	(pip_rep_store(Rep),(Rep=yes;Rep=histo)
	     ->(sort(Bag,SBag),console(['histo ;',Date,';',Lg,';',SBag]));true),

	!.




/* ---------- tests

pip_calc(1,p1,[6-6,5-6,4-5,3-3],Min,Pte,Pos,X-Y).

-------------------------------------------------------------------
Version avec calcul de pente dans v_distance_max_l :
activer_log.
time(test_pip(sp_long_usa,5)).
% 734,470 inferences, 0.578 CPU in 0.781 seconds (74% CPU, 1270435 Lips)

time(test_pip(sp_long_usa,7)).
% 787,505 inferences, 0.734 CPU in 1.875 seconds (39% CPU, 1072347 Lips)

% -----------------------------------------------------------------
version avec calcul de pente dans final :
time(test_pip(sp_long_usa,5)).
% 734,040 inferences, 0.484 CPU in 0.797 seconds (61% CPU, 1515437 Lips)
% 1,516,673 inferences, 0.608 CPU in 0.747 seconds (81% CPU, 2492872 Lips) @home coef_ajust
% 1,055,712 inferences, 0.531 CPU in 0.797 seconds (67% CPU, 1987223 Lips) @Wk coef_ajust

time(test_pip(sp_long_usa,7)).
% 787,304 inferences, 0.609 CPU in 1.875 seconds (32% CPU, 1291986 Lips)
% 889,313 inferences, 0.391 CPU in 0.673 seconds (58% CPU, 2276641 Lips)  Noirmout 1/11/15 optimise true

time(test_pip(a_trend,50)).
% 1,661,107 inferences, 2.172 CPU in 14.185 seconds (15% CPU, 764826 Lips)  Noirmout  1/11/15

---------------------*/

% Fin
