% file : 03_ex_math_et_date
% last_modif : 10/12/2004 (calcul sur dates) / 2012 : variance & co
%		2014 : performance, beta
% TAFs : faire un nettoyage de fond, trop d'ajouts non homog�nes
% -------------------stats -------------------------------------------------------------------
% but  : quelques calculs statistiques
% NB : les series chronologiques sont tjs dans l'ordre : [t0,t-1,t-2,..,t-n] !

:- ensure_loaded('00_ex_util').
:- ensure_loaded('02_ex_math').
:- ensure_loaded(emione).

:- set_prolog_flag(optimise,true).


/* ------  usage de fonctions sp�cifiques � l'utilisateur
:- use_module(library(arithmetic)).
:- arithmetic_function(user:pente/1).
:- arithmetic_function(mean/2).

mean(A, B, C) :-
        C is (A+B)/2.
--------- */

%  Variance  , covariance , pente , correlation, etc..
%  Code � passer en C
% (source possible : C:\Users\pja\Documents\SymphAnie\Alpha_beta_calc\Statistics_C)
%
%
variance(Xs,Variance):-
	length(Xs,N),
	sum_list(Xs,S_Xs),X_moy is S_Xs/N,
	somme_x_xmoy2(Xs,X_moy,0,S_x_xmoy2),
	Variance is S_x_xmoy2/N,
	!.
somme_x_xmoy2([],_,S_x_xmoy2,S_x_xmoy2):-!.
somme_x_xmoy2([X|Xts],X_moy,S_sofar,S_x_xmoy2):-
	S is ((X-X_moy)^2 )+S_sofar,
	somme_x_xmoy2(Xts,X_moy,S,S_x_xmoy2).


covariance(X,Y,Covar):-    % Ordre parametres au standard XL Y:d�pendantes
	length(Y,N),
	length(X,N),	% les deux series doivent etre de mm longueur
	sum_list(Y,Sig_Y),Y_moy is Sig_Y/N,
	sum_list(X,Sig_B),X_moy is Sig_B/N,
	covar(X,Y,X_moy,Y_moy,0,S_X_Xmoy_fois_Y_Ymoy),
	Covar is S_X_Xmoy_fois_Y_Ymoy /N,
	!.

covar([],[],_,_,S_XY,S_XY):-	!.
covar([A|A_t],[B|B_t],A_m,B_m,S_XY_in,S_XY):-
	N_s_XY is ((A-A_m) * (B-B_m)) + S_XY_in,
	covar(A_t,B_t,A_m,B_m,N_s_XY,S_XY).


% ----------------------------------------------------------
% Pente : coef de la droite de regression sur X1,X2,...,Xn
%

pente(Liste,Pente):-              % utilis� ds rgls => sur [t-n,..,t-0]
	mc(Liste,Alpha,_Beta),
	Pente is Alpha* -1,       % ==> Pente is Alpha * -1 !!!
	!.

pente(Y,X,Pente):-    % Ordre parametres au standard XL Y:d�pendantes ? pb...
	length(Y,L),
	length(X,L),	% les deux series doivent etre de mm longueur
	sum_list(Y,Sig_Y),Y_moy is Sig_Y/L,
	sum_list(X,Sig_X),X_moy is Sig_X/L,
	pente_1(Y,X,Y_moy,X_moy,0,0,S_Y_Ymoy_fois_X_Xmoy,S_X_Xmoy_2),
	Pente is S_Y_Ymoy_fois_X_Xmoy / S_X_Xmoy_2,
	!.

pente_1([],[],_,_,S_YX,S_X2,S_YX,S_X2):-	!.
pente_1([A|A_t],[B|B_t],A_m,B_m,S_YX_in,S_X2_in,S_YX,S_X2):-
	N_S_X2 is ((B-B_m) **2 ) + S_X2_in,     %  ((B-B_m)*(B-B_m)) + S_X2_in
	N_YX is ((A-A_m) * (B-B_m)) + S_YX_in,
	pente_1(A_t,B_t,A_m,B_m,N_YX,N_S_X2,S_YX,S_X2).


% ne pas utiliser tel_quel / refaire un mc_0_2_n pour traiter
% correctement le cas de la serie [t-n,..,t-0] afin de calculer
% un Beta exact (il y a actuellement un m�lange dans les sens des s�ries
% taux_pente([1,2,3,4],R) devrait produire 1 et pas 0.16666
%
taux_pente(Liste,Tx_Pente):-            % utilis� ds rgls
	mc(Liste,Alpha,Beta),
	Pente is Alpha* -1 ,
	Tx_Pente is (Pente/(Beta+Pente)),
	!.


% -------------------
%  calcule un taux de variation sur sous-periodes de la Liste
%  Le taux de pente est le taux de variation sur une p�riode initiale..
%  sur [t-0,..,t-n]
%
pentes(X_Liste,Lg,[Pente|T_Pentes]):- taux_pentes(X_Liste,Lg,[Pente|T_Pentes]).

taux_pentes(X_Liste,Lg,[Tx_pente|T_Pentes]):-
	integer(Lg),
	get_sub_dlist(Lg,X_Liste,H_Dlist,Tail),
	get_sublist(Lg,X_Liste,X_l,_),  % plus rapide que dlist2list !?
	numlist(1,Lg,Y_t),rev(Y_t,Y_l),
	sum_list(X_l,S_X),X_m is S_X/Lg,
	Y_m is (1+Lg)/2,	          % Moyenne des Y
	pente_1(Y_l,X_l,Y_m,X_m,0,0,S_Y_Ym_x_X_Xm,S_X_Xm_2),
	Alpha is S_Y_Ym_x_X_Xm / S_X_Xm_2 ,
	S_Y is  ((1+Lg)/2)*Lg,  % Somme des Y
	Beta is (X_m) - (Alpha * Y_m),
	Tx_pente is (Alpha/(Beta+Alpha)),
	pentes_rec1(Tail,H_Dlist,S_X,S_Y,Lg,Y_l,Y_m,T_Pentes),
	!.

pentes_rec1([],_, _SX,_SY,_L,_Y_l,_Y_m,[]):-!.
pentes_rec1([T_head|Ttail],[H_head|H_tail]-H_open, S_x_1,S_Y,Lg,Y_l,Y_m,[Tx_pente|T_Pentes]):-
	S_x is S_x_1-H_head+T_head,
	X_m is S_x/Lg,
	H_open=[T_head|H_new_open],   % constitution nouvel ancrage
	dlist2list(H_tail-H_new_open,X_l),

	pente_1(X_l,Y_l,X_m,Y_m,0,0,S_Y_Ym_x_X_Xm,S_X_Xm_2),
	Alpha is S_Y_Ym_x_X_Xm / S_X_Xm_2 ,
	Beta is (X_m) - (Alpha * Y_m),
	Tx_pente is (Alpha/(Beta+Alpha)),
    % console([Y_m,Alpha,X_m]),
	pentes_rec1(Ttail,H_tail-H_new_open,S_x,S_Y,Lg,Y_l,Y_m,T_Pentes).


% pour calcul du beta entre 2 series
%
pentes(Y_Liste,X_Liste,Lg ,[Pente|T_Pentes]):-
	integer(Lg),
	get_sub_dlist(Lg,X_Liste,H_Dlist,Tail),
	get_sublist(Lg,X_Liste,X_l,_),  % plus rapide que dlist2list !?
	sum_list(X_l,S_X),X_m is S_X/Lg,

	get_sub_dlist(Lg,Y_Liste,H_Y_Dlist,Y_Tail),
	get_sublist(Lg,Y_Liste,Y_l,_),  % plus rapide que dlist2list !?
	sum_list(Y_l,S_Y),Y_m is S_Y/Lg,

	pente_1(Y_l,X_l,Y_m,X_m,0,0,S_Y_Ym_x_X_Xm,S_X_Xm_2),
	Pente is S_Y_Ym_x_X_Xm / S_X_Xm_2,               % ?? * (-1)

	pentes_rec2(Tail,H_Dlist,S_X,Lg,Y_Tail,H_Y_Dlist,S_Y,T_Pentes),
	!.



pentes_rec2([],_, _X,_L,_,_Y_l,_Y_m,[]):-!.
pentes_rec2([T_head|Ttail],[H_head|H_tail]-H_open, S_x_1,Lg,
	    [T_Y_head|T_Y_tail],[H_Y_head|H_Y_tail]-H_Y_open,S_y_1,[Pente|T_Pentes]):-
	S_x is S_x_1-H_head+T_head,
	X_m is S_x/Lg,
	H_open=[T_head|H_new_open],   % constitution nouvel ancrage
	dlist2list(H_tail-H_new_open,X_l),

	S_y is S_y_1-H_Y_head+T_Y_head,
	Y_m is S_y/Lg,
	H_Y_open=[T_Y_head|H_Y_new_open],   % constitution nouvel ancrage
	dlist2list(H_Y_tail-H_Y_new_open,Y_l),

   % console([Y_l,nl,X_l]),
	pente_1(Y_l,X_l,Y_m,X_m,0,0,S_Y_Ym_x_X_Xm,S_X_Xm_2)      ,
	Pente is S_Y_Ym_x_X_Xm / S_X_Xm_2 ,   %  ??  *(-1) ??
	pentes_rec2(Ttail,H_tail-H_new_open,S_x,Lg,T_Y_tail,H_Y_tail-H_Y_new_open,S_y,T_Pentes).


%--------------------------------------
%
beta(Perfs_Titre,Perfs_Marche,Beta):- pente(Perfs_Titre,Perfs_Marche,Beta).
betas(Perfs_Titre,Perfs_Marche,Longueur,Beta):-
	pentes(Perfs_Titre,Perfs_Marche,Longueur,Beta).

% ----------------------------------------------
% Coef de Correl entre deux listes de numeriques
correlation(A,B,Correl):-
	length(A,L),
	length(B,L),	% les deux series doivent etre de mm longueur
	sum_list(A,Sig_A),A_moy is Sig_A/L,
	sum_list(B,Sig_B),B_moy is Sig_B/L,
	correl(A,B,A_moy,B_moy,0,0,0,S_A_ec_2,S_B_ec_2,A_ec_B_ec),
	S_A_ec_2 =\= 0 ,S_B_ec_2 =\= 0 ,
	Var_A is S_A_ec_2/L, A_ect is sqrt(Var_A),
	Var_B is S_B_ec_2/L, B_ect is sqrt(Var_B),
	Correl is A_ec_B_ec / (L* A_ect *B_ect),
	% arrondi(Correl,5,Cor_arrondi),
	!.
correlation(_A,_B,0). % in fine

correl([],[],_A_m,_B_m,S_A_ec2,S_B_ec2,A_B_ec,S_A_ec2,S_B_ec2,A_B_ec):-!.

correl([A|A_t],[B|B_t],A_m,B_m,S_Aec2_in,S_Bec2_in,A_B_ec_in,S_Aec2,S_Bec2,A_B_ec):-
	N_S_Aec2 is ((A-A_m) * (A-A_m)) + S_Aec2_in,
	N_S_Bec2 is ((B-B_m) * (B-B_m)) + S_Bec2_in,
	N_A_B_ec is ((A-A_m) * (B-B_m)) + A_B_ec_in,
	correl(A_t,B_t,A_m,B_m,N_S_Aec2,N_S_Bec2,N_A_B_ec,S_Aec2,S_Bec2,A_B_ec).


% Alpha_beta
alpha_beta(Liste,Xs,Alpha,Beta):-
	length(Liste,N),
	length(Xs,N),
	alpha_beta_1(Liste,Xs,0,0,0,0,Sxiyi,Sxi2,Syi,Sxi),
	Alpha is (Sxiyi - (Sxi*Syi/N))/(Sxi2 -( Sxi*Sxi /N)) ,
	Beta is Syi/N - (Alpha * Sxi/N).

alpha_beta_1([],[],Stiyi,Sti2,Syi,Sti,Stiyi,Sti2,Syi,Sti).	% 12/4/06 : _0 ou 0 en arg#2
alpha_beta_1([H|Tail],[X|Xt],In_Stiyi,In_Sti2,In_Syi,In_Sti,Stiyi,Sti2,Syi,Sti):-
	N_Stiyi is In_Stiyi + (X * H ),
	N_Sti2 is In_Sti2 + (X * X),
	N_Syi is In_Syi + H,
	N_Sti is In_Sti + X,
	alpha_beta_1(Tail,Xt,N_Stiyi,N_Sti2,N_Syi,N_Sti,Stiyi,Sti2,Syi,Sti).




% Calcul par les moindres carres des coefs A et Beta de la droite Ax+B
% s'ajustant a la serie de nombre Liste [t-n,..,t-0]
%
mc(Liste,Alpha,Beta):-
	length(Liste,N),
	mc_1(Liste,N,0,0,0,0,Stiyi,Sti2,Syi,Sti),
	Alpha is (Stiyi - (Sti*Syi/N))/(Sti2 -( Sti*Sti /N)) ,
	Beta is Syi/N - (Alpha * Sti/N).

mc_1([],_,Stiyi,Sti2,Syi,Sti,Stiyi,Sti2,Syi,Sti).     % 12/4/06 : _0 ou 0 en arg#2
mc_1([H|Tail],Pos,In_Stiyi,In_Sti2,In_Syi,In_Sti,Stiyi,Sti2,Syi,Sti):-
	N_Stiyi is In_Stiyi + (Pos * H ),
	N_Sti2 is In_Sti2 + (Pos * Pos),
	N_Syi is In_Syi + H,
	N_Sti is In_Sti + Pos,
	N_Pos is Pos-1,
	mc_1(Tail,N_Pos,N_Stiyi,N_Sti2,N_Syi,N_Sti,Stiyi,Sti2,Syi,Sti).



% VARIANTE POUR AFFICHAGE GRAPHIQUE !!!!
% On reverse la liste pour avoir les coord dans le bon sens a l'affichage
mc_graphique(Liste,Alpha,Beta):-
	mc(Liste,Alpha_1,Beta),
	Alpha is Alpha_1 * (-1.0) .

% les regression sur des portions de la courbe
mc(Liste,Start,Duree,From,To,Alpha,Beta):-
	get_sublist_from_start_point(Liste,Start,Duree,From,To,Sublist),
	mc(Sublist,Alpha,Beta),!.

mc_graphique(Liste,Start,Duree,From,To,Alpha,Beta):-
	get_sublist_from_start_point(Liste,Start,Duree,From,To,Sublist),
	mc(Sublist,Alpha_1,Beta),
	Alpha is Alpha_1 * (-1.0) .


% Calcul de l'ecart type soit simple soit entre pts reels et estimes par regre
% pour l'ecart type simple par rapport a la moyenne il faut mettre Alpha = 0, Beta = Moy
ecart_type(Liste,Alpha,Beta,Ecart_type):-
	length(Liste,Taille),
	ec_t(Liste,Alpha,Beta,Taille,0,Somme_des_carres),
	Res_int is Somme_des_carres/Taille ,
	Ecart_type is sqrt(Res_int),!.

ec_t([],_A,_B,0,Somme,Somme):-!.
ec_t([H|Tail],Alpha,Beta,Pos_in,Somme_in,Somme_out):-
	C is  H - ((Pos_in*Alpha) +Beta),
	Carre is C * C,
	N_somme_in is Somme_in + Carre ,
	Pos_out is Pos_in -1,
	ec_t(Tail,Alpha,Beta,Pos_out,N_somme_in,Somme_out).


% ----------------------------------
% les moyennes mobiles
%

% mav : Moyenne mobile optimis�e calcul� en n_iter vs N**2
%       calcul� avec une difference_list
mav(Liste,Mav_lg,[Mav_1|T_Mavs]):-
	integer(Mav_lg),
	get_sub_dlist(Mav_lg,Liste,H_Dlist,Tail),
	get_sublist(Mav_lg,Liste,Head,_),  % plus rapide que dlist2list !?
	% dlist2list(H_Dlist,Head),
	sum_list(Head,Somme_1),
	Mav_1 is  round(Somme_1/Mav_lg*100000)/100000,
	mav_rec(Tail,H_Dlist,Somme_1,Mav_lg,T_Mavs).

mav_rec([],_, _X,_Y,[]):-!.
mav_rec([T_head|Ttail],[H_head|H_tail]-H_open, Somme,Mav_lg,[Mav|T_mavs]):-
	Somme_1 is Somme-H_head+T_head,
	Mav is round(Somme_1/Mav_lg*100000)/100000,
	H_open=[T_head|H_new_open],   % constitution nouvel ancrage
	mav_rec(Ttail,H_tail-H_new_open,Somme_1,Mav_lg,T_mavs).



% ancienne Moyenne mobile non optimis�e  : redirig�e sur mav
mm(Liste,MA_order,Res):- mav(Liste,MA_order,Res).


% --------------------------------------------------------
% mm_special : produit une liste de Res de mm lgt que Liste
mm_special(Liste,MA_order,Res):-
	MM_order is integer(MA_order),
	mm_sp(Liste,MM_order,Res).
% mm_sp([],_MA_order,[]):-!.
mm_sp([H|Tail],MA_order,[X|Rest]):-
	Lg is MA_order-1,
	get_sublist(Lg,Tail,Sublist,_Residu),!,
	sum_list(Sublist,Somme_1),
	Somme is Somme_1+ H,
	X is round((Somme / MA_order)*10000)/10000,
	% X is Somme / MA_order,     % original anie
	mm_sp(Tail,MA_order,Rest).
mm_sp(Short_list,MA_order,Res):-
	New_MA_order is MA_order -1,
	mm_special_fin(Short_list,New_MA_order,Res).

mm_special_fin([],_MA_order,[]).
mm_special_fin([A|Tail],MA_order,[Res|Rest]):-
	sum_list([A|Tail],Somme),
	Res is round((Somme / MA_order)*10000)/10000,
	New_MA_order is  MA_order-1,
	mm_special_fin(Tail,New_MA_order,Rest).


% --------------------
moyenne([],0):-!.
moyenne(Liste,Moyenne):-
	sum_list(Liste,Somme),	% qplibrary (lists)
	length(Liste,Taille),
	Moyenne is Somme/Taille.

moyenne_hz(L,R):-
	supprimer_leading(L,0,NL),
	% write('moyenne_hz  in'-L-'  out'-NL),nl,
	moyenne(NL,R).

supprimer([],_,[]).
supprimer([X|T],X,L):-
	supprimer(T,X,L),!.
supprimer([H|T],X,[H|L]):-
	supprimer(T,X,L).

supprimer_leading([X|T],X,L):-
	supprimer_leading(T,X,L),!.
supprimer_leading(L,_X,L):-!.


% ----------------------------------------------------------------------
% pch(Liste,Lg, pch_%) : changement en pctage entre les donnees
% !! donn�es dans l'ordre ante_chrono  Xt-0,Xt-1,Xt-n,...
% 5/09/2012
pch(Serie,Lg,T_Pchs):-
	integer(Lg),
	get_sub_dlist_first_and_last(Lg,Serie,H_Dlist,_First,_Last,Tail),
	pch_rec(Tail,H_Dlist,T_Pchs).

pch_rec([],_,[]):-!.
pch_rec([T_head|Ttail],[H_head|H_tail]-H_open,[Pch|T_Pchs]):-
	Pch is round(((H_head/T_head)-1)*1000)/1000,
	H_open=[T_head|H_new_open],   % constitution nouvel ancrage
	pch_rec(Ttail,H_tail-H_new_open,T_Pchs).


% entre les donn�es cons�cutives (cf Anie)
pch([_],[]):-!.
pch([A,B|Tail],[X|Tail_Res]):-
	X_tp is (A/B)-1.0 ,
	X is round(X_tp*1000)/ 1000,  % fait un arrondi
	pch([B|Tail],Tail_Res).


%raw_pch(Liste,Res) : changmt en % de liste sans enlever le 100% de base
raw_pch([_],[]):-!.
raw_pch([A,B|Tail],[X|Tail_Res]):-
	X_tp is (A/B) ,
	X is round(X_tp*1000)/ 1000,  % fait un arrondi (3)
	raw_pch([B|Tail],Tail_Res).


% ach(Liste, Liste_des_delta_en_%) : changement en valeur entre les donnees
ach([_],[]):-!.
ach([A,B|Tail],[X|Tail_Res]):-
	X is A-B ,
	ach([B|Tail],Tail_Res).



% percentage_change(In,Duree,Out) Out= chgmt en % des elements de In sur une dure Duree
%  optimisation : le shift get_sublist se fait dans percentage_change
%  au lieu de le r�peter X fois dans percentage_chg
percentage_change(In,Duree,Out):-
	Longueur is abs(Duree),
	get_sublist(Longueur,In,_,Tail),
	percent_chg(Tail,In,Out),
	!.
% faster
percent_chg([],_,[]):-!.
percent_chg([Y|T_Y],[X|T_X],[Res|Rest_res]):-
	(Y =:= 0.0 -> YY = 0.00001; YY=Y),
	Res_brut is ((X/YY) -1.00) * 100.00 ,
	(Res_brut > 300.00 -> Res=300;
		(Res_brut < (-300.00) -> Res=(-300.00) ;
			(Res is round(Res_brut*100000)/ 100000) )),  % arrondi a 5 chiffres
	percent_chg(T_Y,T_X,Rest_res),!.




%raw_precentage_change idem que supra sauf resultat en brut ie 1.xx %
raw_percentage_change(In,Duree,Out):-
	Longueur is abs(Duree),
	get_sublist(Longueur,In,_,Tail),
	raw_percent_chg(Tail,In,Out),
	!.

raw_percent_chg([],_,[]):-!.
raw_percent_chg([Y|T_Y],[X|T_X],[Res|Rest_res]):-
	(Y =:= 0.0 -> YY = 0.00001; YY=Y),
	Res_brut is (X/YY) ,
	(Res_brut > 4.00 -> Res=4.00;
		(Res_brut < (-4.00) -> Res=(-4.00) ;
			(Res is round(Res_brut*100000)/ 100000) )),  % arrondi a 5 chiffres
	raw_percent_chg(T_Y,T_X,Rest_res),!.


% performance : calcule 2 vecteurs (avec et sans -1.0)
%		+set_performances(O,A,Coeffs,Perfs) dans 18_ex_indicateur.pl
%
performances([_],[],[]):-!.
performances([A,B|Tail],[A_B|AB_tail],[Perf|Perf_tail]):-
	(   B =:= 0 -> BB = 0.00001 ; BB=B),  % sorte de safe_div
	A_B is (A/BB) ,
	Perf is round((A_B -1.0)*100000)/ 100000 ,  % fait un arrondi (5)
	performances([BB|Tail],AB_tail,Perf_tail).


/* CORRECTION DES VARIATIONS SAISONNIERES */
% cvs : Correction des variations saisonnieres (Mensuelles)
cvs(Serie,Serie_corrigee):-
	% calcul du trend
	mc(Serie,Alpha,Beta),
	length(Serie,Lgt),Lg is (Lgt-1) * (-1),
	bagof(X,Tic^(tic(0,Lg,_,Tic),X is (Alpha*Tic)+Beta),Serie_trend),
	% calcul des rapports au trend
	map_op(/,Serie,Serie_trend,Serie_rapport),
	vecteur_des_corrections(Serie_rapport,12,Vecteur_des_corrections),
	map_op_rotatif(/,Serie,Vecteur_des_corrections,Serie_corrigee),!.


vecteur_des_corrections(S_rap,Lg,Vect_corr):-
	bagof(X,Tic^(tic(1,Lg,_,Tic),X=0),Vect_initial),
	calc_vect_corr(S_rap,Vect_initial,0,Vect_corr),!.

calc_vect_corr(S_rap,Vect_in,Iter_count,Res):-
	get_sublist(12,S_rap,Sub_list,Tail),
	map_op(+,Vect_in,Sub_list,Vect_out),
	N_iter is 1+Iter_count,
	calc_vect_corr(Tail,Vect_out,N_iter,Res),!.
calc_vect_corr(_S_rap,Vect_in,Iter_count,Res):-
	% end of recursion
	map_op(/,Vect_in,Iter_count,Vect_corr_brutes),
	ajuste_corr(Vect_corr_brutes,Res),!.

ajuste_corr(Vect_corr_brutes,Res):-
	moyenne(Vect_corr_brutes,Coef_corr),
	map_op(/,Vect_corr_brutes,Coef_corr,Res),!.


/* ---------- RECHERCHE DES CORRELATIONS
principe : pour un Obj-Att de type serie_numerique on desire calculer sa correlation avec
	   un autre Obj'-Att' et evaluer le time lag entre les deux series.

fonctionnement : pour la serie Expliqu�e Obj-Att ont calcule les correlations
		 avec la serie explicative avec des time lag allant de 0-24 mois
		 Cette operation est faite pour une base temps de la serie
		 expliquee variant de 0 a 18 (Ceci permet de voir la
		 transformation dans le temps de la correlation).

structure des resultats :
  Obj :
    Obj'_Att' :
	Att : [T_lag-Best_corr_t_0,T_lag-Worst_corr_0,Base_time-T_lag-Best_corr,Base_time-T_lag-Worst_corr]



illustration des apports :
	en faisant : chercher_correlations([cpi_usa,serie],[m2_usa,serie])
	on obtient : [	18-9-0.933290928759943,17-9-0.945503275070009,
			16-9-0.933449024759238,15-9-0.936028172701414,
			14-10-0.938411675672841,13-10-0.953465873285557,
			12-10-0.950078079260919,11-10-0.939869524832369,
			10-10-0.930167046069769,9-10-0.917244612991712,
			8-11-0.804602417078081,7-11-0.676832842579673,
			6-12-0.52987415341133,5-12-0.434344932028976,
			4-12-0.444500429783559,3-12-0.39845672202044,
			2-3-0.502784596006476,1-3-0.540375795688112,
			0-4-0.459236314330908]

	en novembre 1990 on remarque que cela fait environ 7-8 mois que la
	masse monetaire m2 ne cesse d'avoir moins d'influence sur le cpi

Note Jab : perf(chercher_correlations(cpi_usa,serie,m2_usa,serie)).  cpu used : 44.42 sec Ws Meys

 ----------- */

% calculer les correlations entre deux Objets-Atts
correlation(Obj_1,Att_1,Obj_2,Att_2):-
	correlation(Obj_1,Att_1,Obj_2,Att_2,affichage_reduit),!.
correlation(Obj_1,Att_1,Obj_2,Att_2,Type_affichage):-
	% attribut_ok_pour_correlation(Obj_1,Att_1),
	% attribut_ok_pour_correlation(Obj_2,Att_2),
	purger_calcul_correlations(Obj_1),
	calculer_corrs([Obj_1,Att_1],[Obj_2,Att_2],Type_affichage),
	historise_correlations(Obj_1,Obj_2,Att_2,Att_1),
	purger_calcul_correlations(Obj_1),
	!.
correlation(_Obj_1,_Att_1,_Obj_2,_Att_2,_Type_affichage):-!.

attribut_ok_pour_correlation(Obj,Att):-
	objet(Obj,indicateur),
	((Att=signal_metrique;Att=signal_metrique_exp)->valeur_locale(Obj,rules,_);true),
	!.
attribut_ok_pour_correlation(Obj,Att):-
	objet(Obj,synthese),
	(Att=signal_metrique;Att=signal_metrique_exp),
	!.

calculer_corrs([Obj_1,Att_1],[Obj_2,Att_2],Type_aff):-
	Last_p_1 = sept_06 ,	% valeur(Obj_1,last_periode,Last_p_1),
	Last_p_2 = sept_06 ,	% valeur(Obj_2,last_periode,Last_p_2),
	nl,
	write('CORRELATIONS ENTRE : '),write(Obj_1),write('	'),write(Att_1),write('	'),write(Last_p_1),nl,
	write('                ET : '),write(Obj_2),write('	'),write(Att_2),write('	'),write(Last_p_2),nl,nl,
	descripteur_local(fonctionnement,correlations,variation_base_tps_et_incr,To-Inc),
	descripteur_local(fonctionnement,correlations,max_lag_positif,L_pos),
	descripteur_local(fonctionnement,correlations,max_lag_negatif,L_neg),
	descripteur_local(fonctionnement,correlations,range_lgth,Corr_rg_lg),
	tic(0,To,Inc,T),
	write('EN t-'),write(T),write('	sur '),write(Corr_rg_lg),write(' per.	 :'),
	calculer_correlation_en_t([Obj_1,Att_1],[Obj_2,Att_2],T,L_pos,L_neg,Corr_rg_lg,Type_aff),
	fail.
calculer_corrs([_Obj_1,_Att_1],[_Obj_2,_Att_2],_Type_aff):-!.


purger_calcul_correlations(Obj):-
	reset_val(Obj,z_corrs),
	descripteur_local(Obj,z_corrs,D,_V),
	reset_des(Obj,z_corrs,D),
	fail.
purger_calcul_correlations(_Obj):- !.


% calculer les correlations entre deux Objs sur une plage de temps donnee
calculer_correlation_en_t([Obj_1,Att_1],[Obj_2,Att_2],Base_time,L_pos,L_neg,Corr_rg_lg,Type_aff):-
	correls_en_t(Obj_1,Base_time,L_pos,L_neg,Corr_rg_lg,[Obj_1,Att_1],[Obj_2,Att_2],Type_aff),
	bagof(Pt-C,(Pt^descripteur_local(Obj_1,z_corrs,Pt,C)),Bag),
	bagof(Co,(Pt^descripteur_local(Obj_1,z_corrs,Pt,Co)),Set),
	get_max(Set,Cor_max),
	get_min(Set,Cor_min),
	member(Per_max-Cor_max,Bag),
	member(Per_min-Cor_min,Bag),
	(valeur_locale(Obj_1,z_corrs,Bests_cors)->true;Bests_cors=[]),
	substitue_ou_ajoute(Base_time-_-_-_-_,Base_time-Per_max-Cor_max-Per_min-Cor_min,Bests_cors,New),
	set_val(Obj_1,z_corrs,New),
	write('	Best in '),write(Per_max),write(' = '),write(Cor_max),
	(Cor_max > 0.60 -> (Per_max > 0 ->	write('	'),write(Obj_2),write(' leads ') ;
						write('	'),write(Obj_1),write(' leads ') )
			;write('			')),
	write('		Worst in '),write(Per_min),write(' = '),write(Cor_min),nl,
	!.


% ---
correls_en_t(Obj,Base_time,_L_pos,L_neg,Corr_rg_lg,[Obj_1,Att_1],[Obj_2,Att_2],Type_affichage):-
	End_time is Base_time + L_neg,
	tic(End_time,Base_time+1,-1,T),
	calculer_correlation(Obj_2,Att_2,Base_time,Obj_1,Att_1,T,Corr_rg_lg,Correl),
	Rel_t is (T-Base_time)*(-1),
	affiche_resultat_intermediaire_correlation(Rel_t,Correl,Type_affichage),
	set_des(Obj,z_corrs,Rel_t,Correl),
	fail.
correls_en_t(Obj,Base_time,L_pos,_L_neg,Corr_rg_lg,[Obj_1,Att_1],[Obj_2,Att_2],Type_affichage):-
	End_time is Base_time + L_pos,
	tic(Base_time,End_time,1,T),
	calculer_correlation(Obj_1,Att_1,Base_time,Obj_2,Att_2,T,Corr_rg_lg,Correl),
	Rel_t is T-Base_time,
	affiche_resultat_intermediaire_correlation(Rel_t,Correl,Type_affichage),
	set_des(Obj,z_corrs,Rel_t,Correl),
	fail.
correls_en_t(_Obj,_Base_time,_L_pos,_L_neg,_Corr_rg_lg,[_Obj_1,_Att_1],[_Obj_2,_Att_2],Type_affichage):-
	(Type_affichage=affichage_reduit -> true ; nl),
	!.


affiche_resultat_intermediaire_correlation(_Periode,_Correl,affichage_reduit):-!.
affiche_resultat_intermediaire_correlation(Periode,Correl,_):-
	nl,write('		'),write(Periode),write('	 --> '),write(Correl),!.

% --- calculer les correlations entre deux Objets-Attributs pour un date donnee
calculer_correlation(Obj_1,Att_1,Ptime_1,Obj_2,Att_2,Ptime_2,Corr_rg_lg,Correl):-
	Pti_1 is abs(Ptime_1),
	Pti_2 is abs(Ptime_2),
	valeur(Obj_1, Att_1, Val_1),length(Val_1,L_1),
	valeur(Obj_2, Att_2, Val_2),length(Val_2,L_2),
	get_min([L_1,L_2],L),
	Duree_max is (L - Pti_2) -1 ,
	( Corr_rg_lg >0 -> Duree is ( Corr_rg_lg -1)  ; Duree is abs(Duree_max)),

	Duree > 2,
	Pt_1 is (Pti_1+Duree) ,
	Pt_2 is (Pti_2+Duree) ,
	obj_name=Obj_1 et Att_1 entre (t-Pti_1,t-Pt_1) = Serie_1,
	obj_name=Obj_2 et Att_2 entre (t-Pti_2,t-Pt_2) = Serie_2,
	correlation(Serie_1,Serie_2,Correl),
	!.
calculer_correlation(_Obj_1,_Att_1,_Ptime_1,_Obj_2,_Att_2,_Ptime_2,0.0):-!.


% --- historise les correlations
historise_correlations(Obj,Obj_2,Att_2,Att_1):-
	valeur_locale(Obj,z_corrs,Liste_corrs),

	bagof(Cor_max,(A^B^C^D^member(A-B-Cor_max-C-D,Liste_corrs)),Bag),
	get_max(Bag,Max),
	member(Bt_max-Lg_max-Max-_-_,Liste_corrs),
	bagof(Cor_min,(A^B^C^D^member(A-B-C-D-Cor_min,Liste_corrs)),Bag_min),
	get_min(Bag_min,Min),
	member(Bt_min-_-_-Lg_min-Min,Liste_corrs),
	member(0-Lg_b_0-C_b_0-Lg_w_0-C_w_0,Liste_corrs),
	seuil_et_historise(Obj,Obj_2,Att_1,[Att_2,Lg_b_0-C_b_0,Lg_w_0-C_w_0,Bt_max-Lg_max-Max,Bt_min-Lg_min-Min]),
	!.


seuil_et_historise(Obj,Obj_2,Att_1,[Att_2,Lg_b_0-C_b_0,Lg_w_0-C_w_0,Bt_max-Lg_max-Max,Bt_min-Lg_min-Min]):-
	descripteur_local(fonctionnement,correlations,seuil_historisation,Seuil),
	Seuil_bas is (Seuil* (-1)),
	(C_b_0 > Seuil ; C_w_0 < Seuil_bas ; Max > Seuil ;Min  < Seuil_bas) ,!,
	histo(Obj,Obj_2,Att_1,[Att_2,Lg_b_0-C_b_0,Lg_w_0-C_w_0,Bt_max-Lg_max-Max,Bt_min-Lg_min-Min]),
    write('histo : '-[Obj-Att_1,'  ',Obj_2-Att_2]),nl,
	!.
seuil_et_historise(_Obj,_Obj_2,_Att_1,_):-!.	% ne satisfait pas au seuil / pas historis�


histo(Obj,Obj_2,Att_1,[Att_2,Lg_b_0-C_b_0,Lg_w_0-C_w_0,Bt_max-Lg_max-Max,Bt_min-Lg_min-Min]):-
	descripteur_local(Obj,Obj_2,Att_1,[Att,_Lg_b-C_b,_Lg_w-C_w,_Bt_ma-_Lg_ma-Ma,_Bt_mi-_Lg_mi-Mi]),
	not(Att_2=Att),		% que si Att_2 et Att sont diff�rents
	Corr_new is abs(C_b_0)*abs(C_w_0)*abs(Max)*abs(Min),
	Corr_old is abs(C_b)*abs(C_w)*abs(Ma)*abs(Mi),
	((Corr_new > Corr_old) ->
		set_des(Obj,Obj_2,Att_1,[Att_2,Lg_b_0-C_b_0,Lg_w_0-C_w_0,Bt_max-Lg_max-Max,Bt_min-Lg_min-Min]);
		true),
	!.
histo(Obj,Obj_2,Att_1,[Att_2,Lg_b_0-C_b_0,Lg_w_0-C_w_0,Bt_max-Lg_max-Max,Bt_min-Lg_min-Min]):-
	set_des(Obj,Obj_2,Att_1,[Att_2,Lg_b_0-C_b_0,Lg_w_0-C_w_0,Bt_max-Lg_max-Max,Bt_min-Lg_min-Min]),
	(valeur_locale(Obj,relevant_correlations,Liste_correls);Liste_correls= []),!,
	add_element(Obj_2,Liste_correls,N_liste_corr),
	set_val(Obj,relevant_correlations,N_liste_corr),
	!.

% ---

nettoyer_correlations(Obj_1,Obj_2):-
   write(nettoyer_correlations(Obj_1,Obj_2)),nl,
	valeur_locale(Obj_2,groupe,Gr),
	(descripteur_local(Obj_1,verified_correlations,Gr,L_verif)-> true;L_verif=[]),
	delete(L_verif,Obj_2,N_l_verif),
	set_des(Obj_1,verified_correlations,Gr,N_l_verif),
	(valeur_locale(Obj_1,relevant_correlations,L_relevant)-> true;L_relevant=[]),
	delete(L_relevant,Obj_2,N_l_relevant),
	set_val(Obj_1,relevant_correlations,N_l_relevant),
	vider_desc(Obj_1,Obj_2),
	!.

% ---
enlever_correlations :-
	( valeur_locale(Obj,relevant_correlations,_Liste)-> true;
		descripteur_local(Obj,verified_correlations,_D,_V)),
	enlever_correlations(Obj),
	fail.
enlever_correlations:-!.


enlever_correlations(Obj):-
	valeur_locale(Obj,relevant_correlations,Liste),
	forall(member(X,Liste), ( vider_desc(Obj,X),reset_val(Obj,X)) ),
	reset_val(Obj,relevant_correlations),
	fail.
enlever_correlations(Obj):-
	descripteur_local(Obj,verified_correlations,_D,_V),
	vider_desc(Obj,verified_correlations),
	!.
enlever_correlations(_Obj):-!.




% UTILISATION DE LA CORRELATION
/* correlation entre signal_metrique de synthese_economy et indice boursier
bagof(S,T^(tic(0,12,1,T), indicateur dont nom=synthese_economy et pays =usa
			  et signal_metrique en t-T=S),Bag),
tic(0,12,1,T),T2 is T+12,
indicateur dont nom=indice_boursier et pays=usa et valeur entre (t-T,t-T2)=Serie,
correlation(Serie,Bag,Corel),write(['en ',T,' correl ',Corel ]),nl,fail.
*/

% --- chercher les correlations d'un Obj-Att avec un autre Obj_2,Att_2 selectionne  ...
chercher_correlations(Obj_1,Att_in):-
	(Att_in=historique_signal -> Att_1=signal_metrique; Att_1=Att_in),
	valeur(Obj_1,Att_1,[Val|_]), number(Val),
	set_val(etat_msg,titre,'SEL. GRAPHIC'),
	what_des_gra(Dessin,_Graphique),
	valeur(Dessin,obj_att,Obj_2-Att_aff),
	(Att_aff=historique_signal -> Att_2=signal_metrique_exp ; Att_2=Att_aff),
	% objet(Obj_2,indicateur),
	valeur(Obj_2,Att_2,[First|_]),
	number(First),
	% length(Valeur,Lgt),Lgt > 43,		% teste la longueur de la serie d�pendante ...
	set_val(etat_msg,titre,'WORKING'),
	correlation(Obj_1,Att_1,Obj_2,Att_2,affichage_reduit),
	!.

chercher_correlations(Obj_1):-
	set_val(etat_msg,titre,'SEL. GRAPHIC'),
	what_des_gra(Dessin,_Graphique),
	valeur(Dessin,obj_att,Obj_2-_Att_aff),
	set_val(etat_msg,titre,'WORKING'),
	nettoyer_correlations(Obj_1,Obj_2),
	correlation_objets(Obj_1,Obj_2),
	!.


correlations(Obj):-	% --- les correlations d'un objet avec autres objets du meme groupe
	% purger_evenements,  % primitive EMICAT
	valeur_locale(Obj,groupe,Groupe),
	valeur_locale(Obj,pays,Pays),
	valeur_locale(Obj_2,groupe,Groupe),valeur_locale(Obj_2,pays,Pays),
	correlation_objets(Obj,Obj_2),
    console(['Obj_2 :'-Obj_2]),
	fail.
correlations(Obj):-
	write('Fin de recherche de correlations sur '),write(Obj),nl,
	!.

correlations(Obj,Obj_2):-	% --- toutes les correlations d' Obj avec Obj_2
	valeur_locale(Obj,pays,_),
	valeur_locale(Obj_2,pays,_),
	correlation_objets(Obj,Obj_2),
	!.

correlations(Obj,Att):-		% --- correlations d'Obj,Att avec tous les autres Obj du groupe
	valeur(Obj,groupe,Groupe),
	valeur(Obj,pays,Pays),
	objet(Obj_2,Groupe),valeur(Obj_2,pays,Pays),
	correlations(Obj,Att,Obj_2),
	fail.
correlations(Obj,Att):-
	write('Fin de recherche de correlations sur '),
	write(Obj-Att),nl,
	!.

correlations(Obj,Att,Obj_2):-	% --- les correlations d' Obj,Att avec Obj_2
	member(Att_2,[serie,pch,signal_metrique,signal_metrique_exp]),
	correlation(Obj,Att,Obj_2,Att_2,affichage_reduit),
	fail.
correlations(_Obj,_Att,_Obj_2):-!.


% ---
correlation_objets(Obj,Obj_2):-
	% existe_evenement_qq,	% si existe_evenement_qq on abrege les calculs
   console(['test existe_evenement_qq  /
             si oui interrompre correlations_objets(',Obj,Obj_2,')
	     a refaire sous SWI ']),
	fail.
correlation_objets(Obj,Obj_2):-
	valeur_locale(Obj_2,groupe,Grp),
	descripteur_local(Obj,verified_correlations,Grp,Liste),
	member(Obj_2,Liste),
	rapport_correlations(Obj,Obj_2),
	!.
correlation_objets(Obj,Obj_2):-
	member(Att,[serie,pch,signal_metrique,signal_metrique_exp]),
	member(Att_2,[serie,pch,signal_metrique,signal_metrique_exp]),
	correlation(Obj,Att,Obj_2,Att_2,affichage_reduit),
	fail.
correlation_objets(Obj,Obj_2):-
	valeur_locale(Obj_2,groupe,Grp),
	(descripteur_local(Obj,verified_correlations,Grp,Liste)->true; Liste=[]) ,
	add_element(Obj_2,Liste,N_liste),
	set_des(Obj,verified_correlations,Grp,N_liste),
	rapport_correlations(Obj,Obj_2),
	!.



% --- Rapport de calcul de correlation
rapport_correlations(Obj):-
	valeur_locale(Obj,relevant_correlations,Liste),
	member(Obj_2,Liste),
	rapport_correlations(Obj,Obj_2),
	fail.
rapport_correlations(_Obj):-!.

rapport_correlations(Obj,Obj_2):-
	descripteur_local(Obj,Obj_2,Att,[Att_2,Lg_b_0-C_b_0,Lg_w_0-C_w_0,Bt_max-Lg_max-Max,Bt_min-Lg_min-Min]),
	write('CORRELATION OF :	'),write(Obj),write(' / '),write(Att),
	write('          WITH :	'),write(Obj_2),write(' / '),write(Att_2),nl,
	write('  in t-0 best  = '),write(C_b_0),write('		Lag = '),write(Lg_b_0),
	write('			    worst = '),write(C_w_0),write('	Lag = '),write(Lg_w_0),nl,
	write('       Abs Max = '),write(Max  ),write('		Lag = '),write(Lg_max),write('  Shift = '),write(Bt_max),
	write('		Min. Corr = '),write(Min),write('	Lag = '),write(Lg_min),write('  Shift = '),write(Bt_min),nl,
	nl,
	fail.
rapport_correlations(_Obj,_Obj_2):-!.


% --------------------------------------------------------------------------------
% Utilitaires pour les calculs de date (utilis�s par datastream egalement).

next_periode(jan-X,feb-X):-!.
next_periode(feb-X,mar-X):-!.
next_periode(mar-X,apr-X):-!.
next_periode(apr-X,may-X):-!.
next_periode(may-X,jun-X):-!.
next_periode(jun-X,jul-X):-!.
next_periode(jul-X,aug-X):-!.
next_periode(aug-X,sep-X):-!.
next_periode(sep-X,oct-X):-!.
next_periode(oct-X,nov-X):-!.
next_periode(nov-X,dec-X):-!.
next_periode(dec-X,jan-Y):-
	(var(Y) -> ( X = 99 -> Y = 0 ; Y is X+1)
		; ( Y = 0->X=99; X is Y-1)).
next_periode(q1-X,q2-X):-!.
next_periode(q2-X,q3-X):-!.
next_periode(q3-X,q4-X):-!.
next_periode(q4-X,q1-Y):-(var(Y) -> Y is X+1; X is Y-1) .

% ---
plus_ancien_que(P1,P2):- next_periode(P1,P2),!.
plus_ancien_que(P1,P2):- not(var(P1)), not(var(P2)),
			 next_periode(P1,PZ),
			 plus_ancien_que(PZ,P2,1),!.

plus_ancien_que(_P1,_P2,100):- !,fail.
plus_ancien_que(P1,P2,_):- next_periode(P1,P2),!.
plus_ancien_que(P1,P2,X):- not(var(P1)), not(var(P2)),
			 next_periode(P1,PZ),
			 Y is X+1,
			 plus_ancien_que(PZ,P2,Y),!.

% ---
plus_recent_que(P1,P2):-plus_ancien_que(P2,P1),!.

% ---
nb_periode_ecart(P_1,P_1,0):-!.
nb_periode_ecart(P_1,P_2,X):-
	nb_per_ecart(P_1,P_2,0,X),!.
nb_periode_ecart(P_1,P_2,X):-
	nb_per_ecart(P_2,P_1,0,Y),
	X is Y* (-1),!.
nb_periode_ecart(P_1,P_2,X):-
	var(P_1),
	nb_per_before(P_1,P_2,0,X),!.

nb_per_ecart(_,_,100,_In):-!,fail.
nb_per_ecart(P_1,P_1,In,In):-!.
nb_per_ecart(P_1,P_2,In,Y):-next_periode(P_1,P_Z),Z is In +1 ,
	nb_per_ecart(P_Z,P_2,Z,Y),!.

nb_per_before(_,_,100,_In):-!,fail.
nb_per_before(P_1,P_1,In,In):-!.
nb_per_before(P_1,P_2,In,Y):-next_periode(P_Z,P_2),Z is In +1 ,
	nb_per_before(P_1,P_Z,Z,Y),!.


% -------------------------------------------------------------------------
% ex-file : conv_date
% but  : conversion de dates en nb_de_jours et vice-versa

% version SWI  : 1/1/1970 = origine
nb_jour(J,M,A,Nb_j):- \+ var(J), \+ var(M), \+ var(A),!,
	( A =< 30 -> An is A+2000 ; (A>30,A<100)-> An is A+1900 ; An=A),
	date_time_stamp(date(An,M,J,0,0,0,0,-,-), TimeStamp),
	Nb_j is round(TimeStamp / 86400),!.

nb_jour(J,M,A,Nb_j):- \+ var(Nb_j),!,
	TimeStamp is Nb_j * 86400,
	stamp_date_time(TimeStamp, date(A,M,J,_,_Mn,_S,_Off,_TZ,_DST), 0).


/*------------------------------------------------ */

% appel � Nb_jour avec formats sp�ciaux
% idem avec format de date JJ/MM/AA
nb_jour(J/M/A,Nb_j):-nb_jour(J,M,A,Nb_j),!.

% idem avec un format de date JJ-MM-AA
nb_jour(J-M-A,Nb_j):-nb_jour(J,M,A,Nb_j),!.

% idem avec un format de date date(AA,MM,JJ)
nb_jour(date(A,M,J),Nb_j):-nb_jour(J,M,A,Nb_j),!.

% --------avec un format de date [JJ,MM,AA]
nb_jour([J,M,A],Nb_j):-
	write(predicat(nb_jour)),write(' ancienne maniere - a virer'),nl,
	nb_jour(J,M,A,Nb_j),!.

% ---- !  nb_jour('11-1-19',R). ??? !! dangereux / � laisser dans conversion sp�cifique
% 
% nb_jour(Atom,Nbj):- atomic(Atom),!,atom_to_term(Atom,Term,[]),
%	nb_jour(Term,Nbj).

	
% Calcul du nb de secondes pour un time donn�		utilise dans datastream
nb_secondes(H,M,S,RES):-
	RES is H*3600 + M*60 +S ,!.

% idem avec time au format time(H,M,S)
nb_secondes(time(H,M,S),R):-nb_secondes(H,M,S,R),!.


% ---------------------------------------
% week_day(Date,Week_day[1-7])   samedi= 6 dim=7
week_day(Nb_J,Week_day):- number(Nb_J),!,
	TimeStamp is Nb_J * 86400,
	stamp_date_time(TimeStamp, date(A,M,J,_,_Mn,_S,_Off,_TZ,_DST), 0),
	day_of_the_week(date(A,M,J),Week_day).

week_day(J/M/A,Week_day):- \+ var(J), \+ var(M), \+ var(A),!,
	( A =< 30 -> An is A+2000 ; (A>30,A<100)-> An is A+1900 ; An=A),
	day_of_the_week(date(An,M,J), Week_day).

week_day(date(A,M,J),Week_day):- \+ var(J), \+ var(M), \+ var(A),!,
	day_of_the_week(date(A,M,J), Week_day).

% today(date(AA,M,J),Nbj)
today(Date,Nbj):-
	get_time(TS),
	stamp_date_time(TS,D,local),date_time_value(date,D,Date),
	Nbj is truncate(TS / 86400).

maintenant(Date,time(H,M,S),Num):-
	get_time(TS),
	stamp_date_time(TS,Date_H,local),
	date_time_value(date,Date_H,Date),
	date_time_value(time,Date_H,time(H,M,Sec)),
	S is round(Sec),
	Num_1 is round(TS / 86.4) ,
	Num is Num_1/1000.




% ------------------------------------
% UTILITAIRES pour les calculs sur les dates et les �ch�ances ...
get_month(premier_mois,1).
get_month(deuxieme_mois,2).
get_month(second_mois,2).
get_month(troisieme_mois,3).
get_month(quatrieme_mois,4). % ne devrait pas exister car 3 mois dans un quarter
get_month(X,Y):- integer(X),X =< 4,X=Y,!.


ajuste_mois(M/A,M/A):- M>0,!.
ajuste_mois(M/A,M_n/A_n):-
	M_n is 12+M, % M etant negatif M_n sera plus_petit que 12 ...
	A_n is A-1.

mois_symb(1,jan).
mois_symb(2,feb).
mois_symb(3,mar).
mois_symb(4,apr).
mois_symb(5,may).
mois_symb(6,jun).
mois_symb(7,jul).
mois_symb(8,aug).
mois_symb(9,sep).
mois_symb(10,oct).
mois_symb(11,nov).
mois_symb(12,dec).
mois_symb(3,q1).
mois_symb(6,q2).
mois_symb(9,q3).
mois_symb(12,q4).

% utilis� pour les sorties HTM
mois_symbolique(1,'January').
mois_symbolique(2,'February').
mois_symbolique(3,'March').
mois_symbolique(4,'April').
mois_symbolique(5,'May').
mois_symbolique(6,'June').
mois_symbolique(7,'July').
mois_symbolique(8,'August').
mois_symbolique(9,'September').
mois_symbolique(10,'October').
mois_symbolique(11,'November').
mois_symbolique(12,'December').

jour_symbolique(1,'1st').
jour_symbolique(2,'2nd').
jour_symbolique(3,'3rd').
jour_symbolique(21,'21st').
jour_symbolique(22,'22nd').
jour_symbolique(23,'23rd').
jour_symbolique(31,'31st').
jour_symbolique(X,Symb):- atomic_concat(X,'th',Symb),!.



% Francais
convertit_mois(fr,feb,fev):-!.
convertit_mois(fr,apr,avr):-!.
convertit_mois(fr,may,mai):-!.
convertit_mois(fr,jun,juin):-!.
convertit_mois(fr,jul,juil):-!.
convertit_mois(fr,aug,aou):-!.
convertit_mois(_Lang,X,X):-!.


ajuste_quarter(Q/A,Q/A):- Q>0,!.
ajuste_quarter(Q/A,Q_n/A_n):-
	Q_n is 4-Q,
	A_n is A-1.

quarter_symb(1,q1).
quarter_symb(2,q2).
quarter_symb(3,q3).
quarter_symb(4,q4).


% donn�es pour calcul de periodicite_maj	% 03/2004
days_periode(X,d):- X >0, X<5,!.
days_periode(X,w):- X >6, X<9,!.
days_periode(X,m):- X >27, X<33,!.
days_periode(X,q):- X >89, X<95,!.
days_periode(_X,unk).


% get_periode_symb(+ Periode recherchee (entier positif) ,+ Date_symb de la periode t-0 ,- Date_symb)
% permet de calculer pour une periode (0-120..) la date symbolique lui correspondant
% !!! NE FONCTIONNE QUE POUR DES PERIODES MENSUELLES !!!

get_periode_symb(0,Ref,Ref):-!.
get_periode_symb(X,Ref,Res):-
	next_periode(Prec,Ref),
	X1 is X-1,
	get_periode_symb(X1,Prec,Res).

% calcule_date_a_partir_de_t(+Obj,+T,- Date)
calcule_date_a_partir_de_t(Obj,T,Date):-
	valeur_locale(Obj,last_periode,L_P),
	get_periode_symb(T,L_P,Date),!.

% -------------------------------------

annee_sur_quatre(Annee,Annee):-
	Annee >= 100,!.
annee_sur_quatre(An,Annee):-
	An > 60,An < 100 , Annee is An +1900,!.
annee_sur_quatre(An,Annee):-
	An >= 0,An =< 60 , Annee is An +2000,!.

date_avec_annee_sur_quatre(J/M/AA,J/M/Annee):-
	annee_sur_quatre(AA,Annee).

date_imprimable(D,String):-
	date_avec_annee_sur_quatre(D,J/M/Annee),
	(J<10 -> atomic_concat(0,J,Jour);atom_number(Jour,J)),
	(M<10 -> atomic_concat(0,M,Mois);atom_number(Mois,M)),
	sub_atom(Annee,_,2,0,An),
	atomic_list_concat([Jour,Mois,An],'/',String).

% ----------------------------------------------------
% convertir la date du jour en un entier Nb_jour
today_nbj(Nb_j):-
	get_time(Time),
	convert_time(Time,Y,M,D,_,_,_,_),
	nb_jour(D/M/Y,Nb_j).

% *******************************************************
%  Primitives EMICAT � remettre en jeu avec SWI

%  Quel Dessin est selectionn� (cliqu�)
what_des_gra(Dessin,_Graphique):-
	console(['pred : what_des_gra(',Dessin,
		 '_Graphique) � remettre en jeu - A_14_affichage_ANIE.pl']),
	!.
