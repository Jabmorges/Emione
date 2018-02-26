% file :	s_syntheses
% but :	calculs des synthese d'indicateur et de ce qui gravite autour
% date :	12/2011 - 2012

/* ----------------------------------------------------------------
Reflexions concernant comportement des les règles varihedge à creuser
- pouvoir dire que dans certains cas le signal d'un indic remporte(force)
  le résultat de toute une synthese (expl RSI>70 sur synthese technique)
- notion qu'un indicateur donne des signaux d'un seul coté (vente par expl)
  et qu'il est neutre dans les autres cas (ie pas de signal d'achat)

------------------------------------------------------------------- */


% calcule l'attribut : serie d'une synthese
% TAF : traiter correctement l'effet de bord de set_va(Obj,dates,...) +
% nettoyage du code...
%
calcule_synthese(Objet,Resultat):-
	valeur_locale(Objet,nom,Nom_synthese),
	valeur_locale(Objet,pays,Pays),
	Poids = 1,
	findall({Obj,s_signal}*Poids,
		(   valeur_locale(Obj,s_groupe,Nom_synthese),
		    valeur_locale(Obj,pays,Pays)
		), % ajouter recherche poids
		Bag ),
     console(['calcule_synthese Bag:',Bag]),
	somme_synthese(Bag,Vect_somme),
	findall(Poids, member({Obj,s_signal}*Poids ,Bag), L_poids ),
	sumlist(L_poids,Somme_poids),
	op_vecto(Vect_somme/Somme_poids,Resultat),

	length(Resultat,Lg_Res),
	% a mettre en predicat pour eviter bktg
	member({Obj,s_signal}*Poids,Bag),
	valeur_locale(Obj,dates,Dates),
	length(Dates,Lg_Res),!,
	set_val(Objet,dates,Dates),  % effet de bord à blinder un peu +

      % length(Bag,Lg_bag),
      % console(['calcule_synthese Resultat :', Resultat,nl]),
      % console(['calcule_synthese Lg_Res-Nb_elem-poids :',Lg_Res,Lg_bag,Somme_poids]),
	!.


somme_synthese([],[]).
somme_synthese([H|Tail],Res):-
	op_vecto(H,Vect_init),
	somme_synth(Tail,Vect_init,Res).

somme_synth([],Vect,Vect).
somme_synth([H|T],Vect_in,Res):-
	op_vecto(Vect_in+H,Vect_out),
	somme_synth(T,Vect_out,Res).


% ------------------------------------------------------------------
% calcule de la synthese de plusieures séries de signaux Achat/Vente
%
calcul_synthese_par_moyenne(O,Dates,Synthese):-
	valeur(O,composants,Composants),
      console([calcul_synthese_par_moyenne(O,Dates,Synthese),' composants:',Composants]),
      console(['Reste à faire ...']).



% --------------------------------------------------
% calcule le niveau de couverture pour un objet de type "s_modèle"
%
% stockage :
% couverture_eur :
%      - dates : chercher les dates sur la serie la plus courte des
%      composants  [stockées par effet de bord sur serie]
%      - serie : les niveaux de couverture calculées par calcule_couverture
%      - référence : ? la serie du marché de référence
% ? autre objet pour la strategie ?
%      futures : la serie du "futures" utilisé pour couvrir
%      taux sans risque : la serie pour l'investissement sans risque -
%      type_evaluateur : simple / margin_call
%
% Calculer l'évaluation de la couverture

% appelé par : s_modele:s_signal:si_besoin
%
% calcule_couverture(Obj,Att,L_couv,P_couv):- % calcul en daily(L_couv)
%	et en periodicite (P_couv)
%
calcule_couverture(Obj,Att,L_couv,P_couv):-
    % console(['calcule_couverture -  Start']),
	calcule_metrique_mult(Obj,_L_metrique_princ,L_couv_princ),
	set_val(Obj,couv_principale,L_couv_princ),                 % just for test
	calcule_complement(Obj,_L_metrique_compl,L_couv_compl),
	set_val(Obj,couv_compl,L_couv_compl),        % just for test

    % console(['      - compose_principal_et_complement ','  Start',nl]),
    % length(L_couv_princ,Lg_princ),
    % length(L_couv_compl,Lg_compl),
    % console(['  Lg_princ:',Lg_princ,'  Lg_compl:',Lg_compl]),

	compose_principal_et_complement(L_couv_princ,L_couv_compl,L_couv_gen),
	% set_ref_id_for_s_modele(Obj,Obj_ref-Att_ref),

	% stocke la couverture (daily) sur Obj,serie
	set_couverture_values_dates(Obj,Att,L_couv_gen,L_Dates,L_couv),

	% stocke la couverture en periodicité w-x sur Obj,Att % 2014/10
	(descripteur(Obj,Att,periodicite,Periodicite) ->
		extrait_dates_data(Periodicite,L_Dates,L_couv,P_Dates,P_couv),
		set_des(Obj,Att,dates,P_Dates) ;
		P_couv = L_couv),
	set_val(Obj,Att,P_couv),
	!.


% set la serie de reference sur Obj:s_modele:serie
% 2014_10 : pas utilisé !! servait en phase de dev..
%  ref_id et bench_id doivent être créés avec l'objet
%
set_ref_id_for_s_modele(Obj,Ref_id):-
	valeur_locale(Obj,composants,[Comp1|_Composants]),
	descripteur(Comp1,s_signal,ref_id,Ref_id),
	set_val(Obj,ref_id,Ref_id),
	!.


% set les valeurs de couverture et dates sur Obj,serie et Obj,dates
%     L_couv_in a la longeur de l'indice sur lequel il est calculé
%     ==> l'ajuster à la longueur de l'objet pour lequel il est calculé
%     calcul en daily !!!
%
set_couverture_values_dates(Obj,Att,L_couv_in,Dates,L_couv):-
	descripteur(Obj,Att,ref_id,O_ref-A_ref),
	valeur(O_ref,dates,O_ref_dates),
	length(O_ref_dates,O_ref_dates_lg),
	length(L_couv_in,Lg_couv),
	(Lg_couv > O_ref_dates_lg -> (Dates= O_ref_dates,
				      get_sublist(O_ref_dates_lg,L_couv_in,L_couv,_))
	                                  ;
				     (get_sublist(Lg_couv,O_ref_dates,Dates,_),
				      L_couv=L_couv_in)
	),
	set_val(Obj,dates,Dates),
	set_val(Obj,serie,L_couv),
	!.


% ----------------------------------------------------------------
% calculer le poids induit par les composants principaux
% si possible en une seule passe sur la liste
%
calcule_metrique_mult(Obj,L_Metrique,L_Couverture):-
	valeur(Obj,composants,L_Composant),
	findall(Signal,(member(Comp,L_Composant),valeur(Comp,s_signal,Signal)),Bag_sig),
	map_list_to_pairs(length,Bag_sig,Key_sig_lsts),
	sort(Key_sig_lsts,K_LL_sig),
	pairs_values(K_LL_sig,LL_sig),
	valeur(Obj,poids_couverture,Pds_couv_pairs),
	n_listei_to_i_listen(LL_sig,Pds_couv_pairs,L_Metrique,L_Couverture),
	!.


n_listei_to_i_listen([[]|_Lsts],_,[],[]):-!.
n_listei_to_i_listen([[X|T1]|Lsts],M_couv_pairs,[Metrique|R_Met],[Poids|R_Poids]):-
	extrait_firsts(Lsts,Firsts,T_Lsts),
	sort([X|Firsts],Dom_sig),   % Sorted_sig pourrait être param dom_signaux = [1,2,3]
	findall(M, (member(Y,Dom_sig),
	            aggregate(count,member(Y,[X|Firsts]),Count) , M is Y**Count ),
		Bag_M),
	multlist(Bag_M,Metrique),
	memberchk(Metrique-Poids,M_couv_pairs),
	n_listei_to_i_listen([T1|T_Lsts],M_couv_pairs,R_Met,R_Poids),
	!.

extrait_firsts([],[],[]):-!.
extrait_firsts([[First|T_Lst]|Next_lsts],[First|Nxt_Firsts],[T_Lst|Nxt_lsts]):-
	extrait_firsts(Next_lsts,Nxt_Firsts,Nxt_lsts).


% calcule l'impact des indiateurs complémentaires
% NB : le calcule est effectué de maniere brutaliste,
%      sans utiliser complètement les paramètrages possibles
%
calcule_complement(Obj,LL_sig,L_Couv_compls):-
	valeur(Obj,complements,L_indic_compl),
	findall(Signal,(member(Comp,L_indic_compl),valeur(Comp,s_signal,Signal)),Bag_sig),
	map_list_to_pairs(length,Bag_sig,Key_sig_lsts),
	sort(Key_sig_lsts,K_LL_sig),
	pairs_values(K_LL_sig,LL_sig),
	length(LL_sig,Nb_complements),
	Poids_compl_ind is 100.00/Nb_complements , % répartition du poids pour chaque compl
	descripteur(Obj,complements,impact_max,Impact_max_compls),
	calc_couv_complements(LL_sig,Poids_compl_ind,Impact_max_compls,L_Couv_compls),
	!.


calc_couv_complements([[]|_Lsts],_Pds_compl_ind,_Impact_max_compls,[]):-!.
calc_couv_complements(LL_sig,Pds_compl_ind,Impact_max_compls,[Couv|T_couvs]):-
	extrait_firsts(LL_sig,Elements,T_LL_sig),
	calc_couv_comp(Elements,Pds_compl_ind,Impact_max_compls,0,Couv),
	calc_couv_complements(T_LL_sig,Pds_compl_ind,Impact_max_compls,T_couvs),
	!.

calc_couv_comp([],_Pds_compl_ind,_Impact_max_compls,Couv,Couv):-!.
calc_couv_comp([Signal|T],Pds_compl_ind,Impact_max_compls,Couv_sf,Couv):-
	N_Couv_sf is (-1*(Signal-2)*Pds_compl_ind*Impact_max_compls)+Couv_sf,
	calc_couv_comp(T,Pds_compl_ind,Impact_max_compls,N_Couv_sf,Couv).

% compose lsignal principal et complements
%
compose_principal_et_complement([],_,[]):- !.
compose_principal_et_complement([X|T_X],[Y|T_Y],[Couv|T_couv]):-
	Couv is max(0,min(X+Y,100)) ,
	compose_principal_et_complement(T_X,T_Y,T_couv),
	!.


% --------------------------------------------------
% extraire de 2 vecteurs dates & Données des sous-ensembles
% correspondant à la fréquence souhaitée
extrait_dates_data(d,Dates,Datas,Dates,Datas):- !.
extrait_dates_data(w-DotW,Dates_in,Datas_in,Dates_out,Datas_out):-
	skip_until_day_of_the_week(DotW,Dates_in,Datas_in,Dates,Datas),
	extrait_weekly(Dates,Datas,Dates_out,Datas_out),
	!.

extrait_dates_data(m-DotM,Dates_in,Datas_in,Dates_out,Datas_out):-
	skip_until_day_of_the_month(DotM,Dates_in,Datas_in,Dates,Datas),
	extrait_monthly(Dates,Datas,Dates_out,Datas_out),
	!.


% for Day of the week
%
% skip until Date is equal to DotW (Day_of_the_week)
skip_until_day_of_the_week(DotW,[Date|Dates],[Data|Datas],[Date|Dates],[Data|Datas]):-
	week_day(Date,DotW),!.
skip_until_day_of_the_week(DotW,[_Date|Dates],[_Data|Datas],Dates_out,Datas_out):-
	skip_until_day_of_the_week(DotW,Dates,Datas,Dates_out,Datas_out).

% then extract Date and Data on a weekly basis (Date-7 (days))
extrait_weekly([Date|T_Dates],[Data|T_Datas],[Date|Dates_out],[Data|Datas_out]):-
	ND is Date-7,
	extrait_weekly_2(ND,T_Dates,T_Datas,Dates_out,Datas_out),
	!.

extrait_weekly_2(_ND,[],_T_Datas,[],[]):-	!.
extrait_weekly_2(D,[D|T_Dates],[Data|T_Datas],[D|Dates_out],[Data|Datas_out]):-
	ND is D-7,
	extrait_weekly_2(ND,T_Dates,T_Datas,Dates_out,Datas_out),
	!.
extrait_weekly_2(D,[D_s|T_Dates],[Data|T_Datas],[D|Dates_out],[Data|Datas_out]):-
	D > D_s,
	ND is D_s-7+(D-D_s),
	ND <D ,   % pour parer à des exces
	nb_jour(Date_pb,D),
	console(['  !!! extrait_weekly_2 pb de continuite de dates au :',Date_pb]),

	extrait_weekly_2(ND,T_Dates,T_Datas,Dates_out,Datas_out),
	!.
extrait_weekly_2(D,[_D|T_Dates],[_Data|T_Datas],Dates_out,Datas_out):-
	extrait_weekly_2(D,T_Dates,T_Datas,Dates_out,Datas_out),
	!.


% for Day number in Month
% A FINALISER !!!!!!!!!!!!!!!
skip_until_day_of_the_month(DotM,[Date|Dates],[Data|Datas],[Date|Dates],[Data|Datas]):-
	Date_stamp is Date*86400, stamp_date_time(Date_stamp,date(_A,_M,DotM,_,_,_,_,_,_),0),
	!.
skip_until_day_of_the_month(DotM,[_Date|Dates],[_Data|Datas],Dates_out,Datas_out):-
	skip_until_day_of_the_month(DotM,Dates,Datas,Dates_out,Datas_out).

extrait_monthly([D|T_Dates],[Data|T_Datas],[D|Dates_out],[Data|Datas_out]):-
	ND is D-7,
	extrait_monthly_2(ND,T_Dates,T_Datas,Dates_out,Datas_out),
	!.
% suite à trouver....
extrait_monthly_2(ND,T_Dates,T_Datas,Dates_out,Datas_out):-
	console([extrait_monthly_2,' à finaliser Var=',
		 ND,T_Dates,T_Datas,Dates_out,Datas_out]),
	!.


% --------------------------------------------------
% dev...
test_synthese:-
	member(X,[technique,sentiment]),
	test_synth(X),fail.
test_synthese:-!.

test_synth(Type) :-
	valeur(Obj,s_groupe,Type),
	valeur(Obj,dates,D),
	valeur(Obj,s_signal,S),
	length(D,LD),length(S,LS),
	console(['test length dates et s_signal pour Objet : ',Obj,nl,
		'  Lg_dates =',LD,'  Lg_serie=',LS]).

test_extrait(Obj,DotW,Dates,Serie,Dates_out,Datas_out):-
	valeur(Obj,dates,Dates),
	valeur(Obj,serie,Serie),
	extrait_dates_data(w-DotW,Dates,Serie,Dates_out,Datas_out),
	!.

test_same_length(Res):-
	valeur(couverture_eur,dates,Dates),
	valeur(couverture_eur,serie,Serie),

	valeur(gexcs00_eur,serie,Ge_S),
	same_length_lists_cut_front([Dates,Serie,Ge_S],Res),
	!.

%--- FIN
