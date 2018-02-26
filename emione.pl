% file : emione		esquisse de mécanisme objet style O,A,D,V avec heritage
%			specs reprises de EMICAT (esd)
% version : emione_V2  (avec att(X) de type compound )

:- module(mi_one, [		%JAB		CAB
	creer_obj/2,		% OK
	tuer_obj/1,		% OK
	objet/2,		% problème ?
	pere_de/2,		% OK
	fils_de/2,		% OK
	herite_de/2,		% OK
	objet_qq/1,		% OK
	objet_terminal/1,
	set_val/3,		% OK
	reset_val/2,		% OK
	valeur/3,		% OK
	valeur_locale/3,	% OK
	valeur_trouvable/2,
	activer_val/3,		% OK
	set_des/4,		% OK
	reset_des/3,		% OK
	descripteur/4,		% OK
	descripteur_local/4,	% OK
	activer_des/4,		% OK
	set_mar/4,		% OK
	reset_mar/3,		% OK
	marqueur/4,		% OK
	activer_mar/4,		% OK

	appliquer/3,
	appliquer/4,
	selectionner/1,
	passage/3,
	saturer/4,
	prouver_avant/5,

	sauver/3,
	sauver_sec/3,		% OK
	sauver_sec/4,
	vider_sec/1,
	tuer_sec/1,
	inclure_sec/1,		% OK
	redo_filiation/0,	% ?OK? refait filiation des objets pour héritage

	creer_lien/2,
	tuer_lien/2,
	purger/0,

	montrer_tout/1,		% remplacement de voir MI4
	montrer_plus/1,		% remplacement de voir MI4
	montrer/1	,	% remplacement de voir MI4

	concat/5,		% +- OK
	element_de/4,		% +- OK
	terme_de/2,		% OK

	copy_obj/2,		% copier les att,desc,mar Obj_source ==> Obj_dest
	peres_de/2		% nouveau ds emione ( liste des peres)
	]).
% --- operateurs d'application et non EmiOne dans 00_ex_util.pl
% --- operateurs EmiOne (mais déjà déclarés dans 00_ex_util.pl
%  :- op(980,xfx,user:(::)).
%  :- op(900,fy,user:not) .
%  :- op(895,xfy,user:'&').
% autres :  if then else

% Creer_obj ---------------------------
creer_obj(Obj,Ancetres) :-mi_creer_obj(Obj,Ancetres).

mi_creer_obj(Obj,Ancs):-
	atom(Obj),not(Obj=[]),                  % ou atomic(Obj) ?
	not(m_objet:obj(Obj,_) ),		% test de non exist. de Obj
	list_to_set(Ancs,Ancetres),
	mi_existance_objets(Ancetres),		% les ancetres doivent exister
	mi_avant_inst(Obj,Ancetres),
	assert(m_objet :obj(Obj,Ancetres)),
	mi_create_filiation(Obj,Ancetres),
	mi_apres_inst(Obj,Ancetres),
  %	write('--> fin de creer_obj  : '),write(Obj),write(' OK'),nl,
	!.
mi_creer_obj(Obj,Ancetres) :-
	write('MI_ERR : creer_obj :'),
	write(Obj-Ancetres),write(' Failed'),nl,fail.


mi_create_filiation(Obj,Parents):-
	forall(member(Pere,Parents),assert(m_fils:fils(Pere,Obj))),
	% creer terminal(_,_)
	forall(member(Pere,Parents),m_terminal:retractall(terminal(_,Pere))),
	mi_ancetres_de_liste(Parents,Gd_Ancetres),
	append(Parents,Gd_Ancetres,Ancetres),
	forall(member(Anc,Ancetres),m_terminal:assert(terminal(Anc,Obj))),
	forall(member(Anc,Ancetres),m_ancetre:assert(ancetre(Obj,Anc))).


% refait une filiation complète
% il y a un pb lors du chargement des SEC ==> faire redo_filiation
%
%% redo_filiation palie un problème lors du chargement des SECs lorsque
% des Obj ont plusieurs pères dans des arborescences différentes.
% Le pb pourrait être règlé si au moment de la sauvegarde du SEC on
% stocke les Obj dans l'ordre dans lequel ils sont créés originelement
% Par expl l'arborescence 'groupe' est créé avant les indicateurs
% ce qui permet de bien gérer la notion terminal(Ascendant,Obj)

redo_filiation:-
	purger_module(m_fils),
	purger_module(m_terminal),
	purger_module(m_ancetre),
	%  sans faire appel à ancetre ....
	forall(m_objet :obj(Obj,Ancetres),redo_filiation(Obj,Ancetres)),
	!.

redo_filiation(Obj,Parents):-
	forall(member(Pere,Parents),assert(m_fils:fils(Pere,Obj))),
	% creer terminal(_,_)
	forall(member(Pere,Parents),m_terminal:retractall(terminal(_,Pere))),
	%  sans faire appel à ancetre ..
	mi_ancetres_via_obj(Obj,Ancetres),
	(m_fils:fils(Obj,_)
	  -> true ;
	     forall(member(Anc,Ancetres),m_terminal:assert(terminal(Anc,Obj)))),
	forall(member(Anc,Ancetres),m_ancetre:assert(ancetre(Obj,Anc))).




% tous les éléments de la liste sont des objets existant
mi_existance_objets([]):-!.
mi_existance_objets([Obj|Autres_objs]):-
	m_objet:obj(Obj,_),
	mi_existance_objets(Autres_objs).


% reflexes sur creer_obj
mi_avant_inst(O,Peres):-
	mi_ancetres_de_liste(Peres,Gds_anc),
	append(Peres,Gds_anc,Ancetres),
	forall(member(OO,Ancetres),
		(m_oav:att_val(OO,avant_inst,Code)->mi_inst_obj_exe(Code,O,Peres);true)),!.

mi_apres_inst(O,Peres):-
	mi_ancetres_de_liste(Peres,Gds_anc),
	append(Peres,Gds_anc,Ancetres),
	forall(member(OO,Ancetres),
		(m_oav:att_val(OO,apres_inst,Code)->(mi_inst_obj_exe(Code,O,Peres);true);true)),!.

%------------------------------------------
tuer_obj(Obj):-	mi_tuer_obj(Obj).

mi_tuer_obj(Obj):-	nonvar(Obj),
	m_objet:obj(Obj,Peres),
	not(m_fils:fils(Obj,_)),
	not(m_oav:att_val(Obj,_,_)),
	not(m_oav:desc_val(Obj,_,_,_)),
	not(m_oav:marq_val(Obj,_,_,_)),
	!,
	mi_avant_dest(Obj,Peres),
	m_objet:retract(obj(Obj,Peres)),
	m_fils:retractall(fils(_,Obj)),
	m_ancetre:retractall(ancetre(Obj,_)),
	m_terminal:retractall(terminal(_,Obj)),
	forall((member(P,Peres),not(m_fils:fils(P,_)),m_ancetre:ancetre(P,Anc)),
			m_terminal:assert(terminal(Anc,P))),	% creer les nouveaux terminaux

	m_ancetre:retractall(ancetre(Obj,_)),
	mi_apres_dest(Obj,Peres),!.

mi_tuer_obj(Obj):-	write('MI_ERR : tuer_obj : '),write(Obj),write('        Failed'),nl,fail.


% reflexes sur tuer_obj
mi_avant_dest(O,Peres):-
	mi_ancetres_de_liste(Peres,Gds_anc),append(Peres,Gds_anc,Ancetres),
	forall(member(OO,Ancetres),
		(m_oav:att_val(OO,avant_dest,Code)->mi_inst_obj_exe(Code,O,Peres);true)),!.

mi_apres_dest(O,Peres):-
	mi_ancetres_de_liste(Peres,Gds_anc),append(Peres,Gds_anc,Ancetres),
	forall(member(OO,Ancetres),
		(m_oav:att_val(OO,apres_dest,Code)->(mi_inst_obj_exe(Code,O,Peres);true);true)),!.


% execution des reflexes sur instanciation /
% A faire : une protection anti_bouclage type Mi4 avec passage
% du type avant ou apres et assert(avant_inst(O))
mi_inst_obj_exe((parm(O,Peres),Clause),O,Peres):-mi_executer(Clause,O).


% -----------------------------
objet(Obj,Type):- m_terminal:terminal(Type,Obj).	% mi_object(Obj,Type).

% -----------------------------
pere_de(Pere,Fils):- m_fils:fils(Pere,Fils).

peres_de(Pere,Fils):-nonvar(Pere), findall(F ,m_fils:fils(Pere,F),Fils),!.
peres_de(Peres,Fils):-nonvar(Fils), findall(P ,m_fils:fils(P,Fils),Peres),!.

fils_de(Fils,Pere):- nonvar(Fils),m_objet:obj(Fils,P),!,member(Pere,P).
fils_de(Fils,Pere):- nonvar(Pere),m_fils:fils(Pere,Fils).    % ajouté 2014_01_05  ?pertinence?
% -------------------------------
herite_de(Obj,Pere) :- m_ancetre:ancetre(Obj,Pere).

% -------------------------------
mi_lignee_de(O,[O|Ancetres]):-
	mi_ancetres_de(O,Ancetres),
	!.

mi_ancetres_de(O,Ancetres):-
	findall(Anc,m_ancetre:ancetre(O,Anc),Ancetres),
	!.

mi_ancetres_de_liste(L_Obj,Ancetres):-
	findall(Anc,(member(O,L_Obj),m_ancetre:ancetre(O,Anc)),Ancs),
	list_to_set(Ancs,Ancetres),!.

% ---
mi_ancetres_via_obj(Obj,Ancetres):-
	mi_ancetres_via_objets(Obj,[],Ancs),
	flatten(Ancs,Ancetres),
	!.

mi_ancetres_via_objets(Obj,X,X):-
	m_objet:obj(Obj,[]),
	!.
mi_ancetres_via_objets(Obj,In,Bag):-
	m_objet:obj(Obj,Peres),
	% append(In,Peres,Ancetre_so_far),
	findall(Ancetres,(member(P,Peres),
			  append(In,[P],N_in),
			  mi_ancetres_via_objets(P,N_in,Ancetres)),Bag),
	!.



% ----
mi_descendants_de(O,Descendants):-
	findall(Desc,m_ancetre:ancetre(Desc,O),Descendants),
	!.

mi_descendants_de_liste(L_Obj,Descendants):-
	findall(Desc,(member(O,L_Obj),m_ancetre:ancetre(Desc,O)),Descs),
	list_to_set(Descs,Descendants),
	!.

mi_lo_et_descendants_de_liste(L_Obj,Descendants):-
	findall(Desc,(member(O,L_Obj),(Desc=O;m_ancetre:ancetre(Desc,O))),Descs),
	list_to_set(Descs,Descendants),
	!.

% ------------------------------
objet_qq(Obj):-	m_objet:obj(Obj,_).		% verifie que Obj existe  + bktrable


objet_terminal(Obj):-
	m_objet:obj(Obj,_),
	\+ m_terminal:terminal(Obj,_Desc).
% ------------------------------------------------------------------------------------
set_val(O,A,V):- mi_set_val(O,A,V).

mi_set_val(O,A,V) :-		% controles
	m_objet:obj(O,_),	% O est un Objet
	ground(A),		% permet att(xyz)                  emione_V2
	% atomic(A),		% A est atomic {atom,chaine car,entier,réel}
	mi_setval(O,A,V),
	!.		% ? faut il ! ?

mi_setval(O,A,V):-m_oav:att_val(O,A,OV),not(var(OV)),OV=V,!.	% O,A,V existe déjà ==> ne rien faire + true
mi_setval(O,A,N_V):-
	mi_val_sans_si_b(O,A,O_V),!,		% si Existe valeur(O,A,O_V) sans si_besoin==> !
	mi_avant_modif(O,A,O_V,N_V),	% reflexe
	m_oav:retractall(att_val(O,A,_)),	% ou retract(...)->true ; true
	m_oav:assert(att_val(O,A,N_V)),
	mi_apres_modif(O,A,O_V,N_V),!.	% reflexe
mi_setval(O,A,N_V):-
	mi_avant_ajout(O,A,N_V),		% reflexe
	m_oav:assert(att_val(O,A,N_V)),
	mi_apres_ajout(O,A,N_V),!.		% reflexe



mi_avant_modif(O,A,O_V,N_V):-
	(once(mi_descr(O,A,avant_modif,(parm(O,A,O_V,N_V),Clause)))->mi_att_exe(Clause,O,A);true).

mi_apres_modif(O,A,O_V,N_V):-
	once(mi_descr(O,A,apres_modif,(parm(O,A,O_V,N_V),Clause))),
	mi_att_exe(Clause,O,A),!.
mi_apres_modif(_O,_A,_O_V,_N_V).	% true in fine



mi_avant_ajout(O,A,N_V):-
	(once(mi_descr(O,A,avant_ajout,(parm(O,A,N_V),Clause)))->mi_att_exe(Clause,O,A);true).

mi_apres_ajout(O,A,N_V):-
	once(mi_descr(O,A,apres_ajout,(parm(O,A,N_V),Clause))),
	mi_att_exe(Clause,O,A),!.
mi_apres_ajout(_O,_A,_N_V).	% true in fne


% ------------------------------
reset_val(O,A):- nonvar(O),ground(A),m_objet:obj(O,_),mi_reset_val(O,A),!.

reset_val(O,A):- nonvar(O),compound(A),m_objet:obj(O,_),mi_reset_val_comp(O,A).
% faire une version adaptée au mav(X)

mi_reset_val(O,A):-
	m_oav:att_val(O,A,V), !,	% coupe choix
	mi_avant_retrait(O,A,V),
	m_oav:retract(att_val(O,A,_V)),
	mi_apres_retrait(O,A,V).
mi_reset_val(_,_):- !.

mi_reset_val_comp(O,A):-
	copy_term(A,A_Copy),
	m_oav:att_val(O,A,V), !,	% coupe choix
	mi_avant_retrait(O,A,V),
	m_oav:retractall(att_val(O,A_Copy,_V)),
	mi_apres_retrait(O,A,V).
mi_reset_val_comp(_,_):- !.


mi_avant_retrait(O,A,V):-
	(once(mi_descr(O,A,avant_retrait,(parm(O,A,V),Clause)))->mi_att_exe(Clause,O,A);true).

mi_apres_retrait(O,A,V):-
	once(mi_descr(O,A,apres_retrait,(parm(O,A,V),Clause))),
	mi_att_exe(Clause,O,A),!.
mi_apres_retrait(_,_,_).


% ------------------------------
valeur(O,A,V):- mi_valeur(O,A,V).


mi_valeur(O,A,V):- nonvar(O),nonvar(A),!,  % ground(A) ? % couvre 2 cas : var(V) et nonvar(V)	OK
		once(mi_val(O,A,VT,O)),V=VT.
% pour traiter les attributs composés : att(Composé) ; il faudrait
% utiliser ground(A) et laisser backtracker ...
% Actuellement (09/2013) valeur_locale(Obj,att(Composé),Valeur) fait le
% backtrack

mi_valeur(O,A,V):- nonvar(O),var(A),		% couvre les 2 cas nonvar(V),
		m_objet:obj(O,_),
		mi_liste_att_obj(O,_Lignee,Atts),!,
		member(A,Atts),
		once(mi_val(O,A,V_T,O)),V=V_T.

mi_valeur(O,A,V):-	var(O),nonvar(A), % ground(A)	% base : 813 Inf. pour valeur(O,age,X) !! à battre !!
		findall(O1,(m_oav:att_val(O1,A,V);m_oav:desc_val(O1,A,si_besoin,_G)),B1),
		list_to_set(B1,Set_O),
		mi_lo_et_descendants_de_liste(Set_O,Descendants),
		member(O,Descendants),
		%				 m_objet:obj(O,_),	% ancienne version !!
		once(mi_val(O,A,V_T,O)),V=V_T.

mi_valeur(O,A,V):-	var(O),var(A),		% couvre 2 cas :	var(V) et nonvar(V)	OK
		m_objet:obj(O,_),
		mi_liste_att_obj(O,_Lignee,Atts),
		member(A,Atts),
		once(mi_val(O,A,V_T,O)),V=V_T.


mi_val(O,A,V,_):-	m_oav:att_val(O,A,V).	% cherche la valeur localement
mi_val(O,A,V,O_origine):-			% utilise la methode calcul locale si existe
	m_oav:desc_val(O,A,si_besoin,Clause),
	mi_val_si_besoin(O_origine,A,V,Clause).
mi_val(O,A,V,O_origine):-			% cherche par heritage
	m_ancetre:ancetre(O,Anc),
	(m_oav:att_val(Anc,A,V)->
		true;
		(m_oav:desc_val(Anc,A,si_besoin,Clause),
		mi_val_si_besoin(O_origine,A,V,Clause))
	).


mi_val_si_besoin(O,A,V,(parm(O,A,V),Callable)):-
  %  write('MI_internal  : mi_val_si_besoin/3	:'-Callable),nl,
	mi_att_exe(Callable,O,A),!.
mi_val_si_besoin(O,A,V,parm(O,A,V)):-!.



% valeur(O,A,V)  avec heritage mais pas de déclenchement de si_besoin
mi_val_sans_si_b(O,A,V):-	m_oav:att_val(O,A,V).	% cherche la valeur localement
mi_val_sans_si_b(O,A,V):-				% cherche par heritage bktk
	m_ancetre:ancetre(O,Anc),
	m_oav:att_val(Anc,A,V).


mi_liste_att_obj(O,Lignee,Atts):-	% donne tous les attributs heritables d'un Obj
	mi_lignee_de(O,Lignee),
	setof_or_void(A, Anc^D^(member(Anc,Lignee),
			(m_oav:att_val(Anc,A,D);m_oav:desc_val(Anc,A,si_besoin,D))),Atts),!.

% execution des reflexes sur instanciation /
% une protection anti_bouclage type Mi4 avec passage du type avant ou apres et assert(avant(O,A)) : à faire
mi_att_exe(Clause,O,_A):-mi_executer(Clause,O).

% ------------------------------
valeur_locale(O,A,V):-	% voir contrôles EMICAT
	m_oav:att_val(O,A,V).
        % mettre ! à la fin ? Non car att_val(O,Att(X),V)

% ------------------------------
valeur_trouvable(O,A):-
	\+var(O), \+var(A),
	m_objet:obj(O,_),
	mi_liste_att_obj(O,_Lignee,Atts),
	memberchk(A,Atts).




% ------------------------------
activer_val(O,A,Parm):-	mi_activer_val(O,A,Parm).

mi_activer_val(O,A,parm):- !,
	mi_val(O,A,Code,O),
	mi_executer(Code,O).
mi_activer_val(O,A,Parm):-
	mi_val(O,A,(Parm,Code),O),
	mi_executer(Code,O).

% -----------------------------------------------------------------
set_des(O,A,D,V):- mi_set_des(O,A,D,V).

mi_set_des(O,A,D,V) :-		% controles
	m_objet:obj(O,_),	% O est un Objet
	nonvar(A),		% permet  att(X)                emione_V2
	% atomic(A),		% A est atomic {atom,chaine car ,entier,réel}

	% atomic(D),
	ground(D),		% permet  descr(d1)  % 2014_10
	mi_setdes(O,A,D,V),
	!.		% ? faut il ! ?

mi_setdes(O,A,D,V):-m_oav:desc_val(O,A,D,V),!.	% O,A,D,V existe déjà ==> ne rien faire + true

mi_setdes(O,A,D,V):-
	m_oav:retractall(desc_val(O,A,D,_)),
	m_oav:assert(desc_val(O,A,D,V)).

% ------------------------------
reset_des(O,A,D):-	mi_reset_des(O,A,D).

mi_reset_des(O,A,D):-
	m_objet:obj(O,_),	% O est un Objet
	nonvar(A),		% permet att(X)                   emione_V2
	% atomic(A),		% A est atomic {atom,chaine car ,entier,réel}
	% atomic(D),
	ground(D),		% permet  descr(d1)  % 2014_10
	!,
	m_oav:retractall(desc_val(O,A,D,_)).
mi_reset_des(O,A,D):-
	write('MI_ERR : reset_des('),write(O-A-D),write(')	Failed'),nl,fail.

% ------------------------------
descripteur(O,A,D,V):- mi_descripteur(O,A,D,V) .

mi_descripteur(O,A,D,V):- nonvar(O),nonvar(A),nonvar(D),
	once(mi_descr(O,A,D,V)),!.
mi_descripteur(O,A,D,V):- m_objet:obj(O,_),
	mi_liste_att_desc_obj(O,Lignee,Atts),
	member(A,Atts),
	mi_liste_descr_obatt(Lignee,A,Descs),
	member(D,Descs),
	once(mi_descr(O,A,D,V)).

mi_descr(O,A,D,V):- (m_oav:desc_val(O,A,D,V) ).	% cherche la valeur localement
mi_descr(O,A,D,V):-				% cherche heritage par bktk
	m_ancetre:ancetre(O,Anc),
	m_oav:desc_val(Anc,A,D,V).

mi_liste_descr_obatt(Lignee,A,Descs_sans_doublons):-
	findall(D,(member(Anc,Lignee),m_oav:desc_val(Anc,A,D,_V)),Descs),
	% ajout de sort pour supprimer les doublons sans faire de setof
	sort(Descs,Descs_sans_doublons),
	!.

mi_liste_att_desc_obj(O,Lignee,Atts):-	% donne tous les attributs  d'un Obj
	mi_lignee_de(O,Lignee),
	setof_or_void(A,Anc^V^D^VD^(member(Anc,[O|Lignee]),
		   (   m_oav:att_val(Anc,A,V);m_oav:desc_val(Anc,A,D,VD))),
		Atts),
	!.


% ------------------------------
descripteur_local(O,A,D,V):- m_oav:desc_val(O,A,D,V).

% ------------------------------
activer_des(O,A,D,Parm):-	mi_activer_des(O,A,D,Parm).

mi_activer_des(O,A,D,Res):-var(Res),!,
	mi_descr(O,A,D,(parm(O,A,D,Res),Code)),
	mi_executer(Code,O).
mi_activer_des(O,A,D,Parm):-
	mi_descr(O,A,D,(Parm,Code)),!,
	mi_executer(Code,O).
mi_activer_des(O,A,D,parm):-!,  % était en 2eme position
	mi_descr(O,A,D,Code),
	mi_executer(Code,O).
% ----------------------------------------------------------------------------------
set_mar(O,A,M,V):- mi_set_mar(O,A,M,V).

mi_set_mar(O,A,M,V) :-		% controles
	m_objet:obj(O,_),		% O est un Objet
	atomic(A),		% A est un atomic {atom,chaine car ,entier,réel}
	atomic(M),
	mi_setmar(O,A,M,V),
	!.		% ? faut il ! ?

mi_setmar(O,A,D,V):-m_oav:marq_val(O,A,D,V),!.	% O,A,D,V existe déjà ==> ne rien faire

mi_setmar(O,A,D,V):-
	m_oav:retractall(marq_val(O,A,D,_)),
	m_oav:assert(marq_val(O,A,D,V)).

% -------------------------------
reset_mar(O,A,M):- mi_reset_mar(O,A,M).

mi_reset_mar(O,A,D):-
	m_objet:obj(O,_),	% O est un Objet
	atomic(A),	% A est un atomic {atom,string,entier,réel}
	atomic(D),!,
	m_oav:retractall(marq_val(O,A,D,_)).
mi_reset_mar(O,A,D):-
	write('MI_ERR : reset_mar('),write(O-A-D),write(')	Failed'),nl,fail.

% -------------------------------
marqueur(O,A,M,V):- m_oav:marq_val(O,A,M,V).

% old version
% marqueur(O,A,M,V):- mi_marq(O,A,M,V).
% mi_marq(O,A,M,V):- m_oav:marq_val(O,A,M,V).

% ------------------------------
activer_mar(O,A,D,Parm):-	mi_activer_mar(O,A,D,Parm).

mi_activer_mar(O,A,D,parm):- !,
	m_oav:marq_val(O,A,D,Code),
	mi_executer(Code,O).
mi_activer_mar(O,A,D,Parm):-
	m_oav:marq_val(O,A,D,(Parm,Code)),
	mi_executer(Code,O).


% --------------------------------------------------------------------------------------------
% executer un but avec passage de l'objet d'appel pour traiter : destinataire(Obj)
mi_executer((A->B;C),Obj):-!,
  %  write('--- mi_executer'-A-'	'-B-'	'-C),nl,
	(mi_executer(A,Obj)->mi_executer(B,Obj);mi_executer(C,Obj)).
mi_executer((A,B),Obj):-!, mi_executer(A,Obj),mi_executer(B,Obj).
mi_executer((A;B),Obj):- !, (mi_executer(A,Obj);mi_executer(B,Obj)).
mi_executer(destinataire(Obj),Obj):- !.
mi_executer(Goal,_):- call(Goal).

% --------------------------------------------------------------------------------------------
% implantation des règles et des strategies d'exploitation
% --------------------------------------------------------------------------------------------

appliquer(_,_,_):- write('***appliquer/3		A FAIRE'),nl.
appliquer(_,_,_,_):- write('***appliquer/4		A FAIRE'),nl.
saturer(_,_,_,_):- write('***saturer/4			A FAIRE'),nl.
prouver_avant(_,_,_,_,_):- write('***prouver_avant	A FAIRE'),nl.
selectionner(Res):- console(['***selectionner/1		A FAIRE  Res:',Res]).


passage(Regle::Rgls_pred,Params,Resultats):-
	extraire_parametres(Params,Parametres),
%	console(['***passage/3	In Params:',Parametres,nl]),
	findall(Res,(Rgls_pred,execute_regle(Regle,Parametres,Res)),Resultats),
%	console(['***passage/3	Out ',nl,
%		 '   Res :',Res]),
	!.


extraire_parametres([P::Params],P):- call(Params),!.
extraire_parametres([P::Params|T_p],Parametres):-
	call(Params),
	extraire_parametres_suivants(T_p,P,Parametres),!.

extraire_parametres_suivants([],Params,Params):-!.
extraire_parametres_suivants([P::Params |Tail],P_in,Parametres):-
	call(Params),
	% P_so_far = P_in&P,  % execute P_in&P ==> a ne pas faire !!
	extraire_parametres_suivants(Tail,P_in&P,Parametres).


% petit hack pour tourner autour de la recherche de règle via :
% index(Parent,Objet, Nom_regle)  % utilisé par anie version Emicat
index(Parent,Objet,Nom_regle):-  % donne les valeurs par bktk
	% valeur_locale(Objet,rules,Rgls),
	% member(Rgl,Rgls),
	% objet(Rgl,Parent),
	% ou ci-dessous
	valeur_locale(Nom_regle,obj_name,Objet),
	objet(Nom_regle,Parent).


% --------------
execute_regle(Regle,Param,Res):-
%	console(['     execute_regle  Entrée Regle:',Regle]),
	valeur_locale(Regle,corps,(si (Param&Premisses) alors selectionner(Res))),
	call(Premisses),
/* --	console(['     execute_regle type : graphique Regle:',Regle,nl,
%		 '                   Param:',Param,nl,
%		 '                   Premisses:',Prem,nl,
		 '		     Res :',Res,nl]),
-- */
	!.

execute_regle(Regle,Param,Res):-
%	console(['     execute_regle Entrée Regle :',Regle]),
/* ---	valeur_locale(Regle,corps,
		      (parametres : Param,
		       contexte : Contexte,
		       preambule : Preambule,
		       corpus : Corpus,
		       sortie : Sortie,
		       conclusion : Concl)
		     ),                             --- */
	valeur_locale(Regle,executable,
		      ( Param,(Contexte,Preambule,Corpus,
		       Sortie, Conclusion))),
	catch((Contexte,Preambule,Corpus,Sortie),true,fail),
/*--	console(['     execute_regle Regle:',Regle,nl,
		 '                   Parametres:',Param,nl,
		 '                   contexte:',Contexte,nl,
		 '                   preambule:',Preambule,nl,
		 '                   corpus:',Corpus,nl,
		 '                   sortie:',Sortie,nl,
		 '		     conclusion :',Conclusion,nl]), -- */
	Conclusion=selectionner(Res),
%	console(['   execute_regle ',Regle,' emission_signal =',Res]),
	!.

execute_regle(_Regle,_Param,[]). % true in fine avec Res =[ ]



% --- clauses liées à la création et supression des règles
mi4i_regle_ava(Rule_name,Corps):-
    write('Avant ajout de règle		X : '),write(Rule_name),nl,
    write('			Y : '),write(Corps),nl,nl,
	mi_activer_val(Rule_name,traduction,parm(Rule_name,Corps,Trad)),  % appelle attribut : traduction
    write('Resultats de traduction : '),write(Trad),nl,nl,
	mi_analyser_traduction_regle(Trad,Objets,Types,Attributs,Predicats),
    write('Objets : '),write(Objets),nl,
    write('Attributs : '),write(Attributs),nl,
    write('Types : '),write(Types),nl,
    write('Predicats : '),write(Predicats),nl,nl,

	!.

mi4i_regle_apa(X,Y):-
	write('APRES ajout de règle	X : '),write(X),nl,
	write('			Y : '),write(Y),nl,nl,
	!.

mi4i_regle_apr(X,Y):-
	write('APRES retrait de règle	X : '),write(X),nl,
	write('			Y : '),write(Y),nl,nl,nl,
	!.

% --- Analyse de la Traduction d'une règle
mi_analyser_traduction_regle(Trad,Objets,Types,Attributs,Predicats):-
	findall(Obj,terme_de(objet(_,Obj),Trad),Objets),
	findall(Typ,terme_de(creer_obj(_,Obj),Trad),Types),

	write('EN DEV.  : mi_analyser_traduction_regle '-Typ-Attributs-Predicats),nl,

	!.

% ------------------------------------------------------------------------------------------
% Sauvegardes
% ------------------------------------------------------------------------------------------
%
sauver(X,Y,Z):-
	write('Sauver('),write(X-Y-Z),write(')	A faire'),nl.
	% certainement renvoi sur sauver_sec car qpsave est difficile à gérer

% -----------------------------------------------------------------------------
sauver_sec(Sec,S_Dir,Comment):-
	working_directory(Wk_dir,Wk_dir),
	mi_s_sec_test_dir(S_Dir),
	put_on_file(comment,mi_w_sec_comment(Sec,Comment)),
	put_on_file('oper.pl',mi_w_sec_op(S_Dir)),
	contenu_du_sec(Sec,Objs),			% a compléter selon spec EMI
	put_on_file('peres.pl',mi_w_sec_peres(Objs)),
	put_on_file('contenu.pl',mi_w_sec_att_des_mar(Objs)),

	working_directory(_,Wk_dir),		% reset working_dir
	!,
	write('Sauver_sec('),write(Sec-S_Dir-Comment),write(')	Quasi fini...'),nl.
sauver_sec(X,Y,Z):-
	write('Sauver_sec('),write(X-Y-Z),write(')	Aborted'),nl.


sauver_sec(Sec,S_Dir,Comment,silent):-
	working_directory(Wk_dir,Wk_dir),
	mi_s_sec_test_dir(S_Dir,silent),
	put_on_file(comment,mi_w_sec_comment(Sec,Comment)),
	put_on_file('oper.pl',mi_w_sec_op(S_Dir)),
	contenu_du_sec(Sec,Objs),			% a compléter selon spec EMI
	put_on_file('peres.pl',mi_w_sec_peres(Objs)),
	put_on_file('contenu.pl',mi_w_sec_att_des_mar(Objs)),
	working_directory(_,Wk_dir),		% reset working_dir
	console(['sauver_sec :',Sec,' in :',S_Dir,' done']),
	!.
sauver_sec(X,Y,_Z,silent):-
	console(['Sauver_sec :',X,in,Y,' Aborted']).


mi_s_sec_test_dir(Y):-
	exists_directory(Y),!,
	% version avec xpce
	with_output_to(atom(Message),
		       (  write('Sauver_sec : dir = '),write(Y), nl,
			  write('	Allready exists	Overwrite ? ') )),
	confirmer(Message,Answ),
	!,
	Answ=yes,
	% old version
	% write('Sauver_sec : dir = '),write(Y),
	% write('	Allready exists	Overwrite (o.) : '),
	% read(o),
	working_directory(_Wk_dir,Y).

mi_s_sec_test_dir(New_dir):-		% create dir Y
	make_directory(New_dir),
	working_directory(_Cur_dir,New_dir),!.


mi_s_sec_test_dir(Y,silent):-
	exists_directory(Y),!,
	working_directory(_Wk_dir,Y).

mi_s_sec_test_dir(New_dir,silent):-		% create dir Y
	make_directory(New_dir),
	working_directory(_Cur_dir,New_dir),!.




mi_w_sec_comment(Sec,Comment):-
	write(''''),write(Comment),write(''' .'),nl,
	get_time(Time),convert_time(Time,A,M,J,H,Mi,S,Ms),
	write('saved('),write(A),write(','),write(M),write(','),write(J),write(','),
	write(H),write(','),write(Mi),write(','),write(S),write(','),write(Ms),write(').'),nl,
	current_prolog_flag(version ,SWI),
	write('source('),write('''Emione'','),write('''V0.93'''),write(',''SWI'','),write(SWI),write(').'),nl,
	write(Sec),write('.'),nl.


mi_w_sec_op(S_Dir):-
	write('/* -- oper pour : '),write(S_Dir),write('  --*/'),nl,nl,
	current_op(Prec,Type,Op),
	writeq(oper(Prec,Type,Op)),write('.'),nl,		%write_canonical( ==> nom des variables
	fail.
mi_w_sec_op(_S_Dir):-
	nl,write('/* -- oper		Fin --*/'),!.


contenu_du_sec(sec(X::Code),Objs):-findall(X,Code,Bag),list_to_set(Bag,Objs).

mi_w_sec_peres(Objs):-
	write('/*-- peres -- emione V0.93 ---*/'),nl,nl,
	member(X,Objs),
	m_objet:obj(X,Anc),
	writeq(objet(X,Anc)),write('.'),nl,	%write_canonical( ==> nom des variables
	fail.
mi_w_sec_peres(_):-nl,write('/* -- peres	Fin --*/'),!.

mi_w_sec_att_des_mar(Objs):-
	write('/*-- contenu.pl -- Emione V0.93 ---*/'),nl,nl,
	member(O,Objs),
	ecrire_att(O),ecrire_des(O),ecrire_mar(O),
	fail.
mi_w_sec_att_des_mar(_):- nl,write('/* -- contenu		Fin --*/'),!.


ecrire_att(O):-m_oav:att_val(O,A,Val),
	writeq(att_val(O,A,Val)),write('.'),nl,	% att_val  %write_canonical( ==> nom des variables
	fail.
ecrire_att(_):-!.

ecrire_des(O):-m_oav:desc_val(O,A,D,Val),
	writeq(desc_val(O,A,D,Val)),write('.'),nl,	% desc_val %write_canonical
	fail.
ecrire_des(_):-!.

ecrire_mar(O):-m_oav:marq_val(O,A,D,Val),
	writeq(marq_val(O,A,D,Val)),write('.'),nl,	% marq_val %write_canonical
	fail.
ecrire_mar(_):-!.


% -----------------------------------------------------------------------------
tuer_sec(X):-
	write('Tuer_sec('),write(X),write(')	A faire'),nl.

vider_sec(X):-
	write('Vider_sec('),write(X),write(')	A faire'),nl.


% -----------------------------------------------------------------------------
inclure_sec(X):-
	exists_directory(X),!,
	working_directory(Wk_dir,X),
	mi_i_sec_lire(comment),
	mi_i_sec_lire(oper),
	mi_i_sec_lire(peres),
	% ajouter ici un check_maj des filiations
	mi_i_sec_lire(contenu),
	working_directory(_,Wk_dir),		% reset working_dir
	write('inclure_sec('),write(X),write(') Quasi fini -TAF: règles, etc.'),nl,!.
inclure_sec(X):-
	write('inclure_sec('),write(X),write(')		was aborted'),nl.




mi_i_sec_lire(File):-
	absolute_file_name(File,[extensions(['pl','','mi4']),access(exist),
				 file_errors(fail),solutions(first)],Abs_f),
	gensym(File,File_stream),
	open(Abs_f,read,_Str,[alias(File_stream)]),
	mi_lire_et_traite_fichier(File,File_stream),
	close(File_stream),!.
mi_i_sec_lire(File):-write('Problems reading file :'-File),nl.

mi_lire_et_traite_fichier(File,File_stream):-
	repeat,
	read(File_stream,Pred),
	mi_inclure_traite(File,Pred),
	Pred ='end_of_file' .

mi_inclure_traite(comment,_):-!.
mi_inclure_traite(peres,objet(Obj,Peres)):-
	(m_objet:obj(Obj,P) ->
		(same_list(P,Peres)->true;write('Pb creation'-Obj),nl)
		; m_objet:assert(obj(Obj,Peres)),
		  mi_create_filiation(Obj,Peres),
		  % mi_create_terminal(Obj,Peres),	% !! Pb possible sur index(m_terminal:....)
		 true),
	!.
mi_inclure_traite(contenu,att_val(O,A,V)):-		% att_val
	(m_oav:att_val(O,A,OV) ->
		(V=OV ->true;write('Warning value allready exists('-O-A-V),nl)
		; m_oav:assert(att_val(O,A,V))),
	!.
mi_inclure_traite(contenu,desc_val(O,A,D,V)):-
	(m_oav:desc_val(O,A,D,OV) ->
		(V=OV ->true;write('Pb creation descripteur('-O-A-D-V),nl)
		; m_oav:assert(desc_val(O,A,D,V))),
	!.
mi_inclure_traite(contenu,marq_val(O,A,M,V)):-
	(m_oav:marq_val(O,A,M,OV) ->
		(V=OV ->true;write('Pb creation descripteur('-O-A-M-V),nl)
		; m_oav:assert(marq_val(O,A,M,V))),
	!.
mi_inclure_traite(oper,oper(Priorite,FX,Oper)):-
	(memberchk(Oper,[',']) ->	true
		;op(Priorite,FX,Oper)  ),
	!.
mi_inclure_traite(_,'end_of_file'):-!.

mi_inclure_traite(File,Terme):- write('File : '),write(File),write('	Non traité : '),write(Terme),nl.


traite(Prlg_file-No-Pere):-
	% traiter le chargement des fichier prolog
   write('	'),write('A FAIRE !! création et chargement de '),write(Prlg_file),
   write('	'),write(No),write(' : '),write(Pere),nl,!.


/*-------------------------------------------------------------------------------------------
% expl de sec :
%
% sec(X::(objet_qq(Y), \+fils_de(Y,_),(X=Y;herite_de(X,Y)))) .

sec(X::(objet_qq(Y),\+fils_de(Y,_),
		(X=Y;(herite_de(X,Y),not(objet(X,indic)),
		not( objet(X,groupe)),not(objet(X,emission_signal)),
		not(objet(X,diapositives)),not(objet(X,maps)) ))	 )).

-----*/

% bagof semble etre une bonne voie mais il faut trouver les variables libres
% !!!   findall(X,Goal,Bag) fait le même effet
% voir pour remplacer bagof par findall dans les autres parties de emione !!!!!


%  mi_one:sec(X::Code),findall(X,Code,Bag).
% sauver_sec(sec(X::(objet_qq(Y), \+fils_de(Y,_),(X=Y;herite_de(X,Y)))), ysae,'Ensemble complet de EASy').
% -------------------------------------------------------------------------------------------
creer_lien(X,Y):-
	write('creer_lien('),write(X-Y),write(')	A faire'),nl.
tuer_lien(X,Y):-
	write('tuer_lien('),write(X-Y),write(')		A faire'),nl.

purger:-	write('purger'),write('		A faire'),nl.


% --------------------------------------------------------------------------------------------
%	Utilitaires spéciaux EMI pour traitement de listes
% --------------------------------------------------------------------------------------------
% concat sur des structures avec recursion à gauche
concat(A,B,Res,Motif,gauche):-	Motif=..[Funct,_,_],Fin=..[Funct,_,_],concat_g(A,B,Res,Funct,Fin).
concat(A,B,Res,Motif,droite):-	Motif=..[Funct,_,_],Fin=..[Funct,_,_],concat_d(A,B,Res,Funct,Fin).
concat(A,B,Res,Motif,Fin,gauche):-	Motif=..[Funct,_,_],concat_g(A,B,Res,Funct,Fin).
concat(A,B,Res,Motif,Fin,droite):-	Motif=..[Funct,_,_],concat_d(A,B,Res,Funct,Fin).

% ? peut on faire une recur-gauche sur une liste de forme [a,b,c] ? ==> ? non ?

concat_g(Debut,Tail,Res,Funct,Fin):- % reste à traiter le cas de la fin non vide style .(_,[]) il ne faut pas mettre 2 fois
	Debut=Fin,
	Debut=..[Funct,X,Y],
	(not(X=..[Funct,_,_])->true;var(X)),
	Tail=..[Funct,_A,_B],
	Res1 =.. [Funct,Tail,X],
	Res =.. [Funct,Res1,Y].

concat_g(Debut,Tail,Res,Funct,Fin):-
	Debut=..[Funct,X,Y],
	Res=..[Funct,R,Y],
	concat_g( X,Tail,R,Funct,Fin).

concat_d(Debut,Tail,Res,Funct,Fin):-  % reste à traiter le cas de la fin non vide style .(_,[]) il ne faut pas mettre 2 fois
	Debut=Fin,
	Debut=..[Funct,X,Y],
	(not(Y=..[Funct,_,_])->true;var(Y)),
	Tail=..[Funct,_A,_B],
	Res1 =.. [Funct,X,Tail],
	Res =..[Funct,Res1,Y].

concat_d(Debut,Tail,Res,Funct,Fin):-
	Debut=..[Funct,X,Y],
	Res=..[Funct,X,R],
	concat_d( Y,Tail,R,Funct,Fin).

lire_fichier(File,Terme):-
	absolute_file_name(File,[extensions(['pl',' ','mi4']),access(exist),file_errors(fail),solutions(first)],Abs_f_name),
	gensym(File,File_stream),
	open(Abs_f_name,read,_Str,[alias(File_stream)]),
	repeat,
	read(File_stream,Terme),
	write(Terme),nl,
	(Terme='end_of_file' ->(close(File_stream),!) ;true ).


% ---------
element_de(A,B,Motif,gauche):-	Motif=..[Funct,_,_],Fin=..[Funct,_,_],element_g(A,B,Funct,Fin).
element_de(A,B,Motif,droite):-	Motif=..[Funct,_,_],Fin=..[Funct,_,_],element_d(A,B,Funct,Fin).

element_g(A,B,Funct,_):-
	B=..[Funct,_,A] .
element_g(A,B,Funct,Fin):-
	B=..[Funct,Suite,_],
	element_g( A,Suite,Funct,Fin).

element_d(A,B,Funct,_):-
	B=..[Funct,A,_] .
element_d(A,B,Funct,Fin):-
	B=..[Funct,_,Suite],
	element_d(A,Suite,Funct,Fin) .

% ----------------------------------------------
terme_de(Sub,Terme):- compound(Terme),functor(Terme,_F,N),terme_de(N,Sub,Terme).
terme_de(Sub, Terme) :-
	(var(Sub)->	(functor(Terme,F,_),
		not(member(F,[',' , ';' ,'->',call,setof,bagof,':-'])) )  %'\+'
		;true),
	Sub=Terme.

terme_de(N,Sub,Terme):-
	N>1 , N1 is N-1,terme_de(N1,Sub,Terme).
terme_de(N,Sub,Terme):-arg(N,Terme,Arg),nonvar(Arg),terme_de(Sub,Arg).


% -------------------------------------------
% ex-file : montrer
% but  : une autre presentation des resultats de voir(Obj)
montrer_tout(Obj):- mi_montrer(Obj,[att,desc,mar]).
montrer_plus(Obj):- mi_montrer(Obj,[att]).
montrer(Obj):-mi_montrer(Obj,[]).

mi_montrer(Obj,L):-	objet_qq(Obj),nl,!,
		write('<'),write(Obj),write('>'),tab(5),
		give_peres(Obj,Peres),write('^ : '),write(Peres),tab(5),
		give_aieux(Obj,Aieux),write('^^ : '),write(Aieux),tab(5),
		give_fils(Obj,Fils),write('fils : '),write(Fils),nl,
		montrer_att(Obj,L).
mi_montrer(Obj,_):- nl,write('Objet : '),write(Obj),write(' n''existe pas ').

give_peres(Obj,Peres):- bagof(X,fils_de(Obj,X),Peres),!.
give_peres(_,[]).


give_aieux(Obj,Aieux):- bagof(X,herite_de(Obj,X),Aieux),!.
give_aieux(_,[]).

give_fils(Obj,Fils):-setof(X,pere_de(Obj,X),Fils),!.
give_fils(_,[]).

montrer_att(Obj,L):-
	(memberchk(att,L) ->valeur(Obj,Att,Val) ; valeur_locale(Obj,Att,Val)),
	write('   - '),write(Att),write(' : '),write_term(Val,[max_depth(10)]),nl,
	montrer_des(Obj,Att,L),montrer_mar(Obj,Att),fail.
montrer_att(Obj,L):-
	memberchk(desc,L),
	setof(Att,X^Y^Z^T^((descripteur(Obj,Att,X,Y);
				marqueur(Obj,Att,Z,T)),
				\+ valeur(Obj,Att,_)),Set),
	member(One_att,Set),
	write('   - '),write(One_att),write(' : '),nl,
	montrer_des(Obj,One_att,L),montrer_mar(Obj,One_att),fail.
montrer_att(Obj,L):-
	not(memberchk(desc,L)),
	setof(Att,X^Y^Z^T^((descripteur_local(Obj,Att,X,Y);
				marqueur(Obj,Att,Z,T)),
				\+ valeur_locale(Obj,Att,_)),Set),
	member(One_att,Set),
	write('   - '),write(One_att),write(' : '),nl,
	montrer_des(Obj,One_att,L),montrer_mar(Obj,One_att ),fail.
montrer_att(_,_).

montrer_des(Obj,Att,L):-
	(memberchk(desc,L) ->descripteur(Obj,Att,Desc,Val_des);descripteur_local(Obj,Att,Desc,Val_des)),
	write('     d. '),write(Desc),write(' : '),write_term(Val_des,[max_depth(10)]),
	nl,fail.
montrer_des(_,_,_).

montrer_mar(Obj,Att):-
	marqueur(Obj,Att,Desc,Val_mar),
	write('     m. '),write(Desc),write(' : '),write_term(Val_mar,[max_depth(10)]),
	nl,fail.
montrer_mar(_,_).





% -------------------------------------------------------------------------------------------
% copy_obj/2,		% copier les att,desc,mar depuis Obj_source ==> Obj_dest
copy_obj(Obj_source, Obj_dest) :-
	objet_qq(Obj_source),
	objet_qq(Obj_dest),
	forall(valeur_locale(Obj_source,A,V),set_val(Obj_dest,A,V)),
	forall(descripteur_local(Obj_source,A,D,V),set_des(Obj_dest,A,D,V)),
	forall(marqueur(Obj_source,A,M,V),set_mar(Obj_dest,A,M,V)),
	!.

copy_obj(Obj_source, Obj_dest) :-
	write('MI_ERR : copy_obj :'),
	write(Obj_source-Obj_dest),write(' Failed'),nl,fail.



% -------------------------------------------------------------------------------------------
% UTILITAIRES
setof_or_void(A,B,C) :- setof(A,B,C),!.
setof_or_void(_,_,[]).

bagof_or_void(A,B,C) :- bagof(A,B,C),!.
bagof_or_void(_,_,[]).

% -- ecriture sur fichier très utile car on passe des predicats à executer
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

same_list(L1,L2):- intersection(L1,L2,L2).

% -----------------------------------------------------------------------------
% initialisation de Emione
% -----------------------------------------------------------------------------
:-m_objet:(dynamic obj/2).

:-m_fils:(dynamic fils/2).

:-m_ancetre:(dynamic ancetre/2).

:-m_terminal:(dynamic terminal/2).

:-m_oav:(dynamic att_val/3 , desc_val/4 , marq_val/4).

% pour aller plus vite :
%  :-m_fils:index(m_fils:fils(1,1)).
% :-m_terminal: index(m_terminal:terminal(1,1)).
% :-m_oav:index(att_val(1,1,1)).	% pas très concluant !!!!


% -----------------------------------------------------------------------------
% Preds pour tests
:- export(liste_oav/0).
liste_oav:- valeur(O,A,V),
	write(O),write('	'),write(A),write('	'),write(V),nl,
	fail.
:-export(liste_oadv/0).
liste_oadv:- descripteur(O,A,D,V),
	write(O),write('	'),write(A),write('	'),write(D),write('	'),write(V),nl,
	fail.
:-export(show/0).
show:-	write('***  OBJETS  ***'),nl,m_objet:listing , nl,
	write('***  FILS        ***'),nl,m_fils:listing , nl,
	write('***  TERMINAL        ***'),nl,m_terminal:listing , nl,
	write('***  ANCETRES        ***'),nl,m_ancetre:listing , nl,
	write('*** VALEURS  ***'),nl,m_oav:listing.
