% file	: 30_rule_editor_symph
% Editeur de règles pour symphony
% date	: 15/11/2006

:- use_module(['32_stocke_regle']).
:- ['31_r_edit_symph_DCG_xl.pl'].		% pour variable_xl

:-op(700,xfx,'<>').    % diff ds XL
:-op(700,xfx,'<=').    % plus_petit ou egal de Excel ( =< dans SWI)

creer_objet_regle_vierge(Obj_regle,Obj,Att,Module,Regle_pere):-
	creer_obj(Obj_regle,[Regle_pere]),
	set_val(Obj_regle,litterals,[]),
	set_val(Obj_regle,mandatory_litterals,[]),
	set_val(Obj_regle,connect_to,(Obj,Att)),
	set_val(Obj_regle,csts_par_defaut,[]),
	set_val(Obj_regle,signal_on,none),
	set_val(Regle_pere,rule_module,Module),

	% sur Obj ayant la règle !! A faire après compile !!
	set_des(Obj,Att,rule,Obj_regle),
	!.

% -----------------------------------------------------------
traite_input(Arg,Obj_regle) :-
	downcase_atom(Arg,Arg_down_case),
	nettoyer(Arg_down_case,New_arg),
	% nettoyer(Arg,New_arg),    % on reste en Maj_et_minuscules

	% callable(New_arg),
	catch(atom_to_term(New_arg,T,_B),_Err,fail),
	analyse_input(T,Arg,Obj_regle),
	!.
traite_input(Arg,Obj_regle) :-
	write('traite_input : '- Arg),write(' : is not well formed'-Obj_regle),nl.


%-------------------------------------------
nettoyer(Atom_in,Atom_out):-
	remplace_char(Atom_in,Atom_1,'!','_excl_'),
	remplace_char(Atom_1,Atom_2,'$','_dollar_'),
	remplace_char(Atom_2,Atom_3,'si(','if('),
	remplace_char(Atom_3,Atom_4,'et(','and('),
	remplace_char(Atom_4,Atom_out,'ou(','or(').


restaurer_excl_dollar(Atom_in,Atom_out):-
	remplace_char(Atom_in,Atom_inter,'_excl_','!'),
	remplace_char(Atom_inter,Atom_out,'_dollar_','$').


remplace_char(Atom_in,Atom_out,Aremplacer,Remplacant):-
	atom_length(Aremplacer,Lgt),
	remplace_char(Atom_in,Atom_out,Aremplacer,Lgt,Remplacant).

remplace_char(Atom_in,Atom_out,Aremplacer,Lgt,Remplacant):-
	sub_atom(Atom_in,Befor,Lgt,After,Aremplacer),
	sub_atom(Atom_in,0,Befor,_,A1),Befor_1 is Befor+Lgt,
	sub_atom(Atom_in,Befor_1,After,_,A2),
	atomic_list_concat([A1,Remplacant,A2],Atom_int),
	remplace_char(Atom_int,Atom_out,Aremplacer,Lgt,Remplacant),
	!.
remplace_char(Atom_in,Atom_in,_,_,_):-!.


%-------------------------------------------

analyse_input(J,_,Obj_rgl):- atomic(J),   % si formule : XX ==> edit_litteral(XX)
	(   variable_xl(J,_,_,Lit,_) ; upcase_atom(J,Lit)),
	valeur(Obj_rgl,litterals,Litts),
	memberchk(Lit,Litts),
	edit_litteral(Lit),        % dans 30_r_edit_symph_pce
	!.
analyse_input(J=Commande,_,Obj_rgl):- atomic(J),  % X= delete ou X=''  ==> delete_litteral(X)
	(   Commande ='' ; Commande= delete),
	(   variable_xl(J,_,_,Lit,_) ; upcase_atom(J,Lit)),
	valeur(Obj_rgl,litterals,Litts),
	memberchk(Lit,Litts),
	delete_litteral(Obj_rgl,Lit),
	!.


analyse_input(J=Formule,Chaine_de_chars,Obj_Rgl):- atomic(J),
	variable_xl(J,V_Res,V_Res_Bd,V_Res_L,V_Res_Num),
      write('Var ='-J),nl,

	statement(Formule,V_Res,Clause,[],Vars) ,

      write('après statement'-Formule -'   V_res'-V_Res-'   Vars'-Vars),nl,

	analyse_variables(V_Res,V_Res_Bd,V_Res_L,V_Res_Num,Vars,Variables),
	analyse_cstes_param(Vars,Csts_param),

	write('__________________________________________________'),nl,
	write('Analyse de la formule : '),write(J=Formule),nl,nl,
	write('Clauses :	'),write(Clause),nl,
	write('Vars       :	'),write(V_Res_Bd),tab(5),write(Vars),nl,
	write('Variables  :	'),write(Variables),nl,
	write('Csts param :	'),write(Csts_param),nl,
	write('---------------------------------------------------'),nl,

	valeur_locale(Obj_Rgl,litterals,Lit),
	append_if_not_present(V_Res_L,Lit,New_Lit),
	set_val(Obj_Rgl,litterals,New_Lit),
	set_des(Obj_Rgl,V_Res_L,litteral,Chaine_de_chars),
	set_des(Obj_Rgl,V_Res_L,clause,Clause-V_Res_Bd-Vars),
	set_des(Obj_Rgl,V_Res_L,variables,Variables),
	set_des(Obj_Rgl,V_Res_L,csts_param,Csts_param),
	traite_mandatory_litterals(Obj_Rgl,V_Res_L,Variables),
	!.

analyse_input(J=Obj_emione,Chaine_de_chars,Obj_Rgl):-
	variable_xl(J,_V_Res,V_Res_Bd,V_Res_L,V_Res_Num),
	identifie_emione_obj(Obj_emione,Emione_Obj),         % permettre syntaxe : Obj-Att
	valeur_locale(Obj_Rgl,litterals,Lit),
	append_if_not_present(V_Res_L,Lit,New_Lit),
	set_val(Obj_Rgl,litterals,New_Lit),
	set_des(Obj_Rgl,V_Res_L,litteral,Chaine_de_chars),
	set_des(Obj_Rgl,V_Res_L,clause,emi_obj(Emione_Obj)-V_Res_Bd-[]),
	set_des(Obj_Rgl,V_Res_L,variables,[V_Res_L-V_Res_Num,[]]),
	set_des(Obj_Rgl,V_Res_L,csts_param,[]),
      write('Formule : '),write(J),tab(5),write(' Objet_emione : '),write(Obj_emione),nl,
	!.

analyse_input(J,_,_Obj_Rgl):-
	nl,write('Formule : '),write(J),tab(5),write('  is not well formed or not (obj,att)'),nl,nl,
	!,fail.

identifie_emione_obj(Obj-Att,(Obj,Att)):-valeur_trouvable(Obj,Att),!.
identifie_emione_obj(Obj-Att-Desc,(Obj,Att,Desc)):-valeur_trouvable(Obj,Att),!.
identifie_emione_obj((Obj,Att),(Obj,Att)):-valeur_trouvable(Obj,Att),!.
identifie_emione_obj((Obj,Att,Desc),(Obj,Att,Desc)):-valeur_trouvable(Obj,Att).


% ----- suppression d'un litteral
delete_litteral(Obj_rgl,Lit):-
	descripteur_local(Obj_rgl,Lit,litteral,Litteral),
	(   confirmer(['Delete formule : ',Litteral])-> supp_litteral(Obj_rgl,Lit);true),
	!.

supp_litteral(Obj_rgl,Lit):-
	forall(descripteur_local(Obj_rgl,Lit,A,_D),reset_des(Obj_rgl,Lit,A)),
	valeur(Obj_rgl,litterals,Litts),
	delete(Litts,Lit,New_litts),
	set_val(Obj_rgl,litterals,New_litts),
	reset_des(Obj_rgl,mandatory_litterals,Lit),
	maj_mandatory_litterals(Obj_rgl),
	(   valeur(Obj_rgl,signal_on,Lit)-> reset_val(Obj_rgl,signal_on);true),
      write('supp_Litteral : '),write(Lit),nl,nl,
	!.

% ----- traitement des litteraux  mandatory & surplus
traite_mandatory_litterals(Obj_rgl,V_Res_L,Variables):-
	Variables = [V_Res_L-_,L_vars_nums],
	findall(V,member(V-_,L_vars_nums),Bagset_vars),
	set_des(Obj_rgl,mandatory_litterals,V_Res_L,Bagset_vars),
	maj_mandatory_litterals(Obj_rgl),

      write('for mandatory_litterals local : '-V_Res_L-Bagset_vars), nl,nl,
	!.

maj_mandatory_litterals(Obj_rgl):-           % callable en direct
	findall(Litteraux,descripteur(Obj_rgl,mandatory_litterals,_L,Litteraux),Bag),
	flatten(Bag,List),list_to_ord_set(List,Ordset),
      write('for mandatory_litterals : '-Ordset), nl,
	set_val(Obj_rgl,mandatory_litterals,Ordset).


% ----------------------------------------------------------------------
% Grammaire pour formules de type XL

statement(if(Conds;Then;Else),V_Res,(Cond_Cl->Then_Cl;Else_Cl),Var_in,Var_out):-
	conditions(Conds,Cond_Cl,Var_in,Var_1),  % evt : statement(....
	statement(Then,V_Res,Then_Cl,Var_1,Var_2),
	statement(Else,V_Res,Else_Cl,Var_2,Var_out),
	!.
statement(Expr,V_Res,Code,Var_in,Var_out):-
	expression(Expr,Code_exp,Var_in,Var_out),
	Code =.. [is,V_Res,Code_exp].  % intègre : V_Res is Code_exp
statement(Expr,_V_Res,Code,Var_in,Var_out):-
	conditions(Expr,Code,Var_in,Var_out).

statement(Expr,V_Res,Code_Expr,Var_in,Var_out):-

	expression_non_function(Expr ,Code_Expr,Var_in,Var_out,V_Res),

	!.


% ------------------------------------------------
conditions(Cond,Code ,Var_in,Var_out):-
	Cond =..[Op,Left,Right],
	logical_op(Op,P_op),
	expression(Left,Code_L,Var_in,Var_1),
	expression(Right,Code_R,Var_1,Var_out),
	Code =..[P_op,Code_L,Code_R],
	!.
conditions(and(Cond_1;Cond_2), ( Code_1,Code_2) ,Var_in,Var_out):-
	statement(Cond_1,_,Code_1,Var_in,Var_1),
	statement(Cond_2,_,Code_2,Var_1,Var_out),
	!.
conditions(and(Cond_1;Cond_2), ( Code_1,Code_2) ,Var_in,Var_out):-
      write('AND conditions 2 ..........'),nl,
	statement(Cond_1,_,Code_1,Var_in,Var_1),
	statement(and(Cond_2),_,Code_2,Var_1,Var_out),
	!.
conditions(or(Cond_1;Cond_2), ( Code_1;Code_2) ,Var_in,Var_out):-
	statement(Cond_1,_,Code_1,Var_in,Var_1),
	statement(Cond_2,_,Code_2,Var_1,Var_out),
	% conditions(Cond_1,Code_1,Var_in,Var_1),
	% conditions(Cond_2,Code_2,Var_1,Var_out),
	!.
conditions(or(Cond_1;Cond_2), ( Code_1;Code_2) ,Var_in,Var_out):-
     write('OR conditions 2 ..........'),nl,
	statement(Cond_1,_,Code_1,Var_in,Var_1),
	statement(or(Cond_2),_,Code_2,Var_1,Var_out),
	% conditions(Cond_1,Code_1,Var_in,Var_1),
	% conditions(Cond_2,Code_2,Var_1,Var_out),
	!.


logical_op(=,==).   % test egalité
logical_op(>,>).
logical_op(>=,>=).
logical_op(<>,=\=).
logical_op(<=,=<).
logical_op(<,<).

expression(Expr ,Code,Var_in,Var_out):-
	constante_ou_variable(Expr,Code,Var),
	unifie_variables(Var,Var_in,Var_out),!.
expression(Expr ,Code_Expr,Var_in,Var_out):-
		current_arithmetic_function(Expr),
		Expr=..[Op,Exp_1,Exp_2],
		expression(Exp_1 ,Exp_1_code,Var_in,Var_1),
		expression(Exp_2, Exp_2_code,Var_1,Var_out),
		Code_Expr =..[Op,Exp_1_code,Exp_2_code],
		!.
expression(Expr ,Code_Expr,Var_in,Var_out):-
		current_arithmetic_function(Expr),
		Expr=..[Op,Exp_1],
		expression(Exp_1 ,Exp_1_code,Var_in,Var_out),
		Code_Expr =..[Op,Exp_1_code],
		!.
% rajout 2011
expression(Expr ,Code_Expr,Var_in,Var_out):-
      write('expression XL Expr:'-Expr),nl,
	xl_2_swi_funct(Expr,Function_swi),
      write('   fonction SWI found  :'-Function_swi),nl,
	current_arithmetic_function(Function_swi),
	expression(Function_swi ,Code_Expr,Var_in,Var_out),
	!.

% fonction Xl avec fonction similaire SWI
xl_2_swi_funct(max(X;Y),max(X,Y)).
xl_2_swi_funct(min(X;Y),min(X,Y)).

% traitement des fonctions XL n'ayant pas de fonctin dans SWI
expression_non_function(Expr ,Code_Expr,Var_in,Var_out,V_Res):-
      write('entrée dans Expression_non_fonction '-Expr),nl,
	custom_expr(Expr,Symph_pred),
	Symph_pred =..[Symph_pred_name|Arguments],
	genere_listes_variables(Arguments,[],[],Liste_vars,Vars_L),
      write('expression_non_fonction liste_variables= '- Liste_vars),nl,
	append(Liste_vars,[V_Res],Args),
      write('expression_non_fonction Arguments ='-Args),nl,
	Code_Expr =..[Symph_pred_name|Args],

      write('expression_non_fonction Code_expr ='-Code_Expr),nl,
        flatten(Vars_L,Vars_list),
	append(Var_in,Vars_list,Var_out),

	!.

 % fonction XL avec predicat correspondant dans symphony (sans le dernier Arg: Resultat)
custom_expr(max(A:B) ,max_list(A:B)).
custom_expr(min(A:B) ,min_list(A:B)).
custom_expr(pente(A:A2;_B:_B2),pente(A:A2)).	% 03_ex_math_et_date
custom_expr(somme(A:A2),sum_list(A:A2)).		% native
custom_expr(moyenne(A:A2),moyenne(A:A2)).	% dans 03_ex_math_et_date
custom_expr(moyenne_hz(A:A2),moyenne_hz(A:A2)).



genere_listes_variables([],L_vars_in,Vars_L_in,L_vars_in,Vars_L_in):-!.
genere_listes_variables([Arg|T_args],L_Vars_in,Vars_L_in,L_Vars,Vars_L):-
	write('  genere_liste_variables'-Arg),nl,
	genere_var_list(Arg,Loc_L_vars,Loc_Vars_L),


	genere_listes_variables(T_args,[Loc_L_vars|L_Vars_in],[Loc_Vars_L|Vars_L_in],L_Vars,Vars_L).


genere_var_list(V_From:V_To,Liste_vars,Vars_list):-  % en colonne
	variable_xl(V_From,_R,_Lit_eq_V,Lettre,Num),
	variable_xl(V_To,_R2,_Lit_eq_V2,Lettre,Num2),
	From is min(Num,Num2), To is max(Num,Num2),
	gen_var_liste(From,To,Lettre,Liste_vars,Vars_list),
      write('         genere_var_list '- V_From -V_To -Lettre - Vars_list),nl,
	!.
genere_var_list(V_From:V_To,Liste_vars,Vars_list):-   % en ligne (implantation a revoir pour 'AA' et +)
	variable_xl(V_From,_R,_Lit_eq_V,Lettre,Num),
	variable_xl(V_To,_R2,_Lit_eq_V2,Lettre2,Num),
	atom_char(Lettre,N1), atom_char(Lettre2,N2),
	From is min(N1,N2), To is max(N1,N2),
     write('         genere_var_list horizontale '- V_From -V_To -Lettre- From-To-Num ),nl,
	gen_var_liste_hori(From,To,Num,Liste_vars,Vars_list),
      write('         genere_var_list horizontale '- V_From -V_To -Lettre - Vars_list),nl,
	!.
genere_var_list(Range,_Liste_vars,_Vars_list):-
	write('Range Error :'),write(Range),nl,
	fail.

% -------------------------
gen_var_liste(I,Up_to,_Lettre,[],[]):-
	I >Up_to,
	!.
gen_var_liste(I,Up_to,Lettre,[Arg|T_Args],[Lit_eq_V|T_Vars]):-
	atom_concat(Lettre,I,Cellule),
	variable_xl(Cellule,Arg,Lit_eq_V,Lettre,I),
	J is I+1,
	gen_var_liste(J,Up_to,Lettre,T_Args,T_Vars),
	!.

% -------------------------
gen_var_liste_hori(I,Up_to,_Num,[],[]):-
	I >Up_to,
	!.
gen_var_liste_hori(I,Up_to,Num,[Arg|T_Args],[Lit_eq_V|T_Vars]):-
	atom_char(Lettre,I),
	atom_concat(Lettre,Num,Cellule),
	variable_xl(Cellule,Arg,Lit_eq_V,Lettre,Num),
	J is I+1,
	gen_var_liste_hori(J,Up_to,Num,T_Args,T_Vars),
	!.



constante_ou_variable(Variable,Code,Var):- variable(Variable,Code,Var),!.
constante_ou_variable(Constante,Code,[]):- constante(Constante,Code,[]).
% constante_ou_variable(Emione,Code,[]):- emione_exp(Emione,Code,[]).

constante(Constante,Constante,[]):-	number(Constante).
constante(Atome,Atome,[]):-		atomic(Atome).
constante(String,Atome,[]):-		is_list(String),string_to_atom(String,Atome).

variable(Var,Variable,Bindings) :-
	atomic(Var),
	restaurer_excl_dollar(Var,Var1),
	variable_xl(Var1,Variable,Bindings,_Lettre,_Num).


unifie_variables([],Liste_B_in,Liste_B_in):-!.
unifie_variables([Bind_in],Liste_B_in,Liste_B_in):-
	member(Bind_in,Liste_B_in),!.
unifie_variables([Bind_in],Liste_B_in,Liste_B_out):-
	ord_add_element(Liste_B_in,Bind_in,Liste_B_out).



% intégrer evt appels à emione valeur(Obj,Att,Time) ==> f(descripteur(Obj,Att,T,Res))
% a voir si <Conditions> ne doit pas être de type statement ??? No Lo So


analyse_variables(_V_Res,[_V = _Bd],V_Res_L,V_Res_Num,Vars,[V_Res_L-V_Res_Num,Variables]):-
	findall(L-N,(member(Var=_,Vars),variable_xl(Var,_,_,L,N)),Bag),
	setof(Let,Let^Num^member(Let-Num,Bag),Set),
	findall(L-Nums,(member(L,Set),findall(Num,member(L-Num,Bag),Nums)),Variables),
	% Test de circularité Indices dans Liste doivent etre soit > ou < que V_Res_Num
	% mais pas les deux à la fois
	not( (	member(V_Res_L-Liste_ref,Variables),
			member(Petit,Liste_ref),Petit=<V_Res_Num,
			member(Grand,Liste_ref),Grand>=V_Res_Num )),
	!.
analyse_variables(_V_Res,[_V = _Bd],V_Res_L,V_Res_Num,[],[V_Res_L-V_Res_Num,[]  ]):-!.
analyse_variables(_V_Res,[_V = _Bd],_V_Res_L,_V_Res_Num,_Vars,[circularite]):-!.


% analyse les cstes paramétriques (ie $C$21 )
analyse_cstes_param(Vars,Csts_param):-
	setof(Var,Var^Bdg^(member(Var=Bdg,Vars),not(variable_xl(Var,_,_,_L,_N))),Csts_param),
	!.
analyse_cstes_param(_,[]):-!.


% utilitaire à mettre dans oo_utils  [A remplacer par ord_add_element ]
append_if_not_present(Element,Liste,Liste):-
	memberchk(Element,Liste),!.
append_if_not_present(Element,Liste,New_Liste):-
	append(Liste,[Element],New_Liste).



%==============================================================

compile_regle(Regle,Litteral):-
	% identifie_premier_litteral(Regle,Litteral)  % identifier 1er littéral
	valeur_locale(Regle,signal_on,Litteral),
	valeur(Regle,connect_to,Obj_att),
	ordonnance_parcours(Regle,Litteral,Litteraux),
     write('Litteraux à traiter : '),write(Litteraux),nl,nl,
	parcours_litteraux(Regle,Litteraux,C_prep,Carry,Csts_param,C_recur),
	valeur(Regle,rule_module,Rule_module),
	genere_code(Regle,Litteral,C_prep,Carry,Csts_param,C_recur,Rule_module),
	%     Rule_module:lien_regle(Rule_module,Regle,Obj_att auquel la regle est connectée)),
	( catch(Rule_module:lien_regle(Rule_module,Regle,Obj_att),_,fail) -> true ;
		assert(Rule_module:lien_regle(Rule_module,Regle,Obj_att))  ),   % 1er param = Module
	connecte_code(Rule_module,Regle,Obj_att),              % 1er param = Module
	!.
compile_regle(Regle,Litteral):-
	write('Compile regle : '),
	write(Regle-Litteral),nl,
	write(' Error : possibly missing : signal_on or connect_to'),nl.


% -----------------------------------------------------------
ordonnance_parcours(Regle,Litteral,Litteraux):-
	ordonnance_parcours_v2(Regle,[Litteral],[],Litteraux).


% version corrigée
% ordonnance_parcours_v2
ordonnance_parcours_v2(_Regle,[],Litteraux_traités,Litteraux_traités):-!.

ordonnance_parcours_v2(Regle,[Litteral|L_a_traiter],Litt_traités,R):-
	descripteur(Regle,Litteral,variables,[Litteral-Lit_num,Liste_vars]),
     %write('V2 Litteral : '), write([Litteral-Lit_num,Liste_vars]),nl,

	(setof(V,V^N^(member(V-N,Liste_vars),\+(V=Litteral),member(Lit_num,N)),Litt_mm_niv)
	   ->true ; Litt_mm_niv=[]),
     %write('    Litt_mm_niv : '), write(Litt_mm_niv),tab(5),nl,

	subtract(Litt_traités,Litt_mm_niv,N_L_traités ),
	append(N_L_traités,[Litteral],L_traités  ),
	union(L_a_traiter,Litt_mm_niv,N_litt_a_traiter),

     %write('    Litt à traiter '-L_a_traiter -N_litt_a_traiter),tab(10),write(' traités : '),
     %write(L_traités),nl,nl,

	ordonnance_parcours_v2(Regle,N_litt_a_traiter,L_traités ,R).


% -----------------------------------------------------------
parcours_litteraux(Regle,Litteraux,C_prep,Carry,Csts,C_recur):-
	%rev(Litteraux,Liste),
	write('----------------------------'),nl,
	parcours(Regle,Litteraux,[],[],[],[],C_prep,Carry,Csts,C_recur),
	write('----- RESULTAT  -----------------------'),nl,
	write('Code_prépa : '),write(C_prep),nl,
	write('Carry      : '),write(Carry),nl,
	write('Constantes : '),write(Csts),nl,
	write('C_recur : '),write(C_recur),nl,
	write('----- RESULTAT  -----------------------'),nl,
	nl,
	!.

parcours(_Regle,[],C_prep,Carry,Csts,C_recur,C_prep,Carry,Csts,C_recur):-!.
parcours(Regle,[L|L_tail],Prep_i,Carry_i,Csts_i,Recur_i,C_prep,Carry,Csts,C_recur):-
	compile(Regle,L,Prep_L,Carry_L,Csts_L,_Vect_L,Recur_L),
     % write('     Litteral :'-L-'    Recur_L:'-Recur_L),nl,
	mixer(Prep_i,Carry_i,Csts_i,Recur_i,Prep_L,
	      Carry_L,Csts_L,Recur_L,Prep_n,Carry_n,Csts_n,Recur_n),
     % write('Mixer post:'-L-'   Recur_n:'-Recur_n),nl,nl,nl,
	parcours(Regle,L_tail,Prep_n,Carry_n,Csts_n,Recur_n,C_prep,Carry,Csts,C_recur).



%--------------------------------------------------------------------
% compile un litteral de la regle courante
compile(Regle,Litteral,Prep,Carry,Csts,_Vect,Recur):-
          % write('Compile : '),tab(5),write(Litteral),nl,

	descripteur(Regle,Litteral,variables,[L-N,Liste_vars]),
	% tab(9),write('Var : '),write(L-N),tab(5),write(Liste_vars),nl,

	descripteur(Regle,Litteral,csts_param,Csts),
	% tab(9),write('Csts : '),tab(5),write(Csts),nl,

	get_carry_var(N,Liste_vars,Carry_s_set),

	descripteur(Regle,Litteral,clause,Clause-V_res-Vars),
	% tab(9) ,write('Clause: '),tab(5),write(Clause),tab(5),
	% write(V_res-Vars),nl,

	transpose_var(L,N,Liste_vars,Set_var_inds),
	% tab(5),write('transpose_var :'),tab(3),write(Liste_vars),
	% tab(5),write(Set_var_inds),nl,

	transpose_bindings(L,N,Liste_vars,V_res,Vars,N_v_res,N_vars),
	% tab(9),write('transp_bdg  :'),tab(5),write(N_v_res),tab(5),
	% write(N_vars),nl,

	extrait_constantes_avec_binding(Csts,Vars,[],Csts_vars),

	encode_emione_obj(Clause,N_v_res,L,N,Emi1_obj_code),
	% tab(9),write('prep = emione_obj :'),tab(5),write(Emi1_obj_code),nl,

	encode_expression(Clause,N_v_res,L,N,Code),
	% tab(9),write('recur = Expr :'),tab(5),write(Code),nl,

	(   Emi1_obj_code=[]
	    -> prep_expr_code(Code,N_v_res,N_vars,Set_var_inds,Carry_s_set,Csts_vars,Prep,Carry,Recur);
	       prep_emi1_code(Emi1_obj_code,Prep,Carry,Recur)),
	% tab(9),write('Prep  : '),write(Prep),nl,
	% tab(9),write('Carry : '),write(Carry),nl,
	% tab(9),write('Csts  : '),write(Csts),nl,
	% tab(9),write('Recur : '),write(Recur),nl,nl,
	!.


get_carry_var(N,Liste_vars,[Sens-Set]):-
	setof(L-Dif-Var,L^Nums^Y^( member(L-Nums,Liste_vars),
				 member(Y,Nums),
				 Dif is Y-N,
				 not(Dif =0),
				 atomic_concat(L,Dif,Var)),Set),
	member(_-X-_,Set),
	(X<N -> Sens='up';Sens ='down'),
	!.
get_carry_var(_,_,[]):- !.


% ------------------------
transpose_bindings(L,N,Vars,[_=B_v],Liste_vars,[L-0-L0=B_v],N_vars):-
	atomic_concat(L,0,L0),
	transpose_bindings(N,Liste_vars,Vars,[],N_vars),
	!.

transpose_bindings(_N,_Liste_vars,[]  ,Vars,Vars):-!.
transpose_bindings(N,Liste_vars,[Let-Nums|T_vars],So_far,NN_vars):-
	transpose_binding(N,Liste_vars,Let,Nums,N_vars),
	append(N_vars,So_far,N_sofar),
	transpose_bindings(N,Liste_vars,T_vars,N_sofar,NN_vars),
	!.

transpose_binding(_N0,_Liste_vars,_Let,[],[]):- !.
transpose_binding(N0,Liste_vars,Let,[Num|T_num],[Let-Dif-Ldif=Binding|T_N_var]):-
	Dif is Num-N0,
	atomic_concat(Let,Dif,Ldif),
	atomic_concat(Let,Num,Lcur),
	member(Lcur=Binding,Liste_vars),
	transpose_binding(N0,Liste_vars,Let,T_num,T_N_var),
	!.


%---------------
transpose_var(_L,N,Liste_vars,Set_var_indices):-
	findall(V-L_indices,
	      (member(V-Nums,Liste_vars),
		  setof(Dif,Y^(member(Y,Nums),Dif is Y-N),L_indices)),  % Lste diff vs N
	      Set_var_indices),
	!.
transpose_var(_L,_,_,[]):- !.


%----------------------------
extrait_constantes_avec_binding([],_Vars,Csts_vars,Csts_vars):- !.

extrait_constantes_avec_binding([Cst|Csts_t],Vars,Csts_vars_sf,Csts_vars):-
	member(Cst = Bdg,Vars),
	extrait_constantes_avec_binding(Csts_t,Vars,[Cst=Bdg|Csts_vars_sf],Csts_vars),
	!.


%----------------------------
encode_emione_obj(emi_obj((Obj,Att,Desc)),V_res,Lettre,Num,Code):-
	Code = [descripteur(Obj,Att,Desc,Serie)-Serie-V_res-Lettre-Num],!.
encode_emione_obj(emi_obj((Obj,Att)),V_res,Lettre,Num,Code):-
	Code = [valeur(Obj,Att,Serie)-Serie-V_res-Lettre-Num],!.
encode_emione_obj(_,_,_,_,[]):- !.


encode_expression(emi_obj(_),_V_res,_Lettre,_Num,[]):- !.
encode_expression(Expression,_V_res,_Lettre,_Num,Expression):- !.



prep_expr_code(Code,[V_res],Vars,Set_var_inds,Carry_s_set,Csts_vars,[],Carry,Recur):-
	(Carry_s_set =[] -> Carry=[];
	         (Carry_s_set=[_-List_carry],
		  setof(Carr,L^N^member(L-N-Carr,List_carry),Carry))),
	Recur = [Code,[V_res,Vars],Set_var_inds,Csts_vars,vectors_vars],
	!.

prep_emi1_code([Code_emi-Var-Parm-_L-_N],[Code_emi-Var-Parm],Parm,[]):-
	!.


mixer(Prep_in,Carry_in,Csts_in,[], [],Carry,Csts,Recur,Prep_in,Carry_out,Csts_out,Recur_out):-
	Recur=[Code,Vars,_,Csts_vars,_],
	flatten(Vars,Vars_out),
	Recur_out = [Code,Vars_out,Csts_vars,_All_vars],
	ord_union(Carry,Carry_in,Carry_out),  %append ?
	ord_union(Csts,Csts_in,Csts_out),
	!.
mixer(Prep_in,Carry_in,Csts_in,Recur_in, Prep,Carry,Csts,Recur,Prep_out,Carry_out,Csts_out,Recur_out):-

/*	write('  Prep_in  : '),write(Prep_in),nl,
	write('  Carr_in  : '),write(Carry_in),nl,
	write('  Recur_in : '),write(Recur_in),nl,
	write('-------------------- '),nl,
	write('     Prep  : '),write(Prep),nl,
	write('     Carry : '),write(Carry),nl,
	write('     Recur : '),write(Recur),nl,
*/
	append(Prep,Prep_in,Prep_out),
	(   Recur=[Code,Vars,_,Csts_v,_All_vars_] -> Recur_in=[Code_in,Vars_in,Csts_v_in,Vect],
	    mixer_code(Code,Vars,Csts_v,Code_in,Vars_in,Csts_v_in,Code_out,Vars_out,Csts_v_out),
	    Recur_out=[Code_out,Vars_out,Csts_v_out,Vect] ;
		Recur_out = Recur_in ),
	ord_union(Carry,Carry_in,Carry_out),
	ord_union(Csts,Csts_in,Csts_out),

/*	write('------------------- '),nl,
	write('  OUT Prep  : '),write(Prep_out),nl,
	write('  OUT Carry : '),write(Carry_out),nl,
	write('  OUT Recur : '),write(Recur_out),nl,
*/
	!.


mixer_code(Code,[Vb1,Vars],Csts_v,Code_in,Vars_in,Csts_v_in,(Code,Code_in),Vars_out,Csts_v_out):-
	append_if_not_present(Vb1,Vars_in,V_res1),
	mix_vars(Vars,V_res1,Vars_out),
	mix_vars(Csts_v,Csts_v_in,Csts_v_out),
/*	tab(5),write('Vars_out : '),write(Vars_out),nl,
	tab(5),write('Vars_in  : '),write(Vars_in),nl,
	tab(5),write('Vars     : '),write(Vars),nl,
	write('********************'),nl,
	tab(5),write('Csts_v_out : '),write(Csts_v_out),nl,
	tab(5),write('Csts_v_in  : '),write(Csts_v_in),nl,
	tab(5),write('Csts_v     : '),write(Csts_v),nl,nl,
*/
	!.

mix_vars([],V_res1,V_res1):-!.
mix_vars([Vb|Vars],V_in,V_res):-
	append_if_not_present(Vb,V_in,V_res1),
	mix_vars(Vars,V_res1,V_res),
	!.


% ---------------------------------------------------------------------
% genere le code prolog à partir des composants C_prep C_recur et Carry

genere_code(Rule_name,Lettre,C_prep,Carry,Csts,C_recur,Rule_mod):-
	write('genere_code : Regle = '),tab(5),write(Rule_name),nl,nl,
	write('C_recur = '),write(Lettre),tab(5),write(C_recur),nl,
	enregistre_csts(Csts,Rule_name),               % ajouté ici 20/02/12
	extraire_carry(Carry,C_recur,Carry_prep,Car_rec_in,Car_rec_out),
	/*
	 write('Carry prep ='),tab(5),write(Carry_prep),nl,
	 write('Carry recur_in  ='),tab(5),write(Car_rec_in),tab(5),
	 write('Out ='),write(Car_rec_out),nl,nl,
	 */
	extraire_constantes(Csts,C_recur,Csts_v),
	% write('Csts   :'),write(Csts),nl,
	% write('Csts_v :'),write(Csts_v),nl,nl,

	genere_code_prep(Rule_name,C_prep,Carry_prep,Csts_v,C_recur,Rec_r_name,Nb_series,Rule_mod),
	genere_code_recur(Rec_r_name,Lettre,Nb_series,C_prep,C_recur,Car_rec_in,Car_rec_out,Csts_v,Rule_mod),
       nl,nl,
       write('Fin de la generation du code'),nl,nl,
	enregistre_csts(Csts,Rule_name),

	!.

%---------------------------------
enregistre_csts(Csts,Rule_name):-
	enregistre_cst(Csts,Rule_name,Csts_names),
	set_val(Rule_name,csts_par_defaut,Csts_names),
	!.

enregistre_cst([],_Rule_name,[]):- !.
enregistre_cst([Cst|T_csts],Rule_name,[Cst_name|Csts_names]):-
	restaurer_excl_dollar(Cst,Cst_name),
	(   descripteur_local(Rule_name,csts_par_defaut,Cst_name,_Val) -> true ;
	                          set_des(Rule_name,csts_par_defaut,Cst_name,nil)),
	enregistre_cst(T_csts,Rule_name,Csts_names),
	!.


%---------------------------------
extraire_carry(Carry,C_recur,Carry_prep,Carry_recur_in,Carry_recur_out):-
	findall(Car,(member(Car,Carry),not(Car=(_-_-_=_))),Carry_b),
	C_recur=[_Code,Vars,_Csts_v,Vars_all],
    %  nl,write('APPEL de extraire_carry_recur'),nl,
	extraire_carry_recur(Carry_b,Vars,Vars_all,Carry_recur_in,Carry_recur_out),
	% carry initial regle_prep mis à 0
	findall(0,member(_,Carry_recur_in),Carry_prep),
	!.
extraire_carry(_Carry,_C_recur,[],[],[]):-
	nl,
	!.


extraire_carry_recur([],Vars_at_end,Vars_at_end, [],[]):-!.
extraire_carry_recur([Car_b|T_carry],Vars,V_at_end,
		     [Carry_in_bind|T_carry_prep_in],[Carry_out_bind|T_carry_prep_out]):-
	member(L-N-Car_b=Carry_in_bind,Vars),
     % write('extraire_carry_recur Car_b='-Car_b),
     % write(' N='),write(N),write('       Vars :'-Vars),nl,
	N_out is N+1,
	member(L-N_out-_X=Carry_out_bind,Vars),
	!,
     % write('found '-X-Carry_out_bind),nl,nl,
	extraire_carry_recur(T_carry,Vars,V_at_end, T_carry_prep_in,T_carry_prep_out).


extraire_carry_recur([Car_b|T_carry],Vars,V_at_end,Carry_In,Carry_Out):-
   % nl,write(' 2  extraire_carry_recur Car_b='-Car_b ),
	member(L-N-Car_b=_Carry_in_bind,Vars),
   % write('     Ok Found in 2      N='),write(N),nl,
   % write('Vars'-Vars),nl,
	N_out is N+1,
	N =< 0,  % evite de partir ds infinite_loop
	atomic_concat(L,N_out,Car_N),
	( (memberchk(Car_N,T_carry);N_out=0)
	   ->	T_carry_N=[Car_b|T_carry]  ;
	        append([Car_N],[Car_b|T_carry],T_carry_N)),

   % write('   fin 2 extraire_carry_recur    L-N_out='-L-N_out-'  T_carry_N:'-T_carry_N),nl,nl,

	extraire_carry_recur(T_carry_N,[L-N_out-Car_N=_Carry_bind|Vars],V_at_end,Carry_In,Carry_Out),
	!.

%---------------------------------
extraire_constantes(Csts,C_recur,Csts_variables):-
	C_recur=[_Code,_Vars,Csts_v,_Vect],
	extraire_constante(Csts,Csts_v,Csts_variables),
	!.

extraire_constante([],_,[]):-!.
extraire_constante([Cst|T_csts],Csts_v,[Cst_bind|T_csts_bdg]):-
	member(Cst=Cst_bind,Csts_v),
	extraire_constante(T_csts,Csts_v,T_csts_bdg),
	!.


% --------------------------------------------
genere_code_prep(Regle,C_prep,Carry,Csts_v,C_recur,Recur_rule_name,Nb_series,Rule_module):-
	genere_code_prep_debut(C_prep,Code_debut,Vars_series,C_recur),
	length(Vars_series,Nb_series),
      % write('Prep debut : Code '),write(Code_debut-Vars_series),nl,
	atomic_list_concat([Regle,'_',recur],Recur_rule_name),

	% new 18/11/11
	copy_term(Vars_series,N_Vars_series),
	append([Recur_rule_name],N_Vars_series,Rec_rule_name_series),
	% end new
        % append([Recur_rule_name],Vars_series,Rec_rule_name_series), %(exclussif supra)

	append(Rec_rule_name_series,[Carry,Constantes],Rec_rule_name_series_carry),
	append(Rec_rule_name_series_carry,[Var_res],L_appel_recur),
	Code_appel_recur =.. L_appel_recur,
	% passer un parm pour savoir si reverse ou pas
	Code_fin = rev(Var_res,Resultat),

	atomic_list_concat([Regle,'_',regle],Rule_name),
	R_head =.. [Rule_name,Resultat],
	R_body = ( Code_debut,
		   same_length_lists_cut_front(Vars_series,N_Vars_series),
		   recherche_constantes(Regle,Constantes),
		   Code_appel_recur,
		   Code_fin),

     % write('Code 1 : '),write(R_head),write(' :-'), nl,tab(15),write(R_body),nl,
	stocke_regle(Rule_module,R_head,R_body),
	R_head_csts  =.. [Rule_name,Csts_v,Resultat],
	% petit trick
	Csts_v = Constantes,
	%stockage d'une forme avec passage constantes en params (?utilité --> pour genetic pgm)
	R_body_csts = ( Code_debut,
			same_length_lists_cut_front(Vars_series,N_Vars_series),
			Code_appel_recur,
			Code_fin),
	(   Csts_v = [] -> true ;
	                   stocke_regle(Rule_module,R_head_csts,R_body_csts)),
	!.


genere_code_prep_debut([Obj_val-Var-_Parm],Code,[V_serie],_C_recur):-
	Code = (Obj_val,rev(Var,V_serie)),
	!.
genere_code_prep_debut([Obj_val-Var-_Parm|T_prep],(Code,T_code),[V_serie|T_var],C_recur):-
	% amener une variable décidant si rev(Var,V_serie) ou pas
	% si reverse :
	Code = (Obj_val,rev(Var,V_serie)),
	% Si pas de reverse :
	% Code= (Obj_Val) , V_serie =Var ,
	genere_code_prep_debut(T_prep,T_code,T_var,C_recur),
	!.


%--------------------------------------------
recherche_constantes(Rule_name,Constantes):-
	valeur_locale(Rule_name,csts_par_defaut,Csts_names),
	recherche_constante(Csts_names,Rule_name,Constantes),
	!.

recherche_constante([],_Rule_name,[]):-!.
recherche_constante([Cst_name|T_csts_names],Rule_name,[Constante|T_constantes]):-
	descripteur_local(Rule_name,csts_par_defaut,Cst_name,Constante),
	recherche_constante(T_csts_names,Rule_name,T_constantes),
	!.

% -------------------------------------------
genere_code_recur(R_name,Lettre,Nb_series,C_prep,C_recur,Carry_recur_in,Carry_recur_out,Csts_v,R_module):-
	Nb_args is Nb_series+3,   % +4 pour Vect
	length(Args,Nb_args),
	Recur_pattern =.. [R_name|Args],
	retracte_regle(R_module,Recur_pattern),
	genere_code_recur_terminales(R_name,Nb_series,R_module),
	genere_code_recurent(R_name,Lettre,C_prep,C_recur,Carry_recur_in,Carry_recur_out,Csts_v,R_module),
	!.


genere_code_recur_terminales(R_name,Nb_series,R_module):-
	length(Pattern_series,Nb_series),
	genere_code_recur_terminale(Pattern_series,Pattern_series,1,R_name,R_module)
      ,console(['genere_code_recur_terminales : FIn'])
        .

genere_code_recur_terminale([],_ ,_ ,_,_ ):-!.
genere_code_recur_terminale([_|Tail],Pattern_series,N,R_name,R_module):-
	copy_term(Pattern_series,N_pattern),
	nth1(N,N_pattern,[]),
	append([R_name],N_pattern,Rec_rule_name_series),
	append(Rec_rule_name_series,[_,_,[]],L_appel_recur_terminal),
	Code_appel_recur =.. L_appel_recur_terminal ,

       console(['Code recur_terminal  :',Code_appel_recur,' :- ' ]),

	stocke_regle_recur(R_module,Code_appel_recur,!),
	NN is N+1,
	genere_code_recur_terminale(Tail,Pattern_series,NN,R_name,R_module),
	!.



genere_code_recurent(R_name,Lettre,C_prep,C_recur,Carry_recur_in,Carry_recur_out,Csts_v,R_module):-
	% C_recur =[Expression,C_recur_vars,_,_Parm_out],
	C_recur =[Expression,_Vars,_,C_recur_vars],

    write('genere_code_recurent C_recur_vars:'-C_recur_vars),nl,
	g_listes_series_pattern(C_prep,C_recur_vars,L_series_pattern,L_series_call),

	append([R_name],L_series_pattern ,Rule_name_and_series),
	append(Rule_name_and_series,[Carry_recur_in,Csts_v],Rule_name_series_carry_csts ),

	member(Lettre-0-_Vn=Var,C_recur_vars),
	Resultats =[[  Var |T_res]],
	append(Rule_name_series_carry_csts,Resultats,Head_elemnts ),
	R_head =.. Head_elemnts ,

	append([R_name],L_series_call ,C_rule_name_and_series),
	append(C_rule_name_and_series,[Carry_recur_out,Csts_v],C_rule_name_series_carry_csts ),
	append(C_rule_name_series_carry_csts,[T_res],Call_elmnts ),

	Call_recur =.. Call_elmnts,
	R_body = ( Expression,Call_recur),
	stocke_regle_recur(R_module,R_head,R_body).


g_listes_series_pattern([] ,_C_recur , [] , [] ):- !.
g_listes_series_pattern([_-_-[L-N-Vn=_Bdg]|T_c_prep],C_rec_vars,[[Var|T_var]|T_ptrn],[T_var|T_s_call]):-
	member(L-N-Vn=Var,C_rec_vars),
	g_listes_series_pattern(T_c_prep,C_rec_vars,T_ptrn,T_s_call),
	!.




/*------------------------------------------------------------------------------*/

same_length_lists_cut_front(In_lists,Out_lists):-
	findall(Lg,(member(L,In_lists),length(L,Lg)),Lengths),
	min_list(Lengths,Min_lg),
	cut_lists(In_lists,Lengths,Min_lg,Out_lists),
	!.

cut_lists([],_Lgths,_Min_lg,[]).
cut_lists([In_list|T_lists],[Length|T_lgs],Min_lg,[Out_list|T_out_lists]):-
	cut_to_length(In_list,Length,Min_lg,Out_list),
	cut_lists(T_lists,T_lgs,Min_lg,T_out_lists).

cut_to_length(List,List_lg,Out_lg,Out_list):-
	Lg_to_cut is List_lg-Out_lg,
	length(Cutted,Lg_to_cut),
	append(Cutted,Out_list,List),
	!.



% ------- FIN ----------
