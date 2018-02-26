% file	:	20_hierarchy.pl
% date :		1/5/2006 - 02/02/2007
% version :	0.80
% affichage de hierarchies avec interface utilisateur pour diverses opérations de l'utilisateur

:- pce_autoload(toc_window, library(pce_toc)).
:- use_module(library(pce)).
:- use_module(emione).
:- pce_autoload(report_dialog, library(pce_report)).

% appel du browser objets emicat
browse(Obj):-
	objet_qq(Obj),
	% in_pce_thread( ..),
	send(object_explorer(Obj), open).


%------------------------------------------------------------------------------------------------
% Explorer pour des objets  (ie browser + viewer)
:- pce_begin_class(object_explorer,frame,"Explore Object hierarchy").

initialise(E, Objet_racine:any) :->
	atomic_list_concat(['Browser',Objet_racine],' : ',Titre),
	send_super(E,initialise,Titre),
	send(E,append,new(OB,object_hierarchy_window(Objet_racine))),
	new(V,view),
	send(V,font,normal),
	send(V,right,OB),
	send(new(report_dialog),below,OB).

open_node(E,Obj_emione: any) :->
	get(E,member,view,View),
	send(View,clear),
	pce_open(View,append,Stream),
	current_output(Current),
	set_output(Stream),
	montrer(Obj_emione),
	set_output(Current),
	close(Stream).

:- pce_end_class.


%------------------------------------------------------------------------------------------------
% Browser d'objets dans la hierarcie emicat
:- pce_begin_class(object_hierarchy_window, toc_window, "Browser for a object-hierarchy").

initialise(FB, Root:any) :->	% Root = racine de la hierarchie
	send_super(FB, initialise),
	send(FB, root, toc_folder(Root, Root)),
	% set_val(Root,toc_tree,FB),
	set_mar(Root,browser,toc_tree,FB),
      write('Opening TOC '-FB),nl,
	send(FB,expand_root).	% montrer niveau 1

unlink(FB) :->
	send_super(FB, unlink),
% 	(   valeur_locale(Obj,toc_tree,FB)-> reset_val(Obj,toc_tree);true),
	(   marqueur(Obj,browser,toc_tree,FB)-> reset_mar(Obj,browser,toc_tree);true),
      write('Closing TOC '-FB),nl,
	!.


expand_node(FB, D:any) :->
	peres_de(D,Fils),
	forall(member(L,Fils), (peres_de(L,[]) ->
		  send(FB,son,D,create(toc_file,L,L))
		; send(FB,son,D,create(toc_folder_obj,L,L)))),
	send(FB,send_super,expand_node,D).

open_node(FB,Object:any) :-> "Envoi open_node sur Frame de FB "::
	send(FB?frame,open_node,Object).

select_node(FB,Object:any) :->  "Selection simple " ::
	send(FB?frame,open_node,Object),
	true.


prune(CH) :-> "Delete from the hierarchy except root"::
	get(CH?tree?selection, head, Node),
	get(CH?tree, root, Root),
	(   Root = Node -> true ; send(Node, delete_tree)).

selection(CH, Class:class) :<-	"Get the selected class"::
	get(CH?tree?selection, head, Node),
	get(Node, identifier, Class).


% les menu pop_up sur l'arbre des objets

:- pce_global(@obj_hierarchy_feuille_popup, make_popup(feuil)).
:- pce_global(@obj_hierarchy_regle_popup, make_popup(regle)).
:- pce_global(@obj_hierarchy_noeud_popup, make_popup(noeud)).
:- pce_global(@obj_hierarchy_noeud_regle_popup, make_popup(noeud_regle)).

make_popup(Creator, P) :-
      write(make_popup_1-Creator -'   ' ),portray(@arg1),nl,
	new(P, popup(Creator, message(@arg2?window, @arg1))),
      write('make_popup_3  ' ),portray(@arg2),nl,

	send_list(P, append,
		  [ menu_item(documentation),
		    menu_item(class_browser, end_group := @on) ]),
	(   Creator == noeud
		->  send_list(P, append,
		      [ menu_item(expand_tree,end_group := @on)])
		;   true
	),
	(   Creator == feuil
		->  send_list(P, append,
		      [ menu_item(calcule),
			menu_item(dessine),
			menu_item(edit_regle,end_group := @on)])
		;   true
	),
	(   Creator ==noeud_regle
		->  send_list(P, append,
		      [ menu_item(expand_tree,end_group := @on),
			menu_item(sauver_module),
			menu_item(charger_module),
			menu_item(supprimer_module,end_group := @on)])
		;   true
	),
	(   Creator ==regle
		->  send_list(P, append,
		      [	menu_item(modifier_regle),
			menu_item(supprimer_regle,end_group := @on)])
		;   true
	),
	send(P, append, gap),
	send(P, append, prune).


popup(_CH,Obj_emione:any , Popup:popup) :<-	"Provide appropriate popup"::
	(   herite_de(Obj_emione,regle)   % evt de r_dynamique
	->  ( peres_de(Obj_emione,[]), not(valeur_locale(Obj_emione,rule_module,_))
	    -> Popup = @obj_hierarchy_regle_popup
	    ;  Popup = @obj_hierarchy_noeud_regle_popup
	    )
	;   ( peres_de(Obj_emione,[])
	    -> Popup = @obj_hierarchy_feuille_popup
	    ;  Popup = @obj_hierarchy_noeud_popup
	    )
	).


% ----------------------------specifique aplication -------------
calcule(CH) :->	"Execute Analysis "::
	get(CH, selection, Objet),
     console(['calcule   call: demo_new   with Objet=',Objet,'		id:',CH]),
     % write(calcule -demo_new(CH)-Objet),nl,
	demo_new(Objet).

dessine(CH) :->	"Plot serie attribute "::
	get(CH, selection, Objet),
     write(dessine -Objet),nl,
	dessine_ask_params(Objet).


edit_regle(CH) :->	"Edite règle de Objet "::
	get(CH, selection, Objet),
      write(edit_regle- call(editer_regle_s(CH)-Objet)),nl,
	editer_regle_s(Objet).

modifier_regle(CH) :->	"Edite règle de Objet depuis browse(regle) "::
	get(CH, selection, Objet),
    write(modifier_regle(CH)-Objet),nl,
	valeur_locale(Objet,connect_to,(Obj,Att)),
	valeur(Objet,rule_module,Module),
	editer_regle(Obj,Att,Module).

supprimer_regle(CH) :->	"Edite règle de Objet "::
	get(CH, selection, Objet),
    console(['supprimer_regle_s(',CH,') -',Objet]),
	delete_rule(Objet),
	!.


sauver_module(CH) :->	"Sauver module de règles"::
	get(CH, selection, Objet),
     write(sauver_module(CH)-Objet),
	sauver_module_regles(Objet),
     write('  .... Fin de sauver module '),nl,
	!.
charger_module(CH) :->	"Charger module de règles"::
	get(CH, selection, Objet),
      write(charger_module(CH)-Objet),nl,
	% charger_module_regles(Objet),
	!.
supprimer_module(CH) :->	"Supprimer module de règles"::
	get(CH, selection, Objet),
      write(supprimer_module(CH)-Objet),nl,
	% supprimer_module_regles(Objet),
	!.


class_browser(CH) :->	"Show class browser on selected class"::
	write('choix ClassBrowser : A implanter'-CH),
	nl.

documentation(CH) :->	"Show source of selected class"::
	get(CH, selection, Objet),
	send(CH?frame,open_node,Objet).

expand_tree(CH) :->	"Expand everything below the selected node"::
	get(CH, selection, Class),
	expand_tree(CH, Class).

expand_tree(CH, Class) :-
	(pere_de(Class,Un_fils),get(CH,node,Un_fils,_) -> true ;
		(  peres_de(Class, Fils) -> send(CH, expand_node, Class),
					forall(member(X,Fils),  expand_tree( CH,X) )
					;   true
		) ).
%dessine_ask_params(Objet) : demande params pour dessiner serie-atts
% TAF : passer la creation fenetre dans 45_ex_plot_dyna
%
dessine_ask_params(Objet):-
	new(D, dialog(string('Plotting : %s',Objet))),

	send(D, append, new(Plotter, menu('Plotter',cycle))),
	    peres_de(plotter,Plotters),
	    send_list(Plotter,append,Plotters),
	send(D, append, new(Add_new, menu('Add/New', cycle))),
	    send(Add_new,append,new),
	    send(Add_new,append,add),
        send(D, append, new(Attribut, menu('Attribut',cycle))),
	    send(Attribut,append,serie),
	    send(Attribut,append,pch),
	    send(Attribut,append,pip),
	    send(Attribut,append,serie_ajustee),
	    send(Attribut,append,s_signal),
	    send(Attribut,append,evaluation),
	    send(Attribut,append,ref_evaluation),

	send(D, append,
		 button(create, message(@prolog, dessine_with_params,
					Add_new?selection,
					Objet,
					Attribut?selection,
					Plotter?selection))),
	send(D, append, button(quit, message(D, destroy))),
	send(D, default_button, create),

	send(D,open).

dessine_with_params(Add_new,O,A,Plotter):-
	memberchk(A,[ref_evaluation,evaluation]),
	(   Add_new=new -> plot_indic(O,s_signal,A,Plotter);
	                   plot_add(O,s_signal,A,Plotter)
	),
	!.
dessine_with_params(Add_new,O,pip,Plotter):- !,
	(   Add_new=new -> plot_indic(O,serie,Plotter);
	                   plot_complete(O,serie,Plotter)
	),
	pip(O,serie,4,Plotter,_Pips_Res).

dessine_with_params(Add_new,O,A,Plotter):-
	(   Add_new=new -> plot_indic(O,A,Plotter);
	                   plot_complete(O,A,Plotter)
	).

% ---------------
show_in_browser(Obj):-
	% valeur(Obj,toc_tree,Toc_tree),
	marqueur(Obj,browser,toc_tree,Toc_tree),
	show_node(Toc_tree,Obj),
	send(Toc_tree,select_node,Obj),
	send(Toc_tree?tree,selection,Obj),
	!.
show_in_browser(_Obj):- !.


show_node(T_tree,Obj):-
	get(T_tree,node,Obj,_)
	-> true ;
	(   pere_de(Pere,Obj),
	    node_is_folder_or_deleted(T_tree,Pere),
	    show_node(T_tree,Pere),
	    forall(pere_de(Pere,F),(get(T_tree,node,F,Nde)->send(Nde,delete_tree);true)),
	    send(T_tree,expand_node,Pere)),
	!.

node_is_folder_or_deleted(T_tree,Pere):-
	get(T_tree,node,Pere,Pere_id),
	(   get(Pere_id,class_name,toc_folder) ->true ;
	                                         send(Pere_id,delete_tree)
	).
node_is_folder_or_deleted(_T_tree,_Pere).

:- pce_end_class.


%------------------------------------------------------------------------------------------------
:- pce_begin_class(toc_folder_obj, toc_folder, "Toc_folder  for a object-hierarchy").
initialise(TF, Root:any,Name:any) :->	% Root = racine de la hierarchie
	send_super(TF, initialise,Root,Name).

open(TF) :-> "special to edit folders"::
	get(TF,node,Node),
	get(Node,identifier,Ident),
	send(TF?window,open_node,Ident),
	send_super(Node,open).

:- pce_end_class.

% --- FIN --------------------------------------------------------------------------------

