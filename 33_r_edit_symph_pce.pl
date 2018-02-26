% file	: 33_r_edit_symph_pce
% Partie user interface graphique de l'éditeur de règles pour symphony
% date	: 15/02/2007


editer_regle_s(Obj):-
	Att = s_signal,
	Module = rgls_symph,
	editer_regle(Obj,Att,Module).

editer_regle(Obj,Att,Module) :-
	Obj_pere = Module ,
	( objet_qq(Obj_pere) -> true ; creer_obj(Obj_pere,[r_dynamique])),
	atomic_list_concat([Obj,'_',Att,'_',Module],Obj_regle),
	( objet(Obj_regle,Obj_pere) -> true ;
	                               creer_objet_regle_vierge(Obj_regle,Obj,Att,Module,Obj_pere) ),
	set_val(Obj_pere,current_rule,Obj_regle),
	maj_rule_editor(Obj_regle),
	show_in_browser(Obj_regle),
	!.


maj_rule_editor(Obj_regle):-
	get(@rule_editor,member('Rule Name'),R_name),
	send(R_name,selection,Obj_regle),
	valeur(Obj_regle,connect_to,(Obj,Att)),
	atomic_list_concat([Obj,' - ',Att],OA),
	get(@rule_editor,member('Connection'),Connect_to),
	send(Connect_to,selection,OA),
	valeur(Obj_regle,rule_module,Rule_module),
	get(@rule_editor,member('Module'),Module),
	send(Module,selection,Rule_module),
	% fait la maj des diverses valeurs pour les boutons et labels ..
	maj_menus_r_ed(Obj_regle),
	send(@rule_editor,open),
	send(@rule_editor,fit),
	!.

edit_litteral(Litteral):-
	get(@rule_editor,member(edited),Menu),
	send(Menu,selection,Litteral),
	!.

% ----------------------------------------------------------------
% fait la maj des champs [litteral/signal/constantes] de l'éditeur
maj_menus_r_ed(Obj_regle):-
	maj_menu_item(edited,Obj_regle,litterals,Selected_lit),
	maj_value_item(input,Obj_regle,Selected_lit,litteral),
	maj_menu_item(signal,Obj_regle,litterals,_),
	maj_menu_item(constantes,Obj_regle,csts_par_defaut,Selected_cst),
	maj_value_item(cst_val,Obj_regle,csts_par_defaut,Selected_cst),
	% partie "Done : [x,y]-[a,f]"
	get(@rule_editor,member(done),T_done),
	valeur(Obj_regle,litterals,Litterals),
	valeur(Obj_regle,mandatory_litterals,Mandatory),
		sort(Litterals,Done_l),
	ord_subtract(Mandatory,Done_l,Tobe_done),
	format(atom(Done_tobe_done),'~w - ~w',[Litterals,Tobe_done]),
	send(T_done,selection,Done_tobe_done),
	get(@rule_editor,member(signal),M_signal),
	valeur(Obj_regle,signal_on,Let_sig),
	send(M_signal,selection,Let_sig),
	!.

maj_menu_item(Menu,Obj_regle,Att,Selected_val):-
	get(@rule_editor,member,Menu,M_item),
	(   get(M_item,selection,Cur_selection);true),
	(   valeur(Obj_regle,Att,Valeurs)-> true ; Valeurs=[]),
	(Valeurs=[] -> Choix=[none];Choix=Valeurs ),
	send(M_item,members(Choix)),
	(   member(Cur_selection,Choix) -> ( send(M_item,selection,Cur_selection),
	                                     Selected_val=Cur_selection );
					   ( member(First,Choix),
					     send(M_item,selection,First),
					     Selected_val=Cur_selection  )),
	!.

maj_value_item(Item_name,Obj,Att,Desc):-
	get(@rule_editor,member,Item_name,Item_id),
	(   descripteur(Obj,Att,Desc,Valeur) -> send(Item_id,selection,Valeur);
						send(Item_id,clear) ).


% -----------------------------------------------------------------
% clauses appelées par les message de @rule_editor
maj_input(Litteral,Input,Regle):-
	descripteur(Regle,Litteral,litteral,Exp),
	send(Input,selection,Exp),
      write('maj_input :'-Litteral-Input-Regle),nl,
	!.

maj_signal_id(Regle,Lettre):-
	set_val(Regle,signal_on,Lettre).


get_cst_val(Constante,Cst_txt_id,Obj_regle):-      % get constante sur Obj
	descripteur(Obj_regle,csts_par_defaut,Constante,Valeur),
	send(Cst_txt_id,selection,Valeur),
      write('get_cst_val :'-Constante-Valeur-done),nl,
	!.

maj_cst_val(Menu_id,Constante,Cst_txt_id,Str_valeur,Obj_regle):-      % maj constante sur Obj
	( catch(atom_to_term(Str_valeur,Valeur,_B),_Err,fail)->true ; Valeur=Str_valeur ),
	set_des(Obj_regle,csts_par_defaut,Constante,Valeur),
      write('maj_cst_val :'-Menu_id-Constante-Cst_txt_id-Valeur-Obj_regle),nl,
	!.


% ------------------------------------------
% creation de l'objet @rule_editor
:-pce_global(@rule_editor,make_rule_editor).

make_rule_editor(R_ed):-
	new(R_ed, dialog('Rule Editor')),
	send(R_ed,width,670),
	send(R_ed,gap,size(10,15)),
	% les 3 items du haut : Rule Name/Connection/Module
	send(R_ed, append, new(R_name,text_item('Rule Name',''))),
	send(R_ed, append, new(C_to,text_item('Connection',''))),
	send(R_ed, append, new(R_mod,text_item('Module',''))),
	Items =[R_name,C_to,R_mod],
	send_list(Items,editable,off),
	% les 2 menus deroulants
	send(R_ed,append,new(M_lit,menu(edited,cycle))),  % litteraux
	send(R_ed,append,new(M_signal,menu(signal,cycle)),right),
	send(M_signal,alignment,left),
	% ----
	send(R_ed, append, new(L_done,text_item(done,'[]-[]')),right),
	send(L_done,length,35),
	send(L_done,alignment,left),
	send(L_done,editable,off),
	% ----
	send(R_ed, append, new(Input, text_item(input))),
	send(Input,length,80),

	send(R_ed,append,new(M_csts,menu(constantes,cycle))),
	send(M_csts,value_width,150),  % en pixels
	send(R_ed,append,new(Cst_val,text_item(cst_val,'')),right),
	send(Cst_val,length,20),  % en char
	send(Cst_val,alignment,left),

	send(M_signal,message,message(@prolog,maj_signal_id,R_name?selection,@arg1 )),

	send(M_csts,message,message(@prolog,get_cst_val,M_csts?selection,    % @arg1,
				     Cst_val,R_name?selection) ),
	send(Cst_val,message,message(@prolog,maj_cst_val,M_csts,M_csts?selection,
				     @receiver,@arg1,R_name?selection) ),

	% partie Formule XL
	send(M_lit,message,message(@prolog,maj_input,
				   M_lit?selection,Input,R_name?selection)),
		send(Input,message,and( message(@prolog,traite_input,
					Input?selection,R_name?selection),
				   message(@prolog,maj_menus_r_ed,R_name?selection))),
	% Bouton : CREATE
	send(R_ed, append,
		 button(create, and( message(@prolog,traite_input,
					Input?selection,R_name?selection),
				   message(@prolog,maj_menus_r_ed,R_name?selection))) ),
	% bouton : COMPILE
	send(R_ed, append,
		 button(compile, and( message(@prolog,compile_regle,
					R_name?selection,M_signal?selection),
				      message(@prolog,maj_menus_r_ed,R_name?selection))) ),

	% Bouton : CONNECTE
	send(R_ed, append,
		 button(connect,  message(@prolog,connecte_code_m,
					R_mod?selection,R_name?selection,C_to?selection)) ),

	% Bouton : DELETE   Efface la règle
	send(R_ed, append,
		 button(delete,  message(@prolog,delete_rule,
					R_mod?selection,R_name?selection,C_to?selection)) ),

	send(new(report_dialog),below,R_ed).


% send(D, append, text_item(size, B?size,  message(B, size, @arg1))),

