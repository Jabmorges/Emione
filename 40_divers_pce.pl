% 40_divers_pce
% divers objets pce pour applications
% date : 17/02/07

:- require([ writeln/1
	   , new/2
	   , pce_global/2
	   , send/2
	   , get/3
	   ]).

% confirmer : 	fenetre de Message+confirmation
% fenetre généraliste permettant de confirmer un message 
:- pce_global(@confirmer, make_confirmer).
	
make_confirmer(P) :-
	new(P, dialog(confirmer)),
	send(P, kind, transient),
	send(P, append, label(prompt)),
	send(P, append, button(yes, message(P, return,@receiver?label))),
	send(P, append, button(no, message(P, return, @receiver?label))),
	send(P, append, button(cancel, message(P, return, @nil))).
	
confirmer_dialog(Prompt,[Yes_bt_lab ,No_bt_lab,Cancel_bt_lab],  Reponse) :-
	get(@confirmer,member(prompt),Mess_obj),
	send(Mess_obj, selection, Prompt),
	get(@confirmer,member(yes),Yes_obj),
	send(Yes_obj, selection, Yes_bt_lab),
	get(@confirmer,member(no),No_obj),
	send(No_obj, selection, No_bt_lab),
	get(@confirmer,member(cancel),Cancel_obj),
	send(Cancel_obj, selection, Cancel_bt_lab),
	send(@confirmer,default_button,Yes_obj),
	get(@confirmer, confirm_centered, RawName),
	send(@confirmer, show, @off),
	Reponse = RawName.

% juste le message / les réponses possible sont { yes,no,cancel}
confirmer(Message):-                 		% fails if Answ =\= yes
	terms_to_atom(Message,' ',Mesg_atom),
	confirmer_dialog(Mesg_atom,[yes,no,cancel],Answ),
	Answ == yes. 

confirmer(Message,Answ):-			% returns the Answer 
	terms_to_atom(Message,' ',Mesg_atom),
	confirmer_dialog(Mesg_atom,[yes,no,cancel],Answ). 

confirmer(Message,L_butons_label,Answ):-
	terms_to_atom(Message,' ',Mesg_atom),
	findall(At,(member(X,L_butons_label),format(atom(At),'~w',X)),Labels),
	(   length(Labels,2)-> append(Labels,[cancel],All_labels) ;
	                       All_labels= Labels),
	confirmer_dialog(Mesg_atom,All_labels,Answ).



% transforme une liste de termes en un atom     ===> 00_util
terms_to_atom(Message,Separator,Mesg_atom):-
	is_list(Message),
	findall(At,(member(X,Message),format(atom(At),'~w',X)),Liste_atoms),
	concat_atom(Liste_atoms,Separator,Mesg_atom),
	!.
terms_to_atom(Message,_,Mesg_atom):-
	term_to_atom(Message,Mesg_atom).





% ---------------------------------------------------------------------------------
% asker :  Version encore brute ....
% 
:- pce_global(@asker, make_asker).

make_asker(P) :-
	new(P, dialog),
	send(P, kind, transient),
	send(P, append, label(prompt)),
	send(P, append,
	        new(TI, text_item(name, '',
			 message(P?ok_member, execute)))),
	send(P, append, button(ok, message(P, return, TI?selection))),
	send(P, append, button(cancel, message(P, return, @nil))).
	
	
asker(Prompt, Label, Name) :-
	send(@asker?prompt_member, selection, Prompt),
	send(@asker?name_member, label, Label),
	send(@asker?name_member, clear),
	get(@asker, confirm_centered, RawName),
	send(@asker, show, @off),
	RawName \== @nil,
	Name = RawName.

ask :-
	asker('Quel et la valeur de ....', attribut, Name),
	writeln(Name).




% console
% 
creer_console(Nom_console):-
	atom(Nom_console),
	new(Console,view(Nom_console)),
    write( console:Console),nl,
	send(Console,open),
	pce_open(Console,write,Stream),	% write ==> clear le stream + fenetre
	set_stream(Stream,alias(Nom_console)).

creer_console(_):-
	write('creer_console Failed'),nl.

