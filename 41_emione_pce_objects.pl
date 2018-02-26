% file :	41_emione_pce_objects
% date :	13/09/2006 + 2012 +2013
% but :	Definition des objets PCE utilisés avec Emione

:- use_module(library('plot/plotter')).
:- use_module(library('plot/axis')).
:- use_module(library(autowin)).
:- use_module(library(pce)).
:- use_module(emione).

% définitions des objets Emione - Pce
creer_infrastructure_emione_pce :-	% infrastructure allready exists
	objet_qq(emione_pce),!.
creer_infrastructure_emione_pce :-
	creer_obj(emione_pce,[]),
	% les objets fils ....
	% picture
	creer_obj(picture,[emione_pce]),set_val(picture,area,600-600-0-0),
	% plotter
	creer_obj(plotter,[emione_pce]),
	set_des(plotter,id,
		si_besoin,(parm(O,_A,V),atomic_concat(@,O,N),atom_to_term(N,V,_))),
	set_val(plotter,origine_axes,40-560),
	set_val(plotter,x_length,700),
	set_val(plotter,y_length,350),
	set_val(plotter,recogniser,
	     click_gesture(left,'',single,
			   message(@prolog,display_gesture,left,@receiver,@arg1))),
	%  ajouter les autres objets ci-dessous
	!.

% -----------------------
% PICTURE - (la fenetre qui rassemble plotter + textes + ....)
%
creer_picture(Picture,Pict_id):-
	valeur_locale(Picture,id,Pict_id),
	!.
creer_picture(Picture,Pict_id):-
	new(Pict_id,emione_picture),
	send(Pict_id,open),	    % pour que params de positions fonctionnent
	send(Pict_id,show,false),
	(   objet_qq(Picture) -> true;creer_obj(Picture,[picture])),
	set_val(Picture,id,Pict_id),
	!.
creer_picture(Picture,Pict_id,Parametres):-
	creer_picture(Picture,Pict_id),
	set_val(Picture,param,Parametres),
	set_param_list(Parametres,Pict_id),!.

% PCE spécialisation of "picture" to capture  clossing of page
:- pce_begin_class(emione_picture, picture,"Emione Picture").
initialise(Pict, Title:[name]) :->
	default(Title,'Emione-Picture',Titre),
	send(Pict, send_super, initialise,Titre).

unlink(Pict) :->
	send(Pict, send_super, unlink),
	(valeur_locale(Picture,id,Pict)-> reset_val(Picture,id);true).
	% suppression Picture ==> suppr. (Picture,id)

:- pce_end_class.


% -------------------------------------------------------------------
% PLOTTER
%
creer_plotter(Plotter,Plot_id):-
	valeur_locale(Plotter,id,Plot_id),
	!.
creer_plotter(Plotter,Plot_id):-
	new(Plot_id,emione_plotter),
	(   objet_qq(Plotter) -> true;creer_obj(Plotter,[plotter])),
	set_val(Plotter,id,Plot_id),
	!.
creer_plotter(Plotter,Plot_id,Parametres):-
	creer_plotter(Plotter,Plot_id),
	set_val(Plotter,param,Parametres),
	set_param_list(Parametres,Plot_id),!.


% PCE spécialisation of "plotter" to capture  clossing of page
%
:- pce_begin_class(emione_plotter, plotter,"Emione Plotter").
initialise(Plotter) :->
	send(Plotter, send_super, initialise),
	% recogniser on all plotters (position sur clic_left)
	send(Plotter,recogniser,
	     click_gesture(left,'',single,
			   message(@prolog,display_gesture,left,@receiver,@arg1))).

unlink(Plot_id) :->
	send(Plot_id, send_super, unlink),
	(valeur_locale(Plotter,id,Plot_id)-> reset_val(Plotter,id);true).
	% suppression Picture ==> suppr. Plotter,id [side effect]


:- pce_end_class.		% emione_plotter


% --------------------------------------------------------------------------------
% set_param_list(+Parametres,+Obj_pce_id)
% execute send sur liste Parametres : [height:1000,frame(x:1800,label:'Symp..')]
%
set_param_list([],_):-!.
set_param_list([Param|Tail],Obj_pce_id):-
	Param =.. [':',Arg,Valeur],
	send(Obj_pce_id,Arg,Valeur),
	set_param_list(Tail,Obj_pce_id).
set_param_list([Param|Tail],Obj_pce_id):-
	Param =.. [Objet|Args],
	get(Obj_pce_id,Objet,Obj_id),
	set_param_list(Args,Obj_id),
	set_param_list(Tail,Obj_pce_id).



% -------  Les "gestures" pour récupérer les mouvements des objets (texte,choix,etc).
:- pce_begin_class(move_gest_ja,move_outline_gesture,"JAB outline move gesture").

terminate(G,Ev:event) :->
	"terminate event move_outline and report to Symphanie" ::
	send_super(G,terminate,Ev),
	get(Ev,receiver,Obj),
	get(Obj,area,area(X,Y,W,H)),
	console(['Fin de bouger '-G-Ev- Obj-dim-X-Y-W-H]),
	% si il existe Obj_emione avec id = Obj
	(valeur_locale(Obj_e,id,Obj) ->
		 ( console(['	Objet Emione :',Obj_e,X,Y]),
		  set_val(Obj_e,pos_x,X), set_val(Obj_e,pos_y,Y)	)
		;true ).

:- pce_end_class.


/*******************************
 *	RULER_jab      *
 *******************************/
% Ruler : transformé 

:- pce_begin_class(plot_ruler_jab, plot_ruler, "modif du plotter pour enlever les labels jaunes").

variable(value,		real,		get,	"X/Y position").
variable(value_format,	name := '%g',	both,	"Format used for values").


initialise(R) :->		% juste pour enlever le fond jaune des labels
	send_super(R, initialise),
	get(R, member, text, Text),
	send(Text, background, white). 		% ds Swi : navajo_white
	

% les autres methodes sont heritées de 'plot_ruler'

:- pce_end_class.

% --- Fin de RULER_jab transformé  --- */

/*******************************
 *	RULER_ZONE      *
 *******************************/
% Ruler : transformé pour en faire les zones de couleur

:- pce_begin_class(plot_ruler_zone, device, "draw a zone X1 X2 on plotter").

variable(value,		real,		get,	"X/Y position").
variable(value_format,	name := '%g',	both,	"Format used for values").
variable(value_2,	real,		get,	"Widt or Heigth ").
variable(texte,		atomic,		get,	"Text in zone").

initialise(R) :->
	send_super(R, initialise),

	new(B, box),
	send(B,hide),
	send(R, display, B),
	% send(R, display, new(B, box)),

	send(B, texture, dotted),
	% send(B,hide),
	% send(R, display, new(T, text(0))), % initialement
	send(R, display, new(T, text)),
	send(T, border, 1),
	    % send(T, background, green),
	send(R, slot, value, 0).

%  Haut,Bas,Colour,Txt)

attach(R, Axis:axis=plot_axis, V1:real,V2:number,Colour:atomic,
       Txt:atomic,Length:length=[int]) :->
	send(R, detach),
	get(R, member, box, Box),
	get(R, member, text, Text),
	get(Axis, device, Plotter),
	get(Axis, format, Format),
	send(R, slot, value_format, Format),
	(   get(Axis, type, x)		% X-axis
	->  send(Text, format, center),
	    send(Text, center_x, 0),
	    get(Axis, y, Bottom),
	    send(R, y, Bottom),
	    (	Length == @default
	    ->	get(Plotter, y_axis, YAxis),
		get(YAxis, length, Len)
	    ;	Len = Length
	    ),
	    % send(Line, points, 0, 5, 0, -Len), % avant
	      send(Box,set, 0, 1, 25, -Len),
	    send(Text, y, 5)
	;   send(Text, format, left),	  % Y-axis
	    send(Text, center_y, 5),
	    get(Axis, x, Left),
	    send(R, x, Left),
	    (	Length == @default
	    ->	get(Plotter, x_axis, XAxis),
		get(XAxis, length, YWidth)
	    ;	YWidth = Length
	    ),
	      % send(Line, points, -5, 0, Right, 0),  % avant
	        send(Box, set, 1, 1, YWidth, 25),
	    send(Text, x, +6)  % send(Text, x, -5-Text?width)
	),
	send(Box,colour,colour(Colour)),
	send(Box,fill_pattern,colour(Colour)),
	send(Box,hide),
	send(Plotter, display, R),
	new(_, hyper(Axis, R, rules, axis)),
	send(R, value, V1),
	send(R,slot,value_2,V2),
	send(R,slot,texte,Txt),
	!.

detach(R) :->
	send(R, delete_hypers, axis),
	send(R, device, @nil).

value(R, Val:real) :->
	send(R, slot, value, Val),
	send(R, request_compute, value).

compute(R) :->
	(   get(R, request_compute, value),
	    send(R, slot, request_compute, @default),
	    get(R, hypered, axis, Axis),
	    get(R, device, Plotter),
	    get(R, member, box, Box),
	    Plotter \== @nil
	->  get(R, value, Value),
	    (	get(Axis, type, x)
	    ->	get(Plotter, translate_x, Value, X),
		get(R,value_2,V2),
		get(Plotter, translate_x,V2,Width_P),
		Width is abs(X-Width_P),
		send(Box,width,Width), % sur Box
		send(R, x, X)
	    ;	get(Plotter, translate_y, Value, Y),
		get(R,value_2,V2),
		get(Plotter, translate_y,V2,Heigth_P),
		Height is abs(Y-Heigth_P),
		send(Box,height,Height), % sur Box
		send(R, y, Y)
	    ),
	    send(Box,hide),
	    get(R, member, text, Text),
	    get(R,texte,Txt),
	    % get(R, value_format, Format),
	    send(Text, string, Txt)
	;   true
	),
	send_super(R, compute).

:- pce_end_class.

% --- Fin de RULER_ZONE transformé  --- */


%  associe un comportement aux objets selon les clics souris !! très fort SWI...
% @mover : objet global
:- pce_global(@mover, new(handler_group(
		new(resize_gesture),
		new(move_gest_ja(modifier := sc )),	% milieu +Shift +Ctl
		new(handler(ms_left_up,  message(@prolog,writeln,@receiver)))	))).
% send(Obj,recogniser,@mover) : envoi de @mover sur Obj
% ==> capture les comportements souris sur l'Obj


% ----------------------
activer_recogniser(Recogniser):-
	object(Recogniser),
	send(Recogniser,active,@on).
desactiver_recogniser(Recogniser):-
	object(Recogniser),
	send(Recogniser,active,@off).

% --------------------------------------------------------------------------
% Launch the initialisation just before starting the application
% with  the directive :- creer_infrastructure_emione_pce.
% --------------------------------------------------------------------------
% FIN
