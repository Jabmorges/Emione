% file :	45_xpce_plot
% date :	2006 et ss.
% les parties graphiques de symphony et anie
% TAF :
%    - Creer la fenetre pour demander parametres d'affichage des dessins

:- use_module(library(pce)).
:- use_module(library('plot/plotter')).
:- use_module(library('plot/axis')).
:- use_module(library(autowin)).

:- use_module('03_ex_math_et_date', [date_imprimable/2, nb_jour/2, nb_jour/4]).
% il faut moduliser 03_ex_... pour que les imports prennent effet

:- use_module(emione,[creer_obj/2,
		      descripteur_local/4,
		      set_des/4,
		      set_val/3,
		      valeur/3,
		      valeur_locale/3 ]).

% creation page de reporting Symph : 2 graphiques + reporter de signal (à faire)
%
creer_report_page_1:-
	creer_picture(report_1,Pict_id,
		      [height:1000,width:750,frame(x:1000,y:30,label:'Xyz Rep.')]),
	creer_plotter(plotter_1,Plot_1),
	set_val(plotter_1,origine_axes,40-600),
	creer_plotter(plotter_2,Plot_2),
	set_val(plotter_2,origine_axes,40-1000),
	send(Pict_id,display,Plot_1),send(Pict_id,display,Plot_2),
	send(Pict_id,open).


% Creation de la page graphique standard
%
creer_dessins:-
	% la fenetre @f1
	creer_picture(f1,Pict_id,
		      [height:1000,width:800,frame(x:1000,y:30,label:'Graphic Report')]),
	% objets associées
	creer_plotter(p1,P1),
	creer_plotter(p2,P2),
	set_val(p1,date_ruler,[@d1_1,@d1_2]),
	set_val(p2,date_ruler,[@d2_1,@d2_2]),
	set_val(p2,origine_axes,40-950),
	% XPCE part
	new(@t,text),send(Pict_id,display,@t),
	new(@signaux,dict),
	new(@lbrow,list_browser(@signaux,35,10)),
	send(@signaux,append,
	     dict_item(test_1,string('%s\t%s\t%f',date,signal,valeur),arial)),
	send(Pict_id,display,@lbrow,point(40,30)),
	send(Pict_id,display,P1),send(Pict_id,display,P2),
	send(Pict_id,open),
	!.

creer_web_plotters:-
	creer_plotter(wp1,_P1),
	creer_plotter(wp2,_P2),
	!.

% ----------------------------------------------------------------------
% BORNES d'une série
% bornes(+ O,+ A, V,Min,Max,Ampl,   D,MinD,MaxD,AmplD):-
% recupère : V +bornes Min-Max-Amplitude de V   | idem sur les dates
%
bornes(O,A, V,Min,Max,Ampl, D,MinD,MaxD,AmplD):-
	% recupère le vecteur des valeurs (evt. déclenche le calcul)
	valeur(O,A,V),
	% récupère le vecteur des dates
	(descripteur_local(O,A,dates,D); valeur(O,dates,D)), % ante 10/2014
	/*(descripteur(O,A,periodicite,P)
		-> valeur(O,dates(P),D);    % valeur_locale par la suite ??
	           valeur_locale(O,dates,D)), */

      %	length(V,Lg_s),length(D,Lg_s),
      % V et D doivent être de même longueur !! PAS OBLIGATOIRE!!
	get_min_max_ampl(D,MinD,MaxD,AmplD),
	get_min_max_ampl(V,Minv,Maxv,Amplv),
	(   Amplv=\=0 -> (Max =Maxv,Min= Minv,Ampl=Amplv) ;
	                 (Max is Maxv+1,Min is Minv-1,Ampl is 2)
	),
	!.
bornes(O,A,Desc,V,Min,Max,Ampl, D,MinD,MaxD,AmplD):-
	obj_name = O et Desc de A=V , % ==> activer_desc(O,A,Desc,..
	%
	(descripteur_local(O,A,dates,D);valeur(O,dates,D)),
	/* (descripteur(O,A,periodicite,P)
		-> valeur(O,dates(P),D);
	           valeur_locale(O,dates,D)),  */

      %	length(V,Lg_s),length(D,Lg_s),		% V, D même longueur
	get_max(D,MaxD),get_min(D,MinD), AmplD is (MaxD-MinD),
	get_max(V,Maxv),get_min(V,Minv), Amplv is (Maxv-Minv),
	(   Amplv=\= 0 -> (Max =Maxv,Min= Minv,Ampl=Amplv) ;
	                  (Max is Maxv+1,Min is Minv-1,Ampl is 2)
	),
    % console(['    bornes OUT :', O,'   ',A,' desc:',Desc,minD,MinD]),
	!.

get_time_serie_dates(O,A,Dates):-
	(descripteur(O,A,periodicite,P)
		-> valeur(O,dates(P),Dates);    % valeur_locale par la suite ??
	           valeur_locale(O,dates,Dates)).

% ----------------------------------------------------------------------
set_plotter_parm(Plotter,Color,MinD,MaxD,AmplD,Min,Max,Ampl,Indic_Att_Desc):-
	valeur(Plotter,origine_axes,X_ori-Y_ori),
	valeur(Plotter,x_length,X_lg),
	valeur_locale(Plotter,id,Plot_id),
	send(Plot_id,clear),
	send(Plot_id,axis,
	     new(Plot_axis_x,plot_axis(x,MinD,MaxD,MaxD,X_lg,point(X_ori,Y_ori)))),
	send(Plot_axis_x, format,''),
	set_val(Plotter,x_axis_id,Plot_axis_x),
	set_val(Plotter,axe_x,[MinD,MaxD,AmplD]),

	% libelle
	concat_liste_string_with_delimiter(Indic_Att_Desc,Libelle,'-'),
	send(Plot_axis_x,label,new(Lab,text(Libelle))),
	set_val(Plotter,label_id,Lab),

	% axe Y
	valeur(Plotter,y_length,Y_lg),
	send(Plot_id,axis,
	     new(Plot_axis_y,plot_axis(y,Min,Max,@default,Y_lg,point(X_ori,Y_ori)))),
	set_val(Plotter,y_axis_id,Plot_axis_y),
	set_val(Plotter,axe_y,[Min,Max,Ampl]),
	% les décorations spéciales du graphique (zone_h + libelle)
	% zone_horizontale(Indic_Att_Desc,Plot_id,Plot_axis_y),
	enleve_decoration(Plot_id,plot_ruler_zone),
	positionne_zones_rulers(Indic_Att_Desc,Plot_axis_y),

	% mettre les dates avec des plot_ruler + send du texte de la date
	enleve_decoration(Plot_id,plot_ruler_jab),	% plot_ruler ->background date = navajo_white (plus rapide)
	positionne_dates_rulers(annuel,MinD,MaxD,Plot_axis_x),

	set_val(Plotter,objets,[Color-Indic_Att_Desc]),

	send(Plot_id,compute),   % 26/03/12
	% console([set_plotter_parm,'Fin']),
	!.



% ---------------------------------------------------------------
% enleve les décorations (plot_ruler, plot_ruler_zone)
enleve_decoration(Plotter_id,Deco_type):-  % Deco_type:{plot_ruler_jab,plot_ruler_zone}
	send(Plotter_id?graphicals,
	     for_all,if(message(@arg1,instance_of,Deco_type),message(@arg1,free))).


% ---------------------------------------------------------------
positionne_dates_rulers(annuel,MinD,MaxD,Plot_axis_x):-
	nb_jour(_,_,A_max,MaxD),
	nb_jour(_,_,A_min,MinD), Annee_start is A_min+1,Annee_fin is A_max,
	forall(tic(Annee_start,Annee_fin,1,Annee_c),
	       positionne_dates_rulers(annuel,Annee_c,Plot_axis_x)),
	% la date complète de la dernière valeur
	Ts_max is MaxD*86400,
	format_time(atom(Date_max),'%e/%m/%y',Ts_max),
	new(D_ruler_max,plot_ruler_jab),     % plot_ruler ->background date = navajo_white (plus rapide)
	send(D_ruler_max,attach,Plot_axis_x,MaxD),
	get(D_ruler_max,member,text,Text_max),
	send(D_ruler_max,compute),
	send(Text_max,value,Date_max), % à laisser apres compute....
	% console([' positionne_dates_rulers FIN :',Date_max,
	%	 ' D_ruler_max :',D_ruler_max,' Text_max :',Text_max]),
	!.

positionne_dates_rulers(annuel,Annee_c,Plot_axis_x):-
	nb_jour(1,1,Annee_c,Nbj),
	Time_stamp is Nbj*86400,
	format_time(atom(Date),'%y',Time_stamp),
	new(D_ruler,plot_ruler_jab),             % plot_ruler ->background date = navajo_white (plus rapide)
	send(D_ruler,attach,Plot_axis_x,Nbj),
	send(D_ruler,compute),
	get(D_ruler,member,text,Text),
	send(Text,value,Date),
	!.

% ---------------------------------------------------
%
positionne_zones_rulers([Obj,historique_signal|_],Axe_id):-
	valeur_locale(Obj,groupe,Groupe),
	valeur_locale(Groupe,zone_horizontale_plotter,Zones),
	forall(member(Ht_Bas_Colour_Txt,Zones),
	       positionne_zone_ruler(Axe_id,Ht_Bas_Colour_Txt)
	       ),
	send(Axe_id,compute),
	% console([' Positionne_zones_rulers'.Obj,' FIN']),
	!.
positionne_zones_rulers([_Obj,_Att|_T],_Axe_id):-
	% pour H_zone ou v_zone sur d'autres Att
	% console([' positionne_zones_rulers TRUE in fine ',Obj,Att,' FIN']),
	!.
% ---------------------------------------------------
%
positionne_zone_ruler(Axe_id,[Haut,Bas,Colour,Txt]):-
	new(D_ruler,plot_ruler_zone),
	send(D_ruler,attach,Axe_id,Haut,Bas,Colour,Txt),
	send(D_ruler,compute),
	% get(D_ruler,member,text,Text_id),
	% send(Text_id,value,Txt),
	% console([positionne_zone_ruler,[Haut,Bas,Colour,Txt]]),
	!.


% -------------------------------------------------------------------
% modifie l'echelle de l'axe selon l'amplitude de la serie à dessiner
% + redessine les series déja tracées
modif_add_plotter_parm(Plotter,Color,_MinD,_MaxD,_AmplD,Min,Max,_Ampl,Indic_Att_Desc):-
	valeur(Plotter,origine_axes,X_ori-Y_ori),
	valeur(Plotter,id,Plot_id),
	valeur(Plotter,objets,L_OAD),
	valeur(Plotter,y_length,Y_lg),

	ajuste_axe_y(Plotter,Min,Max,N_Min,N_Max),
	%  ?? faut-il un nouvel "plt_axis" ou juste changer les valeurs de l'existant ?
	send(Plot_id,axis,new(Plot_axis_y,
			      plot_axis(y,N_Min,N_Max,@default,Y_lg,point(X_ori,Y_ori)))),
	set_val(Plotter,y_axis_id,Plot_axis_y),
	% redessiner les anciens graphiques
	send(Plot_id,clear),
	forall(member(Objet,L_OAD),redraw_indic(Plotter,Objet)),
	append(L_OAD,[Color-Indic_Att_Desc],N_L_OAD),
	set_val(Plotter,objets,N_L_OAD),
	!.

modif_compl_plotter_parm(Plotter,Color,_MinD,_MaxD,_AmplD,Min,Max,_Ampl,Indic_Att_Desc):-
	valeur(Plotter,origine_axes,X_ori-Y_ori),
	valeur(Plotter,id,Plot_id),
	valeur(Plotter,objets,L_OAD),
	valeur(Plotter,y_length,Y_lg),

	send(Plot_id,axis,new(Plot_axis_y,
			      plot_axis(y,Min,Max,@default,Y_lg,point(X_ori,Y_ori)))),
	set_val(Plotter,y_axis_id,Plot_axis_y),

	append(L_OAD,[Color-Indic_Att_Desc],N_L_OAD),
	set_val(Plotter,objets,N_L_OAD),
	!.

ajuste_axe_y(Plotter,Min,Max,N_Min,N_Max):-
	valeur_locale(Plotter,axe_y,[Min_y,Max_y,_Ampl_y]),
	N_Min is min(Min,Min_y) ,
	N_Max is max(Max,Max_y),
	N_Ampl is N_Max - N_Min,
	set_val(Plotter,axe_y,[N_Min,N_Max,N_Ampl]).


% ---------------------------------------------------------
%
redraw_indic(Plotter,Color-[O,A]):-
	bornes(O,A, V,_Min,_Max,_Ampl, D,_MinD,_MaxD,_AmplD),
	plot_serie(Plotter,Color,D,V),
	!.
redraw_indic(Plotter,Color-[O,A,Desc]):-
	bornes(O,A,Desc, V,_Min,_Max,_Ampl, D,_MinD,_MaxD,_AmplD),
    % length(D,LD),length(V,LV),
    % console([redraw_indic,O,A,Desc,LV,LD]),
	plot_serie(Plotter,Color,D,V).


redraw_indic(Plotter,[O,A]):-
	bornes(O,A, V,_Min,_Max,_Ampl, D,_MinD,_MaxD,_AmplD),
	plot_serie(Plotter,D,V),
	!.
redraw_indic(Plotter,[O,A,Desc]):-
	bornes(O,A,Desc, V,_Min,_Max,_Ampl, D,_MinD,_MaxD,_AmplD),
    % length(D,LD),length(V,LV),
    % console([redraw_indic,O,A,Desc,LV,LD]),
	plot_serie(Plotter,D,V),
	!.




% ------------------------------------------------------------------
plot_indic(Indic,historique_signal,Plotter):-
	Att = historique_signal,
	Color=black,
	bornes(Indic,Att, V,Min,Max,Ampl, D,MinD,MaxD,AmplD),
	% JAB 2013/11/16
	valeur_locale(Indic,groupe,Groupe),
	(   valeur(Groupe,bornes_y_plotter,[min= Min_yp,max = Max_yp])
	     -> ( Min_y is Min_yp, Max_y is Max_yp, Ampl_y is abs(Max_y-Min_y) );
	        ( Min_y is Min, Max_y is Max, Ampl_y is Ampl )),

	set_plotter_parm(Plotter,Color,MinD,MaxD,AmplD,Min_y,Max_y,Ampl_y,[Indic,Att]),
	plot_serie(Plotter,Color,D,V),
	!.


plot_indic(Indic,Att,Plotter):-
	plot_indic(Indic,Att,Plotter,color:black).
plot_indic(Indic,Att,Plotter,color:Color):-
	bornes(Indic,Att, V,Min,Max,Ampl, D,MinD,MaxD,AmplD),
	set_plotter_parm(Plotter,Color,MinD,MaxD,AmplD,Min,Max,Ampl,[Indic,Att]),
	plot_serie(Plotter,Color,D,V).
% -----
%
plot_indic(Indic,Att,Desc,Plotter):-
	plot_indic(Indic,Att,Desc,Plotter,color:black).
plot_indic(Indic,Att,Descr,Plotter,color:Color):-
	bornes(Indic,Att, Descr, V,Min,Max,Ampl, D,MinD,MaxD,AmplD),
	set_plotter_parm(Plotter,Color,MinD,MaxD,AmplD,Min,Max,Ampl,[Indic,Att,Descr]),
	plot_serie(Plotter,Color,D,V).


% ==============================================================
%
plot_add(Indic,Att,Plotter):-
	plot_add(Indic,Att,Plotter,color:black).

plot_add(Indic,Att,Plotter,color:Color):-
	valeur_locale(Plotter,axe_x,[From,_To,_Ampl]),
	bornes(Indic,Att, V,Min,Max,Ampl, D,MinD,MaxD,AmplD),
	modif_add_plotter_parm(Plotter,Color,MinD,MaxD,AmplD,Min,Max,Ampl,[Indic,Att]),
	dates_down_to(From,D,Dates),
	plot_serie(Plotter,Color,Dates,V),
	!.

plot_add(Indic,Att,Descr,Plotter):-
	plot_add(Indic,Att,Descr,Plotter,color:black).
plot_add(Indic,Att,Descr,Plotter,color:Color):-
     % console([plot_add,5,Indic,Att,Descr,Plotter,color:Color]),
	valeur_locale(Plotter,axe_x,[From,_To,_Ampl]),
	bornes(Indic,Att, Descr, V,Min,Max,Ampl, D,MinD,MaxD,AmplD),
	modif_add_plotter_parm(Plotter,Color,MinD,MaxD,AmplD,Min,Max,Ampl,[Indic,Att,Descr]),
	dates_down_to(From,D,Dates),
	plot_serie(Plotter,Color,Dates,V),
	!.

% -----------------------------
plot_complete(Indic,Att,Plotter):-
	Color=red3,
	valeur_locale(Plotter,axe_x,[From,_To,_Ampl]),
	bornes(Indic,Att, V,Min,Max,Ampl, D,MinD,MaxD,AmplD),
	modif_compl_plotter_parm(Plotter,Color,MinD,MaxD,AmplD,Min,Max,Ampl,[Indic,Att]),
	dates_down_to(From,D,Dates),
	plot_serie(Plotter,Color,Dates,V),
	!.

plot_complete(Indic,Att,Descr,Plotter):-
	Color=red,
	valeur_locale(Plotter,axe_x,[From,_To,_Ampl]),
	bornes(Indic,Att, Descr, V,Min,Max,Ampl, D,MinD,MaxD,AmplD),
	modif_compl_plotter_parm(Plotter,Color,MinD,MaxD,AmplD,Min,Max,Ampl,[Indic,Att,Descr]),
	dates_down_to(From,D,Dates),
	plot_serie(Plotter,Color,Dates,V),
	!.
% ------------------------------------------------------
% l'affichage de la serie de Valeurs

plot_serie(Plotter,Dates,Valeurs):-
	% in_pce_thread( Goal_ci-dessous),
	pce_plot_serie(Plotter,black,Dates,Valeurs).

plot_serie(Plotter,Couleur,Dates,Valeurs):-
	% in_pce_thread( ... ),
	pce_plot_serie(Plotter,Couleur,Dates,Valeurs).

pce_plot_serie(Plotter,Couleur,Dates,Valeurs):-
	valeur(Plotter,id,Plot_id),
	send(Plot_id, graph, new(Graph, plot_graph)),
	send(Graph,colour,Couleur),
	set_val(Plotter,graph,Graph),
	plot_list_val(Dates,Valeurs,Graph),
	send(Plot_id,compute),
	!.


% ??????
plot_graphic(Plotter_id,_X_dates,_Y_valeurs,Couleur):-
	send(Plotter_id, graph, new(Graph, plot_graph)),
	send(Graph,colour,Couleur),
	!.

% -----------------------------------------------------
% dates_down_to : extrait dates(To,[Last...To...First],[Last...To]
dates_down_to(_,[],[]).
dates_down_to(To,[Date|_D_t],[]):-
	To > Date,
	!.
dates_down_to(To,[Date|D_t],[Date|D_suivantes]):-
	dates_down_to(To,D_t,D_suivantes).


% ----------------------------------------------------
% l'affichage de la serie de [Date|..] [Valeur|..s] sur l'objet pce : Plot_graph
% s'interrompt dès qu'une des 2 séries n'a plus de données
plot_list_val([],_,_):-!.
plot_list_val(_,[],_):-!.
plot_list_val([X|XT],[Y|YT],Plot_Graph):-
	send(Plot_Graph,append,X,Y),
	plot_list_val(XT,YT,Plot_Graph).

% ----------------------------------------------------
ruler_label(Id,Label):-
	get(Id, member, text, Text),
	send(Text, value, Label ).

% ---------------------------------------------------
% generate_x_vector(Y_vector, X_vecteur)
% genere un X_vecteur de dates pour dessiner Y_vector
% pour les séries qui n'ont pas de vecteur de Date (x) il faut le générer
% pour plotter Y_vecteur
generate_x_vector(Y_vector, X_vector):-
	length(Y_vector,Length),
	Start_value is Length -1,
	autofill_vector(Y_vector,Start_value,-1, X_vector).

% autofill_vector( Y_Vecteur,Start_value,Increment,Vecteur)
autofill_vector([],_,_,[]):-!.
autofill_vector([_Head|Tail],Value,Inc,[Value|Suite]):-
	N_V is Value+Inc,
	autofill_vector(Tail,N_V,Inc,Suite).



% ---------------------------------------------------
% sauve un graphique de type : picture sur un fichier au format : win_metafile
sauver_graphique(Gr,File):-
	valeur(Gr,id,Gr_id),
	new(MF,win_metafile),
	send(MF,draw_in,Gr_id?graphicals),
	send(MF,save,File).


% ----------------------------------------------------
% indique la valeur sur le clic dans plotter  / temporaire pour developpement
display_gesture(_Bouton,Receveur,Event):-
	get(Event,x,X),get(Event,y,Y),
	get(Receveur,value_from_x(X),VX), I_VX is round(VX),
	nb_jour(D,I_VX), get(Receveur,value_from_y(Y),VY),
	get(Event,id,_Id),
	% console(['[Evt:',Id,' on:',Receveur,']	X/Y:',X,'/',Y,no_nl]),
	console(['Valeurs  X=',I_VX,D,'	Y=',VY]),

	!.

% liste de quelques couleurs utilisable en attendant un interface pour selectionner
% azure , bisque, blue, cadetblue , coral , cyan , dimgrey, gold, grey, green, red,

%  --------------------------- FIN

