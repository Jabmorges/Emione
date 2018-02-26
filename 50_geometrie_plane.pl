% file	:  50_geometrie_plane.pl
% AUTHOR :
% VERSION : 0.93 for SWI-PL
% UPDATE : 10/11/2012
% PURPOSE: calcule des distances dans un triangle  un peu de geometrie plane


% Distance (euclidian) between 2 points from X1-Y1 to X2-Y2
distance(X1-Y1,X2-Y2,Distance):-
	Distance is sqrt( (X1-X2)^2 + (Y1-Y2)^2).


% Vertical Distance from a point C(x3,y3) to line A(x1,y1) to B(x2,y2)
%
v_distance(X1-Y1,X2-Y2,X3-Y3,Distance):-
	ax_plus_b(X1-Y1,X2-Y2,AX,B),
	Distance is (Y3-(AX*X3+B)).


% Perpendicular distance from C(x3,y3) to AB (height of triangle)
p_distance(X1-Y1,X2-Y2,X3-Y3,Distance):-
	ax_plus_b(X1-Y1,X2-Y2,Ax,B),
	Distance is -(Ax*X3-Y3+B)/ sqrt(Ax^2 + 1).


% both vertical and perpendicular distance
vp_distances(X1-Y1,X2-Y2,X3-Y3,V_Dist,P_Dist):-
	ax_plus_b(X1-Y1,X2-Y2,Ax,B),
	V_Dist is (Y3-(Ax*X3+B)),
	P_Dist is -(Ax*X3-Y3+B)/ sqrt(Ax^2 + 1).

% --------------------------------------------------------------
% On Lists : Distance (eucl)  to a fixed point A(X1,Y1)
%
dist(_-_,[],[]):-!.
dist(X1-Y1,[X2-Y2|T_xy],[Distance|T_dist]):-
	Distance is sqrt( (X1-X2)^2 + (Y1-Y2)^2),
	dist(X1-Y1,T_xy,T_dist).


% vertical distance for a list of points(L_x3y3) to line AB
v_dist(X1-Y1,X2-Y2,L_x3y3,L_Dist):-
	ax_plus_b(X1-Y1,X2-Y2,Ax,B),
	vdist_l(L_x3y3,L_Dist,Ax,B).

vdist_l([],[],_,_):-!.
vdist_l([X3-Y3|T_xy],[Dist-X3|T_Dist],Ax,B):-
	Dist is (Y3-(Ax*X3+B)),
	vdist_l(T_xy,T_Dist,Ax,B).



% perpendicular distance sur une liste de  X3-Y3
%
p_dist(X1-Y1,X2-Y2,L_DS,L_Dist):-
	ax_plus_b(X1-Y1,X2-Y2,Ax,B),
	pdist_l(L_DS,L_Dist,Ax,B).

pdist_l([],[],_,_):-!.
pdist_l([X3-Y3|T_xy],[Dist-X3-Y3|T_Dist],Ax,B):-
	Dist is -(Ax*X3-Y3+B)/ sqrt(Ax^2 + 1),
	pdist_l(T_xy,T_Dist,Ax,B).


% vp_distance sur une liste de  X3-Y3 to AB
%
vp_dist(X1-Y1,X2-Y2,L_x3y3,L_Dist):-
	ax_plus_b(X1-Y1,X2-Y2,Ax,B),
	vpdist_l(L_x3y3,L_Dist,Ax,B).

vpdist_l([],[],_,_):-!.
vpdist_l([X3-Y3|T_xy],[V_Dist-P_Dist-X3-Y3|T_Dist],Ax,B):-
	V_Dist is (Y3-(Ax*X3+B)),
	P_Dist is -(Ax*X3-Y3+B)/ sqrt(Ax^2 + 1),
	vpdist_l(T_xy,T_Dist,Ax,B).



% -------------------------------------------------------------------
% coef directeur de la droite A(x1,y1) B(x2,y2)
ax_plus_b(X1-Y1,X2-Y2,Ax,B):-
	Ax is (Y1-Y2)/(X1-X2),
	B is Y1-(Ax*X1) .

% coef directeur de la perpendiculaire à la droite A(x1,y1) B(x2,y2)
%
ax_perp(X1-Y1,X2-Y2,Ax_perp):-
	Ax is (Y1-Y2)/(X1-X2),
	Ax_perp is (-1/Ax).

% Ax+B de la perpendiculaire a A(x1,y1) B(x2,y2) passant par X3-Y3
%
ax_plus_b_perp(X1-Y1,X2-Y2,X3-Y3,Ax_perp,B_perp):-
	Ax is (Y1-Y2)/(X1-X2),
	Ax_perp is (-1/Ax),
	B_perp is Y3-(Ax_perp*X3).


% à compléter :
% Calculer le sin de l'angle C,A,B ==> pente vs droite AB
% calculer le sin  de C,A,horizontale ==> pente en absolu

%  FIN
