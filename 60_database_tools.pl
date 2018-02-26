% file	: database_tools.pl
% AUTHOR :
% VERSION : 0.93 for SWI-PL
% UPDATE : 12/07/2016
% PURPOSE: liens ODBC
%

:- ensure_loaded(emione).		% passer en use_module( emione)
:- ensure_loaded('00_ex_util').	% passer en use_module(
:- ensure_loaded('02_ex_math').	% passer en use_module(
:- ensure_loaded('03_ex_math_et_date').	% passer en use_module(

:- set_prolog_flag(float_format,'%.3f').

%-----------------------------------------------------
% Liste des bases accessibles
% !! à compléter car en cas d'échec une base reste ouverte ==> pb
data_sources(Liste):-
	findall(Source-Desc,(odbc_data_source(Source,Desc)),Liste).

data_source(Source,Desc,Tables):-
	odbc_data_source(Source,Desc),
	% si ERROR ms ==> fail
	catch(odbc_connect(Source,Connection,[]) ,error(odbc(_State,_Native,_Message), _),fail),
	liste_des_tables(Connection,Tables),
	odbc_disconnect(Connection),
	!.


% ----------------------------
% connection to database
connecte(Base):- odbc_connect(Base,_,[alias(db)]).


% ----------------------------
liste_des_tables(Base,Liste):-
	bagof(Table,A^Base^(odbc_current_table(Base,Table,type(A)),not(A='SYSTEM TABLE')),Liste).

tables_de(Base,Table):-odbc_current_table(Base,Table,type(A)),not(A='SYSTEM TABLE').

% ----------------------------
liste_des_attributs(Base,Table,L_Atts):-
	bagof(Att,Att^Base^(odbc_table_column(Base,Table,Att)),L_Atts).

attribut_de(Base,Table,Att):- odbc_table_column(Base,Table,Att).



% -------  FIN --------
