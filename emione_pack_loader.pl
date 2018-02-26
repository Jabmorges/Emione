% file	: emione_pak_loader		charge emione et ses satelittes
% date	: 2016 et +

:- ensure_loaded('00_ex_util.pl').
:- ensure_loaded('02_ex_math.pl').
:- ensure_loaded('03_ex_math_et_date').
:- ensure_loaded('05_format_et_plus').
:- ensure_loaded('10_Emione_utilities.pl').
:- ensure_loaded('17_ex_langage.pl').

% rule editeur peut poser pb ?
:- ensure_loaded('30_rule_editor_symph.pl').
:- ensure_loaded('40_divers_pce.pl').
:- ensure_loaded('41_emione_pce_objects.pl').
:- ensure_loaded('42_hierarchy.pl').
:- ensure_loaded('45_xpce_plot.pl').
% :- ensure_loaded('51_pipts.pl').
:- ensure_loaded('60_database_tools.pl').
:- ensure_loaded(emione).

:- console(['Emione_pack loaded']).
