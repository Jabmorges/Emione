% file : free_data_loader.pl
% but : charger des données depuis url

/* TAF :
1) lecture de l'URL et stockage dans un fichier tmp
1b) lecture de ce fichier tmp par csv_read_file
2) stockage du résultat soit sous forme :
	fx_spot([liste de colonnes],[DEV1-DEV2-[data pour chq colonne]|[]])
	ou serie de faits fx_spot(DEV1,DEV2,Col_att,Valeur)
	ou emione : oav(DEV1_DEV2,fx_spot,Col,Valeur)
3) mettre en place un pricipe de timer qui met à jour régulièrement les données

*/
:- ensure_loaded('csv_JW_JAB.pl').
:- ensure_loaded('vni_utilities.pl').


% les librairies pour le coté CLIENT http (en principe chargé par http_core.pl)
:- use_module(library(http/http_open)).


% les url des sites chargeables
% sp 500 via YAHOO finance (S&P500 depuis yahoo finance
%  url('http://ichart.finance.yahoo.com/table.csv?s=%5EGSPC&a=1&b=1&c=2013&d=06&e=4&f=2057&g=d&ignore=.csv').

% cpi usa  via Fed REserve Dept
% url('http://research.stlouisfed.org/fred2/data/CPIAUCSL.csv').

% CPI en Xml via Fred
url('http://api.stlouisfed.org/fred/series?series_id=CPIAUCSL&api_key=abc4579ea550e0fd0928ea42cca399aa').

%
% Quandl.com est certainement un bon accès aux données
%  Mais il serait peut être préférable d'accèder direcement aux sources elles-mêmes

%
% ci-dessous : posent pb...
url('http://fr.investing.com/economic-calendar/').

% ci-dessous : posent pb...
url('http://ec.fr.forexprostools.com/?columns=exc_currency,exc_importance,exc_actual&category=_employment,_economicActivity&countries=17,25,6,37,26&calType=week&timeZone=58&lang=5').
url('http://ec.fr.forexprostools.com/?columns=exc_currency,exc_importance,exc_actual,exc_forecast,exc_previous&category=_employment,_economicActivity,_inflation,_credit,_centralBanks,_confidenceIndex,_balance,_Bonds&importance=1,2,3&features=datepicker&countries=17,25,6,37,26,5,22,14,10,35,4,12,72&calType=day&timeZone=58&lang=5').


% -------------------

% clauses pour le chargement de fichiers csv via url
http_load_html(URL, DOM) :-
        setup_call_cleanup(http_open(URL, In,[ timeout(10) ]),
                           (   dtd(html, DTD),
                               load_structure(stream(In),
                                              DOM,
                                              [ dtd(DTD),
                                                dialect(sgml),
                                                shorttag(false),
                                                max_errors(-1),
                                                syntax_errors(quiet)
                                              ])
                           ),
                           close(In)).


scrape_no_error(URL,DOM) :-
        catch(http_load_html(URL, DOM), Error,
              (	  print_message(warning, Error),
                  fail
              )), !,
        nl.
scrape_no_error(_,_).

% ------------------------------------------------
open_url(URL, In,File) :-
        tmp_file_stream(text, File, Stream),
        close(Stream),   % JAB
	http_open(URL, Stream,[ timeout(10) ]),
        open(File, read, In),
        % delete_file(File),   % unix-only
	nl.

% lecture de fichier csv sur une url
% ------   version JAB : 2014_01_11
read_csv(URL, In,File) :-
        tmp_file_stream(text, File, Stream),
	http_open(URL, Stream_U,[ timeout(10) ]),
	copy_stream_data(Stream_U,Stream),
	csv_read_file(File,Rows,[functor(ta)]),
	% parametrer correctement le csv_read_file
	maplist(assert,Rows),
        % delete_file(File),
	nl.


% appel de page et décomposition pour le momement (3/5/11)

test:-url(U),scrape_no_error(U,DOM),nl.

% csv_read_file(File, Rows).

% exemples -------------------------------------------------------
%

rss_fetch(URL, XML) :-
  http_open(URL, XmlStream, []),
  load_xml_file(XmlStream, XML),
  close(XmlStream).
