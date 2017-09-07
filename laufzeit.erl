-module(laufzeit).

-author("Uwe Krause").

-compile(export_all).


% Dummy-Funktionen sorgen fuer den richtigen Funktionsaufruf

% ksort:qsort(<pivot-methode>,<Liste>,<switch-number>) 

insert(Liste) -> ssort:insertionS(Liste).

select(Liste) -> ssort:selectionS(Liste).

qleft(Liste) -> ksort:qsort(left, Liste, 12).
qmiddle(Liste) -> ksort:qsort(middle, Liste, 12).
qright(Liste) -> ksort:qsort(right, Liste, 12).
qmedian(Liste) -> ksort:qsort(median, Liste, 12).
qrandom(Liste) -> ksort:qsort(random, Liste, 12).

msort(Liste) -> ksort:msort(Liste).


% Ruft jede moegliche Kombination der Sortieralgorithmen auf.
% Speichert LOG in Datei. (ggf. Datei vorher manuell leeren!)
log_to_file() ->

	%%%%%%%%%%%%%%%%%
	% Einstellungen %
	%%%%%%%%%%%%%%%%%
	
	% Dateiname
	Dateiname = 'results.csv',
		
	% definiert die zu testenden Algorithmen (entsprechende Dummy-Funktion muss vorhanden sein!)
	A = [insert, select, qleft, qmiddle, qright, qmedian, qrandom, msort],
	
	% definiert die zu testenden Cases
	M = [sort, rev, rand, duplicate],

	% 10 verschiedene Listen mit 1 bis 4500 Elementen, im Abstand von jeweils 500
	NumberOfElements = [1] ++ lists:seq(500, 4500, 500),
	
	RandomList_from = 1,
	RandomList_to = 4500,
	
	
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Programmablauf
	%
        % Ab hier keine Einstellungen mehr
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	% Hier werden einmalig die zu sortierenden Listen aufgebaut, welche anschliessend an die jeweiligen Sortieralgorithmen gegeben werden
	D0 = dict:new(),
	D1 = dict:store(sort, lists:map(fun(Y) -> util:sortliste(Y) end, NumberOfElements), D0),
	D2 = dict:store(rev, lists:map(fun(Y) -> util:resortliste(Y) end, NumberOfElements), D1),
	D3 = dict:store(rand, lists:map(fun(Y) -> util:randomliste(Y) end, NumberOfElements), D2),
	D4 = dict:store(duplicate, lists:map(fun(Y) -> util:randomlisteD(Y, RandomList_from, RandomList_to) end, NumberOfElements), D3),
	
	
	% Schreibt in die erste Zeile die Ueberschriften
	util:logging(Dateiname, io_lib:format("Algorithmus-Modus",[])),
	
	lists:foreach(fun(Number) ->
			util:logging(Dateiname, io_lib:format(",~w",[Number]))
	end, NumberOfElements),
	
	util:logging(Dateiname, io_lib:format("~n",[])),

	
	lists:foreach(fun(Algo) ->
	
		lists:foreach(fun(Modus) ->
		
			% Benennt die aktuelle Zeile
			util:logging(Dateiname, io_lib:format("~w-~w",[Algo, Modus])),
			
			% Holt die Liste der zu sortierenden Listen aus dem Dictionary
			{_, Moduslisten} = dict:find(Modus, D4),
			
			% ruft jede moegliche Kombination der Algorithmen und der zu sortierenden Listen auf
			lists:foreach(fun(S) ->
				{InsTime, _ResList} = timer:tc(laufzeit, Algo, [S]), util:logging(Dateiname, io_lib:format(",~w",[InsTime+1]))
			end, Moduslisten),
			
			% Zeilenumbruch
			util:logging(Dateiname, io_lib:format("~n",[]))		
		end, M)
	end, A).
