% Authors: Uwe Krause, Sean Pedersen
-module(liste).

-export([create/0, isEmpty/1, isList/1, equal/2, laenge/1, insert/3, delete/2, find/2, retrieve/2, concat/2, diffListe/2, eoCount/1]).

% zusatzlich zur geforderten
-export([equalSemantic/2]).

%create: ∅ → list / create()
% Die Liste wird intern mit dem Erlang-Tupel realisiert
create() -> {}.



%isEmpty: list → bool / isEmpty(<Liste>)
% Eine Leere Liste ist ein leeres Erlang-Tupel
isEmpty({}) -> true;
isEmpty(_) -> false.



%isList: list → bool / isList(<Liste>)

% Das Ende der Liste wird durch die leere Liste symbolisiert.
isList({}) -> true;

% Eine Liste besteht aus einer Andeinanderreihung von Elementen und dem Rest der Liste
isList({_E, R}) -> isList(R);

% Weder leere Liste, noch ein Objekt, welches mit einer leeren Liste endet
isList(_) -> false.




% Semantische Gleichheit - nicht gefordert
% Prueft auf sematantische Gleichheit, wie in der Vorlesung besprochen.
% Also ob Listen gleich viele Elemenete enthalten, falls Element auch Liste pruefe rekursiv weiter
equalSemantic({H1, T1}, {H2, T2}) -> (laenge({H1, T1}) == laenge({H2, T2})) and
                             case (isList(H1) and isList(H2)) of
                                true -> equalSemantic(H1, H2), equalSemantic(T1, T2);
                                false -> case (not(isList(H1)) and not(isList(H2))) of
                                            true -> equalSemantic(T1, T2);
                                            false -> false
                                         end
                             end;
equalSemantic({}, {}) -> true;
equalSemantic(_,_) -> false.



%equal: list × list → bool / equal(<Liste>,<Liste>)
% Werte / Strukturelle Gleichheit:
% Wenn aktuelles Element gleich ist, wird der Rest der Liste verglichen
equal({E, T1}, {E, T2}) -> equal(T1, T2);

% Ende der Listen erreicht
% Zwei leere Listen sind gleich
equal({}, {}) -> true;

% Fall tritt nur ein, wenn keiner der oberen gegriffen hat
equal(_, _) -> false.



%laenge: list → int / laenge(<Liste>)
laenge({}) -> 0;
laenge({_E, R}) -> 1 + laenge(R).



%insert:  list × pos × elem → list / insert(<Liste>,<Position>,<Element>)
% Wenn die einzufuegende Position gueltig (Innerhalb oder ein Element hinter dem aktuellen Ende) ist,
% rufe Hilfsfunktion auf, damit die aktuelle Position gezaehlt werden kann (Initial 1)
insert(L, P, E) ->
%	case (P > 0) and (P =< laenge(L) + 1 ) of
	case (P > 0) of
		true -> insert(L, P, E, 1);
		% Wenn versucht wird an einer illegalen Position einzufuegen, wird die urspruengliche Liste zurueck gegeben
		% (Fehlerbehandlung durch ignorieren)
		false -> L
	end.


% Wenn die gewuenschte Position noch nicht erreicht wurde,
% reiche Element, gewuenschte Position und den Rest der Liste an die naechste Position weiter
insert({H, T}, P, E, Accu) when Accu =/= P -> {H, insert(T, P, E, Accu + 1)};

% Wenn die aktuelle Position gleich der gewuenschten Position ist, ist die Rueckgabe eine Liste,
% die aus dem einzufuegenden Element und dem Ende der urspruenglichen Liste besteht.
insert(L, P, E, P) -> {E, L};

% Wenn das Ende der Liste erreicht wurde, ohne dass das Element eingefuegt werden konnte, wird die urspruengliche Liste zurueck gegeben
insert({}, _P, _E, _A) -> {}.



%delete: list × pos → list / delete(<Liste>,<Position>)

% Wenn die zu loeschende Position gueltig ist,
% rufe Hilfsfunktion auf, damit die aktuelle Position gezaehlt werden kann (Initial 1)
% Wenn versucht wird, ein Element an einer nicht existierenden Position zu entfernen, wird die urspruengliche Liste zurueck gegeben
delete(L, P) ->
	% case (P > 0) and (P =< laenge(L) ) of
	case (P > 0) of
		true -> delete(L, P, 1);
		false -> L
	end.

%Ende der Liste erreicht, Element nicht gefunden
% Wenn das Ende der Liste erreicht wurde, ohne dass das Element entfernt werden konnte, wird die urspruengliche Liste zurueck gegeben
delete({}, _P, _A) -> {};

% Wenn die gewuenschte Position noch nicht erreicht wurde,
% gehe die Liste weiter durch.
delete({H, T}, P, Accu) when Accu =/= P -> {H, delete(T, P, Accu + 1)};

% Position, an der eingefuegt werden soll erreicht, gib Rest der Liste an aufrufende Funktion zurueck,
% auf diese Art wird die Liste ohne das entfernte Element wieder neu aufgebaut.
delete({_H, T}, P, P) -> T.



%find: list × elem → pos / find(<Liste>,<Element>)
% Liefert bei nicht gefundenem Element 0!

% rufe Hilfsfunktion auf, damit die aktuelle Position gezaehlt werden kann (Initial 1)
find(L, E) -> find(L, E, 1).


% Wenn das aktuelle Element gleich dem gesuchten Element ist, gib aktuelle Position zurueck
find({H, _T}, H, Accu) -> Accu;
% ansonsten suche im Rest der Liste weiter
find({_H, T}, E, Accu) -> find(T, E, Accu + 1);

% Leere Liste oder Ende der Liste erreicht, Element bisher nicht gefunden.
% Rueckgabe 0 oder Fall nicht abfangen? (Let it crash)
find({}, _E, _Accu) -> 0.



%retrieve: list × pos → elem / retrieve(<Liste>,<Position>)
% rufe Hilfsfunktion auf, damit die aktuelle Position gezaehlt werden kann (Initial 1)
retrieve(L, P) ->
	case (P > 0) and (P =< laenge(L) ) of
		true -> retrieve(L, P, 1)
	end.

% Wenn die aktuelle position gleich der gesuchten ist, gib aktuelles Element zurueck
retrieve({H, _T}, P, P) -> H;
% ansonsten suche im Rest der Liste weiter
retrieve({_H, T}, P, Accu) -> retrieve(T, P, Accu + 1).



%concat: list × list → list / concat(<Liste>,<Liste>)
% zerlege erste Liste
concat({H, T}, L2) -> {H, concat(T, L2)};
% wenn Ende erreicht, gib eine neue Liste mit angehangener zweiten Liste zurueck
concat({}, L2) -> L2.



%diffListe: list × list → list / diffListe(<Liste>,<Liste>)

% die leere Liste abzueglich irgendwas bleibt die leere Liste
diffListe({}, _L2) -> {};

% eine Liste abzueglich nichts bleibt unveraendert
diffListe(L1, {}) -> L1;

% Prüfe ob für jedes Element in der 2. Liste, ob es in 1. Liste enthalten ist.
% Wenn ja, dann lösche das gefundene Element in der 1. Liste, wenn nein mache
% mit dem nächsten Element weiter.
diffListe(L1, {H, T}) -> Pos = find(L1, H),
	if (Pos == false) -> diffListe(L1, T);
		true -> diffListe(delete(L1, Pos), T)
	end.



%eoCount: list → [int,int] / eoCount(<Liste>)

% Durchlaufe jedes Element der Liste, zähle dabei einen Counter hoch.
% Wenn das Element eine Liste ist, rufe dich selbst rekursiv auf mit der Liste und Counter = 0.

% Wenn am Ende der Liste angekommen, rechne den Counter % 2 und gebe
% das entsprechende {Even,Uneven}-Tupel zurück.

eoCount(L) -> eoReku(L, {0, 0}, 0).

eoReku({}, {Even, Uneven}, Counter) ->
	case (Counter rem 2 == 0) of
		false -> {Even, Uneven + 1};
		true -> {Even + 1, Uneven}
	end;

eoReku({H, T}, Tupel, Counter) ->
	case (isList(H)) of
		true -> eoReku(T, eoReku(H, Tupel, 0), Counter + 1);
		false -> eoReku(T, Tupel, Counter + 1)
	end.
