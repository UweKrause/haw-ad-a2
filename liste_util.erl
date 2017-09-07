-module(liste_util).

-export([put_first/2, put_last/2, put_inorder/2]).

-export([pick_first/1, pick_last/1, pick_middle/1, pick_min/1, pick_max/1, pick_median3/1, pick_random/1]).

-export([partition/2]).
%-compile(export_all).





%%%%%%%%%%%%%%%%%%%%%%
% Einfuegeoperationen,
% geben jeweils die neue Liste zurueck
%%%%%%%%%%%%%%%%%%%%%%

% Setzt das Element an die erste Position
put_first(Liste, Element) -> [Element | Liste].



% Durchlaeuft die komplette Liste bis ans Ende und fuegt das Element hier an.
put_last(_Liste=[H|T], Element) -> [H | put_last(T, Element)];
put_last([], Element) -> [Element | []].



% Ende des sortierten Bereiches erreicht, (also ist kein Element des sortierten Bereiches groesser), daher muss der richtige Platz am Ende des sortierten Bereiches sein.
put_inorder( _Liste=[], Element) -> [Element];

% Wenn das einzusortierende Element groesser als der Nachfolger ist, ist sein richtiger Platz an der Position VOR dem groesseren Element
put_inorder( Liste=[ Nachfolger | _Rest ], Element) when Nachfolger > Element  -> [ Element | Liste ];

% Wurde der richtige Platz noch nicht gefunden, wird versucht, das Element im Rest der Liste unterzubringen
put_inorder( _Liste=[ Head | Rest], Element) -> [Head | put_inorder(Rest, Element)].





%%%%%%%%%%%%%%%%%%%
% BASIS-LISTENOPERATIONEN,
% die fuer die Sortieralgorithmen gebraucht werden
%
% Alle operationen muessen auf eine liste mit mindestens ein Element aufgerufen werden,
% Rueckgabewerte fuer ungueltige Aufrufe sind nicht definiert!
%%%%%%%%%%%%%%%%%%%



% Entnimmt den ersten Wert der Liste
% (Der Vollstaendigkeit halber)
pick_first(_Liste=[H|T]) -> {H, T}.


% Entnimmt den letzten Wert der Liste
% Eine eigene Implementierung ist angebracht, da so beide Schritte (finden und entfernen) in einem Abwasch durchgefuehrt werden koennen
pick_last(_Liste=[H| T]) when T == [] -> {H, []};

pick_last(_Liste=[H | T]) -> {W, Rest} = pick_last(T), {W, [H|Rest]}.


% Das erste Vorkommen des Elementes wird entfernt, die Reihenfolge bleibt ggf nicht erhalten!
pick_middle(Liste) ->
    Middle = find_middle_pos(Liste),
    ListeOhneElement = delete(Middle, Liste),
    {Middle, ListeOhneElement}.

    
    
% Das erste Vorkommen des Elementes wird entfernt, die Reihenfolge bleibt ggf nicht erhalten!
pick_median3(Liste) ->
    FML = find_first_middle_last(Liste),
    Median = find_median_value_of3(FML),
    ListeOhneElement = delete(Median, Liste),
    {Median, ListeOhneElement}.

    
% Das erste Vorkommen des Elementes wird entfernt, die Reihenfolge bleibt ggf nicht erhalten!  
pick_random(Liste) ->
    % "Returns the length of List"
    Length = erlang:length(Liste),
    % "Returns, for a specified integer N >= 1, a random integer uniformly distributed between 1 and N [...]"
    Random = random:uniform(Length),
    % "Returns the Nth element of List."
    Element = lists:nth(Random, Liste),
    % Wenn das ausgewahelte Element mehrfach vorkommt, wird das erste Vorkommen entfernt, nicht unbdingt das N-te!
    ListeOhneElement = delete(Element, Liste),
    % Rueckgabewert
    {Element, ListeOhneElement}.
  
    
    
% Findet das erste Vorkommen des Minimalwertes innerhalb der List und die Liste ohne diesen Wert
pick_min(Liste) ->
    Min = find_min(Liste),
    ListeOhneElement = delete(Min, Liste),
    {Min, ListeOhneElement}.



% Findet das erste Vorkommen des Maximalwertes innerhalb der List und die Liste ohne diesen Wert
pick_max(Liste) ->
    Max = find_max(Liste),
    ListeOhneElement = delete(Max, Liste),
    {Max, ListeOhneElement}.
    
    
    
% Zerlegt die Liste in zwei Listen,
% eine enthaelt alle Werte, die kleiner oder gleich sind,
% die andere enthaelt alle Werte, die groesser als der Teiler sind
partition(Liste, Teiler) ->
	% "Partitions List into two lists, where the first list contains all elements for which Pred(Elem) returns true, and the second list contains all elements for which Pred(Elem) returns false."
	{_KleinerGleich, _Groesser} = lists:partition(fun(AktuellesElement) -> AktuellesElement =< Teiler end, Liste).
    
    
    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hilfsfunktionen,
% (werden nicht exportiert)
%%%%%%%%%%%%%%%%%%%%%%%%%%%



% "Returns the first element of List that compares less than or equal to all other elements of List."
% Eine eigene Implementierung bringt keinen Vorteil
find_min(Liste) -> lists:min(Liste).



% "Returns the first element of List that compares greater than or equal to all other elements of List."
% Eine eigene Implementierung bringt keinen Vorteil
find_max(Liste) -> lists:max(Liste).


% Gibt das Element zurueck, da sich an der mittleren Position befindet
find_middle_pos(Liste) -> 
    Length = erlang:length(Liste),
    Middle = Length / 2, 
    Round = erlang:round(Middle),
    _Element = lists:nth(Round, Liste).
    

% Gibt eine Liste zurueck, die aus dem ersten, dem mittleren und dem letzten Wert besteht.
find_first_middle_last(Liste) ->
    {Erster, _} = pick_first(Liste),
    {Letzter, _} = pick_last(Liste),
    Mitte = find_middle_pos(Liste),
    _FML = [Erster, Mitte, Letzter].
    

% Gibt den Median3-Wert zuruck, wie in der Vorlesung festgelegt
find_median_value_of3(Liste3) ->
    {_Min, Liste3OhneMin} = pick_min(Liste3),
    {_max, ListeOhneMinOhneMax} = pick_max(Liste3OhneMin),
    {Medianvalue, _} = pick_first(ListeOhneMinOhneMax),
    Medianvalue.



% "Returns a copy of List1 where the first element matching Elem is deleted, if there is such an element."
% Eine eigene Implementierung bringt keinen Vorteil
% Das erste Vorkommen des Elementes wird entfernt, immer zu beachten!
delete(Element, Liste) -> lists:delete(Element, Liste).
