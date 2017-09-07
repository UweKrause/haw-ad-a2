-module(ksort).

-author("Uwe Krause").

-export([qsort/3, msort/1]).





% ksort:qsort(<pivot-methode>,<Liste>,<switch-number>) 
% Die Liste wird mit dem Quicksortverfaren sortiert.
% Dabei kann <pivot-methode> die Werte left / middle / right / median / random annehmen.

% Abruchbedingungen, Listen mit weniger als 2 Elementen sind bereits sortiert
qsort(_Methode, _Liste=[], _Switch) -> [];

qsort(_Methode, _Liste=[Element | []], _Switch) -> [Element];

% Die Liste wird mit dem Quicksortverfahren sortiert
qsort(Methode, Liste, Switch) -> 
	% Wenn die Laenge der Liste kleiner ist als der gewaehlte Switch, wird Insertionsort gewaehlt.
    case (length(Liste) < Switch )of
        % Gerade fuer kleinere Problemgroessen ist InsertionSort besonders effizient.
        true -> ssort:insertionS(Liste);
        
        % Ab einer bestimmten Problemgroesse ist Quicksort schneller 
        false -> 
            % Das Pivotelement wird nach der gewuenschten Methode ermittelt und entfernt
            case Methode of
                left -> {Pivot, ListeOhnePivot} = liste_util:pick_first(Liste);
                middle -> {Pivot, ListeOhnePivot} = liste_util:pick_middle(Liste);
                right -> {Pivot, ListeOhnePivot} = liste_util:pick_last(Liste);
                median -> {Pivot, ListeOhnePivot} = liste_util:pick_median3(Liste);
                random -> {Pivot, ListeOhnePivot} = liste_util:pick_random(Liste)
            end,
            
            % Zerlegt die Liste mit dem bereits entnommenen Pivotelement in zwei Listen
            {KleinerGleich, Groesser} = liste_util:partition(ListeOhnePivot, Pivot),
            
            % Die aufgeteilten Listen werden rekursiv weiter bearbeitet und die Ergebnisse zusammengefuegt
            qsort(Methode, KleinerGleich, Switch) ++ [Pivot] ++ qsort(Methode, Groesser, Switch)
    end.
    
    
    
% Leere Liste ist bereits sortiert    
msort(_Liste=[]) -> [];

% Einelementige Liste ist bereits sortiert.
msort(_Liste=[Element | []]) -> [Element];

msort(Liste) ->     
    % Liste halbieren lassen
    {Links, Rechts} = msort_split_half(Liste),
    % die halbierten Listen weiterverarbeiten und die Ergebnisse zusammensetzen.
    merge(msort(Links), msort(Rechts)).



% Der rechte Bereich ist leer gelaufen, der linke Bereich muss nicht weiter bearbeitet werden
merge(Links, []) -> Links;

% Der linke Bereich ist leer gelaufen, der rechte Bereich muss nicht weiter bearbeitet werden
merge([], Rechts) -> Rechts;

% in beiden Bereichen ist noch mindestens ein Element vorhanden, das mit dem jeweils anderen ersten verglichen werden kann
merge(Links=[ElementLinks|RestLinks], Rechts=[ElementRechts|RestRechts]) ->
    case ElementLinks < ElementRechts of
        true -> [ElementLinks | merge(RestLinks, Rechts)];
        false -> [ElementRechts | merge(Links, RestRechts)]
    end.




    
% Hilfsfunktion, Teilt die Liste in der Haelfte
msort_split_half(Liste) ->
    % Leange fesstellen
    PositionHaelfte = round(erlang:length(Liste)/2),
    lists:split(PositionHaelfte, Liste).
