-module(ssort).

-author("Uwe Krause").

-export([insertionS/1, selectionS/1]).





% Das Ende der Liste wurde erreicht
insertionS([]) -> [];

% Die Liste wird sortiert, indem das aktuelle Element in die richtige Reihenfolge gebracht wird
insertionS(_Liste=[AktuellesElement | Rest]) ->
    % Die Ergebnislite setzt sich zusammen aus dem Ergebnis des rekursiv weiter verarbeiteten Rest und dem aktuellen Element in der richtigen Reihenfolge
    liste_util:put_inorder(insertionS(Rest), AktuellesElement).




% Das Ende der Liste wurde erreicht
selectionS([]) -> [];

% Das kleinste Element der zu sortierenden Liste wird entnommen und an die aktuelle Position getan.
% Der Verleibende unsortierte Bereich wird auf die gleiche Art weiter bearbeitet
selectionS(Liste) ->
    {Min, US} = liste_util:pick_min(Liste),
    [Min | selectionS(US)].
