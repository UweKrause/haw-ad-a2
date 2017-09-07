% Authors: Uwe Krause, Sean Pedersen
-module(stack).

-import(liste, [create/0, isEmpty/1, isList/1, equal/2, laenge/1, insert/3, delete/2, find/2, retrieve/2, concat/2, diffListe/2, eoCount/1]).

-export([createS/0, push/2, pop/1, top/1, isEmptyS/1, equalS/2, reverseS/1]).


% Der Stack besteht aus dem Stack-Tag, einer Liste mit dem Inhalt, sowie einem Indikator fuer die Zugriffsrichtung auf die Liste (nuetzlich fuer schnelle Umdrehung des kompletten Stack!)
% Zugriffsrichtung: true: wie im deutschen gelesen wird, false = RtL

%createS: ∅ → stack / createS()      
createS() -> {stack, liste:create(), true}.



%push:  stack × elem → stack / push(<Stack>,<Element>)
push({stack, Liste, Z}, Element) ->
	case Z of
		% wenn Zugriffsrichtung wie im Buch, dann fuege am Anfang der Liste ein, ansonsten am Ende
		true -> {stack, liste:insert(Liste, 1, Element), Z};
		false -> {stack, liste:insert(Liste, liste:laenge(Liste) + 1, Element), Z}
	end.



%pop: stack → stack / pop(<Stack>)
pop({stack, L, Z}) -> 
	case Z of
		% wenn Zugriffsrichtung wie im Buch, dann entferne erstes Element, ansonsten letztes
		true -> liste:delete(L, 1);
		false -> liste:delete(L, liste:laenge(L))
	end.



%top: stack → elem / top(<Stack>)       
top({stack, L, Z}) -> 
	case Z of
		% wenn Zugriffsrichtung wie im Buch, dann liefere erstes Element, ansonsten letztes
		true -> liste:retrieve(L, 1);
		false -> liste:retrieve(L, liste:laenge(L))
	end.



%isEmptyS: stack → bool / isEmptyS(<Stack>)
isEmptyS({stack, L, _Z}) -> liste:isEmpty(L).



%equalS: stack × stack → bool / equalS(<Stack>,<Stack>)
equalS({stack, L1, Z1}, {stack, L2, Z2}) when Z1 == Z2 -> liste:equal(L1, L2);
equalS(S1, S2) -> equalS(S1, stack.reverseS(S2)).



%reverseS: stack → stack / reverseS(<Stack>)
reverseS(S) -> priv_stack_change(S).


% Private Funktion, liefert einen Stack mit der gleichen Liste, aber umgekehrter Zugriffsrichtung
priv_stack_change({stack, L, Zugriffsrichtung}) -> {stack, L, not Zugriffsrichtung}.









