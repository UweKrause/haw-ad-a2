% Authors: Uwe Krause, Sean Pedersen
-module(stack).

-export([createS/0, push/2, pop/1, top/1, isEmptyS/1, equalS/2, reverseS/1]).


% Der Stack besteht aus dem Stack-Tag und einer Liste mit dem Inhalt

%createS: ∅ → stack / createS()
% Erstellt einen leeren Stack
createS() -> {stack, liste:create()}.



%push:  stack × elem → stack / push(<Stack>,<Element>)
% legt ein Element oben auf den Stack, indem es an die erste Position der Liste geschrieben wird
push({stack, Liste}, Element) -> {stack, liste:insert(Liste, 1, Element)}.



%pop: stack → stack / pop(<Stack>)
% baut einen nuen Stack, dessen interne Liste die interne Liste des ursprünglichen Stacks, ohne das oberste Element ist
% fehlerhafter Aufruf (auf leeren Stack z.B.) fuehrt zu nicht abgefangener Exception
pop({stack, L}) -> {stack, liste:delete(L, 1)}.



%top: stack → elem / top(<Stack>)
% Liefert das oberste Lement des Stacks, welches das erste Element der internen Liste ist
% fehlerhafter Aufruf (auf leeren Stack z.B.) fuehrt zu nicht abgefangener Exception
top({stack, L}) -> liste:retrieve(L, 1).



%isEmptyS: stack → bool / isEmptyS(<Stack>)
% prueft obe der Stack leer ist.
% Der Stack ist ler, wenn seine interne Liste leer ist.
isEmptyS({stack, L}) -> liste:isEmpty(L).



%equalS: stack × stack → bool / equalS(<Stack>,<Stack>)
% prueft, ob zwei Stacks gleich sind. Sie sind gleich, wenn ihre interne Liste gleich ist.
equalS({stack, L1}, {stack, L2}) -> liste:equal(L1, L2).



%reverseS: stack → stack / reverseS(<Stack>)
% liefert einen neuen Stack, dessen interne Liste die umgekehrte Reihenfolge der urspruenglichen Liste ist
reverseS(Sinit) -> rev(Sinit, createS()).

% entfernt das oberste Element des input-Stack und pusht es auf den aufzubauenden Stack,
% solange bis input-Stack leer ist
rev(Sin, Srev) ->
	case isEmptyS(Sin) of
		true -> Srev;
		false -> rev(pop(Sin), push(Srev, top(Sin)))
	end.
