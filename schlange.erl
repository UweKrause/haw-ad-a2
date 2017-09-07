% Authors: Uwe Krause, Sean Pedersen
-module(schlange).

-export([createQ/0, enqueue/2, dequeue/1, front/1, isEmptyQ/1, equalQ/2]).


% Die Schlange besteht aus dem queue-Tag und zwei Stacks

%createQ: ∅ → queue / createQ()
% erstellt eine leere Queue
createQ() -> {queue, stack:createS(), stack:createS()}.



%enqueue:  queue × elem → queue / enqueue(<Queue>,<Element>)
% Ein neuer Queue mit dem Element auf dem In-Stack wird erstellt
enqueue({queue, In, Out}, E) -> {queue, stack:push(In, E), Out}.



%dequeue: queue → queue (Mutator) / dequeue(<Queue>)
% liefert die Schlange ohne das erste Element
dequeue({queue, In, Out}) ->
	case stack:isEmptyS(Out) of
		false ->
			% Vom Out-Stack wird das oberste Element entfernt, In-Stack bleibt unveraendert
			{queue, In, stack:pop(Out)};
		true ->
			% In-Stack wird reversed, Out/In werden miteinander getauscht, oberstes Element des (neuen) Out-Stack wird entfernt
			{queue, Out, stack:pop(stack:reverseS(In))}
	end.
	% Leerer OUT & IN: let it crash



%front: queue → elem / front(<Queue>)
% liefert das erste Element der Schlange
front({queue, In, Out}) ->
	case stack:isEmptyS(Out) of
		false ->
			% Out-Stack hat Daten, liefere oberstes Element
			stack:top(Out);
		true ->
			% Out-Stack hat keine Daten, versuche es im (umgedrehten) In-Stack
			stack:top(stack:reverseS(In))
		% Wenn der In-Stack ebenfalls leer ist, gibt es einen Fehler
	end.



%isEmptyQ: queue → bool / isEmptyQ(<Queue>)
% Die Queue ist leer, wenn sowohl In-STack, als auch Out-Stack leer sind.
isEmptyQ({queue, In, Out}) ->
	(stack:isEmptyS(In) and stack:isEmptyS(Out)).



%equalQ: queue × queue → bool / equalQ(<Queue>,<Queue>)
% Prueft Gleichheit aus interner (nicht externer) Sicht.
% (Zwei intern unterschiedliche Schlangen mit den gleichen Werten und (aus externer Sicht) gleichen Reihenfolge
% sind trotzdem unterschiedlich.)
% Reicht die Gleichheitspruefung an die unterliegenden Schichten weiter.
equalQ({queue, In1, Out1}, {queue, In2, Out2}) -> stack:equalS(In1, In2) and stack:equalS(Out1, Out2).
