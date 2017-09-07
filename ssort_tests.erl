-module(ssort_tests).

-include_lib("eunit/include/eunit.hrl").

-author("Uwe Krause").



% Unit testing
% Aufruf ueber die Konsole:
% (1) Modul Kompilieren 		c(ssort)
% (2) Testmodul kompilieren		c(ssort_tests)
% (3) Aufruf der Testfaelle 	eunit:test(ssort,[verbose]).
% oder mit eunit:test(ssort)




% Generiert die eigentlichen Testfunktionen intern
add_test_() ->
	[test_ssort_insertionS(),
	test_ssort_selectionS()	
	].



% VORLAGE zur Generierung von Tests
% (Aus dieser Liste von Makros werden letztlich die Tests generiert)
% Liste der verfuegbaren Makros
% http://learnyousomeerlang.com/eunit#eunit-whats-a-eunit
test_ssort_insertionS() ->
	[
	% leere Liste
	?_assertEqual([], ssort:insertionS([])),
	% einelementige Liste
	?_assertEqual([1], ssort:insertionS([1])),
	% mehrere Elemente
	?_assertEqual([1,2,3], ssort:insertionS([1,3,2])),
	?_assertEqual([1,2,3], ssort:insertionS([3,1,2])),
	?_assertEqual([3,5,9], ssort:insertionS([9,3,5])),
	?_assertEqual([1,1,3], ssort:insertionS([3,1,1])),
	?_assertEqual([1,1,3], ssort:insertionS([1,1,3])),
	?_assertEqual([1,3,3], ssort:insertionS([3,1,3])),
	% Beispiel aus Buch
	?_assertEqual([1,5,14,15,20,24,28,31,39,54], ssort:insertionS([20,54,28,31,5,24,39,14,1,15])),
	% noch paar mit doppelten
	?_assertEqual([2,2,3,3,3,3,4,5], ssort:insertionS([5,2,3,4,3,3,3,2]))
	].



test_ssort_selectionS() ->
	[
	% leere Liste
	?_assertEqual([], ssort:selectionS([])),
	% einelementige Liste
	?_assertEqual([1], ssort:selectionS([1])),
	% mehrere Elemente
	?_assertEqual([1,2,3], ssort:selectionS([1,3,2])),
	?_assertEqual([1,2,3], ssort:selectionS([3,1,2])),
	?_assertEqual([3,5,9], ssort:selectionS([9,3,5])),
	?_assertEqual([1,1,3], ssort:selectionS([3,1,1])),
	?_assertEqual([1,1,3], ssort:selectionS([1,1,3])),
	?_assertEqual([1,3,3], ssort:selectionS([3,1,3])),
	% Beispiel aus Buch
	?_assertEqual([1,5,14,15,20,24,28,31,39,54], ssort:selectionS([20,54,28,31,5,24,39,14,1,15])),
	% noch paar mit doppelten
	?_assertEqual([2,2,3,3,3,3,4,5], ssort:selectionS([5,2,3,4,3,3,3,2]))
	].
	