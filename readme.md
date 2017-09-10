# Benutzung

Startet die Erlang-shell
```bash
erl
```

Zum Benutzen müssen innerhalb der erlang-shell alle Module kompiliert werden.
```erlang
c(ssort).
c(ksort).
c(liste_util).
c(util).
c(wechsel).
c(laufzeit).
```


# Sortieralgorithmen

## Simple Sortierungen
## Insertionsort
ssort:insertionS(Liste).
```erlang
ssort:insertionS([1, 300, 12, 44]).
```

### Selectionsort
ssort:selectionS(Liste).
```erlang
ssort:selectionS([1, 300, 12, 44]).
```

## Komplexere Sortierungen
### Quicksort
ksort:qsort(Pivotverfahren: left/middle/right/random, Liste, Wechselgroesse)
```erlang
ksort:qsort(left, [1, 300, 12, 44], 2)
```
Die Wechselgröße gibt an, ab wievielen verbleibenden Elementen der unsortierten Liste einer der simplen Algorithmen verwendet werden soll.

### Mergesort
ksort:msort(Liste).
```erlang
ksort:msort([1, 300, 12, 44]).
```

# Testdaten
Erstellen der Testdaten nach  Laden der laufzeit.erl
(Achtung, results.csv muss vorher erstellt und eine leere Datei sein.)
(Wenn sie nicht leer ist, werden die Ergebnise hinten angehangen!)

Aufruf der Testmethode
```erlang
laufzeit:log_to_file().
```

# Wechselgrösse Quicksort
Erstellen der Testdaten nach Laden der wechsel.erl
(Achtung, wechsel.csv muss vorher erstellt und eine leere Datei sein.)
(Wenn sie nicht leer ist, werden die Ergebnise hinten angehangen!)

Aufruf der Testmethode
```erlang
wechsel:log_to_file().
```
