# 1. Stwórz funkcję o nazwie kostka, która będzie symulować n rzutów kostką.
# Wskazówka: Użyj funkcji sample() do losowania liczby oczek od 1 do 6.

kostka = function(x) {
  wynik_rzutu = sample(1:6, x, replace = TRUE)
  return(wynik_rzutu)
}

kostka(5)
