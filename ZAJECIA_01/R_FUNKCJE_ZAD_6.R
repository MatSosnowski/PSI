# 6. Stwórz funkcję o nazwie przyznaj_nagrode()
# która symuluje rzut sześcienną kostką do gry i przyznaje nagrodę w zależności od wyniku rzutu. 
# Funkcja powinna działać według następujących zasad:
# - Jeśli wyrzucona liczba oczek to 6, funkcja powinna zwrócić komunikat: "Super bonus!"
# - Jeśli wyrzucona liczba oczek to 4 lub 5, funkcja powinna zwrócić komunikat: "Nagroda standardowa"
# - Jeśli wyrzucona liczba oczek to 1, 2 lub 3, funkcja powinna zwrócić komunikat: "Brak nagrody..."

przyznaj_nagrode = function() {
  wynik_rzutu = sample(1:6, 1)
  if (wynik_rzutu == 6){
    return(paste("Wynik rzutu: ", wynik_rzutu, " - Super bonus!"))
  } else if (wynik_rzutu %in% c(4,5)) {
    return(paste("Wynik rzutu: ", wynik_rzutu, " - Nagroda standardowa"))
  } else
    return(paste("Wynik rzutu: ", wynik_rzutu, " - Brak nagrody..."))
}

przyznaj_nagrode()
