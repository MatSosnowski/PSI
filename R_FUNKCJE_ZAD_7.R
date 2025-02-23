# 7. Stwórz funkcję obliczającą podatek w zależności od dochodu. 
# Przyjmij następujące założenia:
# a) Jeżeli podatnik rozlicza się liniowo, wtedy niezależnie od kwoty płaci 19% podatku.
# b) Jeżeli podatnik rozlicza się na zasadach ogólnych, wtedy:
# - poniżej kwoty 85528zł płaci 18% podatku minus kwota zmniejszająca, czyli 556zł;
# - powyżej kwoty 85528zł płaci 14839zł + 32% nadwyżki powyżej 85528zł.

liczPodatek = function(x, FormaOpodatkowania){
  if(x < 0) {
    return("Podaj nieujemna kwote!")
  }
  
  if (FormaOpodatkowania == "Liniowa"){
    return(0.19 * x)
  } else if (FormaOpodatkowania == "Ogolna"){
    if (x <= 85528) {
      return(0.18 * x - 556)
    }
    else{
      return(0.32 * (x - 85528) + 14839)
    }
  }
  else{
    return("Wybierz poprawna forme opodatkowania: 'Liniowa' lub 'Ogolna'.")
  }
}

liczPodatek(85528, "Liniowa")
liczPodatek(85528, "Ogolna")
liczPodatek(-100, "Ogolna")
liczPodatek(100000, "Liniowa")
liczPodatek(100000, "Ogolna")
liczPodatek(41234, "ABCD")
