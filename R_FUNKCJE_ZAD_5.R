# 5. Stwórz funkcję będącą najprostszą wersją kalkulatora 
# (dodawanie, odejmowanie, mnożenie albo dzielenie dwóch liczb).

Kalkulator = function(x1, x2, dzialanie) {
  if (dzialanie == "+") {
    wynik = x1 + x2 
  } else if (dzialanie == "-") {
    wynik = x1 - x2
  } else if (dzialanie == "*") {
    wynik = x1 * x2
  } else if (dzialanie == "/") {
    if (x2 != 0){
      wynik = x1 / x2  
    }
    else {
    return("Nie dziel przez zero!")
    }
  } else {
    return("Podaj poprawny symbol dzialania: '+', '-', '*', '/'")
  }
  return(wynik)
}

Kalkulator(2, 3, "*")