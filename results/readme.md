# Szczegóły obliczeń

## Parametry algorytmów

### Metoda Lagrange'a

$\epsilon = 0.1$

### Metoda Newtona-Armijo

$\epsilon = 0.01$
$\alpha = 0.15$
$\rho = 0.1$

### Metoda ekspansji BDS

Użyte długości kroków:

- 0.1
- 0.15
- 0.3

# Opis plików z wynikami

Katalog `results` zawiera wyniki obliczeń z podziałem na długość kroku metody szukania przedziału BDS w katalogach `step1/2/3`, po jednym na długość kroku(oraz w kolejności zgodnej z listą powyżej). Wyniki każdego algorytmu znajdują się w osobym pliku `.csv`.
Pliki z przedrostkiem `mean_` przeznaczone są dla zakładki `Min_wartości_średnie` w skoroszycie projektu; pliki bez tego przedrostka przeznaczone są dla zakładki `Min_wyniki`.