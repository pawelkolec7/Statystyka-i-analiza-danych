#WYKŁAD 4

#Inżynier mechanik, który zaprojektował urządzenie do fizjoterapii, wybrał do badania 12
#pacjentów i sprawdził, ile czasu spędzają korzystając z nowego urządzenia. Otrzymał
#następujące wyniki (w godzinach): 8; 12; 26; 10; 23; 21; 16; 22; 18; 17; 36; 9.
#Oceń średni czas korzystania z urządzenia danego typu przez wszystkich pacjentów poddanych terapii
time = c(8, 12, 26, 10, 23, 21, 16, 22, 18, 17, 36, 9)
#średnia z próby
xbar = mean(time)
#śrendi czas oczekiewnia przez wszytskich pacjentów został oszcowany na 18h i 10m

#P(L < θ < U) = 1 - α
#Wówczas przedział losowy (L, U) nazywamy (1 − α)100% przedziałem
#ufności dla parametru θ a wartość (1 − α) nazywamy współczynnikiem
#ufności przedziału.

#Średnia:

#Zakładając normalność rozkładu czasu użytkowania urządzenia oceń przedziałowo z
#ufnością 95% średni czas korzystania z urządzenia zaprojektowanego przez inżyniera
#przez wszystkich pacjentów poddanych terapii.
n = length(time)
x = mean(time)
s = var(time)

#1-alpha = 0.95
#alpha = 0.05
#alpha/2 = 0.025

t = qt(1-0.025, n-1)
L = x - t
U = x + t

#Zakładając normalność rozkładu czasu użytkowania urządzenia oceń przedziałowo z
#ufnością 95% średni czas korzystania z urządzenia zaprojektowanego przez inżyniera
#przez wszystkich pacjentów poddanych terapii.
m = mean(czas)
s = sd(czas)
n = length(czas)
alpha = 0.05

L = m-qt(1-alpha/2, n-1)*(s/sqrt(n))
U = m+qt(1-alpha/2, n-1)*(s/sqrt(n))

#Zakładając normalność rozkładu czasu użytkowania urządzenia oceń przedziałowo z
#ufnością 95% średni czas korzystania z urządzenia zaprojektowanego przez inżyniera
#przez wszystkich pacjentów poddanych terapii.

mean = t.test(czas,conf.level=0.95)
mean$conf.int

#Z ufnnością 0.95 przedział (13.013220;23.32013) pokrywa nieznaną prawdziwą średnią populacyjną µ.

#Wariancja:

#Zakładając normalność rozkładu czasu użytkowania urządzenia oceń przedziałowo z
#ufnością 95% średni czas korzystania z urządzenia zaprojektowanego przez inżyniera
#przez wszystkich pacjentów poddanych terapii.
n = length(czas)
s = var(czas)

alpha = 0.05
alphapol = alpha/2

Lchi = qchisq(1-alphapol, n-1)
Pchi = qchisq(alphapol, n-1)

L = (n-1)*s/Lchi
P = (n-1)*s/Pchi

Chi2 = sigma.test(czas, conf.level =0.95)
Chi2$conf.int

#Z ufnnością 0.95 przedział (33.014;189.653) pokrywa prawdziwą nieznaną wartość wariancji 
#dla populacji sigma^2

#Proporcja:

#Pewna szkoła chce poznać opinię uczniów o nowym programie nauczania. Aby to zrobić
#wybrano losowo próbę 150 uczniów i zapytano ich o opinię: 70 uczniów pozytywnie wypowiedziało się 
#w sprawie nowego program nauczania. Wyznacz ocenę proporcji wszystkich
#uczniów pozytywnie nastawionych do nowego programu nauczania.

n = 150
t = 70
phat = t/n

alpha = 0.01
z=qnorm(1-alpha/2)

L=phat-z*(sqrt(phat*(1-phat)/n))
U=phat+z*(sqrt(phat*(1-phat)/n))

#Z ufnością 95% przedział od 38,6% do 54,7% pokrywa nieznaną prawdziwą proporcję WSZYTSTKICH
#uczniów pozytywnie nastawionych do nowego programu nauczania
#zaokrąglamy bezpiecznie, dolny w dół, górny w górę
n = 150
t = 70

propCI = binom.test(t,n, conf.level = 1 - alpha)
propCI$conf.int

#24.12.1991 New York Times podał, że 46% Amerykanów jest zadowolonych z polityki
#ekonomicznej prezydenta Busha, z marginesem błędu ±3%. Wiedząc, że media przyjmują
#zazwyczaj 95% poziom ufności wyjaśnij, co oznacza podany wynik. Czy na podstawie
#opublikowanych wyników możemy wywnioskować, jak dużą grupę osób zapytano?
phat = 0.46
ee = 0.03

alpha = 0.05
z = qnorm(1-alpha/2)

z*sqrt(phat*(1-phat)/n) = 0.03

#Obliczyć n
#1060 ludzi zbadano o prezydenta
