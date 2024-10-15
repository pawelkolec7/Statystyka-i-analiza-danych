#LAB 1:

# Z1 - W1
# W celu porównania dwóch pięcioosobowych grup studentów ze względu na oceny uzyskane z przedmiotu STATYSTYKA, 
# zebrano następujące grupa A 3,0 3,0 4,0 4,5 4,5, grupa B 2,0 3,5 4,0 4,5 5,0. 
# Porównaj grupy studentów za pomocą wykresów pudełkowych.

grA = c(3.0, 3.0, 4.0, 4.5, 4.5)
grB = c(2.0, 3.5, 4.0, 4.5, 5.0)

boxplot(grA, grB)

# Z1 - L1
# Oblicz wybrane wyrażenia artytmetyczne
sin(2*pi)
cos(3/4)
tan(pi)
log(100)
log(15, base=exp(1)) 
log(1/7, base=7) 
exp(1)^3
64^(1/3)

# Z2 - L1
# Utwórz wektor o składowych od 1 do 10. Zsumuj składowe wektora
wektor = seq(1, 10, by=1)
suma = sum(wektor)

# Z3 - L1
# Utwórz wektor x którego składowymi są liczby parzyste od 2 to 20
x = seq(2, 20, by=2)

# a) zweryfikuj liczbę składowych wektora x
length(x)

# b) zdefiniuj nowy wektor, y, którego składowe sa takie same jak wektora x, tylko w odwrotnej kolejności
y = seq(20, 2, by=-2)
y = rev(x)

# c) sprawdź, czym jest wynik działania x*x oraz x^2
#potęgowanie każdego elementu wektora
x*x 
#potęgowanie każdego elementu wektora
x^2 

# d) wyznacz długość (euklidesową) wektora x
#pierwastek sumy kwadratów
sqrt(sum(x^2))

# e) sprawdź, czym jest wynik mnożenia (macierzowego) transpozycji wektora x przez wektor y oraz wektora x przez transpozycję wektora y
x%*%t(y)
y%*%t(x)

# Z4 - L1
# Utwórz wektor o 13 składowych, którego pierwsza składowa jest równa 5, ostatnia 10, natomiast
# wszystkie pozostałe są równo oddalone od siebie.
x = seq(5, 10, length = 13)

# Z5 - L1
# Utwórz wektory z1 i z2 będące odpowiednio 5-krotną replikacją wektora (1,2) i 5-krotną replikacją
# składowych wektora (1,2). Wykonaj polecenia
z1 = rep(c(1,2), times = 5)
z2 = rep(c(1,2), times = 5)

# a) dodaj 4 do każdej składowej wektora z1
z1 = z1 + 4

# b) zdefiniuj nowy wektor, z3, przez usunięcie ostatniej składowej wektora z2
z3 = z2[-c(length(z2))] 

# c) zadeklaruj nowy wektor, c, jako sumę wektorów z1 i z3 i zweryfikuj wyniki
c = z1 + z3

# d) zdefiniuj nowy wektor, którego składowe to elementy wektora z1, które są większe niż 1
d = z1[z1 > 1]

# Z6 - L1
# Utwórz macierz
A = rbind(c(2,3,0), c(1,-1,2), c(1,1,-1))

# a) sprawdź wynik działania A^2 oraz A%*%A
A^2
# każda składowa wektora podniesiona do kwardatu
A%*%A
# mnożenie macierzy

# b) wyznacz transpozycję, wyznacznik i odwrotność macierzy A
t(A)
det(A)
solve(A)

# c) zdefiniuj wektor b, będący trzecim wierszem macierzy A
b = A[3,]

sum(diag(A)) #ślad macierzy

# Z7 - L1
# Utwórz dwa dowolne wektory x i y składające się z 10 składowych, a następnie
a1 = seq(10)
a2 = seq(10) + 2

# a) Narysuj punkty (x, y) na wykresie (wykres punktowy)
plot(a1, a2)

# b) Połącz wektory x i y za pomocą polecenia data.frame i narysuj powstały wykres
data.frame(a1,a2) 
plot(data.frame(a1, a2))

# c) Połącz wektory x i y za pomocą poleceń rbind i cbind i narysuj powstałe wykresy
cbind(a1,a2)
rbind(a1,a2)

plot(cbind(a1,a2))
plot(rbind(a1,a2))

# Z8 - L1
#Narysuj funkcję 𝑓(𝑥) = 𝑥2 + 3𝑥 − 5 na przedziale (-3, 4). Spróbuj narysować inne funkcje
funkcja1 = function(x){x^2 + 3*x - 5}
curve(x^2+3*x-5,-3,4)
