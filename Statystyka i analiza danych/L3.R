#LAB3
#Z badania zanieczyszczenia wody pitnej wynika, że 30% wszystkich studni w mieście jest
#zanieczyszczonych. Wybrano losowo pięć studni i sprawdzono jakość wody. Niech zmienna
#losowa S oznacza liczbę zanieczyszczonych studni spośród wybranych.
#(a) Jaki jest rozkład prawdopodobieństwa zmiennej losowej S? Podaj jego nazwę i przedstaw

#Rozkład dwumianowy Bin(n,p)
n = 5
p = 0.3
#przypisanie wartości ziemnnym losowym s
s = c(0:5) 
#gęstość rozkładu, prawdopodbieństwo w punckie
pr = dbinom(s, n, p) 
#tabelka pradowpodbieństw
rbind(s,pr)
#wykres punktowy prawdopodobieństwa
plot(s,pr)
#wykres linowy prawdopodobieństwa
plot(s,pr, type = "h", lwd = "4", xlab = "x", ylab = "f(x)", main="Histogram prawdopodobieństwa Binomial(n,p)", col="RED")
#prawdopodobieńswto dokładnie 3 studnie są zanieczyszczone

#w formie tabeli a następnie narysuj liniowy wykres prawdopodobieństwa.
#(b) Korzystając z funkcji dostępnych w R oblicz prawdopodobieństwo, że:
#dokładnie 3 studnie są zanieczyszczone,
#P(S=3)
dbinom(3,n,p)

#prawdopodobieńswto conajmniej 3 studnie zanieczyszczone
#P(S>=3) = P(S>2) = 1 - P(S=2)
1 - pbinom(2,n,p)

#prawdopodobieńswto mniej niż 3 studnie zanieczyszczone
#P(S<3) = P(S<=2)
pbinom(2,n,p)

#Prawdopodobieństwo, że żarówka danego typu świeci przez przynajmniej 500 godzin wynosi 0,9.
#Niech B oznacza liczbę żarówek wśród 8 wylosowanych, których żywotność przekracza 500
#godzin. Podaj rozkład prawdopodobieństwa zmiennej losowej B i oblicz:
#(a) P(B=8); (b) P(B=7); (c) P(B>5); (d) E(B); (e) SD(B). Zinterpretuj (d) i (e).
n=8
p=0.9

x = c(0:8)
pr = dbinom(x,n,p)
rbind(x,pr)

plot(x,pr, type = "h", lwd = "4", xlab = "x", ylab = "f(x)", main="Histogram prawdopodobieństwa Binomial(n,p)", col="RED")

#P(S=8)
dbinom(8,n,p)

#P(S=7)
dbinom(7,n,p)

#P(S>5) = 1 - F(5)
1 - dbinom(5,n,p)

#E(B)
expect=sum(x*pr)
expect=n*p
#Przecziętnie 7 żarówek przekroczy żywotność 500 godzin

#SD(B)
sd = sqrt(n*p*(1-p))
#Przeciętne odchylenie od średniej wynosi 1 żarówka

#Czas (w dniach) między awariami ogniw zasilających w satelicie jest zmienną losową o
#rozkładzie wykładniczym z λ = 0,01. Obecnie funkcjonują tylko 2 ogniwa. Są one ułożone
#równolegle i mają niezależne życie, dzięki czemu satelita może funkcjonować tak długo, jak
#działa co najmniej 1 ogniwo energetyczne. Narysuj funkcję gęstości czasu między awariami
#pojedynczego ogniwa korzystając z funkcji curve i dexp (dobierz odpowiednio przedział
#argumentów, aby zobaczyć kształt rozkładu wykładniczego). Zastosuj funkcję pexp aby
#wyznaczyć prawdopodobieństwo, że pojedyncze ogniwo:

lambda = 0.01
curve(dexp(x, lambda), 0, 1000)

#a przeżyje co najmniej 200 dni 
#P(X>=200) = P(X>200) = 1 - F(200)
1 - pexp(200, lambda)

#b mniej niż 100 dni
#P(X<100) = P(X<=100) = F(100)
pexp(100, lambda)

#c mniej niż 500 dni
#P(X<500) = P(X<=500) = F(500)
pexp(500, lambda)


#Siłę trzęsień ziemi (mierzoną w skali Richtera) zarejestrowanych w regionie Ameryki Północnej
#można modelować za pomocą rozkładu wykładniczego ze średnią 2,4 stopnia. Narysuj funkcję
#gęstości (dobierz odpowiednio przedział argumentów, aby zobaczyć kształt rozkładu
#wykładniczego). Oblicz prawdopodobieństwo, że nastąpi następne trzęsienie ziemi, 
#które nawiedzi ten region
ex = 2.4
lambda = 1/ex
curve(dexp(x, lambda), 0, 10)

#(a) przekracza 3 stopnie w skali Richtera, czyli P(x>3) = 1 - F(3)
1 - pexp(3, lambda)

#(b) mieści się w przedziale od 2 do 3 stopnie w skali Richtera, czyli P(2<x<3) = F(3) - F(2)
pexp(3, lambda) - pexp(2, lambda)

#Sprawdź, czy wartość oczekiwana wyliczona z definicji (za pomocą całki) jest równa 2,4.
f = function(x){x*dexp(x,lambda)}
integrate(f, 0, Inf)


#Przewody elektryczne przeznaczone do zastosowania w pewnym systemie komputerowym
#powinny mieć opór (rezystancję) pomiędzy 0,12 i 0,14 oma. Rezystancja przewodów
#produkowanych przez pewną firmę jest zmienną losową o rozkładzie normalnym ze średnią
#0,13 oma i odchyleniem standardowym 0,005 oma. Narysuj wykres funkcji gęstości rezystancji
#produkowanych przewodów (dobierz odpowiednio przedział argumentów funkcji, aby zobaczyć                        kształt rozkładu normalnego). Jakie jest prawdopodobieństwo, że losowo wybrany przewód
#produkowany przez tę firmę A spełnia wymagania stawiane przez system?
  
#średnia
mu = 0.13
#odchylenie
sig = 0.005
#3 sigmy twierdzenie 99,7% obserwacji tam się znajduje w tym przedziale
curve(dnorm(x, mu, sig), mu-3*sig, mu+3*sig)

#P(0.12<X<0.14)
pnorm(0.14, mu, sig) - pnorm(0.12, mu, sig)

#Czas schnięcia farby pewnego typu jest zmienną losową o rozkładzie normalnym z wartością
#oczekiwaną 2 godziny i odchyleniem standardowym 15 minut. Narysuj wykres funkcji gęstości
#czasu schnięcia badanej farby. Wyznacz prawdopodobieństwo, że farba schnie między 1h 51min
#i 2h 15 min.
mu = 120
sig = 15
curve(dnorm(x, mu, sig), mu-3*sig, mu+3*sig)

#P(111<X<135)
pnorm(135, mu, sig) - pnorm(111, mu, sig)

#Motorowery (małe motocykle o pojemności silnika poniżej 50cm3) cieszą się w Europie dużą
#popularnością ze względu na ich mobilność, łatwość obsługi i niski koszt. W pewnym
#specjalistycznym czasopiśmie opisano badanie przeprowadzone na stanowisku rolkowym mające
#na celu określenie maksymalnej prędkości pojazdu. Wywnioskowano, że maksymalna prędkość
#jest zmienną losową o rozkładzie normalnym z wartością oczekiwaną 46,8 km/h i odchyleniem
#standardowym 1,75 km/h.
mu = 46.8
sig = 1.75

#Rozważmy losowy wybór jednego takiego motoroweru. Oblicz prawdopodobieństwo, że jego
#maksymalna prędkość
#(a) wynosi co najwyżej 50 km/h, czyli P(x<50)
pnorm(50,mu,sig)

#(b) wynosi co najmniej 48 km/h, czyli P(x>48) = 1 - F(48)
1 - pnorm(48,mu,sig)

#Załóżmy, że 25% wszystkich studentów dużej uczelni publicznej otrzymuje stypendium. Niech
#X będzie liczbą studentów w losowej próbie o wielkości 100, którzy ubiegali się o przyznanie
#stypendium. Korzystając z rozkładu dokładnego zmiennej losowej X oraz jego przybliżenia
#rozkładem normalnym oblicz prawdopodobieństwo, że pomoc otrzyma co najwyżej 15 studentów
n  = 100
p = 25/100

#Przyblizenie rozkład dokładny - dwumianowy
#P(x<=15)
pbinom(15,n,p)

#Rezystancja przewodników danego typu jest zmienną losową o rozkładzie normalnym ze średnią
#200 omów i odchyleniem standardowym 10 omów. W obwodzie użytych zostało
#25 przewodników. Wyznacz prawdopodobieństwo, że
mu = 200
sig = 10
n = 25

#(a) średnia rezystancja wszystkich 25 przewodników zawiera się między 199 i 202 omów;
#P(199<x<202)
#Przybliżenie rozkładem normalnym (aproksymacyjny rozkład)
#avR ma rozkład N(mu, sig/sqrt(n))
#P(199<X<202)
pnorm(202, mu, sig/sqrt(n))-pnorm(199, mu, sig/sqrt(n))

#(b) całkowita rezystancja wszystkich 25 przewodników nie przekracza 5100 omów
#T=X1+X2+X3+...+X25, T ma rozkład N(n*mu, sig*sqrt(n))
#P(T<=5100)
pnorm(5100, n*mu, sig*sqrt(n))

#Poziom cholesterolu we krwi pracowników pewnej firmy jest zmienną losową, dla której średnia
#to 202 mg/dl, a odchylenie standardowe to 14 mg/dl (dl=decylitr). Oblicz prawdopodobieństwo,
#że średni poziom cholesterolu 64 wylosowanych do badania pracowników będzie zawierał się w
#przedziale między 198 a 206 mg/dl?
mu = 202
sig = 14
n = 64

#avR ma rozkład N(mu, sig/sqrt(n))
#P(198<X<206)
pnorm(206, mu, sig/sqrt(n)) - pnorm(198, mu, sig/sqrt(n))

#Wytrzymałość nici jest zmienną losową o średniej 0,5 kg i odchyleniu standardowym 0,2 kg.
#Załóżmy, że lina spleciona została ze 100 nici. Oblicz prawdopodobieństwo, że utrzyma ona
#ciężar 47 kg (Uwaga! Lina utrzyma ciężar, jeśli jej wytrzymałość jest od niego nie mniejsza).
mu = 0.5
sig = 0.2
n = 100

#P(T>47) = 1 - F(47)
#T=X1+X2+X3+...+X25, T ma rozkład N(n*mu, sig*sqrt(n))
1 - pnorm(47, n*mu, sig*sqrt(n))
