#WYKŁAD 1:

# wpiswywanie danych
dane = c(1,2,3,4,5,6,7,8,9)

# wczytywanie danych z CSV
# normalnie (tutaj przecinki zczyta jak wektor)
dane = read.csv("C:\\Users\\kolec\\Desktop\\Semestr 4\\Statystyka\\Wykład\\W1\\ozon.csv", sep = ";")

# dane rozdzielone przecinikiem w excelu, a nie za pomocą kropki
dane = read.csv("C:\\Users\\kolec\\Desktop\\Semestr 4\\Statystyka\\Wykład\\W1\\ozon.csv", sep = ";", dec = ",")

# dane z etykietami
read.csv("nazwa", sep = ";", head = TRUE)

# szereg rozdzielczy punktowy z danych - ile wystąpień danego wyniku
table(dane)

# histogram odcinkowy - rozkład częstości (pakiet arm) - musi być to wektor
discrete.histogram(dane)

# histogram odcinkowy - rozkład liczebnośc (pakiet arm) - musi być to wektor
discrete.histogram(dane, freq = T)

# wykres słupkowy
plot(table(dane)/length(dane))
plot(table(dane))

# wykres kołowy
pie(table(dane))

# szereg rozdzielczy przedziałowy (k – liczba przedziałów klasowych)
table(cut(dane,20))

# histogram liczebności
hist(dane, main="tytuł", xlab="etykieta osi OX")

# histogram liczebności
hist(dane, main="tytuł", xlab="etykieta osi OX", freq=FALSE)

# wykresy kołowe liczebności - ile części koła
pie(table(cut(dane,3)))

# maximum
max(dane)

# minimum
min(dane)

# zaokrąglenie w górę
ceiling(dane)

# średnia
mean(dane)

# dominanta - wartość występująca najczęśćiej

# pierwszy kwartyl to 25-kwantyl, drugi kwartyl lub mediana to 50-kwantyl, trzeci kwartyl to 75-kwantyl
quantile(dane, probs = 0.5)
quantile(dane)

# miary tendencji centralnej razem - najważniejsze dane
summary(dane)

# wariancja
var(dane)

# odchylenie standardowe
sd(dane)

# boxplot
boxplot(dane)

#Wpisywanie danych:
dane = read.csv("C:\\Users\\kolec\\Desktop\\Semestr 4\\Statystyka\\Wykład\\W2\\ozon.csv", sep = ";")

#Wyciągnie i stowrzy wektor oz z kolumny "ozon" z pliku, który wczytaliśmy i oznaczyliśmy "dane"
oz = dane$ozon

#Utworzy wykres z danych oz w przedziałach, które wyznaczyliśmy
przedziały = seq(0, 12, length = 6)
hist(oz, breaks=przedziały)

#Funkcja podłoga na każdej danej
floor(oz)

#Posortuje nam dane
sort(oz)

#Średnia z danych
mean(oz)

#25Kwantyl - 25% obserwacji przyjmujących wartość mniejszą lub równą wartości kwartyla pierwszego. 
#Pozostałe 75% przyjmuje wartości większe lub równe wartości kwartyla.

#50Kwantyl - mediana

#75Kwantyl - 75% obserwacji przyjmujących wartość mniejszą lub równą wartości kwartyla pierwszego. 
#Pozostałe 25% przyjmuje wartości większe lub równe wartości kwartyla.

#Aktualny katalog roboczy
getwd()

#Dane:
a <- c(3,3,4,4.5,4.5)
b <- c(2,3.5,4,4.5,5)

#W a mamy 2 dwie dominanty 3.0 i 4.5
#W w b nie mamy dominanty

#Śrenia ocen ze statystyki w gupie a wynosi 4
mean(a)

#interpretacja: 25 kwantyl: conajmiej 25% osób dosatło ocenę niewiększą niż 3.0 i conajmniej 
#75% otrzymało ocenę niemniejszą niż 3.0
quantile(a) 

#Przeciętnie odeny odchylają się od średniej o pół lub 1 stopień (wyszło odchylenie 0.75)
var(a) 
sd(a) 

#Miary rozproszenia (zmienności):
#1.Rozstęp:
R = max(a)-min(a)
#2.Rozstęp ćwiartkowy:
Rq = quantile(a, prob = 0.75) - quantile(a, prob = 0.25)
#3.Współczynnik zmienności:
v = sd(a)/mean(a)*100 
#Wniosek: dane są słabo zróźnicowane

#Boxploty - wykres pudełkowy zawiera
boxplot(a)
boxplot(b)
boxplot(a,b)
#To na dole kwantyl 1, to na góre kwantyl 3, linia pogrubiona do mediana, 
#jeśli nie ma wąsów to oznacza to, że nie ma wartości największej i najmnieszej,
#jeśli są to wartości największą i najmnieszą

#Wykres danych:
plot(table(dane)/length(dane))
plot(table(dane))

#Dzieli dane na 8 równych przedziałów i zlicza ilość wystąpień w danym przedziale
dane1 = c(1,2,3,4,5,3,2,2,5,5)
table(cut(dane1,8))

#Ilość wstąpień
hist(dane1, main = 'tytuł', xlab = 'etykieta osi OX')
#Gęstość danych
hist(dane1, main = 'tytuł', xlab = 'etykieta osi OX', freq = FALSE)

#Wykres kołowy podzielony na 5 równych części
pie(table(cut(dane1,5)))

#Funkcja sufit
ceiling(dane)

#Podsumowanie danych
summary(dane1)

#Kasuj znaczenie x
rm(x)

#WYKŁAD 2:

#ROZKŁAD DWUMIANOWY:
#dla pojedynczego aparatu możliwe są tylko dwa zdarzenia: zrobienie zdjęcia (’sukces’) lub awaria
#(’porażka’) – na drzewie probabilistycznym na każdym poziomie rysujemy tylko dwie gałęzie

#ZMIENNE LOSOWE - WYKŁAD 2

#Przykład 1
#W pewnym eksperymencie wykorzystano trzy automatyczne aparaty fotograficzne w celu
#dokumentowania jego przebiegu. W danych warunkach prawdopodobieństwo wykonania
#poprawnej fotografii dla każdego aparatu jest takie samo i wynosi p = 0, 6. Oblicz
#prawdopodobieństwo:

#a) nieudokumentowania eksperymentu, czyli F(0)
dbinom(0,3,0.6)
#rozkład prawdopodobieństwa
#0 zadziała, są 3 aparaty, każdy działa z prawdopodobieństwem 0.6

#dystrybuanta zmiennej losowej dyskretnej - pnazwa_rozkładu(x, parm)

#b) zarejestrowania eksperymentu przez co najmniej dwa aparaty, czyli dwa lub 3, odrzucamy 1, czyli P(X>1) = 1 − F(1)
1 - pbinom(1, 3, 0.6)

#Przykład 2
#W pewnym eksperymencie wykorzystano trzy automatyczne aparaty fotograficzne w celu
#dokumentowania jego przebiegu. W danych warunkach prawdopodobieństwo wykonania
#poprawnej fotografii dla każdego aparatu jest takie samo i wynosi p = 0,6. 
#Zdjęć zrobionych przez ile aparatów można się spodziewać (ile średnio aparatów udokumentuje eksperyment).

#Wartość oczekiwana - srednia liczba zdjęć jakie zrobiliśmy jak byśmy je robiliśmy w nieskończość, 
#średnio powinno byc tyle zdjęć. Ile powinniśmy się spowiedziewać poprawnych wyników
x=seq(0,3)

#Wektor rozkładu prawdopodobieńs
p=c(dbinom(0,3,0.6), dbinom(1,3,0.6), dbinom(2,3,0.6), dbinom(3,3,0.6))

#Prawdopodobieństwo sumue się do 1, wszytsko ok
sum(p)

#Zrobi nam tabelkę z danych x i p, macierz, najpierw wiersz z x, potem wiersz z p
rozkład = rbind(x,p)

#Funkcja do liczenia wartości oczekiwanej
expect=0
for(i in 1:4){
  expect = expect + rozkład[1,i]*rozkład[2,i]
}

#Sposób 2 - LEPSZY:
n=3
x=0:n
p=0.6

#Utworzenie tabelki:
prob = dbinom(x,n,p)
rbind(x,prob)

#Wartość oczekiwana:
expect=sum(x*prob)

#Wariancja:
variance=sum((x^2)*prob)-(expect^2)

#Odchylenie standardowe:
sd=sqrt(variance)

#FUNKCJE:
#name = nazwa rozkładu
#param = parametry rozkładu
#Gęstość: d (density) + name = dname(x, param)
#Dystrybuanta: p (probability) + name = pname(x, param)
#Kwantyl: q (quantile) + name = qname(α, param)
#Losowa obserwacja: r (random) + name = rname(N, param)
#dwumianowy: binom
#Poissona: pois
#Histogram rozkładu dyskretnego (wykres liniowy): plot(x, dname(x, param),type = "h")
x = seq(0,3)
parm = 0.6
size = 3
plot(x, dbinom(x, size, prob = prob), type = "h")

#Genreowanie danych - 5 różnych wyników liczb do 3, z rozkładem prawdopodobieństwa 0.6
n = 3
p = 0.6
rbinom(5,n,p)

#Prawdopodobieństwo - pole pod wykresem fukcji gęstości - pole pod krzywą równe 1, leży nad osią
#punkt nie ma pola - prawdopodobieństwo w pukcie równe 0

#rozkład wykładniczy - brak pamięci - zadanie z tańmą magnetofonową

#PRZYKŁAD:
#Czujnik śledzący stację wymaga dużej liczby wysokiej jakości taśm magnetycznych. Na
#taśmie magnetycznej mogą pojawić się rysy. Niech zmienna losowa X oznacza odległość
#(w cm) między kolejnymi rysami na powierzchni taśmy, a jej rozkład opisany jest funkcją gęstości

#Wykres fukcji:
curve(0.01*exp(-0.01*x),0,500)

#Fukcja gęstośi z zadania:
f=function(x){0.01*exp(-0.01*x)}

#Załóżmy, że została znaleziona pierwsza rysa na taśmie. Oblicz prawdopodobieństwo, że
#kolejna zostanie znaleziona na kolejnych 50 cm taśmy.

#P(X <= 50) - całka z pola pod wyrkresem od 0 do 50 z naszej funkcji:
wynik = integrate(f,0,50)

#Automatycznie policzone porawdopodobieństwo z rozkładu wykładniczego:
lam = 0.01
wynik = pexp(50, lam)

#Jaka przeciętnie odległość dzieli kolejne rysy na taśmie?
f=function(x){x*0.01*exp(-0.01*x)}
wynik = integrate(f, 0, Inf)
ex = wynik$value

#Wariancja:
f=function(x){x^2*0.01*exp(-0.01*x)}
dx = integrate(f, 0, Inf)
var = dx$value - ex^2

#ROZKŁAD NORMALNY:
#Niech X (w calach) będzie średnicą łożysk kulkowych produkowanych w pewnym zakładzie. 
#Wedząc, że X podlega rozkładowi normalnemu z wartością oczekiwaną 1 cal oraz
#odchyleniem standardowym 0,001 cala, tzn. X ∼ N(1, 0,001), oblicz prawdopodobieńswto, że średnica łożyska
#(a) nie przekracza 1,0015 cala;
#(b) przekracza 0,9995 cala;
#(c) znajduje się w przedziale od 0,9998 do 1,0004 cala.

#F(b) = pnorm(b, u, mi)
#b - to co liczymy
#mu - wartość oczekiwana
#sig - odchylenie standardowe

#a P(X<1.0015)
pnorm(1.0015, 1, 0.0001)

#b P(X>0.9995)=1-F(0.9995)
1-pnorm(0,995, 1, 0.0001)

#c P(0.9998 < X < 1.0004)=F(1.0004)-F(0.9998)
pnorm(1.0004, 1, 0.0001) - pnorm(0.9998, 1, 0.0001)

#Przykład - alkohol USA
a = pbinom(280, 1000, 0.3)
b = pnorm(280, 1000*0.3, sqrt(1000*0.3*0.7))

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

#LAB 2:
loty = read.csv("C:/Users/kolec/Desktop/Semestr 4/Statystyka/Lab/L2/loty.csv", sep = ";" )
oceny = read.csv("C:/Users/kolec/Desktop/Semestr 4/Statystyka/Lab/L2/oceny.csv", sep = ";", dec="," )
truskawki = read.csv("C:/Users/kolec/Desktop/Semestr 4/Statystyka/Lab/L2/truskawki.csv", sep = ";", dec="," )

#1. Wczytaj plik loty.csv zawierający dane dotyczące liczby pasażerów pewnej linii lotniczej w
#kolejnych miesiącach i latach, a następnie wykonaj polecenia:

#a) sprawdź, jakie wartości zawiera plik i jaki jest typ danych (class(dane))

class(loty)
#typ data frame - dane w postaci tabelarycznej

#b) wyznacz i zinterpretuj podstawowe miary statystyczne (średnia, mediana, pierwszy i
#trzeci kwartyl, odchylenie standardowe, współczynnik zmienności);

loty$X1956 #odwołanie do 2 kolumny
loty[,2] #odwołanie do 2 kolumny
nazwy = names(loty)#nazy kolumn w tabeli loty

#średnie z danych lat
for (i in 1:6){
  print("średnia w roku")
  print(nazwy[i])
  print(mean(loty[,i]))
}

#ŚREDNIA
srednia=mean(loty[,2])
#ŚREDNIA LICZBA PASAŻERÓW W 1956 WYNOSIŁA 328 OSÓB (ZAOKRĄGLENIE MATEAMTYCZNE)

#1 KWANTYL
q1=quantile(loty[,2])[2]
#(25% - 3 MIESIĄCE) W 3 MEISIĄCACH W ROKU 1956 BYŁA MNIEJSZA LUB RÓWNA 301 OSÓB 
#I W 9 MIESIĄCACH LICZBA PASAŻERÓW BYŁA WIĘKSZA BĄDŹ RÓWNA 301

#2 KWANTYL - MEDIANA
q2=quantile(loty[,2])[3]

#W 6 MIESIĄCACH LICZBA W ROKU 1956 LICZBA PASAŻERÓW BYŁA MNIESZA BĄDŹ RÓWNA 315
#I W POZOSTAŁYCH 6 MIESIĄCACH LICZBA PASAŻERÓW BYŁA WIĘKSZA BĄDŹ RÓWNA 315

#3 KWANTYL
q3=quantile(loty[,2])[4]
#(25% - 3 MIESIĄCE) W 9 MEISIĄCACH W ROKU 1956 BYŁA MNIEJSZA LUB RÓWNA 360 OSÓB 
#I W 3 MIESIĄCACH LICZBA PASAŻERÓW BYŁA WIĘKSZA BĄDŹ RÓWNA 360

#ODCHYLENIE STANDARDOWE
sd=sd(loty[,2])
#PRZECIĘTNIE LICZBA PASAŻERÓW ODCHYLA SIĘ OD ŚREDNIEJ O 48 OSÓB

#WSPÓŁCZYNNIK ZMIENNOŚCI
wz = ((sd(loty[,2]))/mean(loty[,2]))*100
#SŁABE ZRÓŻNICOWANIE LICZBY PASAŻERÓW W ROKU 1956

#c) narysuj histogramy liczebności dla danych z kolejnych lat; zautomatyzuj rysowanie za
# pomocą pętli „for”; zadeklaruj tytuły kolejnych histogramów odwołując się do etykiet
# danych; przedstaw wszystkie wykresy w jednym oknie;

#minimum
min(loty)

#maksimum
max(loty)

#przedziały
przedzialy=seq(200,650,length=10)

#kolory
kolory=c("red", "yellow", "pink", "blue", "green", "orange")

#Utwórz mi 2 wiersze na 3 kolumny miejsc na wykresy
par(mfrow=c(2,3))

for (i in 1:6){
  hist(loty[,i], main = paste('loty w ',nazwy[i]), xlab = "liczba pasażerów", breaks = przedzialy, col = kolory[i])
}

boxplot(loty[,1], loty[,2], loty[,3], loty[,4], loty[,5], loty[,6])


#ZAD2
#Wczytaj plik oceny.csv i wykonaj następujące polecenia:
#a) sprawdź typ danych wczytanych z pliku i zwróć uwagę na długości kolejnych zmiennych oraz sposób zapisu;
class(oceny)

#b) wczytaj dane ponownie, zamieniając przecinki na kropki (opcja dec=”,”);
oceny = read.csv("C:/Users/kolec/Desktop/Semestr 4/Statystyka/Lab/L2/oceny.csv", sep = ";", dec="," )

#c) wyznacz i zinterpretuj podstawowe miary statystyczne; w przypadku niepełnej
#długości danych posłuż się funkcją na.omit;

#apply odcina wszytskie kolumny w pustymi wartościami (równo ucina)
#omijanie NULLI w danych oceny i po kolumnach tutaj 2 liczy średnią
#apply działa od razu na wszyskie kolumny i zwraca wynik dla każdej kolumny

apply(na.omit(oceny), 2, mean) 
apply(na.omit(oceny), 2, quantile)

#na.omit - po czym będzie liczona średnia, bez NULLI, bo będzie bład
mean(na.omit(oceny[,2]))
mean(na.omit(oceny$grupa.M2))

#d) narysuj diagramy odcinkowe dla danych z kolejnych grup; zautomatyzuj rysowanie za
# pomocą pętli „for”; zadeklaruj tytuły kolejnych histogramów odwołując się do etykiet
# danych; wszystkie wykresy umieść w jednym oknie;
# UWAGA! Polecenie discrete.histogram należy do pakietu „arm”
par(mfrow=c(2,2))
grupy=names(oceny)

for (j in 1:4){
  title = paste("histogram", grupy[j])
  discrete.histogram(oceny[,j], freq = TRUE, main = title)
}

# e) porównaj dane z kolejnych lat za pomocą wykresów pudełkowych;
boxplot(oceny[,1], oceny[,2], oceny[,3], oceny[,4])

# f) sporządź szeregi rozdzielcze punktowe ocen w poszczególnych grupach (table);
table(oceny[,1])

# g) przedstaw dane z szeregów rozdzielczych na wykresach kołowych.
par(mfrow=c(2,2))
for (j in 1:4){
  title=paste("wykres kołowy", grupy[j])
  pie(table(oceny[,j]), main=title)
}

#3. Wczytaj plik truskawki.csv i wykonaj następujące polecenia:
# a) sprawdź typ danych wczytanych z pliku i zwrócić uwagę na długości kolejnych
# zmiennych; wyświetl dane zwracając uwagę na brakujące pomiary;
class(truskawki)

#b) wyznacz i zinterpretuj podstawowe miary statystyczne; w przypadku danych
# „plon2010” wykorzystaj funkcję na.omit;
summary(na.omit(truskawki$plon.2010))

# c) sporządź szeregi rozdzielcze przedziałowe plonów w poszczególnych latach (cut);
plon2000=truskawki$plon.2000
plon2010=na.omit(truskawki$plon.2010)
table(cut(plon2000, breaks = 4))
table(cut(plon2010, breaks = 4))

# d) przedstaw dane z szeregów rozdzielczych na wykresach kołowych;
par(mfrow=c(1,2))
lata = names(truskawki)
for (i in 1:2){
  title=paste("wykres kołowy", lata[i])
  pie(table((cut(truskawki[,i], breaks = 4))), main=title)
}

# e) narysuj histogramy probabilistyczne (freq=FALSE) dla plonów z kolejnych lat
#wykorzystując szeregi rozdzielcze z punktu (c); zautomatyzuj rysowanie za pomocą
#pętli „for”; zadeklaruj tytuły kolejnych histogramów odwołując się do etykiet danych;
#wszystkie wykresy przedstaw w jednym oknie;
plon2000=truskawki$plon.2000
plon2010=na.omit(truskawki$plon.2010)
table(cut(plon2000, breaks = 4))
table(cut(plon2010, breaks = 4))
nazwy = names(truskawki)
par(mfrow=c(1,2))

for (i in 1:2){
  title = paste("histogram", nazwy[i])
  discrete.histogram(table(cut(na.omit(truskawki[,i]), breaks = 4)), freq = FALSE, main = title)
}

# f) porównaj dane z kolejnych lat za pomocą wykresów pudełkowych.
boxplot(truskawki[,1], truskawki[,2])

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

#LAB4
dane = read.csv("C:/Users/kolec/Desktop/Semestr 4/Statystyka/Lab/L4/dane_est.csv", sep = ";", dec = "," )

#Funkcja do przedziału ufności
#Czy sigma jest znane
#Sigma - odchylenie
przedzial_ufnosci=function(srednia,odchylenie,sigma,liczebnosc,ufnosc){
  alfa=1-ufnosc
  Lt=srednia-qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Pt=srednia+qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Lz=srednia-qnorm(1-alfa/2)*(odchylenie/sqrt(liczebnosc))
  Pz=srednia+qnorm(1-alfa/2)*(odchylenie/sqrt(liczebnosc))
  return(
    if(liczebnosc<30){
      if(sigma==FALSE){print(paste("(",Lt,":",Pt,")"))}
      else {print(paste("(",Lz,":",Pz,")"))}
    }
    else{print(paste("(",Lz,":",Pz,")"))})
}


#W celu oceny nowego procesu produkcji syntetycznych diamentów sprawdzono wagę [karaty] diamentów wyprodukowanych 
#tą metodą uzyskując następujące wyniki:0,46 0,61 0,52 0,48 0,57 0,54 0,47 0,63 0,51 0,49 0,58 0,55.
#Przyjmijmy, że badana zmienna ma rozkład normalny.
#(a) Określ populację, próbę i badaną zmienną.
diamenty=na.omit(dane$diamenty)
#populacja - wszystkie sysntetyczne diamnety wypodukowanę nową metodą
#próba - 12 syntetyczneych diamentów wyprodukownaych nową metodą
#dana zmienna - waga syntetycznych diamentów wyprodukowanych nową metodą

#(b) Wyznacz oceny punktowe średniej, wariancji i odchylenia standardowego wagi diamentów produkowanych
#tą metodą. 0,534167; 0,00308; 0,0555073
liczebnosc = length(diamenty)
srednia = mean(diamenty)
wariancja = var(diamenty)
odchylenie = sd(diamenty)

#(c) Oszacuj z 95% pewnością średnią wagę wszystkich syntetycznych diamentów produkowanych badaną
#metodą (skonstruuj własną funkcję i porównaj wynik z wynikami odpowiedniej funkcji dostępnej w R).
#(0,498; 0,57)
ufnosc = 0.95
przedzial_ufnosci(srednia, odchylenie, FALSE, liczebnosc, ufnosc)
t = t.test(diamenty, conf.level = 0.95)
t$conf.int
#Z ufnością 95% przedział (0,498; 0,57) pokrywa nieznaną prawdziwą średnią populacyjną.

#(d) Zwiększ ufność z jaką chcemy wnioskować i porównaj długości uzyskanych przedziałów ufności.
ufnosc = 0.99
przedzial_ufnosci(srednia, odchylenie, FALSE, liczebnosc, ufnosc)
#Z ufnością 99% przedział (0.48440; 0.58393) pokrywa nieznaną prawdziwą średnią populacyjną.


#Agencja Ochrony Środowiska jest zaniepokojona ilością PCB – toksycznej substancji chemicznej – w mleku
#matek karmiących piersią. W próbie 20 kobiet poziom PCB (w liczbie cząsteczek na milion) był następujący:
#16, 0, 0, 2, 3, 6, 8, 2, 5, 0, 12, 10, 5, 7, 2 , 3, 8, 17, 9, 1.
#Załóżmy, że rozkład analizowanej zmiennej losowej jest normalny
#(a) Zdefiniuj populację, próbkę i badaną zmienną.
dane_est = read.csv("C:/Users/kolec/Desktop/Semestr 4/Statystyka/Lab/L4/dane_est.csv", sep = ";", dec = "," )
PCB = na.omit(dane_est$mleko)
#populacja - wszystkich kobiet karmiących piersią
#proba - u 20 kobiet karmiących piersią
#badana zmienna - poziom PCB w liczbie cząsteczek na milion

#(b) Oblicz szacunkowy średni poziom PCB w mleku wszystkich matek karmiących piersią. 5.8
srednia = mean(PCB)

#(c) Oszacuj wariancję i odchylenia standardowe poziomu PCB w mleku wszystkich matek karmiących piersią. 
#(25,85; 5.08)
wariancja = var(PCB)
odchylenie = sd(PCB)
liczebnosc = 20

#(d) Oceń z ufnością 95% średni poziom PCB w mleku wszystkich matek karmiących piersią. Zinterpretuj wynik. 
#(3,42; 8,18)
ufnosc = 0.95
przedzial_ufnosci(srednia, odchylenie, FALSE, liczebnosc, ufnosc)
t = t.test(PCB, conf.level = 0.95)
t$conf.int
#z ufnością 0.95 przedzial (3.42, 8.18) pokrywa nieznaną rzeczywistą średnią poziomu 
#PCB w mleku wszystkich matek karmiących piersią

#(e) Oceń z ufnością 95% wariancję i odchylenie standardowe poziomu PCB w mleku wszystkich matek
#karmiących piersią. Zinterpretuj wyniki. (14,95; 55,16); (3,86; 7,43)

chi2 = sigma.test(PCB, conf.level=0.95)
chi2 = chi2$conf.int

L_wariancja = chi2[1]
P_wariancja = chi2[2]
#z ufnością 0.95 przedzial (14.95, 55.15) pokrywa nieznaną prawdziwą wariancje dla populacji sigma^2
#średnia poziomu PCB w mleku WSZYTSKICH matek karmiących piersią

L_odchylenie= sqrt(ci2[[1]])
R_odchylenie = sqrt(ci2[[2]])
#z ufnością 0.95 przedzial (3.86, 7.43) pokrywa nieznaną prawdziwą wartość odchylenia standardowego dla popujacji sigma
#Wartość ochylenia standardowego zawartości PCB w mleku WSZYSTKICH matek karmiących piersią


#Aby oszacować średnią zawartość nikotyny w nowej marce papierosów, wybrano 15 paczek papierosów i
#zbadano w nich zawartość nikotyny otrzymując dane (w mg):
#1,87 2,28 1,77 2,13 1,43 1,64 2,38 1,39 1,94 2,68 1,95 0,86 1,98 1,69 1,15.
#Z wcześniejszych badań wiadomo, że rozkład zawartości nikotyny jest normalny z odchyleniem standardowym
#równym 0,7 mg. 
#(a) Oceń z ufnością 95% średnią zawartości nikotyny we wszystkich papierosach? (1,455; 2,164)
dane_est = read.csv("C:/Users/kolec/Desktop/Semestr 4/Statystyka/Lab/L4/dane_est.csv", sep = ";", dec = "," )
nikotyna = na.omit(dane_est$papierosy)
odchylenie = 0.7
ufnosc = 0.95
srednia = mean(nikotyna)
liczebnosc = length(nikotyna)

przedzial_ufnosci(srednia, odchylenie, TRUE, liczebnosc, ufnosc)
t = t.test(nikotyna, conf.level = 0.95)
t$conf.int
#z ufnością 0.95 przedzial (1.455 2.164) pokrywa nieznaną prawdziwą średnią populację 
#zawartość nikotytny we WSZYTSKICH papierosach

#(b) Jak duża próbka jest potrzebna, aby długość 95% przedziału ufności była nie większa niż 0,3 mg? 84
nie_większa = 0.3
alpha = 0.05
n = ((2*qnorm(1-alpha/2)*odchylenie)/nie_większa)^2
ceiling(n)

n = ((2*qnorm(1-0.05/2)*0.7)/0.3)^2
ceiling(n)

#c) Oblicz odchylenie standardowe z próby i porównaj wynik z podanym odchyleniem standardowym populacji
sd(nikotyna)

#Badacz zajmujący się możliwością zastosowania wodorostów do karmienia zwierząt badał zawartość białka w
#wodorostach. Wyniki 18 pomiarów z 50-kilogramowych próbek wodorostów przedstawiają się następująco:
#4,28 3,3 4,22 2,77 2,75 2,93 3,86 3,05 4,12 2,88 3,94 4,99 2,08 4,35 2,7 4,09 2,81 2,82
#Przyjmijmy, że zawartość białka w wodorostach ma rozkład normalny.
#(a) Oszacuj średnią i wariancję populacji.
wodorosty = na.omit(dane_est$wodorosty)
mean(wodorosty)
var(wodorosty)

#(b) Oceń z ufnością 90% prawdziwą średnią zawartość białka w 50-kilogramowych porcjach wodorostów.
#(3,115; 3,767)
ufnosc = 0.9
przedzial_ufnosci(mean(wodorosty), sd(wodorosty), FALSE, length(wodorosty), ufnosc)

t = t.test(wodorosty, conf.level = 0.90)
t$conf.int
#z ufnością 0.9 przedzial (3.115 3.767) pokrywa nieznaną prawdziwą średnią zawartość 
#białka w 50-kilogramowych porcjach wodorostów

#c) Oceń z ufnością 90% wariancję populacyjną badanej zmiennej. 
s = sigma.test(wodorosty, conf.level = 0.9)
s$conf.int
#z ufnością 0.9 przedzial (0.388, 1.235) pokrywa nieznaną prawdziwą wariancje dla populacji sigma^2
#zawartości białka w 50-kilogramowych porcjach wodorostów


#Załóżmy, że jeśli sygnał o natężeniu μ pochodzi z lokalizacji A, to natężenie zarejestrowane w lokalizacji B ma
#rozkład normalny ze średnią μ i odchyleniem standardowym 3. Oznacza to, że z powodu „szumu” zarejestrowane
#natężenie różni się od rzeczywistego natężenia sygnału o wielkość będącą zmienną losową o rozkładzie
#normalnym ze średnią 0 i odchyleniem standardowym 3. Aby zmniejszyć błąd, ten sam sygnał jest niezależnie
#rejestrowany 10 razy. Jeżeli kolejne zarejestrowane wartości to: 17, 21, 20, 18, 19, 22, 20, 21, 16, 19, oszacuj
#punktowo rzeczywiste natężenie sygnału μ, a następnie oceń je przedziałowo z ufnością 95%. Zinterpretuj wynik.
#19.3; (17.44; 21.16)
szum = c(17, 21, 20, 18, 19, 22, 20, 21, 16, 19)
mean(szum)
odchylenie = 3
n = 10
ufnosc = 0.95
przedzial_ufnosci(mean(szum), odchylenie, TRUE, length(szum), ufnosc)
#z ufnością 0.95 przedzial (17.44; 21.16) pokrywa nieznaną prawdziwą średnią populacyjną natężenia sygnału μ.

#Aby określić średni czas trwania połączenia telefonicznego realizowanego w godzinach południowych, operator
#telefoniczny wybrał losowo próbę 1200 takich połączeń. Obliczona średnia zmierzonego czasu trwania połączeń
#wynosi 4,7 minuty, a ich odchylenie standardowe to 2,2 minuty. Oszacuj z 95% ufnością średnią długość trwania
#wszystkich takich połączeń oraz ich odchylenie standardowe. Zinterpretuj wyniki. (4,57; 4,83); (2.11; 2.3)
n = 1200
srednia = 4.7
odchylenie = 2.2
ufnosc = 0.95
alpha = 0.05
przedzial_ufnosci(srednia, odchylenie, TRUE, n, ufnosc)

z = zsum.test(srednia, odchylenie, n, conf.level=0.95)
z$conf.int
#Z ufnością 95% przedział (4,57; 4,83) pokrywa nieznaną prawdziwą śrendnią długość trwania 
#wszystkich połączeń telefonicznych

L = sqrt(((n-1)*odchylenie^2)/qchisq(1-alpha/2, n-1))
P = sqrt(((n-1)*odchylenie^2)/qchisq(alpha/2, n-1))
#Z ufnością 95% przedział (2.11; 2.3) pokrywa nieznaną prawdziwą wartość odchylenia standardowego
#długość trwania wszystkich połączeń telefonicznych


#Zużycie wody w fabryce podlega losowym wahaniom w kolejnych dniach roku. Na podstawie 365 obserwacji
#stwierdzono, że średnie dzienne zużycie wynosi 102 hl, a wariancja 81 hl2
#(a) Przyjmując współczynnik ufności 0,98 oceń przedziałowo średnie dzienne zużycie wody w fabryce.
n = 365
srednia = 102
wariancja = 81
odchylenie = sqrt(wariancja)
ufnosc = 0.98
przedzial_ufnosci(srednia, odchylenie, TRUE, n, ufnosc)

z = zsum.test(srednia, odchylenie, n, conf.level=0.98)
z$conf.int
#Z ufnością 98% przedział  (100,9;103,1) pokrywa nieznaną prawdziwą śrendnie dzienne zurzycie wody w fabryce

#W następnym roku cena wody ma wzrosnąć. Produkcja będzie musiała być ograniczona, jeżeli średnie
#dzienne zużycie wyniesie co najmniej 122 hl. Czy na podstawie uzyskanego wyniku jest to prawdopodobna
#sytuacja? 
alpha = 0.02
sqrt(((n-1)*odchylenie^2)/qchisq(1-alpha/2, n-1))
sqrt(((n-1)*odchylenie^2)/qchisq(alpha/2, n-1))

sredniaL = 102+8.283504
sredniaP = 102+9.845168
#Jest to nie możliwe, ponieważ 122 nie mieści w przedziale unfości


#Inżynier chce ustalić wielkość próbki niezbędną do uzyskania zadanej precyzji w szacowaniu średniego czasu
#wiązania nowej mieszanki cementowej. Z dotychczasowych doświadczeń wiadomo, że czas wiązania mieszanki
#cementowej jest zmienną losową o rozkładzie normalnym i wariancji 25. Jaka powinna być liczebność
#próby, aby uzyskać 95% pewność, że błąd estymacji średniego czasu wiązania mieszanki nie przekroczy 1? 97
wariancja = 25
odchylenie = sqrt(wariancja)
ufnosc = 0.95
kw = ((qnorm(1-0.05/2)*odchylenie)/1)^2
ceiling(kw)
#liczebność próby, aby uzyskać 95% pewność, że błąd estymacji średniego czasu wiązania mieszanki 
#nie przekroczy 1 powinna mieć 97


#Z wcześniejszych doświadczeń wiadomo, że waga łososia hodowanego w wylęgarni komercyjnej jest zmienną
#losową o rozkładzie normalnym, przy czym średnia waga zmienia się w zależności od sezonu, ale odchylenie
#standardowe pozostaje stałe na poziomie 0,3 funta. Jeśli chcemy mieć 90% ufności, że oszacowana średnia waga
#łososia jest prawidłowa z dokładnością do ±0,1 funta, to jak dużą próbę należy pobrać? Jak zmieni się wynik,
#jeśli chcemy mieć 99% ufności? 25; 60
odchylenie = 0.3
ufnosc = 0.9
błąd = 0.1
alpha1 = 0.10

kw = ((qnorm(1-alpha1/2)*odchylenie)/0.1)^2
ceiling(kw)

alpha2 = 0.01
kw = ((qnorm(1-alpha2/2)*odchylenie)/0.1)^2
ceiling(kw)
#Próba powinna mieć od 25 do 60 łososi.


#Automat dozujący w browarze wymaga regulacji, gdy proporcja p niedopełnionych puszek wynosi 1,5% lub
#więcej. Ponieważ skontrolowanie zawartości puszki powoduje jej zniszczenie, nie ma możliwości wyznaczenia
#prawdziwej proporcji wszystkich niedopełnionych puszek. Dlatego co jakiś czas wybiera się próbę 100 puszek i
#sprawdza się ich zawartość. 
#W ostatnio pobranej próbie stwierdzono 4 niedopełnione puszki. Oceń z 95% ufnością rzeczywisty odsetek
#niedopełnionych puszek. Napisz własną funkcję wyznaczającą oceniającą proporcję niedopełnionych puszek, a
#następnie porównaj wynik z rezultatem funkcji binom.test i prop.test w R. Zinterpretuj wynik.
n = 100
T = 4
phat = T/n
alpha = 0.05

L = phat-qnorm(1-alpha/2)*sqrt(phat*(1-phat))/sqrt(n)
U = phat+qnorm(1-alpha/2)*sqrt(phat*(1-phat))/sqrt(n)
#Z ufnością 95% przedział (0,16%;7,84%) ten pokrywa nieznaną rzeczywitą proprocję 
#WSZYSTKICH niedopełnoinych puszek

b = binom.test(T, n, conf.level = 1- alpha)
b$conf.int

p = prop.test(T, n, conf.level = 1- alpha)
p$conf.int

x10 = c(p_hat - qnorm(1-alpha/2) * sqrt(p_hat*(1-p_hat)/n), p_hat + qnorm(1-alpha/2) * sqrt(p_hat*(1-p_hat)/n))


#Asystent inżyniera przemysłowego przeprowadził 120 przypadkowych obserwacji zespołu monterów tapicerek w
#zakładzie montażu samochodów. W 24 przypadkach zaobserwował, że pracownicy układali materiały poza
#swoim stanowiskiem pracy (co może stwarzać niebezpieczeństwo dla innych pracowników zakładu, a więc jest
#niezgodne z przepisami BHP). Oceń z ufnością 90% prawdziwy odsetek monterów nie przestrzegających
#wspomnianych przepisów BHP. Zinterpretuj wynik.
n = 120
T = 24
phat = T/n
ufnosc = 0.90

propCI = binom.test(70, 150, conf.level = 0.90)
propCI$conf.int
#Między 39.72911%, a 53.70432% pracowników nie przestzrega zasad.


#Badacz zainteresowany jest oszacowaniem frakcji osób mających problemy ze wzrokiem w danej grupie
#wiekowej. Ile osób należy zbadać, aby na poziomie ufności 98% uzyskać błąd oszacowania ±0,05 jeżeli:
#(a) z wcześniejszych doświadczeń wiadomo, że p wynosi 0,3.
ufnosc = 0.98
alpha = 0.02
p = 0.3
blad = 0.05

kw = ((qnorm(1-alpha/2)^2)*p*(1-p))/(blad^2)
ceiling(kw)
#Próba powinna mieć 455 osób.