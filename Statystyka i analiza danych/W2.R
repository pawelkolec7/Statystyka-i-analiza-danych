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
