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
