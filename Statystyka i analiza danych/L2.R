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
