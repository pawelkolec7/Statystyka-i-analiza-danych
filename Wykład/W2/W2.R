dane = read.csv("C:\\Users\\kolec\\Desktop\\Semestr 4\\Statystyka\\Wykład\\W2\\ozon.csv", sep = ";")

oz = dane$ozon

przedziały = seq(0, 12, length = 6)
hist(oz, breaks=przedziały)

floor(oz)
sort(oz)

mean(oz)

#25Kwantyl co najmiej 25 jest niemniejszych i 75 niewiększych
#Odrzuca z brzegów po 25% danych
#Interpretacja kwantyla np: kwantyl 50, 25, 75

getwd()

a <- c(4, 5, 7, 100, 101)
b <- c(21, 30, 37, 88)

quantile(a)
summary(a)

quantile(b)
summary(b)

#Wariancji nie interpretujemy
#Odchylenie standardowe - średnio coś waży 20g, ale może sie róźnić o jakąś ilość
#e przy 100g, napewno 100g, ale może być więcej

#Kwantyle
a <- c(3,3,4,4.5,4.5)
b <- c(2,3.5,4,4.5,5)

mean(a) #śrenia ocen ze statystyki w gupie a wynosi 4 !ładny opis i interpretacja!
quantile(a) #interpretacja: 25 kwantyl: conajmiej 25% osób dosatło ocenę niewiększą niż 3.5 i conajmniej 75% otrzymało ocenę niemniejszą niż 3.5

#Dominanta:
#W a mamy 2 dwie dominanty 3.0 i 4.5
#W w b nie mamy dominanty

Var(a) 
sd(a) #Przeciętnie odeny odchylają się od średniej o pół lub 1 stopień

R = max(a)-min(a)
Rq = quantile(a, prob = 0.75) - quantile(a, prob = 0.25)
v = sd(a)/mean(a)*100 #oceny są słabo zróźnicowany

#Boxploty - wykres pudełkowy zawiera
boxplot(a)
boxplot(b)
boxplo
#To na dole kwantyl 1, to na góre kwantyl 3, linia do mediana, nie ma wąsów oznacza, że nie ma wartości największej i najmnieszej
#Wąsy to wartość min i max

par(mfrow = c(1,2)) #ona na wykresy 1 w pion i dwa miejsca w poziomo
boxplot(a)
boxplot(b)
#nie można ich porównać bo mamy różne wartości

boxplot(a,b) #teraz można bo na jendym wykresie

#Reguła Czebryszewa - dane znajdują się w określonych przedziałach

#ZMIENNE LOSOWE - WYKŁAD 2
dbinom(0,3,0.6) 
#rozkład dwumianowy Od 0 do 3, prawdobieństwo 0.6 (dokładnie 0 zadziała)
#0 zadziała, są 3 aparaty, każdy działa z prawdopodobieństwem 0.6

#pnazwa_rozkładu(x, parm)
#rozkład dwumianowy suma prawdopodobieńswt do danej liczby
1 - pbinom(1, 3, 0.6)

x=seq(0,3)
p=c(dbinom(0,3,0.6), dbinom(1,3,0.6), dbinom(2,3,0.6), dbinom(3,3,0.6))
sum(p)
rozkład = rbind(x,p)

expect=0
for(i in 1:4){
  expect=expect+rozkład[1,i]*rozkład[2,i]
}