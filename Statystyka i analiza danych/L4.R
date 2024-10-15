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