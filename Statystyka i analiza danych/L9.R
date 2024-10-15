# Zad1.
# H0: rozkład częstotliwości emerytów, którzy wróciili do pracy w hrabstwie Allegheny, 
# odpowiada ogólnemu rozkładowi podanemu przez stowarzyszenie Russela Reyolda.
# H1: ~H0

# Emeryci: ZIO, S, FK, W
observedf = c(122, 85, 76, 17) #zaobserwowana częstość
expectedp = c(0.38, 0.32, 0.23, 0.07) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# alpha = 0.1 < 0.35 = p_value  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.1 dane nie potwierdzają hipotezy, że rozkład częstotliwości emerytów, którzy wróciili do pracy w hrabstwie Allegheny, 
# NIE odpowiada ogólnemu rozkładowi podanemu przez stowarzyszenie Russela Reyolda.
# Zatem rozkład częstotliwości emerytów, którzy wróciili do pracy w hrabstwie Allegheny, 
# odpowiada ogólnemu rozkładowi podanemu przez stowarzyszenie Russela Reyolda.

#Zad2.
#H0: rozkład częstotliwości liczby zgonów związanych z bronią palną wśród osób w wieku od 1 do 18 lat rozkłada się jak opisano w artykule
#H1: ~H0
#Możliwości: W, Z, S
observedf = c(68, 27, 5) #zaobserwowana częstość
expectedp = c(0.74, 0.16, 0.10) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# alpha = 0.1 > 0.005 = p_value  ->  odrzucamy H0
# Na poziomie istotności 0.1 dane nie potwierdzają hipotezy, że rozkład częstotliwości liczby zgonów związanych z bronią palną wśród osób w 
# wieku od 1 do 18 lat NIE rozkłada się jak opisano w artykule

#Zad3.
# H0: rozkład częstotliwości mieszanki smakowej wynosi po 20% dla każdego smaku cukierków producenta Skittels.
# H1: ~H0
observedf = c(7+20+4+12, 20+5+16+9, 10+5+13+16, 7+13+21+3, 14+17+4+17) #zaobserwowana częstość
expectedp = c(0.2, 0.2, 0.2, 0.2, 0.2) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# alpha = 0.05 < 0.8369 = p_value  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.1 dane nie potwierdzają hipotezy, że mieszanka smakowa wynosi 20% dla każdego smaku.

#Zad4.
dane = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L9\\L\\normalnosc_ozon.csv", sep = ";", dec = ",")

# H0: stężenie ozonu MA rozkład normalny 
# H1: stężenie ozonu NIE MA rozkładu normalnego

pearson.test(dane$ozon, adjust = FALSE)
# alpha = 0.05 < 0.269 = p_val  ->  brak podstaw do odrzucenia H0
pearson.test(dane$ozon, adjust = TRUE)
# alpha = 0.05 < 0.146 = p_val  ->  brak podstaw do odrzucenia H0
lillie.test(dane$ozon)
# alpha = 0.05 < 0.2774 = p_val  ->  brak podstaw do odrzucenia H0
shapiro.test(dane$ozon) # <-- to najlepsze (jak są dane auu)
# alpha = 0.05 < 0.1098 = p_val  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 dane nie potwierdzają hipotezy, że stężenie ozonu NIE MA rozkładu normalnego.
# Zatem rozkład stężenia ozonu jest normalny.

#Zad6.
dane = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L9\\L\\normalnosc_punkty.csv", sep = ";", dec = ",")

# H0: stężenie ozonu MA rozkład normalny 
# H1: stężenie ozonu NIE MA rozkładu normalnego

shapiro.test(dane$punkty)
# alpha = 0.01 > 0.0006= p_val  ->  odrzucamy H0
# Na poziomie istotności 0.01 dane potwierdzają hipotezę, że liczba punków NIE MA rozkładu normalnego.

#Zad7.
# H0: miejsce zamieszkania danej osoby nie zależy od liczby lat studiów.
# H1: miejsce zamieszkania danej osoby zależy od liczby lat studiów
miej = c(15, 12, 8)
podmiej = c(8, 15, 9)
wiej = c(6, 8, 7)
MPW = data.frame(miej, podmiej, wiej)
chisq.test(MPW)
# alpha = 0.05 < 0.557 = p_val  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 dane nie potwierdzają hipotezy, że miejsce zamieszkania danej osoby zależy od liczby lat studiów.

#Zad8.
# H0: odsetek pasażerów, którzy zagubili bagaż w trakcie lotu, nie zależy od linii lotniczej
# H1: odsetek pasażerów, którzy zagubili bagaż w trakcie lotu, zależy od linii lotniczej
l1 = c(10, 90)
l2 = c(7, 93)
l3 = c(4, 96)
LLL = data.frame(l1, l2, l3)
chisq.test(LLL)
# alpha = 0.05< 0.251 = p_val  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 dane nie potwierdzają hipotezy, że odsetek pasażerów, którzy zagubili bagaż w trakcie lotu, zależy od linii lotniczej

#Zad9.
# H0: odczucia danej osoby w związku z ograniczeniami dotyczącymi anten satelitarnych nie są powiązane z jej wiekiem
# H1: odczucia danej osoby w związku z ograniczeniami dotyczącymi anten satelitarnych są powiązane z jej wiekiem
g1 = c(96, 201, 3)
g2 = c(96, 189, 15)
g3 = c(90, 195, 15)
g4 = c(36, 234, 30)
A = data.frame(g1, g2, g3, g4)
chisq.test(A)
# alpha = 0.05 > 0 = p_val  ->  odrzucamy H0
# Na poziomie istotności 0.05 dane potwierdzają hipotezy, że odczucia danej osoby w związku z ograniczeniami 
# dotyczącymi anten satelitarnych są powiązane z jej wiekiem