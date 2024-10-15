#KOLOS 2
#Zad4.
#H0: rozkład częstotliwości liczby zgonów związanych z bronią palną wśród osób w wieku od 1 do 18 lat rozkłada się jak opisano w artykule.
#H1: ~H0
#Możliwości: W, Z, S
observedf = c(67, 28, 6) #zaobserwowana częstość
expectedp = c(0.74, 0.16, 0.10) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# h = 1
# alpha = 0.01 > 0.003 = p_value  ->  odrzucamy H0
# Na poziomie istotności 0.1 dane potwierdzają hipoteze, że rozkład częstotliwości liczby zgonów związanych z bronią palną wśród osób w 
# wieku od 1 do 18 lat NIE rozkłada się jak opisano w artykule.

#Zad3.
odpady_x = c(120.8, 122.7, 124.6, 124.4, 132.2)
odpady_y = c(65.6, 66.9, 69.5, 80.1, 91.7)

# a)
cor(odpady_x, odpady_y) 
# współczynnik korelacji r = |0.9249135| > 0.8, zatem
# istnieje bardzo silna zależność liniowa między wytworzonymi odpadami, a wykorzystanymi wtórnie.

#b)
prosta = lm(odpady_y ~ odpady_x) 
wspolczynniki <- coef(prosta)
rownanie = paste("y =", round(wspolczynniki[1], 2), "+", round(wspolczynniki[2], 2), "*x")
rownanie
# y = y = -219.73 + 2.36*x
# równanie regresji liniowej między wytworzonymi odpadami, a wykorzystanymi wtórnie.
abline(prosta)

#c)
predict(prosta, data.frame(odpady_x=125))
# Jeżeli liczba wytworzonych odpadów wyniesie 125 mln ton, to wykorzystanych wynisie 74.9 mln ton.

#Zad2.
dane = read.csv("ANOVA_powietrze.csv", sep = ";", dec = ",")

#a)
stareMiasto = na.omit(dane$Stare_Miasto)
noweMiasto = na.omit(dane$Nowe_Miasto)
wilda = na.omit(dane$Wilda)
jezyce = na.omit(dane$Jezyce)
grunwald = na.omit(dane$Grunwald)

# H0: mu1 = mu2 = mu3  H1: ~H0
obiekty = rep(names(dane), c(length(stareMiasto), length(noweMiasto), length(wilda), length(jezyce), length(grunwald)))
wyniki = c(stareMiasto, noweMiasto, wilda, jezyce, grunwald)
powTest = data.frame(obiekty,wyniki) 

#b)
TukeyHSD(aov(wyniki~obiekty))
#grupy jednoronde: Jezyce-Grunwald, Nowe_Miasto-Grunwald, Stare_Miasto-Grunwald 
# -> Jezyce-Grunwald-Nowe_Miasto,  Jezyce-Grunwald-Stare_Miasto
  
#Zad1.
#Procedura testowa
#1. 
#H0: mu = 85 H1: mu =! 85%
#2.
alfa = 0.01
prop.test(360, 400, p = 0.85, alternative  = "two.sided", conf.level = 1 - alfa)
#3.
#p_value = 0.1521
#4.
#alfa = 0.01 > p_value = 0.006323 - odrzucamy H0
#Na poziomie istotności alpha = 0.01 dane nie potwierdzają hipotezy, że 85% opakowań nadaje się do recyklingu.

#KOLOS 3
#Zad4.
#H0: odsetek poszczegółnych kobiet o różnych włosach rozkłada się jak opisano w artykule.
#H1: ~H0
#Możliwości: BR, SZ, BL, IN
observedf = c(15, 12, 20, 13) #zaobserwowana częstość
expectedp = c(0.38, 0.32, 0.20, 0.10) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# h = 1
# alpha = 0.05 > 0.0002911 = p_value  ->  odrzucamy H0
# Na poziomie istotności 0.05 dane potwierdzają hipoteze, że rozkład częstotliwości poszczególnych kobiet różni się od przedstawionego w artykule.

#Zad3.
cena_x = c(4, 4.3, 4.5, 5, 5, 5.5)
lody_y = c(80, 73, 70, 61, 60, 50)

# a)
cor(cena_x, lody_y) 
# współczynnik korelacji r = |-0.998| > 0.8, zatem
# istnieje bardzo silna zależność liniowa między ceną lodów, a ilością sprzedanych sztuk.
#b)
prosta = lm(lody_y ~ cena_x) 
wspolczynniki <- coef(prosta)
rownanie = paste("y =", round(wspolczynniki[1], 2), "+", round(wspolczynniki[2], 2), "*x")
rownanie
# y = 157.81 + -19.54 *x
# równanie regresji liniowej między ceną lodów, a ilością sprzedanych sztuk.

plot(cena_x, lody_y, main = "Wykres regresji liniowej", xlab = "cena", ylab = "liczba sprzedanych sztuk")
abline(prosta)

#c)
predict(prosta, data.frame(cena_x = 5.2))
# Jeżeli liczba sprzedanych sztuk wyniesie 56.22, jeśli cena wyniesie 5.2

#Zad2.
dane = read.csv("ANOVA_magnez.csv", sep = ";", dec = ",")

#a)
muszynianka = na.omit(dane$Muszynianka)
muszynianka_plus = na.omit(dane$Muszynianka_plus)
piwnicznaka = na.omit(dane$Piwniczanka)
galicjanka = na.omit(dane$Galicjanka)


# H0: mu1 = mu2 = mu3  H1: ~H0
obiekty = rep(names(dane), c(length(muszynianka), length(muszynianka_plus), length(piwnicznaka), length(galicjanka)))
wyniki = c(muszynianka, muszynianka_plus, piwnicznaka, galicjanka)
powTest = data.frame(obiekty,wyniki) 
anova(lm(wyniki~obiekty))
#alfa = 0.05 > p-value = 0 -> odrzucamy H0
#na poziomie istotnosci 5% odrzucamy H0
#stwierdzamy zatem, że sa istotne roznice miedzy rzeczywistymi srednimi
#zawartosciami magnezu w wodach mineralnych czterech rodzajow.

#b)
#Chcemy sprawdzic ktore grupy sa do siebie podobne (nie roznia sie istotnie).
TukeyHSD(aov(wyniki~obiekty))
#Grupy ktore nie roznia sie miedzy soba istotnie:
#Grupy jednorodne.
#0 musi być w przedziale
#P-G

#Zad1.
#Procedura testowa
#1. 
#H0: p <= 60% H1: p > 60%
#2.
alfa = 0.05
prop.test(140, 200, p = 0.6, alternative  = "greater", conf.level = 1 - alfa)
#3.
#p_value = 0.002
#4.
#alfa = 0.01 > p_value = 0.002 - odrzucamy H0
#Na poziomie istotności alpha = 0.05 dane potwierdzają hipotezy, że 60% Polaków nie wie co oznacza termin GMO.
