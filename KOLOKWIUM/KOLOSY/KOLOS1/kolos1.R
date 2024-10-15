#α > p-wartość: odrzucamy hipotezę H0 (przyjmujemy H1)
#α < p-wartość: brak podstaw do odrzucenia H0
#α>p-wartość: na poziomie istotności α dane potwierdzają hipotezę badacza (H1)
#α<p-wartość: na poziomie istotności α dane nie potwierdzają hipotezy badacza (H1)

#ZAD1:
# H0: p <= 60%
# H1: p > 60% (Więcej niż 60% Polaków nie wie co oznacza termin GMO)

alfa = 0.03
# pierwsza liczba - ile udzieliło odpowiedzi na H1
# druga liczba - ile było wszystkich
# p - testowane prawdopodobieństwo
prop.test(140, 200, p = 0.6, alternative  = "greater", conf.level = 1 - alfa)

# alfa = 0.03 > p-value = 0.002442 - odrzucamy hipotezę H0 
# na poziomie istotności 0.03 dane potwierdzają hipotezę badacza, że więcej niż 60% Polaków nie wie co oznacza termin GMO.

#ZAD2:
dane = read.csv("C:\\Users\\kolec\\Desktop\\KOLOKWIUM\\KOLOSY\\KOLOS1\\ANOVA_magnez.csv", sep = ";", dec = ",")

muszynianka = na.omit(dane$Muszynianka)
muszynianka_plus = na.omit(dane$Muszynianka_plus)
piwnicznaka = na.omit(dane$Piwniczanka)
galicjanka = na.omit(dane$Galicjanka)

obiekty = rep(names(dane), c(length(muszynianka), length(muszynianka_plus), length(piwnicznaka), length(galicjanka)))
wyniki = c(muszynianka, muszynianka_plus, piwnicznaka, galicjanka)
Test = data.frame(obiekty,wyniki) 

#a)
# H0: mu1 = mu2 = mu3 = mu4 (Brak istotnych różnic między rzeczywistymi średnimi zawartościami magnezu w wodach mineralnych czterech rodzajów.)
# H1: ~H0 
anova(lm(wyniki~obiekty))

# alfa = 0.05 > p-value = 2.452e-14 - odrzucamy hipotezę H0 
# na poziomie istotności 0.05 odrzucamy hipotezę H0,
#stwierdzamy zatem, że sa istotne roznice miedzy rzeczywistymi srednimi
#zawartosciami magnezu w wodach mineralnych czterech rodzajow.

#b) Musi być w przedziale 0:
#Chcemy sprawdzic ktore grupy sa do siebie podobne (nie roznia sie istotnie).
TukeyHSD(aov(wyniki~obiekty))
#Grupy ktore nie roznia sie miedzy soba istotnie:
#Grupy jednorodne: P-G.

#ZAD3:
dane1 = read.csv("C:\\Users\\kolec\\Desktop\\KOLOKWIUM\\KOLOSY\\KOLOS1\\Reg_lody.csv", sep = ";", dec = ",")
cena_x = na.omit(dane1$cena)
lody_y = na.omit(dane1$sprzedaz)

# a)
cor(cena_x, lody_y) 
# współczynnik korelacji r = |-0.998| > 0.8, zatem
# istnieje bardzo silna zależność liniowa między ceną lodów, a ilością sprzedanych sztuk.

#b)
prosta = lm(lody_y ~ cena_x) 
wspolczynniki = coef(prosta)
rownanie = paste("y =", round(wspolczynniki[1], 2), "+", round(wspolczynniki[2], 2), "*x")
rownanie
# y = 157.81 + -19.54*x
# równanie regresji liniowej między ceną lodów, a ilością sprzedanych sztuk.

plot(cena_x, lody_y, main = "Wykres regresji liniowej", xlab = "cena", ylab = "liczba sprzedanych sztuk")
abline(prosta)

#c)
predict(prosta, data.frame(cena_x = 5.2))
# Jeżeli cena lodów wyniesie 5.2 zł, to liczba sztuk sprzedanych lodów w ciągu godziny wyniesie 56.

#ZAD4:
#H0: odsetek poszczegółnych kobiet o różnych włosach rozkłada się jak opisano w artykule.
#H1: ~H0
#Możliwości: BR, SZ, BL, IN
observedf = c(15, 12, 20, 13) #zaobserwowana częstość
expectedp = c(0.38, 0.32, 0.20, 0.10) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# alpha = 0.03 > 0.0002911 = p_value  ->  odrzucamy H0
# Na poziomie istotności 0.05 dane potwierdzają hipoteze, że rozkład częstotliwości poszczególnych kobiet NIE rozkłada się jak opisano w artykule.






