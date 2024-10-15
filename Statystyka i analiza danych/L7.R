chomiki = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_chomiki.csv", sep = ";", dec = ",")
cisnienie = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_cisnienie.csv", sep = ";", dec = ",")
kopalnie = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_kopalnie.csv", sep = ";", dec = ",")
mikrometr = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_mikrometr.csv", sep = ";", dec = ",")
pulapki = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_pulapki.csv", sep = ";", dec = ",")
sportowcy = read.csv("C:\\Users\\kolec\\Desktop\\Statystyka\\L7\\L7\\Anova_sportowcy.csv", sep = ";", dec = ",")

#Zad1.
obiekty = rep(names(cisnienie), each = length(cisnienie$Niskie))
wyniki = c(cisnienie$Niskie, cisnienie$Srednie, cisnienie$Silne, cisnienie$BardzoSilne)
cisnienieTest = data.frame(obiekty,wyniki) 

# Średnie próbkowe.
srednie = sapply(split(cisnienieTest$wyniki, cisnienieTest$obiekty), mean)

#H0: sig1^2 = sig2^2 = sig3^2 = sig4^2    H1: ~H0.
bartlett.test(wyniki~obiekty, cisnienieTest) 
#p_value = 0.5009

# 0.05 = alpha < p_val = 0.5  ->  brak podstaw od odrzucenia H0.
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy o jednorodności wariancji.
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE.

# H0: mu1 = mu2 = mu3 = mu4   H1: ~H0 
anova(lm(wyniki~obiekty)) 
# p_value = 0.09735
# 0.05 = alpha < p_val = 0.09735   -> brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 nie mamy podstaw do odrzucenia hipotezy H0, zatem ciśnienie nie ma wpływ na wielkość produkcji.

#Zad2.

obiekty = rep(names(kopalnie), each = length(kopalnie$K1))
wyniki = c(kopalnie$K1, kopalnie$K2, kopalnie$K3, kopalnie$K4, kopalnie$K5)
kopalnieTest = data.frame(obiekty,wyniki) 

# Średnie próbkowe.
srednie = sapply(split(kopalnieTest$wyniki, kopalnieTest$obiekty), mean)

#H0: sig1^2 = sig2^2 = sig3^2 = sig4^2    H1: ~H0.
bartlett.test(wyniki~obiekty, kopalnieTest) 
#p_value = 0.03

# 0.01 = alpha < p_val = 0.03  ->  brak podstaw od odrzucenia H0.
# Na poziomie istotności 0.01 nie ma podstaw do odrzucenia hipotezy o jednorodności wariancji.
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE.

# H0: mu1 = mu2 = mu3 = mu4   H1: ~H0 
anova(lm(wyniki~obiekty)) 
# p_value = 0.46
# 0.05 = alpha < p_val = 0.46  -> brak podstaw do odrzucenia H0
# Na poziomie istotności 0.01 nie mamy podstaw do odrzucenia hipotezy H0, zatem zawartości popiołu dla ekogroszku produkowanego w pięciu kopalniach 
# można uznać za jednakowe.

#Zad3.
mikrometr1 = na.omit(mikrometr$mikrometrI)
mikrometr2 = na.omit(mikrometr$mikrometrII)
mikrometr3 = na.omit(mikrometr$mikrometrIII)
# H0: mu1 = mu2 = mu3  H1: ~H0
obiekty = rep(names(mikrometr), c(length(mikrometr1), length(mikrometr2), length(mikrometr3)))
wyniki = c(mikrometr1, mikrometr2, mikrometr3)

anova(lm(wyniki~obiekty)) 
#p_value = 0.069
# alpha = 0.05 < p_value = 0.069 ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 nie mamy podstaw do odrzucenia hipotezy H0, zatem wybór mikrometru nie ma wpływu na uzyskane wyniki.

#Zad4.
obiekty = rep(names(sportowcy), each = length(sportowcy$Niepalacy))
wyniki = c(sportowcy$Niepalacy, sportowcy$Lekkopalacy, sportowcy$Sredniopalacy, sportowcy$Duzopalacy)

# a)  H0: sig1^2 = sig2^2 = sig3^2 = sig4^2    H1: ~H0
bartlett.test(wyniki~obiekty) 
#p_value = 0.8517
# alpha = 0.01 < p_val  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.01 nie ma podstaw do odrzucenia hipotezy o jednorodności wariancji, 
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE

# H0: mu1 = mu2 = mu3 = mu4  H1: ~H0
anova(lm(wyniki~obiekty)) 
#p_value = 0.003979
# alpha = 0.01 > p_val  ->  odrzucamy H0
# Na poziomie istotności 0.05 odrzucamy hipotezy H0, zatem palenie papierosów może wpływać na rytm zatokowy serca.

# b) Chcemy sprawdzić, któe grupy są do siebie podobne (nie różnią się istotnie)
TukeyHSD(aov(wyniki~obiekty), ordered = T)
# Grupy jednorodne:(ujemne lwr)
# Sredniopalacy-Niepalacy, Duzopalacy-Niepalacy, Duzopalacy-Sredniopalacy, Lekkopalacy-Duzopalacy
# => Sredniopalacy-Niepalacy-Duzopalacy, Lekkopalacy-Duzopalacy
plot(TukeyHSD(aov(wyniki~obiekty), ordered = T))

#Zad5.
chomiki1 = na.omit(chomiki$I)
chomiki2 = na.omit(chomiki$II)
chomiki3 = na.omit(chomiki$III)
chomiki4 = na.omit(chomiki$IV)

obiekty = rep(names(chomiki), c(length(chomiki1), length(chomiki2), length(chomiki3), length(chomiki4)))
wyniki = c(chomiki1, chomiki2, chomiki3, chomiki4)

# a)  H0: sig1^2 = sig2^2 = sig3^2 = sig4^2    H1: ~H0
bartlett.test(wyniki~obiekty) 
#p_value = 0.213
# alpha = 0.05 < p_val  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy o jednorodności wariancji, 
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE

# H0: mu1 = mu2 = mu3 = mu4 H1: ~H0
anova(lm(wyniki~obiekty)) 
#p_value = 0.024
# alpha = 0.05 > p_value = 0.024 -> odrzucamy H0
# Na poziomie istotności 0.05 odrzucamy hipotezy H0, masa gruczołu tarczycowego zależy od poziomu inbredu.

# b) Chcemy sprawdzić, któe grupy są do siebie podobne (nie różnią się istotnie)
TukeyHSD(aov(wyniki~obiekty), ordered = T)
# Grupy jednorodne:(ujemne lwr)
# II-I, III-I, III-II, IV-II, IV-III
# => I-II-III, II-III-IV
plot(TukeyHSD(aov(wyniki~obiekty), ordered = T))

#Zad6.
obiekty = rep(names(pulapki), each = length(pulapki$rozsiany))
wyniki = c(pulapki$rozsiany, pulapki$skoncentrowany, pulapki$roslina.zywicielka, pulapki$powietrzny, pulapki$gruntowy)
Test = data.frame(obiekty,wyniki) 

# Średnie próbkowe.
srednie = sapply(split(Test$wyniki, Test$obiekty), mean)

#H0: sig1^2 = sig2^2 = sig3^2 = sig4^2    H1: ~H0.
bartlett.test(wyniki~obiekty, Test) 
#p_value = 0.07
# alpha = 0.05 < p_val = 0.07  ->  brak podstaw do odrzucenia H0
# Na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy o jednorodności wariancji, 
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE

# H0: mu1 = mu2 = mu3 = mu4 H1: ~H0
anova(lm(wyniki~obiekty)) 
#p_value = 0
# alpha = 0.05 > p_value = 0 -> odrzucamy H0
# Na poziomie istotności 0.05 odrzucamy hipotezy H0, strategia lokalizacji może mieć wpływ na liczbę uwięzionych ciem cygańskich.

# b) Chcemy sprawdzić, któe grupy są do siebie podobne (nie różnią się istotnie)
TukeyHSD(aov(wyniki~obiekty), ordered = T)
# Grupy jednorodne:(ujemne lwr)
# rozsiany-gruntowy, skoncentrowany-roslina.zywicielka, powietrzny-roslina.zywicielka, powietrzny-skoncentrowany   
# => powietrzny-roslina.zywicielka-skoncentrowany, rozsiany-gruntowy
plot(TukeyHSD(aov(wyniki~obiekty), ordered = T))
