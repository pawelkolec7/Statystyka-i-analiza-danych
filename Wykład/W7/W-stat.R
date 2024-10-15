#Tradycyjna
tt = 63
nt = 100
#Nowa
tn = 107
nn = 150

alpha = 0.05
z = qnorm(1-alpha/2)

phatn = tn/nn
phatt = tt/nt

#Nowa - tradycyjna
L = phatn - phatt - z*sqrt(phatn*(1-phat)/nn + pahtt*(1-phatt/nt))
U = L = phatn - phatt - z*sqrt(phatn*(1-phat)/nn + pahtt*(1-phatt/nt))