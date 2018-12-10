############################################################################
### Deskriptive Statistik - Uebungserie 5: Zusammenhang
###
### Nicolas Stern
### 17.11.2018
###
#############################################################################
# Uebungserie 6 - Trace
getwd()
setwd("C:/ieu/workspace/R/descriptive-statistik/")


# Aufgabe: Binomialverteilung
# Aufgabe: Die Wahrscheinlichkeit, dass man im Roulette bei einmaligem Setzen 
# auf „rot“ gewinnt, ist p = 18/37 = 0.486. 
# Deﬁnieren wir mit x jene Anzahl der Spiele,
# bei denen man bei fünfmaligem Setzenauf „rot“ gewinnt.
# - Wie gross ist bei fünfmaligem Setzen auf „rot“ die Wahrscheinlichkeit, 
# dass man öfter gewinnt als verliert?

# anz. male gewinnen > anz. Male verloren (auf 5 Spiele) => anzahl erfolge = 3, 4, oder 5
# prob fuer 3, 4, 5 male gewinnen:
dbinom(3, size=5, prob=18/37)
dbinom(3, size=5, prob=18/37) + dbinom(4, size=5, prob=18/37)
dbinom(3, size=5, prob=18/37) + dbinom(4, size=5, prob=18/37) + dbinom(5, size=5, prob=18/37)
pbinom(5, size=5, prob=18/37) - pbinom(2, size=5, prob=18/37)

# - Welche Anzahl der Gewinne wird in 90% der Fälle höchstens erreicht?
pbinom(0, size=5, prob=18/37)
pbinom(1, size=5, prob=18/37)
pbinom(2, size=5, prob=18/37)
pbinom(3, size=5, prob=18/37)
pbinom(4, size=5, prob=18/37)
barplot(pbinom(1:5, size=5, prob=18/37, lower.tail = T),names=1:5)
?qbinom
qbinom(1, size=5, prob=18/37)
qbinom(0., size=5, prob=18/37, lower.tail = F)
dbinom(3, size=5, prob=18/37)
dbinom(4, size=5, prob=18/37)
dbinom(5, size=5, prob=18/37)

barplot(dbinom(1:5, size=5, prob=18/37))
barplot(pbinom(1:5, size=5, prob=18/37))
vec <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
qbinom(vec, size = 5, prob=18/37)
qbinom(0.9, size = 1:5, prob=18/37)

qbinom(0.38,51,1/2)
pbinom(1:50, size = 50, prob=1/2)
pbinom(24, size = 51, prob=1/2)

x <- 0:5
y <-  pbinom(0:5, size=5, prob=18/37)
max <- which (mat[,2] > 0.9)
mat <- matrix(c(x, y), nrow = 6, byrow = FALSE)
max <- which (mat[,2] > 0.9)
mat[max, ]

# recherche
dbinom(50, size = 50, 0.5)

# recherche
pbinom(1, size= 7, prob=1/6)
barplot(pbinom(1:7, size= 7, prob=1/6))


barplot(dbinom(1:5, size=5, prob=18/37), names=1:5)
barplot(dbinom(1:5, size=5, prob=0.5), names=1:5)

barplot(pbinom(1:6, size=6, prob=1/6), names=1:6)


# recherche
# on achète un ticket pendant 4 semaines
# probabilité de gagner 2 X ?
# n: Anz. Erreignisse = 4
# p: Wahrscheinlichkeit fuer 1 gewinn : 
# x: Anz. Erfolge 2
d1 <- dbinom(x= 2, size = 4, prob = 0.003)
d1

6/49
n <- 1:50
barplot(pbinom(n, size = 50, prob=0.5), names=n)
?pbinom

a## Aufgabe: Hypergeometrische Verteilung
### Aufgabe
# Poker wird mit 52 Karten gespielt, jeweils 13 Karten der Farben ♥, ♦, ♣ und ♠. 
# Bei der Variante Five-Card-Stud erhält jeder Spieler 5 Karten zufällig zugeteilt.
# 1 Wie gross ist die Wahrscheinlichkeit, dass der Spieler ein Flush (5 Karten derselben Farbe) erhält?
# 2 Wie gross ist die Wahrscheinlichkeit, dass der Spieler vier gleiche Karten erhält?
dhyper()
choose (6, 4)*choose (14, 1) / choose(20, 5)
dhyper (x=4, m=6, n=14, k=5)

# q1 : formel. 
# N: Anzahl der Elemente in der Grundgesamtheit: 52
# M: Anzahl der Elemente, die für uns günstig sind : 13
# n: Grösse der Stichprobe : 5 
# k: Anzahl der Elemente aus M, die in n enthalten sind : 5
# Formel in R: choose (M, k) x choose (N-M, n-k) / choose(N,n)
choose (13, 5)*choose (39, 0) / choose(52, 5)
dhyper (x=5, m=13, n=39, k=5)
  

## Aufgabe: Poissonverteilung
## Aufgabe: Das Restaurant Fat’s Pizza führt Buch über die Anzahl an Gästen, 
# die das Restaurant betreten. Laut der Aufzeichnungen ist der Erwartungswert 
# µ = 12.1 zwischen 20:00 und 22:00 Uhr. 
# Bestimmen Sie mit der Poisson-Verteilung die Wahrscheinlichkeit, 
# dass zwischen 20 Uhr und 22 Uhr folgende Szenarien auftauchen:

barplot(dpois(0:20, lambda = 12))


#Es sind genau 8 Gäste im Restaurant.
dpois(8, lambda = 12)
#Es sind höchstens 10 Gäste im Restaurant.
ppois(10, lambda = 12, lower.tail = T )
#Es sind zwischen 9 und 15 Gäste im Restaurant.
ppois(15, lambda = 12, lower.tail = T) - dpois(9, lambda = 12)
#Es sind mindestens 11 Gäste anwesend.
ppois(11, lambda = 12, lower.tail = F)


# example: radioactive decay
# cf https://www.youtube.com/watch?v=jmqZG6roVqU
# avg. is 2.3 decays per second
# what is the probability of having 3 decays in any period of 2 seconds?
dpois(3, lambda = 4.6)
barplot(dpois(0:10, lambda = 4.6))
ppois(3, lambda = 4.6, lower.tail = T)

