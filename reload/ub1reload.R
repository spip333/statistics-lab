#################################################################
## Uebung 1 - reloaded
##
#################################################################
library(MASS)

# Aufgabe: Häuﬁgkeitsverteilung
#1 Bestimmen Sie die Häuﬁgkeitsverteilung der Variablen Composition.
str(painters)
head(painters)

painters$Composition

hist(painters$Composition)

comp.freq <- table(painters$Composition)
comp.freq

# n.b. : Contingency table 2 - dims:
a <- c(1,2,3,4,5,6)
b <- c(11,22,33,44,55,66)
tab <- table (a, b)
tab

#2 Bestimmen Sie mit R (d.h. nicht mit den eigenen Augen aus der Tabelle ablesen) diejenige Schule mit den meisten Malern.
sf <- table(painters$School)
sf
max(sf)

dfs <- data.frame(sf)
dfs

dfs[dfs[2] == max(sf)]

subset(dfs, dfs[2] == max(sf))[1]

#Aufgabe: Relative Häuﬁgkeitsverteilung
#Problem: Bestimmen Sie die relative Häuﬁgkeitsverteilung der Variablen Composition.
n <- sum(painters$Composition)
n

rel.freq1 <- painters$Composition / n
rel.freq1

rel.freq2 <- comp.freq / n
rel.freq2

#Aufgabe: Balkendiagramm
#Problem: Stellen Sie die Häuﬁgkeitsverteilung der Variablen Composition mit einem Balkendiagramm dar.
barplot(comp.freq)

#Aufgabe: Kuchendiagramm
#Problem: Stellen Sie die Häuﬁgkeitsverteilung der Variablen Composition mit einem Kuchendiagramm dar.
pie(comp.freq)

#Aufgabe 1: Gruppenstatistik
#Problem: Finden Sie mit R die Schule mit dem höchsten Wert der Variable Composition.
head(painters)

m <- max(painters$Composition)
m

painters[painters$Composition == m,]$School
# or
painters[which(painters$Composition == m),]$School

#Aufgabe 2: Gruppenstatistik
#Problem: Bestimmen Sie mit R den Anteil aller Maler, deren Color-Wert mindestens 14 beträgt.
painters[which(painters$Colour >= 14),]