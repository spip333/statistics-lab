#############################################################################
### Deskriptive Statistik - Aufgabenserie 1: Qualitative Daten 
###
### Nicolas Stern
### 27.10.2018
###
### Kommentar:
### - Diese Datei als R-Skript laufen lassen.
### - Antwort zu den Übungen werden in der Console von R-Studio herausgegeben.
#############################################################################


library(MASS)

#############################################################################
### Aufgabe: Häufigkeitsverteilung

# Uebung 1 : Bestimmen Sie die Häufigkeitsverteilung der Variablen Composition
print("Antwort 1: Häufigkeitsverteilung der Variablen Composition: ")
table(painters$Composition)

# Uebung 2 : Bestimmen die Schule mit dem meisten Malern

# 1. Bestimme der Anzahl Malern per Schule
school.as.table = table(painters$School)

# 2. Bestimme, der Maximale Anzahl Malern per Schule
max.anzahl.malern.per.school <- max(school.as.table)

# 3. Formattierung as frame
school.as.frame <- as.data.frame(school.as.table)
names(school.as.frame)[1] <- "Schule"

# 4. Auswählen der Schulen, bei welchen die Anzabl Malern gleich der maximal Anzahl Malern per Schule ist
schule.mit.meisten.malern <- subset(school.as.frame, school.as.frame[2] == max.anzahl.malern.per.school)[1]

# 5. Ausgabe des Resultates
print("Antwort 2: Schulen mit den Meisten Malern: ")
schule.mit.meisten.malern

#############################################################################
### Aufgabe: Relative Häufigkeitsverteilung
# Bestimmen Sie die relative Häufigkeitsverteilung der Variablen Composition

# 1. Betismme die Häufigkeit der Composition - Werten
col.composition <- painters$Composition
frequenz.composition <- table(col.composition)

# 2. Betismme die relative Häufigkeit der Composition - Verhältnis Auftritt / Gesamte Anzahl
rel.freq <- frequenz.composition / length(col.composition)

# 3. Ausgabe aller  relativen Häufigkeiten gerundet
print("Antwort 3: Relative Häufigkeitsverteilung der Variablen Composition: ")
round(rel.freq, 2)

#############################################################################
### Aufgabe: Balkendiagramm
# Stellen Sie die Häufigkeitsverteilung der Variablen Composition mit einem Balkendiagramm dar.
barplot(rel.freq, main="Häufigkeitsverteilung der Variablen Composition",
        xlab="Composition",
        ylab="Häufigkeit")

#############################################################################
### Aufgabe: Kuchendiagramm
#  Stellen Sie die Häufigkeitsverteilung der Variablen Composition mit einem Kuchendiagramm da
pie(rel.freq, col=rainbow(length(rel.freq)),  main="Häufigkeitsverteilung der Variablen Composition")

#############################################################################
### Aufgabe 1: Gruppenstatistik
# Problem: Finden Sie mit R die Schule mit dem höchsten Wert der Variable Composition.
#
# Hier stelle ich 3 Lösungsvorschläge vor:

# Lösungsvorschlag 1 : 
# Hier interpretiere ich die Frage wie folgt: 
# "Welche ist die Schule, bei welcher der höchste Wert der Variablen Composition auftritt"
max.value.of.composition <- max(col.composition)

print("Antwort Aufgabe 1 Gruppenstatistik (Var. 1): Schulen mit dem höchsten Wert der Variable Composition: ")
painters[which(col.composition == max.value.of.composition),]$School

# Lösungsvorschlag 2 : 
# Diesmal interpretiere ich die Frage wie folgt: 
# "Welche ist die Schule, bei welcher die Summe der Composition maximal ist"

# 1. Bestimme die Gesamtssumme der composition per schule -> verwendung der aggregate Funktion
sum.composition.per.school <- aggregate(painters$Composition, by=list(School=painters$School), FUN=sum)

# 2. Bestimme die maximale Gesamtssumme der composition per schule
max.value.of.composition <- max(sum.composition.per.school[2])

# 3. Bestimme der Index der Schule mit der maximale Summe der Composition
index.of.school.with.max.composition <- which(sum.composition.per.school$x == max.value.of.composition)

# 4. Die Schule, be welcher die Summe der Compostion maximal ist :
print("Antwort Aufgabe 1 Gruppenstatistik (Var. 2): Schule mit dem höchsten Summe der Variable Composition: ")
sum.composition.per.school[index.of.school.with.max.composition,]$School

# Lösungsvorschlag 3 : 
# Diesmal interpretiere ich die Frage wie folgt: 
# "Welche ist die Schule bei welcher die Mittelwert der Composition maximal ist"

# 1. Bestimme den Mittelwert der composition per schule
mean.composition.per.school <- aggregate(painters$Composition, by=list(School=painters$School), FUN=mean)

# 2. Bestimme die maximale Mittelwert der composition per schule
max.mean.composition <- max(mean.composition.per.school[2])

# 3. Bestimme der Index der Schule mit der maximalen Mittelwert der Composition
index.of.school.with.max.mean.composition <- which(mean.composition.per.school$x >= max.mean.composition)

# 4. Ausbabe der Schule, bei welcher der Mitelwert der Compostion maximal ist :
print("Antwort Aufgabe 1 Gruppenstatistik (Var. 3): Schule mit dem höchsten Mittlewert der Variable Composition: ")
mean.composition.per.school[index.of.school.with.max.mean.composition,]$School

#############################################################################
### Aufgabe 2: Gruppenstatistik

# Bestimmen Sie mit R den Anteil aller Maler, deren Color-Wert mindestens 14 beträg

# - 1. Bestimme welche painters haben einen Colour - Wert > 14
col.colour <- painters$Colour
index.of.painters.with.colour.greater.than.14 <- which(col.colour >= 14)

# - 2. Bestimme der Anteil der painters haben einen Colour - Wert > 14
anteil.painters.with.colour.greater.14 <- length(index.of.painters.with.colour.greater.than.14) / length(col.colour)

# - 3. Ausbabe der Anteil gerundet
print("Antwort Aufgabe 2 Gruppenstatistik: Anteil aller Maler, deren Colour - Wert mindestens 14 beträgt :  ")
round(anteil.painters.with.colour.greater.14, 2)
