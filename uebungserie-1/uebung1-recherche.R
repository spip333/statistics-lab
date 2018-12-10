# Uebung 1 : Häufigkeitverteilung der Variablen Composition

library(MASS)
painters
head(painters)

col.composition <- painters$Composition
col.composition
frequency.composition <- table(col.composition)
frequency.composition

pie(frequency.composition)

# Uebung 2 : Bestimmen die Schule mit dem meisten Malern
col.schule <- painters$School
col.schule
schule.count <- table(col.schule)
schule.count
max.painters.in.school = max(schule.count)

a <- c(schule.count)
a[1]


which.max(schule.count)
schule.count[which.max(schule.count)]
x <- schule.count[2]
x.School

a <- c(1,7,3,5)


numbers <- c(4,23,4,23,5,43,54,56,657,67,67, 435, 453,435,324,34,456,56,567,65,34,435)
numbers
c(numbers)
n <- table(numbers)
n[names(n)==435]


a <- painters$School
b <- table(a)
b[names(b)=="A"]
b[1]
names(b)
b
str(b)
row.names(b)
b2 <- as.data.frame(b)
b2
names(b2)[1] <- "School"
b2[b2[1] == "A"]
b2[b2[2] == 10]
b2[2]
<subset(b2, b2[2] == max(b) )

max(b)

summary(painters$School)[2]

summary(painters)

factor(painters$School)

?table

is.table(b)


school <- painters$School
table.school <- table(school)
dfr.school <- as.data.frame(table.school)
names(dfr.school)[1] <- "School"
subset(dfr.school, dfr.school[2] == max(table.school))[1]


# Aufgabe: Relative Häu???gkeitsverteilung
# Bestimmen Sie die relative Häu???gkeitsverteilung der Variablen Composition

col.composition <- painters$Composition
col.composition
frequency.composition <- table(col.composition)
frequency.composition
rel.freq <- frequency.composition / length(col.composition)
rel.freq.in.percent <- round(rel.freq, 2)*100
print(rel.freq.in.percent)


### Aufgabe 1: Gruppenstatistik
# Problem: Finden Sie mit R die Schule mit dem höchsten Wert der Variable Composition.

head(painters)

col.composition <- painters$Composition
col.composition

table.school <- table(col.school)

dfr.school <- as.data.frame(table.school)
names(dfr.school)[1] <- "School"
subset(dfr.school, dfr.school[2] == max(table.school))[1]

max(col.composition)
which(col.composition == max(col.composition))
painters[37,]$School
painters[48,]$School
painters[which(col.composition == max(col.composition)),]$School

max.value.of.composition <- max(col.composition)
painters[which(col.composition == max(col.composition)),]$School

# Lösungsvorschlag 2 : 
# die Frage versteht sich wie folgt: "Welche ist die Schule,
# bei welcher die Summe aller Variablen Composition maximal ist

# 1 bestimme die summe der composition per schule
painters$School
aggregate(painters$Composition, by=list(School=painters$School), FUN=sum)
# ... und die Schule, be welcher die Summe der Compostion maximal ist :
composition.per.school <- aggregate(painters$Composition, by=list(School=painters$School), FUN=sum)
max.value.of.composition <- max(composition.per.school[2])
index.of.school.with.max.composition <- which(composition.per.school$x == max.value.of.composition)

composition.per.school[index.of.school.with.max.composition,]$School
$
  
  # Lösungsvorschlag 2 : 
  # die Frage versteht sich wie folgt: "Welche ist die Schule,
  # bei welcher die Summe aller Variablen Composition maximal ist
  # -> verwendung der aggregate Funktion
  
  
  # - 1. Bestimme die Gesamtssumme der composition per schule
  sum.composition.per.school <- aggregate(painters$Composition, by=list(School=painters$School), FUN=sum)

# - 2. Bestimme die maximale Gesamtssumme der composition per schule
max.value.of.composition <- max(sum.composition.per.school[2])

# - 3. Bestimme der Index der Schule mit der maximale Summe der Composition
index.of.school.with.max.composition <- which(sum.composition.per.school$x == max.value.of.composition)

# - 4. Die Schule, be welcher die Summe der Compostion maximal ist :
sum.composition.per.school[index.of.school.with.max.composition,]$School

# Lösungsvorschlag 3 : 
# die Frage versteht sich wie folgt: "Welche ist die Schule,
# bei welcher die Mittelwert der Composition maximal ist

# - 1. Bestimme den Mittelwert der composition per schule
mean.composition.per.school <- aggregate(painters$Composition, by=list(School=painters$School), FUN=mean)
mean.composition.per.school
# - 2. Bestimme die maximale Mittelwert der composition per schule
max.mean.composition <- max(mean.composition.per.school[2])
max.mean.composition
  
# - 3. Bestimme der Index der Schule mit der maximalen Mittelwert der Composition
index.of.school.with.max.mean.composition <- which(mean.composition.per.school$x >= max.mean.composition)
index.of.school.with.max.mean.composition

# - 4. Die Schule, be welcher die Summe der Compostion maximal ist :
mean.composition.per.school[index.of.school.with.max.mean.composition,]$School

#############################################################################333
### Aufgabe 2: Gruppenstatistik

# Bestimmen Sie mit R den Anteil aller Maler, deren Color-Wert mindestens 14 beträg
head(painters)
col.colour <- painters$Colour
index.of.painters.with.colour.greater.than.14 <- which(col.colour >= 14)
length(index.of.painters.with.colour.greater.than.14)

# 1. Betismme die Häufigkeit der Composition - Werten
col.composition <- painters$Composition
frequenz.composition <- table(col.composition)

# 2. Betismme die relative Häufigkeit der Composition - Verhältnis Auftritt / Gesamte Anzahl
rel.freq <- frequenz.composition / length(col.composition)

# 3. Ausgabe aller  relativen Häufigkeiten gerundet
print("Relative Häu???gkeitsverteilung der Variablen Composition: ")
round(rel.freq, 2)

#############################################################################
### Aufgabe: Balkendiagramm
# Stellen Sie die Häu???gkeitsverteilung der Variablen Composition mit einem Balkendiagramm dar.
barplot(rel.freq)

barplot(rel.freq, main="Häu???gkeitsverteilung der Variablen Composition",
        xlab="Composition",
        ylab="Häufigkeit")

#############################################################################
### Aufgabe: Kuchendiagramm
#  Stellen Sie die Häu???gkeitsverteilung der Variablen Composition mit einem Kuchendiagramm da
pie(rel.freq, col=rainbow(length(rel.freq)),  main="Häu???gkeitsverteilung der Variablen Composition")

pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")


############################################################################
# Uebung 2 : Bestimmen die Schule mit dem meisten Malern

# 1. Bestimme der Anzahl Malern per Schule
school.as.table = table(painters$School)
school.as.table

# 2. Bestimme, der Maximale Anzahl Malern per Schule
max.anzahl.malern.per.school <- max(school.as.table)

##
painters$School
##
# 3. Formattierung as frame
school.as.frame <- as.data.frame(school.as.table)
names(school.as.frame)[1] <- "Schule"

# 4. Auswählen der Schulen, bei welchen die Anzabl Malern gleich der maximal Anzahl Malern per Schule ist
schule.mit.meisten.malern <- subset(school.as.frame, school.as.frame[2] == max.anzahl.malern.per.school)[1]

# 5. Ausgabe des Resultates
print("Antwort 2: Schulen mit den Meisten Malern: ")
schule.mit.meisten.malern


### Arraya

# Create two vectors of different lengths.
vector1 <- c(1,2,3,4)
vector2 <- c(11,12,13,14,15,16)

# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(6,2))
print(result)

########### Matrices
M <- matrix(c(1:4), nrow = 4, byrow = TRUE)
print(M)

# Elements are arranged sequentially by column.
N <- matrix(c(11:14), nrow = 4, byrow = FALSE)
print(N)

# Define the column and row names.
rownames = c("row1", "row2", "row3", "row4")
colnames = c("col1", "col2")
V1 <- c(1,2,3,4)
V2 <- c(11,12,13,14)

P <- matrix(c(V1, V2), nrow = 4, byrow = FALSE, dimnames = list(rownames, colnames))
print(P)

P[1,2]
P[3,2]
