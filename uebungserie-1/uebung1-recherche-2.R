library(MASS)
painters
head(painters)


# Uebung 2 : Bestimmen die Schule mit dem meisten Malern

# 1. Bestimme der Anzahl Malern per Schule
school.as.table = table(painters$School)

v_ps <- painters$School

v_ps[A]
# 2. Bestimme, der Maximale Anzahl Malern per Schule
max.anzahl.malern.per.school <- max(school.as.table)

# 3. Formattierung as frame
school.as.frame <- data.frame(school.as.table)
names(school.as.frame)[1] <- "Schule"

# 4. Auswählen der Schulen, bei welchen die Anzabl Malern gleich der maximal Anzahl Malern per Schule ist
schule.mit.meisten.malern <- subset(school.as.frame, school.as.frame[2] == max.anzahl.malern.per.school)[1]

# 5. Ausgabe des Resultates
print("Antwort 2: Schulen mit den Meisten Malern: ")
schule.mit.meisten.malern

is.table(school.as.table)
?table

y <- table(1:10, 101:110)
y

head(mtcars)
