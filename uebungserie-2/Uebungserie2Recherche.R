#############################################################################
### Deskriptive Statistik - Übungserie 2
###
### Nicolas Stern
### 02.11.2018
###
### Kommentar:
#############################################################################


# Übung1 : Bestimmen Sie die Häu???gkeitsverteilung der Wartezeiten aus faithful.
faithful
head(faithful)
faithful$waiting
freq <- table(faithful$waiting)
freq

# Ubung 2 : Welches Intervall der Eruptionsdauer enthält die meisten Eruptionen?
y <- data.frame(freq)
head(y)
max(y$Freq)
y[,y$Freq == max[y$Freq]]
y
max(y$Freq)
y[y$Freq == max(y$Freq), c("Var1")]

str(faithful)

x <- faithful[faithful$waiting == 78, c("waiting")]
x
length(x)

attributes(faithful)

head(swiss)
swiss[swiss$Catholic > 50, c("Fertility", "Catholic")]

barplot (faithful$waiting)
pie(faithful$waiting)

# Übung 3 : Bestimmen Sie die Häu???gkeitsverteilung der Eruptionszeiten aus faithful mit der Funktion hist.
# Histogramm demo
x <- c(1,2,2,2,3,4,5,1,5,1,5,6,6,3,2,4)        
y <- table(x)
hist(x)
y.relfreq <-  round(y/sum(y), 3)
y.relfreq
sum(y)
hist(x)
b <- c(0,1,2,3,4,5,6)
hist(x, breaks = b, freq=TRUE, include.lowest = TRUE, main = "Histogramm demo")

hist(faithful$eruptions)

### Aufgabe: Relative Häu???gkeitsverteilung stetiger Daten
# Problem: Bestimmen Sie die relative Häu???gkeitsverteilung der Wartezeiten aus faithful.
# Demo: relative häufigkeit: 
x <- c(1,2,2,2,3,4,5,1,5,1,5,6,6,3,2,4)        
y <- table(x)
y.relfreq <-  round(y/sum(y), 3)
y.relfreq
head(faithful$waiting)
waiting <- table(faithful$waiting)
sum(waiting)
waiting.relfreq <-  round(waiting/sum(waiting), 2)

# Aufgabe: Kumulierte Häu???gkeitsverteilung
# Problem: Bestimmen Sie die kumulierte Häu???gkeitsverteilung der Wartezeiten aus faithful.

# recherche
waiting <- faithful$waiting
length(waiting)
cbind(waiting)
max(waiting)
max(faithful$waiting)
min(faithful$waiting)
40:100
x <- c(1,1,1,2,2,3,3,3,4,4,4,4,4,5,5,6,6)
breaks <- seq(0, 10, by = 1)
s.cut <- cut(x, breaks, right=TRUE) 
table(s.cut)

# Lösung
waiting <- faithful$waiting
breaks <- seq(40,100, by=1)
waiting.cut <- cut(waiting, breaks, right=TRUE) 
waiting.freq <- table(waiting.cut)
waiting.cumfreq = cumsum(waiting.freq)
waiting.cumfreq 

### Aufgabe: Kumulierte Häu???gkeitsverteilungskurve
# Problem: Bestimmen Sie die kumulierte Häu???gkeitsverteilungskurve der Wartezeiten aus faithful.
breaks <- seq(43,96, by=1)
plot(breaks, waiting.cumfreq, type="l", main="Wartezeiten des Old Faithful Eruptions", xlab="Wartezeit (Minuten)",ylab="Kumulative Häufigkeit") 

# Aufgabe: Kumulierte relative Häu???gkeitsverteilung
# Problem: Bestimmen Sie die kumulierte relative Häu???gkeitsverteilung der Wartezeiten aus faithful
# recherche

# Lösung
waiting <- faithful$waiting
breaks <- seq(40,100, by=1)
waiting.cut <- cut(waiting, breaks, right=TRUE) 
waiting.freq <- table(waiting.cut)
waiting.cumfreq = cumsum(waiting.freq / sum(waiting.freq))
waiting.cumfreq 


# Lösung
waiting <- faithful$waiting
breaks <- seq(40,100, by=1)
waiting.cut <- cut(waiting, breaks, right=TRUE) 
waiting.freq <- table(waiting.cut)
waiting.cumfreq = cumsum(waiting.freq / sum(waiting.freq))
waiting.cumfreq 

# Aufgabe: Kumulierte relative Häu???gkeitsverteilung
# Problem: Bestimmen Sie die kumulierte relative Häu???gkeitsverteilung der Wartezeiten aus faithful
waiting.cumrelfreq = cumsum(waiting.freq / sum(waiting.freq))
print("Kumulierte relative Häu???gkeitsverteilung der Wartezeiten aus faithful:")
waiting.cumrelfreq 

# Aufgabe: Kumulierte relative Häu???gkeitskurve
# Problem: Bestimmen Sie die kumulierte relative Häu???gkeitsverteilungskurve der Wartezeiten aus faithful.
plot(breaks, waiting.cumrelfreq, type="l", main="kumulierte relative Häu???gkeit der Wartezeiten des Old Faithful", xlab="Wartezeit (Minuten)",ylab="Kumulierte relative Häufigkeit") 

# Aufgabe: Histogramm
# Problem: Die Datei "Daten_Wachstum.RData" enthält die Ergebnisse
# einer Umfrage unter 100 Jungunternehmern. 
# Erstellen Sie ein Histogramm, das die Altersverteilung in der Stichprobe darstellt. 
# Die Klassenbreiten sollen 3 Jahre betragen, 
# das erste Intervall soll bei 15 Jahren beginnen 
# und die rechte Intervallgrenze soll nicht zum Intervall gehören
# Daten laden
Daten_Wachstum
head(Daten_Wachstum)
max(Daten_Wachstum$Alter)
min(Daten_Wachstum$Alter)
breaks <- seq(15, 53, by=3)
breaks
hist(Daten_Wachstum$Alter, breaks, include.lowest = TRUE, xaxis)
?hist
