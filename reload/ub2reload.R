#################################################################
## Uebung 2 - reloaded
##
#################################################################

# Aufgabe: Häuﬁgkeitsverteilung quantitativer Daten
#1 Bestimmen Sie die Häuﬁgkeitsverteilung der Wartezeiten aus faithful.
#2 Welches Intervall der Eruptionsdauer enthält die meisten Eruptionen?
#3 Bestimmen Sie die Häuﬁgkeitsverteilung der Eruptionszeiten aus faithful mit der Funktion hist.

head(faithful)

#1. Häufigkeitsverteilung

# Schritt 1: Spannweite bestimmen
waiting = faithful$waiting 
range(waiting)

# Schritt 2: Spannweite runden und aufteilen 
breaks = seq(40,100,by=5)
breaks

# Schritt 3: Werte in die Intervalle aufteilen 
waiting.cut = cut(waiting, breaks, right=FALSE)
waiting.cut

# Schritt 4: Häufigkeiten bestimmen 
waiting.freq = table(waiting.cut)
# Schritt 5: Ergebnis formatieren 
head(cbind(waiting.freq),7)

#2. Welches Intervall der Eruptionsdauer enthält die meisten Eruptionen?

# Schritt 1: Häufigkeitsverteilung von duration
duration = faithful$eruptions 
breaks = seq(1.5,5.5,by=0.5) 
duration.cut = cut(duration, breaks, right=FALSE) 
duration.freq = table(duration.cut)

# Schritt 2: Maximum in duration.freq finden 
duration.freq.max = max(duration.freq)
duration.freq.max

# Schritt 3: Welches Intervall enthält das Maximum? 
x = which(duration.freq == duration.freq.max) 
names(x)
x

#3 Bestimmen Sie die Häuﬁgkeitsverteilung der Eruptionszeiten aus faithful mit der Funktion hist.
# Schritt 1
duration = faithful$eruptions 
breaks = seq(1.5, 5.5, by=0.5) 
h = hist(duration, breaks, right=FALSE, plot=FALSE)
duration.freq = h$counts
duration.freq

# Schritt 2: Bezeichnungen für die Intervalle 
len = length(h$breaks)
a = h$breaks[1:len-1] # rechte Seiten
b = h$breaks[2:len] # linke Seiten 
labels = paste("[", a, ", ", b, ")", sep="")
# Schritt 3: names von duration.freq bestimmen und formatieren 
names(duration.freq) = labels 
head(cbind(duration.freq),3)

waiting = faithful$waiting 
hist(waiting, right=FALSE)

#Aufgabe: Relative Häuﬁgkeitsverteilung stetiger Daten
#Problem: Bestimmen Sie die relative Häuﬁgkeitsverteilung der Wartezeiten aus faithful

# relative Häufigkeiten
n <- sum(cbind(table(faithful$waiting)))
cbind(table(faithful$waiting))/n

waiting = faithful$waiting 
breaks = seq(40, 100, by=5) 
waiting.cut = cut(waiting, breaks, right=FALSE) 
waiting.freq = table(waiting.cut) 
waiting.relfreq = waiting.freq/nrow(faithful) 
waiting.percentage = 100*waiting.relfreq 
old = options(digits=1) 
head(cbind(waiting.freq, waiting.percentage),2)

# Aufgabe: Kumulierte Häuﬁgkeitsverteilung
# Problem: Bestimmen Sie die kumulierte Häuﬁgkeitsverteilung der Wartezeiten aus faithful.
waiting = faithful$waiting 
breaks = seq(40, 100, by=5) 
waiting.cut = cut(waiting, breaks, right=FALSE) 
waiting.freq = table(waiting.cut) 
waiting.cumfreq = cumsum(waiting.freq) 
head(cbind(waiting.cumfreq),4)
waiting.cumfreq


# Aufgabe: Kumulierte Häuﬁgkeitsverteilungskurve
# Problem: Bestimmen Sie die kumulierte Häuﬁgkeitsverteilungskurve der Wartezeiten aus faithful
waiting = faithful$waiting 
breaks = seq(40, 100, by=5) 
waiting.cut = cut(waiting, breaks, right=FALSE) 
waiting.freq = table(waiting.cut) 
waiting.cumfreq = cumsum(waiting.freq) 
waiting.cumfreq0 = c(0,waiting.cumfreq) 
plot(breaks, waiting.cumfreq0, main="Old Faithful Eruptions",
     xlab="Waiting minutes", ylab="Cumulative Eruptions")
lines(breaks, waiting.cumfreq0)

#Aufgabe: Kumulierte relative Häuﬁgkeitsverteilung
#Problem: Bestimmen Sie die kumulierte relative Häuﬁgkeitsverteilung der Wartezeiten aus faithful.
waiting = faithful$waiting 
breaks = seq(40, 100, by=5) 
waiting.cut = cut(waiting, breaks, right=FALSE) 
waiting.freq = table(waiting.cut) 
waiting.cumfreq = cumsum(waiting.freq) 
n <- nrow(faithful)
waiting.cumrelfreq = c(0,waiting.cumfreq) /n
plot(breaks, waiting.cumrelfreq, main="Old Faithful Eruptions",
     xlab="Waiting minutes", ylab="Cumulative Eruptions")
lines(breaks, waiting.cumrelfreq)


# Aufgabe: Histogramm
# Problem: Die Datei „Daten_Wachstum.RData“ enthält die Ergebnisse einer Umfrage
# unter 100 Jungunternehmern. Erstellen Sie ein Histogramm, das die Altersverteilung 
# in der Stichprobe darstellt. Die Klassenbreiten sollen 3 Jahre betragen, das erste 
# Intervall soll bei 15 Jahren beginnen und die rechte Intervallgrenze soll nicht zum 
#Intervall gehören.
data <- load("C:/ieu/workspace/R/descriptive-statistik/data/Daten_WachstumX.RData");

head(Daten_Wachstum$Alter)

low <- min(Daten_Wachstum$Alter)
high <- max(Daten_Wachstum$Alter)
range <- seq(low-1, high+1, by=3)
range
hist(Daten_Wachstum$Alter, range, right=T)

