#################################################################
## Uebung 3 - reloaded
##
#################################################################

# Aufgabe: zweidimensionale Häuﬁgkeitsverteilung
# Aufgabe: Bestimmen Sie die zweidimensionale Häuﬁgkeitsverteilung der Merkmale 
# Geschlecht und Motiv aus dem Datenframe Daten_WachstumX

getwd()
load("./reload/Daten_WachstumX.RData")
head(Daten_Wachstum)
head(Daten_Wachstum$Motiv)
head(Daten_Wachstum$Geschlecht)

tab = table(Daten_Wachstum$Geschlecht, Daten_Wachstum$Motiv)
tab

# Aufgabe: Randverteilungen
# Aufgabe: Fügen Sie die Randverteilungen zur zweidimensionalen Häuﬁgkeitsverteilung
# der Merkmale Geschlecht und Motiv hinzu.
addmargins(tab)

# Aufgabe: Relative Zweidimensionale Verteilung
# Aufgabe: Bestimmen Sie die relative zweidimensionale Häuﬁgkeitsverteilung der Merkmale 
# Geschlecht und Motiv.
prop.table(tab)

# Aufgabe: Bedingte Verteilung 1
# Aufgabe: Wie verteilen sich die verschiedenen Motive innerhalb der Geschlechtergruppen
addmargins(prop.table(tab, 1))

# Aufgabe: Bedingte Verteilung 2
# Aufgabe: Wie verteilen sich die beiden Geschlechter auf die verschiedenen Motive?
addmargins(prop.table(tab, 2))


