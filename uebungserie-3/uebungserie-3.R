#############################################################################
### Deskriptive Statistik - Übungserie 3
###
### Nicolas Stern
### 07.11.2018
###
#############################################################################

## Aufgabe: zweidimensionale Häuﬁgkeitsverteilung
# Bestimmen Sie die zweidimensionale Häuﬁgkeitsverteilung der Merkmale Geschlecht und Motiv aus dem Datenframe Daten_WachstumX
class(Daten_Wachstum)
colnames(Daten_Wachstum)
head(Daten_Wachstum$Motiv)
head(Daten_Wachstum$Geschlecht)
table.gender.motiv <- table(Daten_Wachstum$Geschlecht, Daten_Wachstum$Motiv)
table.gender.motiv

## Aufgabe: Randverteilungen
# Fügen Sie die Randverteilungen zur zweidimensionalen Häuﬁgkeitsverteilung der Merkmale Geschlecht und Motiv hinzu.
addmargins(table.gender.motiv)

## Aufgabe: Relative Zweidimensionale Verteilung
# Bestimmen Sie die relative zweidimensionale Häuﬁgkeitsverteilung der Merkmale Geschlecht und Motiv.
prop.table(table.gender.motiv)
addmargins(prop.table(table.gender.motiv))

## Aufgabe: Bedingte Verteilung 1
# Wie verteilen sich die verschiedenen Motive innerhalb der Geschlechtergruppen
round(prop.table(table.gender.motiv, 2),2)

## Aufgabe: Bedingte Verteilung 2
# Wie verteilen sich die beiden Geschlechter auf die verschiedenen Motive?
round(prop.table(table.gender.motiv, 1),2)
