############################################################################
### Deskriptive Statistik - Uebungserie 5: Zusammenhang
###
### Nicolas Stern
### 17.11.2018
###
#############################################################################
## Aufgabe: Zusammenhang nominaler Merkmale
# Problem: Berechnen Sie ein geeignetes Mass für den Zusammenhang zwischen diesen beiden Merkmalen und interpretieren Sie Ihr Ergebnis.
# Daten formatieren
getwd()
require(stats); 
require(graphics)

absatz <- matrix(c(19,27,4,7,8,5,1,13,16), nrow=3, byrow = T)
rownames(absatz) <- c("A", "B", "C")
colnames(absatz) <- c("stark", "mittel", "schwach")

addmargins(absatz)
prop.table(absatz)
addmargins(prop.table(absatz))

absatz.erwartet <- matrix(rep(0, 9),nrow=3, byrow = T)
for (i in 1:3){
  for (j in 1:3){
    absatz.erwartet[i,j] <- (sum(absatz[i,]) * sum(absatz[,j])) / sum(absatz)
  }
}
absatz.erwartet
addmargins(absatz.erwartet)
# Bestimmung der Zusammenhänge:
chisq.test(absatz)
chisq.test(absatz.erwartet)

chisq.test(absatz)$statistic
round(sqrt(chisq.test(absatz)$statistic / (sum(absatz)*(min(dim(absatz))-1))),2)

sum(absatz)
dim(absatz)
min(dim(absatz))

# note : Zusammenhang: nominale Merkmale
studis <- matrix(c(110, 120, 20, 30, 20, 90, 60, 30, 10, 10), nrow = 2, byrow = T )
studis
rownames(studis) <- c("weiblich", "männlich")
colnames(studis) <- c("BWL", "Soz", "VWL", "SoWi", "Stat")
studis
chisq.test(studis)
chisq.test(studis)$statistic
sqrt(chisq.test(studis)$statistic / (sum(studis)*(min(dim(studis))-1)))


# Statistischer Zusammenhang: Metrische Merkmale
# Problem: Laden Sie den Data Frame StorchBabies aus der Datei StorchBabies.RData.
# Die Tabelle zeigt neben der Anzahl der Storchenpaare auch 
# die Geburtenrate (in 1000 Geburten pro Jahr) in 17 europäischen Ländern. 
# Bestimmen Sie mit einer passenden Kennzahl den Zusammenhang zwischen den Merkmalen 
# Storchenpaare und Geburtenrate.

# covarianz, korrelationskoeffizient
load("StorchBabies.RData")
summary(StorchBabies)
storch <- StorchBabies$Storchenpaare
geb <- StorchBabies$Geburtenrate
plot(storch, geb)
cor(storch, geb)
abline(lm(geb~storch))
ls
getwd()
rmarkdown::render("./uebungserie-5/uebungserie5-Stern-Nicolas.Rmd")

# Statistischer Zusammenhang: Ordinale Merkmale
# Problem: Auf einer Whiskydegustation wurden verschiedene Whiskysorten sowohl von einem professionellen Tasting-Master als auch von einem privaten Whiskyfreund begutachtet. Beide konnten pro Whisky Punkte zwischen 0 („furchtbar schlecht“) und 12 („fantastisch“) vergeben. Es ergab sich folgende Bewertung:
# Whiskysorte Nr. 1 2 3 4 5 6
# Punkte des Tasting-Masters 9 1 10 6 5 8 
#  Punkte des Whiskyfreunds 7 5 12 10 8 3
# Berechnen Sie eine passende Kennzahl des Zusammenhangs und interpretieren Sie diese.

whisky <- c(1:6)
rating.from.master <- c(9,1,10,6,5,8)
rating.from.friends <- c(7,5,12,10,8,3)

rank(rating.from.master)
rank(rating.from.friends)

cor(rating.from.friends, rating.from.master)
cor(rank(rating.from.friends), rank(rating.from.master))

cor(rating.from.friends, rating.from.master, method = "spearman")

plot(rating.from.friends, rating.from.master)

r1 <- c(1,2,3,4,5,6)
r2 <- c(1,2,3,4,5,6)
r3 <- c(10,200,3000,40000,500000,6000000)
plot(r1, r2)
cor(r1, r2)
cor(r1, r3)
plot(r1, r3)
cor(r1,r3)

# note Zusammenhang: ordinale Merkmale
math.note <- c(1,1,5,5,4,2)
stat.note <- c(2,2,5,4,4,3)

#umwandeln in rang - spalten
cor(math.note, stat.note, method = "spearman")
?cor

