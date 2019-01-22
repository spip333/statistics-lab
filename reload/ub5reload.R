#################################################################
## Uebung 5 - reloaded
##
#################################################################
rm(list = ls())
setwd("C:/ieu/workspace/R/descriptive-statistik/uebungen/reload")



# Aufgabe: Zusammenhang nominaler Merkmale
tab = matrix(c(19, 27, 4, 7, 8, 5, 1, 13, 16), nrow=3, byrow=T) 
rownames(tab) = c("Kampagne A", "Kampagne B", "Kampagne C") 
colnames(tab) = c("stark", "mittel", "schwach")
tab

# zusammenhang mit chi-quadrat prüfen
chisq.test(tab)

# zusammenhang mit cramer v prüfen
cramer.v <- sqrt(chisq.test(tab)$statistic / (sum(tab)*(min(dim(tab))-1)))
cramer.v

# Statistischer Zusammenhang: Metrische Merkmale
# Problem: Laden Sie den Data Frame StorchBabies aus der Datei StorchBabies.RData. 
# Die Tabelle zeigt neben der Anzahl der Storchenpaare auch die Geburtenrate 
# (in 1000 Geburten pro Jahr) in 17 europäischen Ländern. Bestimmen Sie mit einer passenden Kennzahl den Zusammenhang zwischen den Merkmalen Storchenpaare und Geburtenrate
load("StorchBabies.RData")

cor(StorchBabies$Storchenpaare, StorchBabies$Geburten)

# plot
plot(StorchBabies$Storchenpaare, StorchBabies$Geburtenrate) 
abline(lm(StorchBabies$Geburtenrate~StorchBabies$Storchenpaare))


## Statistischer Zusammenhang: Ordinale Merkmale
# - Problem: Auf einer Whiskydegustation wurden verschiedene Whiskysorten sowohl von einem professionellen Tasting-Master als auch von einem privaten Whiskyfreund begutachtet. Beide konnten pro Whisky Punkte zwischen 0 („furchtbar schlecht“) und 12 („fantastisch“) vergeben. Berechnen Sie eine passende Kennzahl des Zusammenhangs und interpretieren Sie diese.

rating.master <- c(9,1,10,6,5,8)
rating.friend <- c(7,5,12,10,8,3)

corr <- cor(rating.master, rating.friend, method="spearman")

