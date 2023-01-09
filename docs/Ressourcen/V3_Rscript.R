## Regressionsdiagnostik mit R
## Teil 3: Modellspezifikation
## Dr. Uwe Remer



#### 1. Einlesen der Daten #### 
library(foreign)
df <- read.spss("./Daten/ZA5270_v2-0-0.sav",
                to.data.frame = TRUE,
                use.value.labels = FALSE,
                reencode =TRUE)




#### 2. Auswahl der Variablen #### 

# Konsultieren Sie immer das Codebuch!

# Prüfen Sie 
# - das Skalennivau
# - korrekte kodierung von fehlenden Werte
# - Anzahl der fehlenden Werte
# - Verteilung und Varianz der Variablen



# y-Variable: Demokratiezufriedenheit
df$ps03
attr(df$ps03, "value.labels")

# Drehen der Variable Demokratiezufriedenheit
# Ziel: 1 sehr unzufrieden, 6 sehr zufrieden
df$demzufr <- abs(df$ps03-7)
df$demzufr 
cor.test(df$demzufr, df$ps03)



library(DescTools)
Desc(df$demzufr)

# Variable normalverteilt?
# Shapiro-Wilk-Test
shapiro.test(df$demzufr)
# H0: Normalverteilung -> kann verworfen werden

# Und visuell?
qqnorm(df$demzufr)
qqline(df$demzufr)



# x-Variable: Links-Rechts-Selbsteinstufung 

df$lire <- df$pa01
Desc(df$lire)


# Erster bivariater Blick auf den Zusammenhang
Desc(df$demzufr ~ df$lire)



# Kontrollvariablen 

df$educ

df$bildungsjahre <- df$S01
df$bildungsjahre
Desc(df$bildungsjahre)

df$alter <- df$age 
df$alter
Desc(df$alter)



#### 3. Fehlende Werte #### 

variablen <- c("demzufr", "lire", "bildungsjahre", "alter")

# Fehlende Werte anzeigen
is.na(df[,variablen])

# Wie viel listenweiser Fallausschluss?
anzahl.fehlende <- rowSums(is.na(df[,variablen]))
table(anzahl.fehlende)
prop.table(table(anzahl.fehlende))

# Sollten wir Missing Values beheben?
# Wenn ja, wie? Hier: Missings bei Bildungsjahre durch Median ersetzen

df$bildungsjahre[is.na(df$bildungsjahre)] <- median(df$bildungsjahre, na.rm=T)

anzahl.fehlende <- rowSums(is.na(df[,variablen]))
table(anzahl.fehlende)
prop.table(table(anzahl.fehlende))





#### 4. Regression #### 

fit.0 <- lm(demzufr ~ 
              lire + bildungsjahre + alter, 
            data = df)

summary(fit.0)


#### 5. Multikollinearität prüfen ####

# VIF = Varianz-Inflations-Faktor
library(car)
# Sollte < 5 sein, besser < 3
vif(fit.0) 

# Eigenständiger Varainzanteil, sollte > 30% sein
1/vif(fit.0)




# Und zum Schluss: aufräumen, was wir nicht weiter benötigen...
rm(anzahl.fehlende, variablen)
