#Read & Tidy Komponenten und Einzelteile
library("readr")
library("tidyr")
library("dplyr")
library("readr")
library("rlang")
library("datasets")
library("graphics")
library("grDevices")
library("methods")
library("stats")
library("utils")

#Komponenten
setwd("Documents/Data/Einzelteile")

# 
# Tidy Einzelteile Gero
# 
# 


tidy.csv <- function(df)
{
  # Suchen nach Anzahl der Spalten
  if(ncol(df) < 13 && ncol(df) > 7){
    #suche nach dem Relevanten Zielwort 'origin'. Wenn dieses nicht Vorhanden ist passiert garnichts
    if( "origin" %in% names(df)){
      df$Produktionsdatum_Origin_01011970 <- as.Date(df$Produktionsdatum_Origin_01011970, as.Date(df$origin, "%d-%m-%Y"))
      df <- unite(df, col = "Produktionsdatum", "Produktionsdatum_Origin_01011970")
      df <- unite(df, col = "ID", paste(c("ID_", df_tot[i]), collapse = ""))
    }
  }
  
  #Abfrage für die Dateien, die mehr als 12 Variablen besitzen, und somit die o.g. anderen Probleme aufweisen
  if (ncol(df) > 12) {
    #erstellt einen logischen Vektor der angibt, ob ein Spaltenname "ID" enthält
    c <- startsWith(names(df), "ID")
    
    #erstellt einen Vektor der alle Spaltennamen mit ID enthält
    v <- vector()
    d <- 1
    for(i in 1:length(c)){
      if(c[i]){
        v[[d]] <- names(df)[i]
        d <- d + 1
      }
    }
    
    #Hier wird Geprüft ob es weniger als 20 Variablen gibt. D.h. Dass die relevanten Datensätze nur in zwei Spalten aufgeteilt sind
    if (ncol(df) < 20){
      #jeweilige Spalten uniten
      df <- df %>%
        unite(col ="Produktionsdatum", c("Produktionsdatum.x", "Produktionsdatum.y"), sep = "") %>%
        unite(col = "Herstellernummer", c("Herstellernummer.x", "Herstellernummer.y"), sep = "") %>%
        unite(col = "Werksnummer", c("Werksnummer.x", "Werksnummer.y"), sep = "") %>%
        unite(col = "Fehlerhaft", c("Fehlerhaft.x", "Fehlerhaft.y"), sep = "") %>%
        unite(col = "Fehlerhaft_Datum", c("Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y"), sep = "") %>%
        unite(col = "Fehlerhaft_Fahrleistung", c("Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y"), sep = "") %>%
        unite(col = "ID", v, sep = "")
    }
    
    #Falls mehr als 19 Variablen vorliegen, sind die jeweiligen Spalten in drei aufgeteilt:
    if (ncol(df) >= 20){
      df <- df %>%
        unite(col ="Produktionsdatum", c("Produktionsdatum", "Produktionsdatum.x", "Produktionsdatum.y"), sep = "") %>%
        unite(col = "Herstellernummer", c("Herstellernummer" , "Herstellernummer.x", "Herstellernummer.y"), sep = "") %>%
        unite(col = "Werksnummer", c("Werksnummer", "Werksnummer.x", "Werksnummer.y"), sep = "") %>%
        unite(col = "Fehlerhaft", c("Fehlerhaft", "Fehlerhaft.x", "Fehlerhaft.y"), sep = "") %>%
        unite(col = "Fehlerhaft_Datum", c("Fehlerhaft_Datum", "Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y"), sep = "") %>%
        unite(col = "Fehlerhaft_Fahrleistung", c("Fehlerhaft_Fahrleistung", "Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y"), sep = "") %>%
        unite(col = "ID", v, sep = "")
    }
    
    #Durch unite entstanden viele Einträge mit NA. Diese können durch folgende Zeile Entfernt werden
    df <- as.data.frame(lapply(df, gsub, pattern = "NA", replacement = ""))
  }
  
  
  #select the correct Colums
  df <- select(df, ID, Produktionsdatum, Herstellernummer, Werksnummer, Fehlerhaft, Fehlerhaft_Datum, Fehlerhaft_Fahrleistung)
}

#Iteriert über jedes DF. For schleife zum Ausführen des tidy.Csv Codes. Iteriert über alle Dokumente
#Assigned denen jeweils den richtigen Namen
for(i in 1: length(df_tot))
{
  dataframe.tidy <- tidy.csv(eval(parse(text = df_tot[i])))
  dataframe.tidy <- type.convert(dataframe.tidy)
  assign( df_tot[i],dataframe.tidy)
}

# 
# 
# 
# merging all df
# 
# 


# Henrik Anpassung
# Umbenennen der Eintelteile von Alex damit sie später mit df_tot aufgerufen werden können
# es muss noch der erste Spaltenname umbenannt werden der Einzelteile von Alex !!!!!!!!!!
# evtl. auch bei den Komponenten von Alex Namensschwierigkeiten
# es sollten  auch die eigelesenen txt Datien von readLines aus dem Workspace gelöscht werden, habe ich letztes mal noch per Hand gemacht
# ist nicht im Code

Alex1 <- c("T01","T02","T03","T07", "T09","T11","T16","T20","T22","T24", "T27", "T31", "T34","T35","T36", "T39")
Alex2 <- c("T01","T02","T03","T07", "T09", "T11","T16","T20",
           "T22","T24", "T27", "T31", "T34","T35","T36", "T39")

for(i in 1:length(Alex1)){
  
  t<-eval(parse(text = Alex2[i]))
  # hat sich beim mergen über x1 beschwert
  
  #findet die relevante Spalte mit "ID" heraus
  c <- startsWith(names(t), "ID")
  
  #benennt die Spalte mit ID um.
  for(j in 1:length(c))
  {
    if(c[j])
    {
      names(t)[j] <- "ID_T"
    }
  }
  
  t <- select(t, ID_T, Produktionsdatum, Herstellernummer, Werksnummer, Fehlerhaft, Fehlerhaft_Datum, Fehlerhaft_Fahrleistung)
  t <- type.convert(t)
  
  assign( Alex1[i],t)
  
}
remove(t)


# neues df_tot mit Alex txt

csv <- c("01","02","03","05","07", "06", "08","09","11","16", "19","20", "22","24", "25","27", "30", "31", "33","34","35","36", "37","38","39")
csv2 <- c("04", "10", "12", "13", "14", "15", "17", "18", "21", "23", "26", "32", "40")

csv_tot = c(csv, csv2)

df_tot <- vector()

d <- 1
for(i in 1:length(csv_tot)){
  df_tot[i] <- paste(c("T", csv_tot[i]), collapse = "")
  d <- d +1
}



merge_df_csv_T <- T01

#fangen bei der 2. Datei an

for(i in 2:length(df_tot))
{
  #  df_character[,] <- sapply(eval(parse(df_tot[i][,], FUN = as.character)))
  merge_df_csv_T <- full_join(merge_df_csv_T, (eval(parse(text = df_tot[i]))))
  #  merge_df <- full_join(merge_df, df_character)
  
}

# 
# 
# 
# AutoEinlesenKomponenten Gero
# 
# 

Komponenten.B.csv2 <- c("K1BE1", "K1BE2", "K1DI1", "K1DI2", "K2LE1", "K2LE2", "K2ST1", "K2ST2", "K3AG1", "K3AG2", "K3SG1", "K3SGS2", "K4", "K5", "K6", "K7")
Komponenten.csv2 <- c("K1BE2", "K2ST2", "K4", "K6")
Komponenten.csv <- c("K1BE1", "K1DI1", "K3AG1", "K3SG1", "K3SG2", "K5")
pathKomponenten <- "Documents/Data/Komponente/"

#Importieren der einfachen csv-Dateien
for(i in 1:length(Komponenten.B.csv2))
{
  #für Die Bestandsliste
  path.Bestandteil.csv2 <- paste(c(pathKomponenten, "Bestandteile_Komponente_", Komponenten.B.csv2[i], ".csv"), collapse = "")
  df.Bestandteil.csv2 <- read.csv2(path.Bestandteil.csv2)
  assign(paste(c("B", Komponenten.B.csv2[i]), collapse = "_"), df.Bestandteil.csv2)
}

for(i in 1:length(Komponenten.csv2))
{
  #für Die Komponenten und csv2
  path.Komponente.csv2 <- paste(c(pathKomponenten, "Komponente_", Komponenten.csv2[i], ".csv"), collapse = "")
  df.Komponente.csv2 <- read.csv2(path.Komponente.csv2)
  assign(paste(Komponenten.csv2[i]), df.Komponente.csv2)
}

for(i in 1:length(Komponenten.csv))
{
  #für Die Komponenten und csv
  path.Komponente.csv <- paste(c(pathKomponenten, "Komponente_", Komponenten.csv[i], ".csv"), collapse = "")
  df.Komponente.csv <- read.csv(path.Komponente.csv)
  assign(paste(Komponenten.csv[i]), df.Komponente.csv2)
}


# 
# 
# 
# tidy.Komponenten Gero
# 
# 

# SampleSize 1
#df_tot <- c(Komponenten.csv)

# SampleSize all csv
df_tot <- c(Komponenten.csv, Komponenten.csv2)

#Alle Dateien
#df_tot <- Komponenten.B.csv2

tidy.csv.Komponenten <- function(df)
{    
  
  #erstellt einen logischen Vektor der angibt, ob ein Spaltenname "ID" enthält
  c <- startsWith(names(df), "ID")
  
  #erstellt einen Vektor der alle Spaltennamen mit ID enthält
  v <- vector()
  d <- 1
  for(i in 1:length(c)){
    if(c[i]){
      v[[d]] <- names(df)[i]
      d <- d + 1
    }
  }
  
  # Suchen nach Anzahl der Spalten
  if(ncol(df) == 10){
    #suche nach dem Relevanten Zielwort 'origin'. Wenn dieses nicht Vorhanden ist passiert garnichts
    if( "origin" %in% names(df)){
      df$Produktionsdatum_Origin_01011970 <- as.Date(df$Produktionsdatum_Origin_01011970, as.Date(df$origin, "%d-%m-%Y"))
      df <- unite(df, col = "Produktionsdatum", "Produktionsdatum_Origin_01011970")
      df <- unite(df, col = "ID_K", v)
      #      df <- unite(df, col = "ID_K", paste(c("ID_", df_tot[i]), collapse = ""))
    }
  }
  
  #Abfrage für die Dateien, die mehr als 12 Variablen besitzen, und somit die o.g. anderen Probleme aufweisen
  if (ncol(df) > 12) {
    
    
    #Hier wird Geprüft ob es weniger als 20 Variablen gibt. D.h. Dass die relevanten Datensätze nur in zwei Spalten aufgeteilt sind
    if (ncol(df) < 20){
      #jeweilige Spalten uniten
      df <- df %>%
        unite(col ="Produktionsdatum", c("Produktionsdatum.x", "Produktionsdatum.y"), sep = "") %>%
        unite(col = "Herstellernummer", c("Herstellernummer.x", "Herstellernummer.y"), sep = "") %>%
        unite(col = "Werksnummer", c("Werksnummer.x", "Werksnummer.y"), sep = "") %>%
        unite(col = "Fehlerhaft", c("Fehlerhaft.x", "Fehlerhaft.y"), sep = "") %>%
        unite(col = "Fehlerhaft_Datum", c("Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y"), sep = "") %>%
        unite(col = "Fehlerhaft_Fahrleistung", c("Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y"), sep = "") %>%
        unite(col = "ID_K", v, sep = "")
    }
    
    #Falls mehr als 19 Variablen vorliegen, sind die jeweiligen Spalten in drei aufgeteilt:
    if (ncol(df) >= 20){
      df <- df %>%
        unite(col ="Produktionsdatum", c("Produktionsdatum", "Produktionsdatum.x", "Produktionsdatum.y"), sep = "") %>%
        unite(col = "Herstellernummer", c("Herstellernummer" , "Herstellernummer.x", "Herstellernummer.y"), sep = "") %>%
        unite(col = "Werksnummer", c("Werksnummer", "Werksnummer.x", "Werksnummer.y"), sep = "") %>%
        unite(col = "Fehlerhaft", c("Fehlerhaft", "Fehlerhaft.x", "Fehlerhaft.y"), sep = "") %>%
        unite(col = "Fehlerhaft_Datum", c("Fehlerhaft_Datum", "Fehlerhaft_Datum.x", "Fehlerhaft_Datum.y"), sep = "") %>%
        unite(col = "Fehlerhaft_Fahrleistung", c("Fehlerhaft_Fahrleistung", "Fehlerhaft_Fahrleistung.x", "Fehlerhaft_Fahrleistung.y"), sep = "") %>%
        unite(col = "ID_K", v, sep = "")
    }
    
    #Durch unite entstanden viele Einträge mit NA. Diese können durch folgende Zeile Entfernt werden
    df <- as.data.frame(lapply(df, gsub, pattern = "NA", replacement = ""))
  }
  
  
  #select the correct Colums
  df <- select(df, ID_K, Produktionsdatum, Herstellernummer, Werksnummer, Fehlerhaft, Fehlerhaft_Datum, Fehlerhaft_Fahrleistung)
  #  names(df) <- c("ID_K", "Produktionsdatum_K", "Hersteller_K", "Werksnummer_K", "Fehlerhaft_K", "Fehlerhaft_Datum_K", "Fehlerhaft_leistung_K")
}

#Iteriert über jedes DF. For schleife zum Ausführen des tidy.Csv Codes. Iteriert über alle Dokumente
#Assigned denen jeweils den richtigen Namen
for(i in 1:length(df_tot))
{
  dataframe.tidy <- data.frame()
  dataframe.tidy <- tidy.csv.Komponenten(eval(parse(text = df_tot[i])))
  dataframe.tidy <- type.convert(dataframe.tidy)
  assign(df_tot[i], dataframe.tidy)
}

# 
# 
# 
# left.join Bestandteile K
# 
# 

rbind_B <- data.frame()
for(i in 1:length(df_tot))
{
  df <- (eval(parse(text = paste(c("B_", df_tot[i]), collapse = ""))))
  for (j in 2:(ncol(df)-1)) 
  {
    df_selected <- select(df, j, (ncol(df)))
    names(df_selected) <- c("ID_T", "ID_K")
    dfK <- (eval(parse(text = df_tot[i])))
    names(dfK) <- c("ID_K", "Produktionsdatum_K", "Hersteller_K", "Werksnummer_K", "Fehlerhaft_K", "Fehlerhaft_Datum_K", "Fehlerhaft_leistung_K")
    df_merged <- left_join(df_selected, dfK, by = "ID_K")
    
    #Fügt das jeweils letzte df_selected an das vorherige
    rbind_B <- rbind(rbind_B, df_merged)
    #    if(ncol(merge) == 7)
    #   {
    #    merge <- left_join(merge, df_selected, by ="ID_T")
    # } else {
    #  merge <- full_join(merge, df_selected, by = "ID_T")
    #}
  }
}

merge <- select(merge, -X1)
names(merge) <- c("ID_T", "Productionsdate_T", "Herstellernummer_T", "Werksnummer_T", "Fehlerhaft_T", "Fehlerhaft_Date_T", "Fehlerhaft_Fahrleistung_T")
TB <- left_join(merge, rbind_B, by = "ID_T")


# 
# 
# 
# Fahrzeug
# 
# 


#Dieses Skript läd zunächst die notwendigen Dateien der Fahrzeuge rein
#Eine Automatisierung wird nicht vorgenommen, da es sich hierbei nur um zwei dateien handelt

#Einlesen der Fahrzeuge
Fahrzeug_11 <- read.csv("Documents/Data/Fahrzeug/Fahrzeuge_OEM1_Typ11.csv")
Fahrzeug_21 <- read.csv("Documents/Data/Fahrzeug/Fahrzeuge_OEM2_Typ21.csv")

#Tidy
#Fahrzeug 11
Fahrzeug_11 <- select(Fahrzeug_11, "ID_Fahrzeug", "Produktionsdatum", "Herstellernummer", "Werksnummer", "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung")
names(Fahrzeug_11) <- c("ID_Fahrzeug", "Produktionsdatum_F", "Herstellernummer_F", "Werksnummer_F", "Fehlerhaft_F", "Fehlerhaft_Datum_F", "Fehlerhaft_Fahrleistung_F")

#Fahrzeug 21
Fahrzeug_21$origin <- as.Date(Fahrzeug_21$origin, "%d-%m-%Y")
Fahrzeug_21$Produktionsdatum <- as.Date(Fahrzeug_21$Produktionsdatum, origin = Fahrzeug_21$origin)
Fahrzeug_21 <- select(Fahrzeug_21, "ID_Fahrzeug", "Produktionsdatum", "Herstellernummer", "Werksnummer", "Fehlerhaft", "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung")
names(Fahrzeug_21) <- c("ID_Fahrzeug", "Produktionsdatum_F", "Herstellernummer_F", "Werksnummer_F", "Fehlerhaft_F", "Fehlerhaft_Datum_F", "Fehlerhaft_Fahrleistung_F")

#Einlesen der Bestandslisten
B_Fahrzeug_11 <- read.csv2("Documents/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv")
B_Fahrzeug_21 <- read.csv2("Documents/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv")

#---------------------------------------------------------------------------

#Erstellen einer left_join -baren Datei

Fahrzeuge <- c("Fahrzeug_11", "Fahrzeug_21")

rbind_F <- data.frame()

for(i in 1:2)
{
  df <- (eval(parse(text = paste(c("B_", Fahrzeuge[i]), collapse = ""))))
  
  for (j in 2:(ncol(df)-1)) 
  {
    df_selected <- select(df, j, (ncol(df)))
    names(df_selected) <- c("ID_K", "ID_Fahrzeug")
    dfF <- (eval(parse(text = Fahrzeuge[i])))
    df_merged <- left_join(df_selected, dfF, by = "ID_Fahrzeug")
    
    #Fügt das jeweils letzte df_selected an das vorherige
    rbind_F <- rbind(rbind_F, df_merged)
  }
}

#---------------------------------------------------------------------------

#Left_join mit TBK

TBKBF <- left_join(TBK, rbind_F, by = "ID_K")