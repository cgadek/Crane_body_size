### Crane Body Size Data Processing File ####

library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)


morpho_massive <-
  read_csv("~/Dropbox/Research/Crane_Trachea/data/Crane harvest data-WORKING COPY.csv")


morpho_big <- read.csv("~/Dropbox/Research/Crane_Trachea/data/morpho_large.csv", header = T)

morpho_big <- morpho_big %>%
  dplyr::rename(taxon.name = taxon_name,
                posterior.culmen = posterior_culmen) %>%
  mutate(data.origin = "MSB")


morpho_massive <- morpho_massive %>%
  dplyr::select(
    County,
    Date,
    Year,
    Subspecies,
    Weight,
    Sex,
    Tail,
    `Wing Chord`,
    `Anterior Culmen`,
    `Posterior Culmen`,
    Tarsus,
    Age,
    `Trachea length`,
    `Trachea diameter`,
  ) %>%
  filter(County %in% c("SOCORRO", "VALENCIA", "Socorro- 2", "2", "3")) %>%
  dplyr::rename(
    mean_trachea_length = `Trachea length`,
    mass = Weight,
    trachea.diameter = `Trachea diameter`,
    anterior.culmen = `Anterior Culmen`,
    posterior.culmen = `Posterior Culmen`,
    wing.chord = `Wing Chord`,
    taxon.name = Subspecies,
    tail = Tail,
    tarsus = Tarsus,
    sex = Sex
  ) %>%
  mutate(
    mass = as.numeric(mass),
    tail = as.numeric(tail),
    wing.chord = as.numeric(wing.chord),
    anterior.culmen = as.numeric(anterior.culmen),
    posterior.culmen = as.numeric(posterior.culmen),
    tarsus = as.numeric(tarsus),
    Date = as.Date(Date, tryFormats = c("%d-%m-%Y", "%m/%d/%Y")),
    sample_year = year(Date),
    taxon.name = ifelse(
      taxon.name == "Canadensis",
      "Antigone canadensis canadensis",
      if_else(
        taxon.name == "TABIDA",
        "Antigone canadensis tabida",
        if_else(
          taxon.name == "CANADENSIS",
          "Antigone canadensis canadensis",
          ifelse(
            taxon.name == "ASSIGNED TABIDA",
            "Antigone canadensis tabida",
            ifelse(
              taxon.name == "Tabida",
              "Antigone canadensis tabida",
              if_else(
                taxon.name == "Immature Lesser",
                "Antigone canadensis canadensis",
                taxon.name
              )
            )
          )
        )
      )
    ),
    sex = if_else(sex == "MALE", "M", if_else(
      sex == "m", "M", if_else(sex == "FEMALE", "F", if_else(sex == "f", "F", sex))
    )),
    data.origin = "NMDGF"
  ) %>%
  bind_rows(., morpho_big)%>%
  dplyr::rename(
    taxon_name = taxon.name)%>%
  mutate(mass = ifelse(taxon_name == "Antigone canadensis canadensis" & mass < 4000 & taxon_name == "Antigone canadensis canadensis" & mass > 2500 | 
           taxon_name == "Antigone canadensis tabida" & mass > 4000 & taxon_name == "Antigone canadensis tabida" & mass < 7000, mass, NA),
         wing.chord = ifelse(taxon_name == "Antigone canadensis canadensis" & wing.chord < 510 & taxon_name == "Antigone canadensis canadensis" & wing.chord > 410 | 
                               taxon_name == "Antigone canadensis tabida" & wing.chord > 470 & taxon_name == "Antigone canadensis tabida" & wing.chord < 580, wing.chord, NA),
         tarsus = ifelse(taxon_name == "Antigone canadensis canadensis" & tarsus < 220 & taxon_name =="Antigone canadensis canadensis" & tarsus > 150 | 
                               taxon_name == "Antigone canadensis tabida" & tarsus > 200 & taxon_name == "Antigone canadensis tabida" & tarsus < 270, tarsus, NA),
         tail = ifelse(taxon_name == "Antigone canadensis canadensis" & tail < 190 & taxon_name =="Antigone canadensis canadensis" & tail > 140 | 
                           taxon_name == "Antigone canadensis tabida" & tail > 170 & taxon_name == "Antigone canadensis tabida" & tail < 230, tail, NA),
         anterior.culmen = ifelse(taxon_name == "Antigone canadensis canadensis" & anterior.culmen < 70 & taxon_name =="Antigone canadensis canadensis" & anterior.culmen > 50 | 
                         taxon_name == "Antigone canadensis tabida" & anterior.culmen > 70 & taxon_name == "Antigone canadensis tabida" & anterior.culmen < 100, anterior.culmen, NA),
        posterior.culmen = ifelse(taxon_name == "Antigone canadensis canadensis" & posterior.culmen < 85 & taxon_name =="Antigone canadensis canadensis" & posterior.culmen > 55 | 
                                    taxon_name == "Antigone canadensis tabida" & posterior.culmen > 85 & taxon_name == "Antigone canadensis tabida" & posterior.culmen < 120, posterior.culmen, NA),
        wt.index = wing.chord/tarsus)

mm.g <- morpho_massive%>%filter(taxon_name =="Antigone canadensis tabida", sex %in% c("M", "F"))
mm.l <- morpho_massive%>%filter(taxon_name =="Antigone canadensis canadensis", sex %in% c("M", "F"))


### Examine distributions

# mass

hist(mm.g$mass) #removed below 4000 and above 7000
hist(mm.l$mass) #removed above 4000 to reduce potential intermediates

# wing.chord

hist(mm.g$wing.chord) #removed below 470 and above 580
hist(mm.l$wing.chord) #removed above 410 and below 510

# tarsus

hist(mm.g$tarsus) #removed below 200 and above 270
hist(mm.l$tarsus) #removed above 150 and below 250

# Tail

hist(mm.g$tail) #removed below 170 and above 230
hist(mm.l$tail) #removed above 190 and below 140


# Anterior culmen

hist(mm.g$anterior.culmen) #removed below 70 and above 100
hist(mm.l$anterior.culmen) #removed above 70 and below 50

# Posterior culmen

hist(mm.g$posterior.culmen) #removed below 85 and above 120
hist(mm.l$posterior.culmen) #removed above 85 and below 55

#write out separate datasets



write.csv(mm.g, "data/greater_body_size.csv")
write.csv(mm.l, "data/lesser_body_size.csv")
