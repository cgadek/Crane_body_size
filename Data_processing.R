### Crane Body Size Data Processing File ####


library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)


morpho_massive <-
  read_csv("~/Dropbox/Research/Crane_Trachea/data/Crane harvest data-WORKING COPY.csv")


# morpho_big <-
#   read.csv("~/Dropbox/Research/Crane_Trachea/data/morpho_large.csv",
#            header = T)

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
    Collector
  ) %>%
  filter(Age %in% c("ADULT", "IMMATURE")) %>%
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
    row.id = seq.int(nrow(.)),
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
    County = if_else(
      County %in% c("Socorro- 2", "2"),
      "SOCORRO",
      if_else(
        County %in% c("3"),
        "VALENCIA",
        if_else(County %in% c("Dona Ana"), "DONA ANA", County)
      )
    ),
    Collector = if_else(Collector == "Barry Hale", "BARRY HALE", if_else(Collector %in% c("G. SCHMITT", "C. Gregory Schmitt", "GREG SCHMITT"), "C. GREGORY SCHMITT", if_else(Collector =="CARD BILL SURVEY", "BILL CARD SURVEY", if_else(Collector == "Janell M. Ward", "JAN WARD", if_else(Collector == "KIM", "KIM FALZONE", if_else(Collector %in% c("TIM MITCHUSOSN", "Tim Mitchusson"), "TIM MITCHUSSON", if_else(is.na(Collector), "UNKNOWN", Collector)))))))) %>%
      dplyr::rename(taxon_name = taxon.name) %>%
      mutate(
        mass = ifelse(
          taxon_name == "Antigone canadensis canadensis" &
            mass < 4000 &
            taxon_name == "Antigone canadensis canadensis" &
            mass > 2500 |
            taxon_name == "Antigone canadensis tabida" &
            mass > 4000 &
            taxon_name == "Antigone canadensis tabida" &
            mass < 7000,
          mass,
          NA
        ),
        wing.chord = ifelse(
          taxon_name == "Antigone canadensis canadensis" &
            wing.chord < 510 &
            taxon_name == "Antigone canadensis canadensis" &
            wing.chord > 410 |
            taxon_name == "Antigone canadensis tabida" &
            wing.chord > 470 &
            taxon_name == "Antigone canadensis tabida" &
            wing.chord < 580,
          wing.chord,
          NA
        ),
        tarsus = ifelse(
          taxon_name == "Antigone canadensis canadensis" &
            tarsus < 220 &
            taxon_name == "Antigone canadensis canadensis" &
            tarsus > 150 |
            taxon_name == "Antigone canadensis tabida" &
            tarsus > 200 &
            taxon_name == "Antigone canadensis tabida" &
            tarsus < 270,
          tarsus,
          NA
        ),
        tail = ifelse(
          taxon_name == "Antigone canadensis canadensis" &
            tail < 190 &
            taxon_name == "Antigone canadensis canadensis" &
            tail > 140 |
            taxon_name == "Antigone canadensis tabida" &
            tail > 170 &
            taxon_name == "Antigone canadensis tabida" & tail < 230,
          tail,
          NA
        ),
        anterior.culmen = ifelse(
          taxon_name == "Antigone canadensis canadensis" &
            anterior.culmen < 70 &
            taxon_name == "Antigone canadensis canadensis" &
            anterior.culmen > 50 |
            taxon_name == "Antigone canadensis tabida" &
            anterior.culmen > 70 &
            taxon_name == "Antigone canadensis tabida" &
            anterior.culmen < 100,
          anterior.culmen,
          NA
        ),
        posterior.culmen = ifelse(
          taxon_name == "Antigone canadensis canadensis" &
            posterior.culmen < 85 &
            taxon_name == "Antigone canadensis canadensis" &
            posterior.culmen > 55 |
            taxon_name == "Antigone canadensis tabida" &
            posterior.culmen > 85 &
            taxon_name == "Antigone canadensis tabida" &
            posterior.culmen < 120,
          posterior.culmen,
          NA
        ),
        wt.index = wing.chord / tarsus, #make index proportion wing/tarsus length
        wm.index = wing.chord / (mass^(1/3)) #make index proportion wing/mass^1/3
      ) %>%
      filter(
        County %in% c("SOCORRO", "VALENCIA", "DONA ANA", "LUNA", "SIERRA", "TORRANCE"),
        taxon_name %in% c("Antigone canadensis tabida", "Antigone canadensis canadensis"),
        Year %in% c(1983:2010),
        sex %in% c("M", "F")
      )

# determine sex ratio by year,  separately

sex_ratio <- morpho_massive%>%
  dplyr::select(taxon_name, Year, sex)%>%
  group_by(taxon_name,  Year, sex)%>%
  dplyr::summarise(n = n())%>%
  ungroup()%>%
  group_by(taxon_name, Year)%>%
  pivot_wider(names_from = sex, values_from = n) %>%
  mutate(
         ratio = M/`F`)

  
morpho_massive <- morpho_massive%>%
  left_join(., sex_ratio, by = c("Year", "taxon_name"))
    
  
mm.g <-
  morpho_massive %>% filter(taxon_name == "Antigone canadensis tabida", sex %in% c("M", "F"))
mm.l <-
  morpho_massive %>% filter(taxon_name == "Antigone canadensis canadensis",
                            sex %in% c("M", "F"))


# PCA dataset. This will be inherently smaller given missing data

select.cols.big<-c(1, 3:6, 15, 22, 12, 8:11) #select all non measure columns you want in final dataset
select.cols.little<-c(8:11)

Years <- morpho_massive%>%  #list of years for which we will organize pc data
  filter(!Year%in% c(1983, 1984, 1986, 1987, 2002))%>%
  dplyr::select(Year)%>%
  distinct()%>%
  pull(Year)

pca.years <- replicate(length(Years), vector("list", 2), simplify = FALSE)


for(i in 1:length(pca.years)){ 
  
  pca.cols.to.add <-morpho_massive%>%
    filter(sex %in% c("M", "F"),
           Year == Years[i],
           taxon_name %in% c("Antigone canadensis tabida", "Antigone canadensis canadensis"))%>%
    dplyr::select(all_of(select.cols.big))%>%
    filter_at(vars(9:12), all_vars(!is.na(.)))%>%
    dplyr::select(1:8)
  
  
  pca.f <-morpho_massive%>%
    filter(sex %in% c("M", "F"),
           Year == Years[i],
           taxon_name %in% c("Antigone canadensis tabida", "Antigone canadensis canadensis"))%>%
    dplyr::select(all_of(select.cols.little))%>%
    na.omit()%>%
    prcomp(., center=T,
           scale=T)
  
  
  pca.data<- as.data.frame(pca.f$x)
  
  pca.data$taxon_name <- pca.cols.to.add$taxon_name
  pca.data$sex <- pca.cols.to.add$sex
  pca.data$Year <- pca.cols.to.add$Year
  pca.data$County <- pca.cols.to.add$County
  pca.data$sex.ratio <- pca.cols.to.add$ratio
  pca.data$mass <- pca.cols.to.add$mass
  pca.data$collector <- pca.cols.to.add$Collector
  pca.data$Age <- pca.cols.to.add$Age
  
  pca.years[[i]][[1]] <- pca.data
  
  pca.years[[i]][[2]] <- pca.f
  
}


names(pca.years) <- Years

pca.score.years <- pca.years[[1]][[1]]

for(i in 2:length(Years)){
  temp <- pca.years[[i]][[1]]
  pca.score.years <- rbind(pca.score.years, temp)
}

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


hist(pca.data%>%filter(taxon_name =="Antigone canadensis tabida")%>%dplyr::select(PC1)%>%pull())
hist(pca.data%>%filter(taxon_name =="Antigone canadensis canadensis")%>%dplyr::select(PC1)%>%pull())

#write out separate datasets



write.csv(mm.g, "data/greater_body_size.csv")
write.csv(mm.l, "data/lesser_body_size.csv")
write.csv(morpho_massive, "data/morpho_massive.csv")
write.csv(pca.data, "data/pca_combined_subspecies.csv")

