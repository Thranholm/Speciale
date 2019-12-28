####### Parlgov data set #############
rm(list = ls())
library(pacman)

pacman::p_load(tidyverse,ivpack, stargazer,rio, ivprobit, 
               gmodels, haven, estimatr, lubridate, clipr,
               janitor, stringr, AER, multiwayvcov, miceadds, margins,
               dbplyr, DBI, RSQLite, ivmodel, RCurl, mctest, devtools)
#devtools::install_github("ChandlerLutz/starpolishr")
library(starpolishr)


setwd("/Users/ethranholm/Dropbox/noter/Speciale/Data til speciale")

##### Vil gerne kunne rykke variable i frames, så definerer denne funktion fra starten.
source("/Users/ethranholm/OneDrive/R/order_funktion.R")

### Indlæser data fra SQL
ParlGov <- dbConnect(SQLite(),
                     "/Users/ethranholm/OneDrive/Specialedata/parlgov-experimental.db")
as.data.frame(dbListTables(ParlGov))
dbListTables(ParlGov)

v <- (dbListTables(ParlGov))
dflist <- lapply(v, function(t) as_data_frame(dbReadTable(ParlGov, paste0(t))))
list2env(setNames(dflist,v), envir = .GlobalEnv)
dbDisconnect(ParlGov)
rm(ParlGov)



## Indlæser data
# election <- import("election.csv")
# view_election <-  import("view_election.csv")
# election_result <- import("election_result.csv")

#external_country_iso <- import("external_country_iso.csv")
# party_name_change <- import("party_name_change.csv")
# view_cabinet <- import("view_cabinet.csv")
# calelectionpara <- import("viewcalc_election_parameter.csv")
# calparliamentcompo <- import("viewcalc_parliament_composition.csv")
# polpresidennt <- import("politician_president.csv")

 tavits <- import("Schleiter_Tavits_JoP_2015_replication_data.dta")

# View(tavits)
tavits$election_date <- dmy(tavits$pd)
tavits$start_date <- dmy(tavits$fd)

# ERDDA <- import("ERD-e_SA_SOE_N-29_2014_B_Stata_version_12.dta")

election_result <- election_result %>% 
    arrange(-desc(election_id))

head(view_election)
head(view_cabinet)
head(election_result)
head(election)

view_election <- view_election %>% 
    arrange(-desc(election_id))
head(view_election)
head(election)
head(view_cabinet)


#### Danner et cross-country election panel og udregner variable. ####

## danner overblik over hvilke data, der skal merges og hvordan.

head(view_election) ## Skal gøres, så der kun er én række per valg. lige nu er den
  ## inddelt på partier.

head(view_cabinet) ## også inddelt på partier, skal laves om til valg.

head(election) ### er på valgniveau.

head(election_result) ##partiniveau, data findes nok i de andre allerede.


## View cabinet og partier kan slås sammen
head(view_election)
head(view_cabinet)

x <- view_cabinet %>% 
  filter(election_id==1001)



## Udregner ændringen i mandater for de forskelle partier.

view_election <- view_election %>% 
  filter(election_type=="parliament") %>% 
  arrange(desc(election_date)) %>%
  group_by(country_id, party_id) %>% 
  mutate(seats_andel=seats/seats_total*100) %>% 
  mutate(seats_next=lag(seats_andel)) %>% 
  mutate(delta_seats=seats_next-seats_andel)

view_election <- view_election %>% 
  filter(election_type=="parliament") %>% 
  arrange(desc(election_date)) %>%
  group_by(country_id, party_id) %>% 
  mutate(vote_next=lag(vote_share)) %>% 
  mutate(delta_vote=vote_next-vote_share)



partcab <- left_join(view_cabinet, view_election,
                 by=c("election_id", "country_name", "country_name_short",
                      "party_name_english", "party_name_short", 
                      "election_date",
                      "left_right", "previous_cabinet_id", "seats", 
                      "party_name", "country_id", "party_id"))


## Tjekker for dupletter
identical(partcab$election_seats_total, partcab$seats_total)
sum(!is.na(partcab$election_seats_total))
sum(!is.na(partcab$seats_total))
## Duppletter/grundet flere cabs per valg, mangler værdier på nogle variable fra
## valgdataen, så de laves her

partcab <- partcab %>% 
  group_by(election_id, party_id) %>% 
  mutate(previous_parliament_election_id = first(previous_parliament_election_id)) %>% 
  mutate(seats_next = first(seats_next)) %>% 
  mutate(vote_next = first(vote_next)) %>% 
  mutate(vote_share = first(vote_share)) %>% 
  mutate(seats_andel = first(seats_andel)) %>% 
  mutate(election_type = first(election_type)) %>% 
  mutate(seats_total = first(seats_total)) %>% 
  mutate(delta_seats = first(delta_seats)) %>% 
  mutate(delta_vote = first(delta_vote))

## Skulle gerne have flere "election id" i det samlede data, end de to andre, da 
## der kan være flere cabinets pr. election. Dog er tallet i det samlede 200 større
## end det er i data for cabinets. Tjek om jeg tilføjer støj.
length(partcab$election_id)
NROW(na.omit(partcab$election_id))
length(view_cabinet$election_id)
length(view_election$election_id)
### Når man byttede rundt på dem oppe i left.join, så er det nyes antal 
## election_id = View_cabinet, og større end det for election_id.

### Schweiz kører med et vildt system, tjekker lige deres data
table(partcab$country_name_short)
x <- partcab %>% 
  filter(country_name_short=="CHE")
View(x)

head(partcab, n=10)

head(election)
ls(partcab)

## Sætter parti-navn "none" til NA og caretaker til NA.
partcab <- partcab %>% 
  mutate(party_name_english=if_else(party_name_english=="no party affiliation","NA",party_name_english)) %>% 
  mutate(party_name_english=if_else(caretaker==1,"NA",party_name_english)) %>% 
  ungroup(party_id) %>% 
  mutate(party_id=if_else(caretaker==1,as.integer(NA),party_id))
partcab$party_name_english[partcab$party_name_english=="NA"] <- NA

### laver variabel for hvilket parti der havde PM.
## Sætter dem, der ikke har et parti til missing.
partcab$Prime_party <- if_else(partcab$prime_minister==1, 
                               partcab$party_name_english, "NA")
partcab$Prime_party[partcab$Prime_party=="NA"] <- NA
partcab$Prime_id <- if_else(partcab$prime_minister==1,
                            partcab$party_id, as.integer(NA))
partcab$Prime_id <- if_else(is.na(partcab$party_name_english),as.integer(NA),partcab$Prime_id)

partcab <- partcab %>% 
  arrange(-desc(election_id))






#### Finder valg, hvor der ikke er listet en prime minister.
udenprime <- partcab %>% 
  group_by(cabinet_id) %>% 
  mutate(PM= max(prime_minister)) %>% 
  filter(PM==0 & !country_name_short=="CHE")
View(udenprime)


### Alle valg i Schweiz, grundet deres rotationsprincip, frasorteres senere.
## Ellers få andre.
# Estland, valg 92, cab 94 Tarand.  Han er socialdemokrat, SDE|M. cab id 4 
# Franking, valg 32, cab 33 Chautemps II  han er Radikal socialist (PR) 157 sæder. cab id 1333
# Frankrig, valg 46, cab 50, Queuille II  Han er Radikal socialist (PR) 43 sæder. cab id 1142
# Frankrig, valg 2012, cab 2014, Valls II  Han er socialist (PS) 292 sæder. cab id 1141
# Litaun, valg 90, cab 91, Vagnorius I  Tyder på han er Konservativ, SK, 2 sæder caid 546
# Litaun, valg 90, cab 92, Abisala I   Tyder også på at han er Konservativ, SK, 2 sæder. cabid= 512
## Østrig Berlien, teknisk.

## Dog frasorteres Litauns første valg i 1990 senere, da det er et valg, mens de stadig er Sovjet.


### De ovenstående skal ændres, hvorefter der skal filtreres på Prime minister.


## Tester ændringer virker i dette datasæt, før det gøres i det rigtige.
udenprime$prime_minister[udenprime$party_name_short=="SDE|M" 
                         & udenprime$cabinet_id==4] <- 1
udenprime$prime_minister[udenprime$party_name_short=="PR" 
                         & udenprime$cabinet_id==1333] <- 1
udenprime$prime_minister[udenprime$party_name_short=="PR" 
                         & udenprime$cabinet_id==1142] <- 1
udenprime$prime_minister[udenprime$party_name_short=="PS" 
                         & udenprime$cabinet_id==1141] <- 1
udenprime$prime_minister[udenprime$party_name_short=="SK" 
                         & udenprime$cabinet_id==546] <- 1
udenprime$prime_minister[udenprime$party_name_short=="SK" 
                         & udenprime$cabinet_id==512] <- 1

udenprime <- udenprime %>% filter(prime_minister==1)
## Det virkede.

# Gør det i det rigtige datasæt
partcab$prime_minister[partcab$party_name_short=="SDE|M" 
                       & partcab$cabinet_id==4] <- 1
partcab$prime_minister[partcab$party_name_short=="PR" 
                       & partcab$cabinet_id==1333] <- 1
partcab$prime_minister[partcab$party_name_short=="PR" 
                       & partcab$cabinet_id==1142] <- 1
partcab$prime_minister[partcab$party_name_short=="PS" 
                       & partcab$cabinet_id==1141] <- 1
partcab$prime_minister[partcab$party_name_short=="SK" 
                       & partcab$cabinet_id==546] <- 1
partcab$prime_minister[partcab$party_name_short=="SK" 
                       & partcab$cabinet_id==512] <- 1

## Ekstra validering på caretaker.
pm2 <- partcab %>% 
  group_by(cabinet_id) %>% 
  filter(caretaker==1) %>% 
  summarise(pm2=max(prime_minister))
pm2==0
rm(pm2)


### Smid alt andet end Prime minister kabinet og caretaker ud.
## Fjerner Schweiz og Cypern, da de ikke er parlamentariske demokratier
kunprime <- partcab %>% 
  filter(prime_minister==1 | cabinet_party==1 | caretaker==1) %>% 
  filter(!(country_name_short=="CHE" | country_name=="Cyprus"))
unique(kunprime$country_name)

### Variabel for valgår
kunprime$election_date <- date(kunprime$election_date)
kunprime$aar <- year(kunprime$election_date)

## Fjerner valg før WWII.
alt <- kunprime

kunprime <- kunprime %>% 
  filter(aar>1945)


head(kunprime, n=10)
table(kunprime$election_id)
View(kunprime)

## Udregner antal mandater i regering og om den er minoritet eller majortietsregering
kunprime <- kunprime %>% 
  filter(cabinet_party==1) %>% 
  group_by(cabinet_id) %>% 
  mutate(cabseats=sum(seats))

## Udregner om det er en majoritet eller minoritetsregering
## Udregner om det er single party-regering eller ej.
kunprime <- kunprime %>% 
  mutate(majoritet=if_else(cabseats>(election_seats_total/2), 1, 0))


kunprime <- kunprime %>% 
  group_by(cabinet_id) %>% 
  mutate(regeringspartier=sum(cabinet_party)) %>% 
  mutate(et_parti_reg=if_else(regeringspartier==1,1,0))

##### Fjerner alle andre end primeminister==1
kunprime <- kunprime %>% 
  filter(prime_minister==1)

# Tjekker for dupletter.
length(unique(kunprime$cabinet_id))
n_occur <- data.frame(table(kunprime$cabinet_id))
kunprime[kunprime$cabinet_id %in% n_occur$Var1[n_occur$Freq>1],]

x <- kunprime[kunprime$cabinet_id %in% n_occur$Var1[n_occur$Freq>1],]
### Der er 0




#### Skal have fjernet så kun det kabinet, der sidder op til valg fremgår.
date(kunprime$election_date)
date(kunprime$start_date)

## Tid mellem valg og startdato for kabinet
kunprime$cabstart <- kunprime$election_date %--% kunprime$start_date
kunprime$cabfravalg <- 
  dseconds(kunprime$cabstart) %>% 
  time_length(unit = "year") %>% 
  round(digits = 4)

### Se på caretaker-regeringer.
kunprime$cabstart <- NULL ## Kan åbenbart ikke lide at lubridate formatter flyttes mellem dfs.

View(kunprime)

## Fjerner caretaker, hvis de kommer lige efter valg, og der dannes en anden "ikke-caretaker" regering senere
## Og sætter caretaker-governments til ikke at tælle med deres stemmer.
kunprime <- kunprime %>% 
  group_by(election_id) %>% 
  mutate(cab_per_el=sum(prime_minister)) %>% 
  mutate(care_per_el=sum(caretaker)) %>% 
  mutate(pm_lig_care=if_else(cab_per_el==care_per_el,1,0)) %>% 
  mutate(foerstecab=if_else(min(cabfravalg)==cabfravalg,1,0)) %>% 
  mutate(sidstecab=if_else(max(cabfravalg)==cabfravalg,1,0)) %>% 
  filter(!(foerstecab ==1 & caretaker ==1 & pm_lig_care != 1)) %>% 
  filter(!(sidstecab !=1 & caretaker ==1 & pm_lig_care != 1)) %>% 
  mutate(seats_next=if_else(caretaker==1,as.double(NA),seats_next)) %>% 
  mutate(vote_next=if_else(caretaker==1,as.double(NA),vote_next)) %>% 
  mutate(seats_andel=if_else(caretaker==1,as.double(NA),seats_andel)) %>% 
  mutate(delta_vote=if_else(caretaker==1,as.double(NA),delta_vote)) %>% 
  mutate(delta_seats=if_else(caretaker==1,as.double(NA),delta_seats))

x <- kunprime %>% 
  filter(caretaker==1) %>% 
  arrange(-desc(country_name), election_date)
## 64 tekniske regerigner i alt, der går ind og påvirker, altså sidder op til valg
## eller som eneste mellem to valg.
View(x)

### Skal lave en variabel for om PM og PM-parti overlever.
View(kunprime)
#kunprime$cabstart <- NULL
date(kunprime$election_date)
kunprime$aar <- year(kunprime$election_date)


kunprime <- kunprime %>% 
  group_by(country_id) %>% 
  arrange(country_id, desc(aar), desc(start_date))

# SKRIVE KODE TIL OM PM OVERLEVER.
View(kunprime)

### Udregner om der er nyvalg, og hvis det er efter et valg, lagges PM partiet.
kunprime <- kunprime %>% 
  group_by(country_id) %>% 
  mutate(prevELdate=lead(election_date)) %>% 
  mutate(valg = if_else(prevELdate != election_date,1,0))
kunprime <-  kunprime[moveme(names(kunprime), "valg before election_date")]
kunprime <-  kunprime[moveme(names(kunprime), "prevELdate before election_date")]


kunprime <- kunprime %>% 
  group_by(country_id) %>% 
  mutate(lastprime=if_else(valg==1, lead(Prime_id), as.integer(NA)))
#kunprime$lastprime[kunprime$lastprime=="NA"] <- NA
kunprime <- kunprime[moveme(names(kunprime), "Prime_id after start_date")]
kunprime <- kunprime[moveme(names(kunprime), "lastprime after Prime_id")]


### Udregner variabel for om PM overlever. Streng definition. Hvis det er et andet parti, der overtog magten efter valget,
## er man ude selvom den blev vundet tilbage i løbet af perioden.
kunprime <- kunprime %>%
  mutate(PMsurvive=if_else(Prime_id==lastprime,1,0)) %>% 
  group_by(election_id) %>% 
  mutate(PMsurvive=last(PMsurvive))
kunprime <- kunprime[moveme(names(kunprime), "PMsurvive after start_date")]


## Udregner variabel for PM overlever. Blødere definition. Ser på det parti, der sidder op til valget, og det kommer til at stå
## som regeringsparti for hele perioden.
kunprime <- kunprime %>%
  group_by(election_id) %>%
  mutate(lastprime1=last(lastprime)) %>% 
  mutate(PMsurvive_b=if_else(lastprime1==Prime_id,1,0))
kunprime <- kunprime[moveme(names(kunprime), "lastprime1 after Prime_id")]
kunprime <- kunprime[moveme(names(kunprime), "PMsurvive_b after PMsurvive")]
kunprime$lastprime <- kunprime$lastprime1
kunprime$lastprime1 <- NULL

View(kunprime)
table(kunprime$PMsurvive, kunprime$PMsurvive_b)
sum(is.na(kunprime$PMsurvive))
sum(is.na(kunprime$PMsurvive_b))
    
    
## Fjerner dem der ikke er primæreministre op til valg.
kunprime <- kunprime %>% 
  group_by(election_id) %>% 
  filter(cabfravalg==max(cabfravalg))

sum(is.na(kunprime$PMsurvive)) ## 109
sum(is.na(kunprime$PMsurvive_b)) ## 147

# Datovariable.
date(kunprime$election_date)
kunprime$aar <- year(kunprime$election_date)
kunprime$month <- month(kunprime$election_date)
kunprime$dag <- day(kunprime$election_date)
kunprime$ugedag <- wday(kunprime$election_date, label=TRUE)
head(kunprime)

##sorterer data og laver en lead for at regne afstand mellem valg.
kunprime <- kunprime %>% 
  arrange(desc(country_id, election_date)) %>% 
  group_by(country_id) %>% 
  mutate(prevELdate=lead(election_date)
  )

kunprime <- kunprime[moveme(names(kunprime), "election_id before country_name_short")]
kunprime <- kunprime[moveme(names(kunprime), "cabinet_id after election_id")]

kunprime <-  kunprime[moveme(names(kunprime), "prevELdate before election_date")]

kunprime$Tidmellemvalg <-  kunprime$prevELdate %--% kunprime$election_date

kunprime$aarmellemvalg <- 
  dseconds(kunprime$Tidmellemvalg) %>% 
  time_length(unit = "year") %>% 
  round(digits = 2)


### vil gerne koble ParlGovs early election variabel på.

## Ændrer id-navn så det er det sammen
election$election_id <- election$id

## Vælger variable ud i nyt element.
election1 <- election %>%
  select(election_id, early)

## Fjerner lubridate-interval
kunprime$Tidmellemvalg <- NULL

kunprime <- left_join(kunprime, election1, by = "election_id")

kunprime <-  kunprime[moveme(names(kunprime), "early after start_date")]


# Oprydning.
rm(excludes, n_occur, x, x1, dummy)

table(kunprime$aarmellemvalg)

x <- kunprime %>% 
  filter(aarmellemvalg>5)


kunprime$valgperiode <- NULL

### Hvis jeg nu bare tager medianen mellem valg og afrunder
## Hvordan passer den så med valgperioder? 
## Ikke godt
## Forsøger med typetallet.
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

kunprime <- kunprime %>% 
  group_by(country_name) %>% 
  mutate(valgperiode=Mode(ceiling(aarmellemvalg)))

kunprime <- kunprime[moveme(names(kunprime), "valgperiode after country_name")]

## Skriver data ud med kun én observation per land
valgper <- kunprime %>% 
  group_by(country_id) %>% 
  distinct(valgperiode, .keep_all = TRUE) %>% 
  arrange(-desc(country_name))


### Importerer data for antal år i parlament.
valgp <- import("chamber--field_parliamentary_term.csv")
View(valgp)
valgp <- valgp[-c(1,2),]
colnames(valgp) <- valgp[1,]
valgp <- valgp[-1,]
valgp$Chamber <- NULL
valgp$country_name <- valgp$Country
valgp$Country <- NULL
valgp$parliamentary_term <- valgp$`Parliamentary term (years)`
valgp$`Parliamentary term (years)` <- NULL
valgp$parliamentary_term <-  as.numeric(valgp$parliamentary_term)

## laver ny dataframe med ikke-europa, Japan, AUS, NZ, Israel og Canada
asia <- data.frame(country_name=c("Japan", "Australia", "New Zealand", "Canada", "Israel"),
                   "parliamentary_term"= c(4, 3, 3, 5, 4))
View(asia)
valgp <- bind_rows(valgp, asia)

## Joiner
kunprime <- left_join(kunprime, valgp, by = "country_name")

### Kigger på uoverensstemmelse:
kunprime$dis_term <- kunprime$parliamentary_term==kunprime$valgperiode

### Kigger på dem jeg misser:
x <- kunprime %>% 
  group_by(country_id) %>% 
  filter(dis_term==FALSE) %>% 
  distinct(country_id, .keep_all = TRUE)
# Okay, ramte ret meget forbi med typetallet. 11 miss, rammer 6 forbi
# Med en median rundet op, også helt skidt

## Sætter valgperiode variablen ind i data, og ryder op.
kunprime$dis_term <- NULL
kunprime$valgperiode <- kunprime$parliamentary_term
kunprime$parliamentary_term <- NULL


### Der er lande, der fx først går over til deres nuværende grundlov efter WWII,
# startåret for dem ændres.

## Japan får deres nuværende grundlov i 1947, så fjerner det første valg i 1946.
kunprime <- kunprime %>% 
  filter(!(country_name=="Japan" & aar==1946))

#### Nogle skal sorteres fra allerede her.
### I Israel er der mellem 1996 til og IKKE med 2003 direkte valg til PM
## Det har vi ikke data på, vi har kun på parlamentsvalgsresultatet, hvis det anvendes kan det give
## misvisende resutalt, derfor skal de valg kodes ud.
## De valg der kodes ud for Israel: 1996 og 1999 

kunprime <- kunprime %>% 
  filter(!(country_name=="Israel" & aar==1996)) %>% 
  filter(!(country_name=="Israel" & aar==1999))

### Se også på Tyrkiet.
#View(kunprime)
x <- kunprime %>% 
  filter(country_name=="Turkey")
## Tyrkiet kommer kun til at være med til 2011, så det er okay, det er inden kupforsøget i 2016 og reformen, der
## gør dem til et præsidentielt demokrati i 2017, og dermed ikke et parlamentarisk system.


### Også Frankrig, de skal kun medtages fra den femte republik (Det er den femte de er på?)
## Det er fra 1958, så alt før det i Frankrig skal ud. Det bliver 3 valg, der ryger, 56, 51 og 46
kunprime <- kunprime %>% 
  filter(!(country_name=="France" & aar<1958))


### Ved at i Finland er man gået fra 3 års perider til 4 års.
## Mulighed for andre lande, der kan have det samme.

## Der er nogle, der skal sorteres fra.
## Malta før selvstændighed i 1962. ## Der ryger 6 valg.
kunprime <- kunprime %>% 
  filter(!(country_name == "Malta" & aar<1964))

## Sverige har 3 års perioder mellem 68 og 94. Hermansson (2015) Oxford Handbook of Swedish Politics
## sætter til 1969, fordi tidlige valg udregnes ved at lagge det kommende valg.
kunprime <- kunprime %>% 
  mutate(valgperiode=if_else(country_name == "Sweden" & between(aar, 1969, 1994), 3, valgperiode))

## Finland har 3 års perioder fra 45 til 54 (von Schoultz, 2018) Oxford of electoral system.
kunprime <- kunprime %>% 
  mutate(valgperiode=if_else(country_name == "Finland" & 
                               between(aar, 1945, 1954), 3, valgperiode))

## Belgien ser ud til at være fejlkodet, kan det virkeligt passe at de har valgperiode på 5 år?
## Før 2011 med den sjette belgiske stats reform har de en valgperiode på 4 år.
kunprime <- kunprime %>% 
  mutate(valgperiode=if_else(country_name=="Belgium" & aar<2015, 4, valgperiode))

## Se nærmere på Portugal 2009 valg, mulig fejlkodet.
    ## Nej, den er god nok, bare forkert reference i databasen.

## 92 valget i Slovenien skal være landets første.
kunprime <- kunprime %>% 
  filter(!(country_name=="Slovenia" & aar<1993))

## Østrig går  fra 4 års perioder til 5 års.
## De har haft 4 års frem til 06. electionressources.org
kunprime <- kunprime %>% 
  mutate(valgperiode=if_else(country_name=="Austria" & aar<2007, 4, valgperiode))

## Tjek Island fra 46 til 56. ## Nej, de har hele tiden kørt med 4 år.
x <- kunprime %>% 
  filter(country_name=="Iceland")

## Letland 93 skal sættes til første valg.
kunprime <- kunprime %>% 
  filter(!(country_name=="Latvia" & aar<1994))


### Tysklands WWII valg fra 49 skal lige fjernes.
kunprime <- kunprime %>% 
  filter(!(country_name=="Germany" & aar<1950))

## Italiens første valg skal være efter 49, det i 46 er til forfatningsforsamling.
kunprime <- kunprime %>% 
  filter(!(country_name=="Italy" & aar<1949))

## Grækenland skal også tjekkes, særligt 2007.
    ## er rigtigt nok.

## Irland ser ud til at kalde tidligt ofte, men det ser også rigtigt nok ud, men tjek lige op.
      ## Den er sgu god nok. De er værre end os med tidlige valg. 
## http://www.irishstatutebook.ie/eli/1963/act/19/section/10/enacted/en/html#sec10
## Irish Statute Book. Electoral act 1963 og 1992.


## Israel skal der også ses nærmere på. Dog ser den okay ud, måske er 4 måneder en for lille buffer.
x <- kunprime %>% 
  filter(country_name=="Israel")
## Israel ser særlig sjov ud, fordi de et par gange går over valgperioden, men ved et hurtigt kig på de
## givne, står det som at intifadaerne og krige (Yum Kippur i 73) skyldes at valg udskydes. Fair nok.


## Spanien skal der kigges nærmere på, skal slå deres perioder op.

## De har også en tendens til lige at skube valg lidt over perioden, men den er god nok.
## PM kan også frit udskrive valg der.
## Dog er deres første valg for at godkende/skrive forfatning, så det skal kodes fra.
## Deres første reele valg er i 79.
kunprime <- kunprime %>% 
  filter(!(country_name=="Spain" & aar<1980))

## Rumæniens valg i 92 skal måske sættes som deres første.
 ## Ja, deres første valg kommer lige efter kommunismens fald, og er ikke helt demokratisk.
kunprime <- kunprime %>% 
  filter(!(country_name=="Romania" & aar<1993))

## Overvej om de nyeste Tyrkiske valg skal fjernes.
  ## Hvor fed er Erdogan lige, nogle ville nok sige de ikke har fair valg længere.
## De har valgperioder på 5 år før 2007. Derudover fjernes 2015 valgene i Tyrkiet.
x <- kunprime %>% 
  filter(country_name=="Turkey")
kunprime <- kunprime %>% 
  mutate(valgperiode=if_else(country_name=="Turkey" & 
                              between(aar, 1983, 2007), 5, valgperiode))
kunprime <- kunprime %>% 
  filter(!(country_name=="Turkey" & aar==2015))

## Lituans første valg bør være 92, ellers medtages Sovjet valg.
kunprime <- kunprime %>% 
  filter(!(country_name=="Lithuania" & aar<1993))

## Luxemborg valg i 50'erne skal der ses på.
x <- kunprime %>% 
  filter(country_name=="Luxembourg")
## http://luxembourg.public.lu/en/le-grand-duche-se-presente/systeme-politique/constitution-lois/index.html
## Luxemborgs forfatning. Der skal ændres i deres valgperiode, 3 år fra 48 til 54.
kunprime <- kunprime %>% 
  mutate(valgperiode=if_else(country_name=="Luxembourg" & 
                              between(aar, 1948, 1954), 3, valgperiode))

## Slovakiet og Tjekkiet har valg før selvstændighed, hvilket vist faktisk er demokratiske regionale valg i
## Tjekkoslovakiet, men koder dem fra, da de det ikke er nationale parlamenter endnu.
x <- kunprime %>% 
  filter(country_name_short=="CZE" | country_name=="Slovakia")
kunprime <- kunprime %>% 
  filter(!(country_name_short=="CZE" & aar<1993))
kunprime <- kunprime %>% 
  filter(!(country_name=="Slovakia" & aar<1993))

## Portugal 1976 skal være det første, 75 er forfatningsvalg.
## Der er noget der ligner en fejlkodning i portugal 1979, 
kunprime <- kunprime %>% 
  filter(!(country_name=="Portugal" & aar<1977)) %>% 
  filter(!(country_name=="Portugal" & aar==1979))

## Polen, 1991 skal være udgangspunkt ikke 89
kunprime <- kunprime %>% 
  filter(!(country_name=="Poland" & aar<1992))

## Vil gerne lave en variabel for valgsystem, majoritet versus PR
kunprime <- kunprime %>% 
  mutate(valgsystem_majo=if_else(country_name=="France" | country_name=="United Kingdom"
                                 | country_name=="Canada" | country_name=="Australia",1,0))



### Sætter første år til missing
kunprime <- kunprime %>%
  group_by(country_id) %>%
  mutate(PMsurvive=if_else(election_date==min(election_date), as.double(NA), PMsurvive))

#### Udregner time-left variabel
kunprime <- kunprime %>% 
  group_by(country_id) %>% 
  mutate(pred_next_elec=prevELdate+years(valgperiode))
  
kunprime$temp_int <- interval(kunprime$election_date, kunprime$pred_next_elec)

kunprime$timeleft <- dseconds(kunprime$temp_int) %>% 
  time_length(unit = "year") %>% 
  round(digits = 2)

kunprime$timeleft_days <- dseconds(kunprime$temp_int) %>% 
  time_length(unit = "days") %>% 
  round(digits = 2)

### Dummy for tre års valgperioder

kunprime <- kunprime %>% 
  mutate(tre_aar=if_else(valgperiode==3,1,0))

# Valg op til tre måneder før, sættes time_left til 0
kunprime <- kunprime %>% 
  mutate(timeleft=if_else(between(timeleft_days,(-90),90),0,timeleft)) %>% 
  mutate(timeleft=if_else(valg_type==3,as.double(NA),timeleft))

## Fjerner lubridate element
kunprime$temp_int <- NULL

### Laver variable for opportunistiske valg.
kunprime <- kunprime %>% 
  group_by(election_id) %>% 
  mutate(tidlig03=if_else(near(aarmellemvalg, valgperiode, 0.25),0,1))

kunprime <- kunprime %>% 
  group_by(election_id) %>% 
  mutate(tidlig06=if_else(near(aarmellemvalg, valgperiode, 0.5),0,1))

kunprime <- kunprime %>% 
  group_by(election_id) %>% 
  mutate(tidlig=if_else(near(aarmellemvalg, valgperiode, 0.3333),0,1))


kunprime <- kunprime[moveme(names(kunprime), "tidlig after country_name")]
kunprime <- kunprime[moveme(names(kunprime), "aarmellemvalg after country_name")]
kunprime <- kunprime[moveme(names(kunprime), "aar before prevELdate")]

table(kunprime$tidlig03) ## 263
table(kunprime$tidlig06) ## 205
table(kunprime$tidlig) ## 240


x <- kunprime %>% 
  filter(tidlig03==1)
View(x)


#### Joiner Schleiter & Tavits data på kunprime vha. af valgdato
## Først findes de variable jeg gerne vil have med.
glimpse(tavits)

## Ikke enighed om hvad landene skal hedde, så det kigges der på.
## Kun 4 omkodninger.
x <- tavits %>% 
  distinct(countryn, country) %>% 
  select(country) %>% 
  arrange(-desc(country))
x1 <- kunprime %>% 
  group_by(country_id) %>% 
  distinct(country_name) %>% 
  arrange(-desc(country_name))

tavits <- tavits %>% 
  mutate(country=(if_else(country=="Czech Rep","Czech Republic", country))) %>% 
  mutate(country=(if_else(country=="UK","United Kingdom", country))) %>% 
  mutate(country=(if_else(country=="Luxemburg","Luxembourg", country))) %>% 
  mutate(country=(if_else(country=="the Netherlands","Netherlands", country)))


tavits_ex <- tavits %>% 
  select(country, year, starts_with("term"), starts_with("diss"),
         major_elect, election_date, start_date, fsu_eeu, ciep3, surv_pm)
View(tavits_ex)
## Tilføjer prefix, så jeg ved at de er fra tavits-data
colnames(tavits_ex) <- paste0("tav_",colnames(tavits_ex))
tavits_ex <- tavits_ex %>% 
  rename(country_name=tav_country) %>% 
  rename(election_date=tav_election_date)

kunprime$election_date <- date(kunprime$election_date)
kunprime <- left_join(kunprime, tavits_ex, by=c("election_date", "country_name"))



### Importerer og cleaner verdensbank data.
wb_raw <- import("8470b499-ee0e-425b-acd9-cf681a07daa4_Data.csv")
# View(wb_raw)
wb_raw <- wb_raw %>% 
  clean_names() %>%
  remove_empty("rows") %>% 
  remove_empty("cols") %>% 
  group_by(country_code) %>% 
  na_if("..")

names(wb_raw) <- gsub("_yr[[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]]","",names(wb_raw))


wb_raw <- wb_raw[-c(456:460),]
wb_raw$series_code <- NULL
wb <- wb_raw %>% 
  group_by(series_name, country_name) %>% 
  gather("x1960:x2018", gdp, -series_name, -country_name, -country_code) %>% 
  mutate(aar=str_replace(`x1960:x2018`, "x","")) %>% 
  select(-`x1960:x2018`) %>% 
  mutate(gdp=as.numeric(gdp)) %>% 
  mutate(aar=as.numeric(aar)) %>% 
  group_by(country_name) %>% 
  spread(series_name, gdp) %>% 
  clean_names()
names(wb) <- gsub("_annual_percent","",names(wb))
wb$country_code <- NULL


## Laver lags med økonomisk data, tilføjer tomme rækker med år 2019 til hvert land
x1 <- wb %>% 
  filter(aar==1960) %>% 
  mutate(aar=2019)
x1$inflation_consumer_prices <- NA
wb <- bind_rows(x1,wb)

wb <- wb %>% 
    group_by(country_name) %>% 
    arrange(-desc(country_name), desc(aar))%>% 
    dplyr::mutate_at(vars(-country_name, -aar), .funs =funs(lead=lead(.)))
names(wb) <- gsub("lead","lag",names(wb))
unique(wb$country_name)
## Slovakiet skal ændres.
wb$country_name[wb$country_name=="Slovak Republic"] <- "Slovakia" 

kunprime <- left_join(kunprime,wb, by=c("country_name", "aar"))

## Laver et lag på BNP, der skal bruges, når der ses på vote next som afhængig variabel.
kunprime <- kunprime %>% 
  arrange(-desc(country_name), desc(election_date))

kunprime <- kunprime %>% 
  group_by(country_name) %>% 
  mutate(gdp_growth_lag_next=lag(gdp_growth_lag)) %>% 
  mutate(inflation_gdp_deflator_lag_next=lag(inflation_gdp_deflator_lag))



kunprime <- kunprime[moveme(names(kunprime), "tav_surv_pm before PMsurvive")]
kunprime <- kunprime[moveme(names(kunprime), "early after tidlig")]


## Ser på overensstemmelse mellem min estimering og parlgovs variabel.
## og koder opportunist variabel
## i udgangspunktet antages alle valg kaldt over 4 måneder før, at være oppurtunistiske.
kunprime$hit_parlgov <- kunprime$tidlig03==kunprime$early
kunprime$opportunist <- kunprime$tidlig03
kunprime <- kunprime[moveme(names(kunprime), "opportunist after valgperiode")]
table(kunprime$hit_parlgov, kunprime$tidlig03)

kunprime <- kunprime %>%
  group_by(country_name) %>%
  mutate(tav_disspm=median(tav_disspm, na.rm = TRUE)) %>%
  mutate(tav_dissjointgov=median(tav_dissjointgov, na.rm = TRUE)) %>%
  mutate(tav_disspm_0=median(tav_disspm_0, na.rm = TRUE)) %>%
  mutate(tav_disspm_8=median(tav_disspm_8, na.rm = TRUE)) %>%
  mutate(tav_major_elect=median(tav_major_elect, na.rm = TRUE))

table(kunprime$tav_disspm, kunprime$country_name)



## Omkoder de lande hvor det har ændret sig.
## UK efter 2011, skal omkodes til at PM er 0, gov_diss er 7.
## Belgien 95 og frem, govdiss 5,5.
## Tjekkiet efter 09, gov_diss 2,44
## Finland fra 91 og frem, skal være 5 for både pmdiss og gov.
## Portugal før 82, gov_diss 3,44
## Slovakiet før 99 skal være 1,75
## Sverige efter 75 skal have 8,29 for gov_diss
## Australien skal være 10.
## Canada har også 10.
## Kroatien skal være 5,5
# Israel, før 96: PM=0, gov=7
#         efter 96 PM=5, gov=5
## Tyrkiet skal have 2,38
## Malta skal have 5.
## NZ skal have 10.

kunprime <- kunprime %>% 
  mutate(tav_dissjointgov=if_else(country_name=="Australia",10,tav_dissjointgov)) %>% 
  mutate(tav_disspm=if_else(country_name=="Australia",10,tav_disspm)) %>% 
  
  mutate(tav_dissjointgov=if_else(aar>=1995 & country_name=="Belgium",5.5,tav_dissjointgov)) %>% 
  
  mutate(tav_dissjointgov=if_else(country_name=="Canada",10,tav_dissjointgov)) %>% 
  mutate(tav_disspm=if_else(country_name=="Canada",10,tav_disspm)) %>% 
  
  mutate(tav_dissjointgov=if_else(country_name=="Croatia",5.5,tav_dissjointgov)) %>% 
  mutate(tav_disspm=if_else(country_name=="Croatia",0,tav_disspm)) %>% 
  
  mutate(tav_dissjointgov=if_else(aar>=2009 & country_name=="Czech Republic",2.44,tav_dissjointgov)) %>% 
  
  mutate(tav_dissjointgov=if_else(aar>=1991 & country_name=="Finland",5,tav_dissjointgov)) %>% 
  mutate(tav_disspm=if_else(aar>=1991 & country_name=="Finland",5,tav_disspm)) %>% 
  
  mutate(tav_dissjointgov=if_else(aar<1996 & country_name=="Israel",7,tav_dissjointgov)) %>% 
  mutate(tav_disspm=if_else(aar<1996 & country_name=="Israel",0,tav_disspm)) %>% 
  mutate(tav_dissjointgov=if_else(aar>=1996 & country_name=="Israel",5,tav_dissjointgov)) %>% 
  mutate(tav_disspm=if_else(aar>=1996 & country_name=="Israel",5,tav_disspm)) %>% 
  mutate(tav_disspm=if_else(aar>=2003 & country_name=="Israel",2.5,tav_disspm)) %>% 
  mutate(tav_dissjointgov=if_else(aar>=2003 & country_name=="Israel",2.5,tav_dissjointgov)) %>% 
  
  mutate(tav_disspm=if_else(country_name=="Japan",8.5,tav_disspm)) %>% 
  mutate(tav_dissjointgov=if_else(country_name=="Japan",8.5,tav_dissjointgov)) %>% 
  
  mutate(tav_disspm=if_else(country_name=="Malta",5,tav_disspm)) %>% 
  mutate(tav_dissjointgov=if_else(country_name=="Malta",5,tav_dissjointgov)) %>%
  
  mutate(tav_disspm=if_else(country_name=="New Zealand",10,tav_disspm)) %>% 
  mutate(tav_dissjointgov=if_else(country_name=="New Zealand",10,tav_dissjointgov)) %>% 
  
  mutate(tav_dissjointgov=if_else(aar<1982 & country_name=="Portugal",3.44,tav_dissjointgov)) %>% 
  
  mutate(tav_dissjointgov=if_else(aar<1999 & country_name=="Slovakia",1.75,tav_dissjointgov)) %>% 
  
  mutate(tav_dissjointgov=if_else(aar<1971 & country_name=="Sweden",8.5, tav_dissjointgov)) %>%
  mutate(tav_disspm=if_else(aar<1971 & country_name=="Sweden",0, tav_disspm)) %>%
  mutate(tav_disspm=if_else(aar>=1971 & country_name=="Sweden",9.58,tav_disspm)) %>% 
  mutate(tav_dissjointgov=if_else(aar>=1971 & country_name=="Sweden",9.58,tav_dissjointgov)) %>% 
  mutate(tav_disspm=if_else(aar>=1975 & country_name=="Sweden",0,tav_disspm)) %>% 
  mutate(tav_dissjointgov=if_else(aar>=1975 & country_name=="Sweden",8.29,tav_dissjointgov)) %>% 
  
  mutate(tav_dissjointgov=if_else(country_name=="Turkey",2.38,tav_dissjointgov)) %>% 
  mutate(tav_disspm=if_else(country_name=="Turkey",2.38,tav_disspm)) %>% 
  
  mutate(tav_disspm=if_else(aar>=2011 & country_name=="United Kingdom",0,tav_disspm)) %>% 
  mutate(tav_dissjointgov=if_else(aar>=2011 & country_name=="United Kingdom",7,tav_dissjointgov))
  
#Tjek
x <- kunprime %>% 
  select(country_name, tav_disspm, tav_dissjointgov, aar) %>% 
  arrange(country_name) %>% 
  filter(country_name=="Sweden")



### Koder opportunistiske valg
x <- kunprime %>% 
  filter(tidlig03==1) %>% 
  arrange(-desc(country_name), -desc(election_date))
View(x)

x <- x[moveme(names(x), "aar after country_name")]
x <- x[moveme(names(x), "tidlig03 after tidlig")]
## Vær lige opmærksom på de valg de går over valgperioden, de er også med.



## Koder mistillidsvalg eller trække støtten kodes til fejl-valg.

## Østrig 1953 skyldes mindretal i finanslov og regering går af (11).
## Østrig 2008 regeringskollaps, koalitionspartner trækker sig (607).
## Østrig 2017 lederskifte i koalitionsparti der forårsager tidligt valg (1019)
## Belgien 1950 parti der trækker sig fra regeringen (217)
## Belgien 1968 regeringskollaps som følge af lingvistisk krise, Leuven krise (184)
## Bulgarien 1994 mistillidsvotum (355)
## Bulgarien 2014 som følge af tekniske regerigner (840)
## Bulgarien 2017 fordi PM-parti taber præsidentvalg (1011)
## Canada 1963 PM trækker sig over amerikanske atomvåben, stod overfor mistillidsvotum, flere ministre trukket (424)
## Canada 1980 Mistillidsvotum (139)
## Canada 2006 Mistillidsvotum (137)
## Canada 2011 Mistillidsvotum (703)
## Canada 2015 fordi valget i 2011 var tidligt, så bringer valgperioden tilbage under fixed term, kodes som regulært (994)
## Kroatien 2016 mistillidsvotum
## Tjekkiet 1998 regeringssammenbrud (27)
## Danmark april 1953 sker grundet grundlovsændring og kodes derfor som nødvendigt (213)
## Danmark september 1953 nødvendigt efter grundlovsændring (630)
## Danmark 1973 fordi regeringen bringes i mindretal (64)
## Danmark 1981 fordi lov bliver vedtaget med flertal udenom regeringen, regeringen bringes i mindretal (179)
## Estland 1995 fordi en valglov bliver vedtaget (600)
## Finland 1954 mistillid (71)
## Finland 1958 fordi regulære valg udskydes (616)
## Frankrig 1962 mistillid (407)
## Frankrig 1986 er regulært (623)
## Grækenland 1990 politisk dødvande, tekniske regeringer (284)
## Grækenland 1993 regering mister flertal, spg over makedonien (654)
## Grækenland 2012 may grundet dødvande og teknisk regering (783)
## Grækenland 2012 juni skyldes teknisk regering (784)
## Grækenland 2015 januar parlamentet kan ikke opnå flertal for præsident (851)
## Grækenland 2015 september medlemer af PM-partiet skifter parti og bringer regeringen i mindretal (992)
## Island 1953 valgperiode tilbage til valg i juni måned (88)
## Island 1956 regeringen bringes i mindretal (665)
## Island oktober 1959 dødvande og teknisk regering (320)
## Island 1963 skyldes forrige tidlige valg (379)
## Island 1974 skyldes regeringsparti trækker støtten (637)
## Island 1979 dødvande og teknisk regering (436)
## Island 1983 skyldes sidste tidlige valg (114)
## Island 2009 regeringssammenbrud, parti trækker støtte, årsag:finanskrise (36)
## Island 2017 regeringssammenbrud, parti trækker sig (1021)

kunprime <- kunprime %>% 
  mutate(opportunist=if_else(election_id==11,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==607,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==1019,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==217,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==184,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==663,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==486,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==355,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==840,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==1011,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==424,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==139,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==137,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==703,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==994,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==1007,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==27,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==805,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==213,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==630,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==531,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==64,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==179,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==600,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==71,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==616,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==407,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==623,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==95,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==284,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==654,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==783,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==784,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==851,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==992,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==88,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==665,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==320,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==379,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==637,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==436,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==114,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==36,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==1021,3,opportunist))
  

## Starter en ny kode, så den anden ikke bliver alt for lang

## Irland 1951 regeringssammenbrud over sundhedsminister trækker sig over mother and child service (515)
## Irland 1954 Udskriver valg efter mistet flertal (584)
## Irland 1957 Står overfor mistillidsvotum, og udskriver valg (469)
## Irland 1965 Regeringen taber by-election (323)
## Irland feb 1982 Regering kommer i mindretal, kan ikke få flertal for finanslov (228)
## Irland nov 1982 Regering kommer i mindretal, efter støtter trækker sig, løsgængere trækker støtten (307)
## Irland 1987 parti trækker sig fra regering efter uenighed om finanslov (43)
## Irland 1992 støtteparti trækker sig over skandalesag (359)
## Irland 2011 regeringssammenbrud da koalitionspartner trækker sig (698)
## Israel 1951 Regering trækker sig, da de ikke kan få flertal for lovgivning (705)
## Israel 1959 regering bragt i mindretal og salg af våben til Tyskland (707)
## Israel 1961 mistillidsvotum (708)
## Israel 1977 mistillidsvotum (712)
## Israel 2006 Koalitionspartner trækker sig (720)
## Israel 2009 PM trækker sig, og den nye partileder kan ikke få forhandlet sig til ny regering (721)
## Italien 1987 mistillidsvotum (17)
## Italien 1994 kan ikke opnås flertal for ny regering (146)
## Italien 1996 kan ikke opnås flertal for ny regering (551)
## Japan 1953 mistillidsvotum (693)
## Japan 1955 mistillidsvotum i udsigt (692)
## Japan 1980 mistillidsvotum (113)
## Japan 1993 mistillidsvotum (57)
## Japan 2012 regering mister flertal (788)
## Letland 2014 fordi det i 2011 var tidligt, så kodes regulært (831)
## Lux 1959 grundet koalitionsparti trækker (390)
## Lux 1964 fordi det forrige var tidligt, så skal tilbage på valgperioden, kodes regulært (579)
## Lux 1974 fordi det forrige var tidligt, så dette er regulært (446)
## Lux 2013 koalitionsparti trækker sig (803)
## Malta 1976 er regulært, det er fordi det går over perioden (533)
## Malta 1987 er regulært, det er fordi det går over perioden (370)
## Malta 1998 mistillidsvotum (173)
## Holland 1959 bragt i mindretal i parlamentet (667)
## Holland 1972 koalitionspartner trækker sig (51)
## Holland 1977 koalitionspartner trækker sig (635)
## Holland 1982 koalitionspartner trækker sig (116)
## Holland 1989 koalitionspartner trækker sig (627)
## Holland 1994 valg senere end valgperioden, ligner derfor tidligt i data, kodes regulært (570)
## Holland 2003 koalitionspartner trækker sig (335)
## Holland 2010 regeringssammenbrud, parti trækker sig (530)
## Holland 2012 regeringssammenbrud (785)
## Holland 2017 ligner tidligt i data fordi det går over valgperidoen, kodes regulært (1012)

kunprime <- kunprime %>% 
  mutate(opportunist=if_else(election_id==515,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==584,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==469,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==323,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==228,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==307,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==43,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==359,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==698,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==705,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==707,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==708,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==712,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==720,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==721,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==17,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==146,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==551,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==693,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==692,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==113,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==57,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==788,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==831,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==390,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==579,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==446,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==803,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==533,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==370,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==173,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==667,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==51,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==635,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==116,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==627,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==570,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==335,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==530,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==785,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==1012,0,opportunist))
  
  

### Starter ny kode, da den anden blev lang. 
  
## Polen 1993 mistillidsvotum (268)
## Portugal 1979 koalitionspartner trækker sig (248)
## Portugal 1980 skyldes valget i 1979 var tidligt, kodes regulært (281)
## Portugal 1983 ministre trækker sig, og resulterer i parlamentarisk dødvande (598)
## Portugal 1985 koalitionspartner trækker sig, bringer regering i mindretal (106)
## Portugal 1987 mistillidsvotum (480)
## Portugal 2002 Regering, der trækker sig tilbage (657)
## Portugal 2009 Ligger efter valgperioden, ligner tidligt valg i data, men er regulært (405)
## Portugal 2011 Regering kommer i mindretal (702)
## Portugal 2015 kommer til at ligne tidligt, fordi det ligger et stykke efter forventet, kodes regulært (993)
## Slovakiet 1994 mistillidsvotum (119)
## Slovakiet 2006 Koalitionspartner trækker sig og bringer regering i mindretal (192)
## Slovakiet 2012 Mistillidsvotum (781)
## Slovenien 2011 Mistillidsvotum (778)
## Slovenien 2014 koalitionspartnere trækker sig (817)
## Spanien 1996 Støtteparti bringer regering i mindretal og blokerer finanslov (99)
## Spanien 2016 dødvande efter sidste valg fører til nyvalg (1005)
## Sverige 1958 mistillidsvotum (251)
## Sverige 1960 skyldes det i 1958 var tidligt (295)
## Sverige 1970 skyldes ny grundlov (258)
## Tyrkiet 1995 mistillidsvotum (810)
## Tyrkiet 2002 regeringssammenbrud (808)
## Storbritannien 1979 mistillidsvotum (243)
  
kunprime <- kunprime %>% 
  mutate(opportunist=if_else(election_id==268,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==248,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==281,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==598,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==106,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==480,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==657,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==405,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==702,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==993,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==119,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==192,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==781,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==778,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==817,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==99,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==1005,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==251,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==295,0,opportunist)) %>%
  mutate(opportunist=if_else(election_id==258,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==810,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==808,3,opportunist)) %>%
  mutate(opportunist=if_else(election_id==243,3,opportunist))
  



kunprime %>% 
  group_by(country_name) %>% 
  filter(!opportunist==3) %>% 
  summarise(qwer=sum(opportunist, na.rm = TRUE)) %>% 
  arrange(desc(qwer)) %>% 
  print(n = 35)

### Laver en variabel for strategiske valg og de forskellige tidsperioder,
## 3 måneder før, 4 måneder før og 6 måneder før.

kunprime <- kunprime %>% 
  mutate(valg_type=opportunist) %>% 
  mutate(strategisk=if_else(valg_type<=2,opportunist,as.double(NA))) %>% 
  ### Laver for 4 måneder
  mutate(strategisk04=if_else(tidlig==1,strategisk,tidlig)) %>% 
  mutate(strategisk04=if_else(valg_type<=2,strategisk04,as.double(NA))) %>% 
  ### Laver for 6 måneder
  mutate(strategisk06=if_else(tidlig06==1,strategisk,tidlig)) %>% 
  mutate(strategisk06=if_else(valg_type<=2,strategisk06,as.double(NA)))

View(kunprime)

x <- kunprime %>% 
  filter(country_name=="Denmark")
View(x)



### Opportunist sammenholder strategisk timede med regulære valg.

### laver en variabel, der også inkluderer mistillidsvalg.
kunprime <- kunprime %>% 
  mutate(alle_valg03=if_else(valg_type==0 | valg_type==3,0,strategisk)) %>% 
  mutate(alle_valg04=if_else(valg_type==0 | valg_type==3,0,strategisk04)) %>% 
  mutate(alle_valg06=if_else(valg_type==0 | valg_type==3,0,strategisk06))


### Laver decade dummies

kunprime <- kunprime %>% 
  mutate(decade=0) %>% 
  mutate(decade=na_if(decade,0)) %>% 
  mutate(decade=if_else(between(aar,1940,1949),40,decade)) %>% 
  mutate(decade=if_else(between(aar,1950,1959),50,decade)) %>% 
  mutate(decade=if_else(between(aar,1960,1969),60,decade)) %>% 
  mutate(decade=if_else(between(aar,1970,1979),70,decade)) %>% 
  mutate(decade=if_else(between(aar,1980,1989),80,decade)) %>% 
  mutate(decade=if_else(between(aar,1990,1999),90,decade)) %>% 
  mutate(decade=if_else(between(aar,2000,2009),00,decade)) %>% 
  mutate(decade=if_else(between(aar,2010,2019),10,decade))


kunprime <- kunprime %>% 
  arrange(-desc(country_name), desc(election_date)) %>% 
  group_by(country_name) %>% 
  mutate(next_election=lag(election_date)) %>% 
  mutate(next_aar_election=year(next_election))

# Lave lag til strategisk variabel, så den kan køres i modellerne,
# med stemmeandel og mandatandel, da den afhængige variabel i disse modeller
# er stemmeandel og mandat opnået ved valget, derfor skal strategiske valg lagges
# ift modellen for om pm overlever.

kunprime <- kunprime %>% 
  group_by(country_id) %>% 
  mutate(strat_lag03=lag(strategisk)) %>% 
  mutate(strat_lag04=lag(strategisk04)) %>% 
  mutate(strat_lag06=lag(strategisk06)) %>% 
  mutate(alle_valg03_lag=lag(alle_valg03)) %>% 
  mutate(alle_valg04_lag=lag(alle_valg04)) %>% 
  mutate(alle_valg06_lag=lag(alle_valg06))


### Fjerne valg, hvor det er caretaker, der sidder op til næste valg
## Kigger lige på Italien, da det er godt eksempel med caretaker, og PM uden parti.
x <- kunprime %>% 
  filter(country_name=="Italy")

kunprime <- kunprime %>% 
  filter(!(caretaker==1))
## Datasæt på 487 obs.
sum(is.na(kunprime$PMsurvive))
# 408 med obs på PM survive.
# måske kun 392.



#### Den laver noget skidt med de partier der er i valgalliancer, fx CDU og CSU i Tyskland.
## Deres resultater tilføjes manuelt. 42 styks.
x <- kunprime %>% 
  filter(is.na(vote_share))


kunprime <- kunprime %>% 
  mutate(vote_share=if_else(election_id==126,49.2,vote_share)) %>% 
  mutate(vote_next=if_else(election_id==126,18.2,vote_next)) %>% 
  mutate(seats_andel=if_else(election_id==126,57.1,seats_andel)) %>% 
  mutate(seats_next=if_else(election_id==126,21.3,seats_next)) %>% 

  mutate(vote_share=if_else(election_id==496,22.8,vote_share)) %>% 
  mutate(vote_next=if_else(election_id==496,36.0,vote_next)) %>% 
  mutate(seats_andel=if_else(election_id==496,21.5,seats_andel)) %>% 
  mutate(seats_next=if_else(election_id==496,56.5,seats_next)) %>% 

  mutate(vote_share=if_else(election_id==386,38.0,vote_share)) %>% 
  mutate(vote_next=if_else(election_id==386,26.0,vote_next)) %>% 
  mutate(seats_andel=if_else(election_id==386,60,seats_andel)) %>% 
  mutate(seats_next=if_else(election_id==386,37.6,seats_next)) %>%
    
  mutate(vote_share=if_else(election_id==291,33,vote_share)) %>% 
  mutate(vote_next=if_else(election_id==291,38,vote_next)) %>% 
  mutate(seats_andel=if_else(election_id==291,40.6,seats_andel)) %>% 
  mutate(seats_next=if_else(election_id==291,60,seats_next)) %>%

  mutate(vote_share=if_else(election_id==1016,33,vote_share)) %>% 
  mutate(seats_andel=if_else(election_id==1016,34.7,seats_andel)) %>% 
    
  mutate(vote_share=if_else(election_id==800,41.5,vote_share)) %>% 
  mutate(vote_next=if_else(election_id==800,33,vote_next)) %>% 
  mutate(seats_andel=if_else(election_id==800,49.3,seats_andel)) %>% 
  mutate(seats_next=if_else(election_id==800,34.7,seats_next)) %>%
    
  mutate(vote_share=if_else(election_id==93,33.8,vote_share)) %>% 
  mutate(vote_next=if_else(election_id==93,41.5,vote_next)) %>% 
  mutate(seats_andel=if_else(election_id==93,38.4,seats_andel)) %>% 
  mutate(seats_next=if_else(election_id==93,49.3,seats_next)) %>%
    
  mutate(vote_share=if_else(election_id==118,35.2,vote_share)) %>% 
  mutate(vote_next=if_else(election_id==118,33.8,vote_next)) %>% 
  mutate(seats_andel=if_else(election_id==118,36.8,seats_andel)) %>% 
  mutate(seats_next=if_else(election_id==118,38.4,seats_next)) %>%
    
  mutate(vote_share=if_else(election_id==595,41.5,vote_share)) %>% 
  mutate(vote_next=if_else(election_id==595,35.1,vote_next)) %>% 
  mutate(seats_andel=if_else(election_id==595,43.7,seats_andel)) %>% 
  mutate(seats_next=if_else(election_id==595,36.6,seats_next)) %>%
    
  mutate(vote_share=if_else(election_id==445,43.8,vote_share)) %>% 
  mutate(vote_next=if_else(election_id==445,41.5,vote_next)) %>% 
  mutate(seats_andel=if_else(election_id==445,48.2,seats_andel)) %>% 
  mutate(seats_next=if_else(election_id==445,43.7,seats_next)) %>%
  
  mutate(vote_share=if_else(election_id==588,44.3,vote_share)) %>% 
  mutate(vote_next=if_else(election_id==588,43.8,vote_next)) %>% 
  mutate(seats_andel=if_else(election_id==588,44.9,seats_andel)) %>% 
  mutate(seats_next=if_else(election_id==588,48.2,seats_next)) %>%
    
    mutate(vote_share=if_else(election_id==484,48.8,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==484,44.3,vote_next)) %>% 
    mutate(seats_andel=if_else(election_id==484,49,seats_andel)) %>% 
    mutate(seats_next=if_else(election_id==484,44.9,seats_next)) %>%
    
    mutate(vote_share=if_else(election_id==547,44.5,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==547,48.8,vote_next)) %>% 
    mutate(seats_andel=if_else(election_id==547,45.5,seats_andel)) %>% 
    mutate(seats_next=if_else(election_id==547,49.0,seats_next)) %>%
    
    mutate(vote_share=if_else(election_id==250,47.8,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==250,46.1,vote_next)) %>% 
    mutate(seats_andel=if_else(election_id==250,49.4,seats_andel)) %>% 
    mutate(seats_next=if_else(election_id==250,48.8,seats_next)) %>%
    
    mutate(vote_share=if_else(election_id==381,45.4,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==381,47.8,vote_next)) %>% 
    mutate(seats_andel=if_else(election_id==381,48.5,seats_andel)) %>% 
    mutate(seats_next=if_else(election_id==381,49.4,seats_next)) %>%
    
    mutate(vote_share=if_else(election_id==331,50.2,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==331,45.4,vote_next)) %>% 
    mutate(seats_andel=if_else(election_id==331,54.4,seats_andel)) %>% 
    mutate(seats_next=if_else(election_id==331,48.5,seats_next)) %>%
    
    mutate(vote_share=if_else(election_id==103,45.2,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==103,50.2,vote_next)) %>% 
    mutate(seats_andel=if_else(election_id==103,49.9,seats_andel)) %>% 
    mutate(seats_next=if_else(election_id==103,54.3,seats_next)) %>%
    
    mutate(vote_share=if_else(election_id==227,48.1,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==227,45.8,vote_next)) %>% 
    mutate(seats_andel=if_else(election_id==227,57.3,seats_andel)) %>% 
    mutate(seats_next=if_else(election_id==227,53.7,seats_next)) %>% 
  
    mutate(vote_share=if_else(election_id==814,44.9,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==814,49.3,vote_next)) %>% 
      
    mutate(vote_share=if_else(election_id==688,52.7,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==688,44.9,vote_next)) %>% 
    
    mutate(vote_share=if_else(election_id==327,42.4,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==327,16.0,vote_next)) %>% 
    mutate(seats_andel=if_else(election_id==327,64.2,seats_andel)) %>% 
    mutate(seats_next=if_else(election_id==327,11.9,seats_next)) %>% 
    
    mutate(vote_share=if_else(election_id==511,35.0,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==511,38.2,vote_next)) %>% 
    mutate(seats_andel=if_else(election_id==511,50.2,seats_andel)) %>% 
    mutate(seats_next=if_else(election_id==511,61.7,seats_next)) %>% 
    
    mutate(vote_share=if_else(election_id==57,36.6,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==57,32.8,vote_next)) %>% 
    mutate(seats_andel=if_else(election_id==57,43.6,seats_andel)) %>% 
    mutate(seats_next=if_else(election_id==57,47.8,seats_next)) %>%
    
    mutate(vote_share=if_else(election_id==161,49.4,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==161,43.4,vote_next)) %>% 
    
    mutate(vote_share=if_else(election_id==678,49.7,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==678,49.4,vote_next)) %>% 
    
    mutate(vote_share=if_else(election_id==102,48.0,vote_share)) %>% 
    mutate(vote_next=if_else(election_id==102,49.7,vote_next))

x <- kunprime %>% 
  filter(is.na(PMsurvive) & is.na(vote_next) & is.na(seats_next))

x <- kunprime %>% 
  filter(country_name=="Belgium")

########################################################################
######################                        ##########################
######################  DESKRIPTIV STATISTIK  ##########################
######################                        ##########################
########################################################################

x <- kunprime %>% 
  filter(is.na(vote_next) & !(is.na(seats_next)))
View(x)



### Sletter alt undtagen kunprime og moveme-funktionen
rm(list=ls()[! ls() %in% c("kunprime","moveme")])


### Deskriptiv statistik
deskriptiv <- kunprime %>% 
  select(PMsurvive, vote_share, seats_andel, vote_next, seats_next,
         strategisk, inflation_gdp_deflator_lag,
         gdp_growth_lag, tav_disspm, country_name, alle_valg03, strat_lag03, alle_valg03_lag)

deskriptiv <- deskriptiv %>% 
  group_by(country_id) %>% 
  mutate(disspm_lag=lag(tav_disspm)) %>% 
  mutate(tav_disspm=if_else(
    tav_disspm == disspm_lag,as.double(NA),tav_disspm, missing=tav_disspm)) %>% 
  mutate(test=min_rank(tav_disspm))
deskriptiv$disspm_lag <- NULL


#deskriptiv$country_name <- NULL
deskriptiv <- as.data.frame(deskriptiv)
class(deskriptiv)
## tekst
write_clip(
stargazer(deskriptiv[c("strategisk", "PMsurvive", "vote_next", 
                       "seats_next", "tav_disspm", 
                       "gdp_growth_lag", "inflation_gdp_deflator_lag")], 
           covariate.labels = c("Strategisk timet valg", "PM bevarer posten",
                                "Stemmeandel", "Andel af mandater", "PM opløsningsmagt",
                                "BNP vækst", "Inflation"),
           
          type = "text", digits = 2, style = "commadefault",
          omit.summary.stat =  c("p25", "p75"), median = TRUE,
          notes = c("Note: variable med udgangspunkt i premierministerpartiet.",
          "Udover PM opløsningsmagt er de øvrige variable på valgniveau.",
          "Der indgår 35 lande. For PM opløsningsmagt er antallet af observationer",
          "over 35, da nogle lande skifter loven for opløningsmagt."),
          title = "Tabel X: Deskriptiv statistik")
)
### Husk at PM opløsningsmagt skal der fjernes 1, ellers tæller Sveriges 0 to gange.




## Tekst.
write_clip(
stargazer(subset(deskriptiv[c("PMsurvive", "vote_next", "seats_next")], deskriptiv$strategisk==1),
          title = "Strategisk timet valg", type = "text", 
          omit.summary.stat = c("p25", "p75", "sd", "min", "max"),
          digits = 2, style = "commadefault",
          covariate.labels = c("PM bevarer posten", "Stemmeandel", "Andel af mandater"))
)

write_clip(
  stargazer(subset(deskriptiv[c("PMsurvive", "vote_next", "seats_next")], deskriptiv$strat_lag03==1),
            title = "Strategisk timet valg", type = "text", 
            omit.summary.stat = c("p25", "p75", "sd", "min", "max"),
            digits = 2, style = "commadefault",
            covariate.labels = c("PM bevarer posten", "Stemmeandel", "Andel af mandater"))
)


write_clip(
  stargazer(subset(deskriptiv[c("PMsurvive", "vote_next", "seats_next")], deskriptiv$strategisk==0),
            title = "Regulært valg", type = "text", 
            omit.summary.stat = c("p25", "p75", "sd", "min", "max"),
            digits = 2, style = "commadefault",
            covariate.labels = c("PM bevarer posten", "Stemmeandel", "Andel af mandater"))
)


write_clip(
stargazer(subset(deskriptiv[c("PMsurvive", "vote_next", "seats_next")], deskriptiv$strat_lag03==0),
          title = "Regulært valg", type = "text", 
          omit.summary.stat = c("p25", "p75", "sd", "min", "max"),
          digits = 2, style = "commadefault",
          covariate.labels = c("PM bevarer posten", "Stemmeandel", "Andel af mandater"))
)

stargazer(subset(deskriptiv[c("PMsurvive", "vote_next", "seats_next")], deskriptiv$alle_valg03_lag==0),
          title = "Regulære og valg af politisk nødvendighed", type = "text", 
          omit.summary.stat = c("p25", "p75", "sd", "min", "max"),
          digits = 2, style = "commadefault",
          covariate.labels = c("PM bevarer posten", "Stemmeandel", "Andel af mandater"))

t.test(kunprime$vote_next ~ kunprime$strat_lag03)
t.test(kunprime$seats_next ~ kunprime$strat_lag03)
t.test(kunprime$PMsurvive ~ kunprime$strategisk)
prob <- table(kunprime$PMsurvive, kunprime$strategisk)
prop.test(prob)

t.test(kunprime$vote_next, kunprime$seats_next, paired = TRUE)
t.test(kunprime$vote_next, kunprime$seats_next)

### Israel 1981 (125) og 1984 (149) er inflation på over 100
## også i Tyrkiet i 1999 (143), men den er god nok.
x <- kunprime %>% 
  filter(inflation_gdp_deflator_lag>100)
View(x)

#tekst
disspm <- kunprime %>% 
  select(country_name, tav_disspm, aar) %>% 
  distinct(country_name, tav_disspm, .keep_all = T) %>% 
  arrange(-desc(country_name), -desc(aar))
disspm <- as.data.frame(disspm)

write_clip(
stargazer(disspm[c("country_name", "aar", "tav_disspm")],
          covariate.labels = c("", "Land", "År", "PM opløsningsmagt"),
          type = "html", style = "commadefault", summary = FALSE,
          notes = c("Landenavnene er på engelsk, da det anvendte indeks",
          "er fra Goplerud [& Schleiter (2016)"))
)
stargazer(disspm, type = "html",
          median = TRUE)


### Minimum og maksimum år for landene der er med
tidsperiode <- kunprime %>% 
  group_by(country_name) %>% 
  filter(aar == min(aar) | aar==max(aar)) %>% 
  select(country_name, aar) %>% 
  arrange(-desc(country_name),-desc(aar))

tidsperiode %>% 
  tbl_df() %>% 
  print(n=70)

x <- kunprime %>% 
  filter(country_name=="Israel")
View(x)



########################################################################
######################                        ##########################
######################      KØRER MODELLER    ##########################
######################                        ##########################
########################################################################


kunprime$seats_andel
sum(is.na(kunprime$seats_andel))
kunprime$vote_share
sum(is.na(kunprime$vote_share))
kunprime$PMsurvive

mean(kunprime$seats_next, na.rm = TRUE)




####################################################################################
#####################                                  #############################
#####################     OLS OG LOGIT MODELLER        #############################
#####################                                  #############################
####################################################################################

rm(list=ls()[! ls() %in% c("kunprime","moveme")])
#### PM Survive

PMsurv_ols1 <- lm_robust(PMsurvive~strategisk,
                  data = kunprime,
                  clusters = country_id,
                  se_type = "stata")
summary(PMsurv_ols1)
## LM-objekt til stargazer
star_PMsurv_ols1 <-lm(PMsurvive~strategisk, data = kunprime)


PMsurv_logit1 <- glm.cluster(PMsurvive ~ strategisk,
                            data = kunprime,
                            cluster = "country_id",
                            family = "binomial")
summary(PMsurv_logit1)
# Gemmer standardfejl for logit 1
PMsurv_logit1_se <- sqrt(diag(PMsurv_logit1$vcov))

### Udregner Odds ratio
PMsurv_logit1_OR <- exp(cbind(coef(PMsurv_logit1), confint(PMsurv_logit1)))
PMsurv_logit1_OR

### Udregner margins for GLM modellen, margins-funktionen kan ikke spise
## glm.cluster objekter, så den må lige laves med en almindelige glm.
PMsurv_logit_marginal1 <- glm(PMsurvive ~ strategisk,data = kunprime,family = "binomial")
summary(PMsurv_logit_marginal1)

AME_pmsurv <- summary(margins(PMsurv_logit_marginal1))

### OLS og GLM (AME) - estimatet er næsten ens.

### Tiljøjer variable, årti-dummies
PMsurv_ols2 <- lm_robust(PMsurvive ~ strategisk+
                        factor(decade),
                        data = kunprime,
                        clusters = country_id,
                        se_type = "stata")
summary(PMsurv_ols2)
summary(PMsurv_ols2$N)
star_PMsurv_ols2 <- lm(PMsurvive ~ strategisk+factor(decade),data = kunprime)

PMsurv_logit2 <- glm.cluster(PMsurvive ~ strategisk+
                               factor(decade),
                             data = kunprime,
                             cluster = "country_id",
                             family = "binomial")
summary(PMsurv_logit2)
## Gemmer robuste standardfejl
PMsurv_logit2_se <- sqrt(diag(PMsurv_logit2$vcov))
## OR
PMsurv_OR_logit2 <- exp(coef(PMsurv_logit2))
PMsurv_OR_logit2

### Udregner margins for logit 2
PMsurv_logit_marginal2 <- glm(PMsurvive ~ strategisk+factor(decade), data = kunprime, family = "binomial")
summary(PMsurv_logit_marginal2)
AME_pmsurv2 <- summary(margins(PMsurv_logit_marginal2))
AME_pmsurv2

### Tilføjer økonomi
PMsurv_ols3 <- lm_robust(PMsurvive ~ strategisk+
                           gdp_growth_lag+
                           inflation_gdp_deflator_lag+
                           factor(decade),
                         data = kunprime,
                         clusters = country_id,
                         se_type = "stata")
summary(PMsurv_ols3)
summary(PMsurv_ols3$N)
## Går fra 361 til 306 (305 med inflation) observationer.
star_PMsurv_ols3 <- lm(PMsurvive ~ strategisk+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),data = kunprime)



PMsurv_logit3 <- glm.cluster(PMsurvive ~ strategisk+
                               gdp_growth_lag+
                               inflation_gdp_deflator_lag+
                               factor(decade),
                             data = kunprime,
                             cluster = "country_id",
                             family = "binomial")
summary(PMsurv_logit3)
## Gemmer robuste standardfejl
PMsurv_logit3_se <- sqrt(diag(PMsurv_logit3$vcov))
## OR
PMsurv_OR_logit3 <- exp(coef(PMsurv_logit3))
PMsurv_OR_logit3
## Margins for logit 3
PMsurv_logit_marginal3 <- glm(PMsurvive~strategisk+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade), data = kunprime, family = "binomial")
summary(PMsurv_logit_marginal3)
AME_pmsurv3 <- summary(margins(PMsurv_logit_marginal3))
AME_pmsurv3



?stargazer

setwd("/Users/ethranholm/OneDrive/R/Speciale output")

#### Kører data ud i stargazer

write_clip(
stargazer(star_PMsurv_ols1, PMsurv_logit_marginal1, 
          star_PMsurv_ols2, PMsurv_logit_marginal2,
          star_PMsurv_ols3, PMsurv_logit_marginal3,
          
          se = list(PMsurv_ols1$std.error, PMsurv_logit1_se,
                    PMsurv_ols2$std.error, PMsurv_logit2_se,
                    PMsurv_ols3$std.error, PMsurv_logit3_se),
          
          covariate.labels = c("Strategisk timet valg", "BNP vækst (1 år lag)",
                              "Inflation (1 år lag)", "Konstant"),
          
          dep.var.caption = "Afhængig variabel:", dep.var.labels = "premierministerparti bevarer posten",
          
          omit = "decade", omit.labels ="årti-dummies", omit.yes.no = c("Nej", "Ja"),
          notes = c("Lande-robuste standardfejl i parentes.", "For de logistiske regressioner vises log-odds."),
          omit.stat = c("ser","f"),
          notes.align = "l",
          
          type = "text", style = "commadefault")
)




### Tabel med marginale effekter. OBS STANDARDFEJL TILFØJES MANUELT.
write_clip(
stargazer(PMsurv_logit_marginal1, PMsurv_logit_marginal2, 
          PMsurv_logit_marginal3,
          
          coef = list(AME_pmsurv$AME, AME_pmsurv2$AME, AME_pmsurv3$AME),
          se = list(AME_pmsurv$p, AME_pmsurv2$p, AME_pmsurv3$p),
          
          keep = "strategisk",
          omit = c("decade", "gdp_growth_lag"), 
          omit.labels = c("årti-dummies", "Kontrol økonomi"),
          omit.yes.no = c("Nej", "Ja"),
          covariate.labels = "Strategisk timet valg",
          dep.var.caption = "Afhængig variabel:", dep.var.labels = "premierministerparti bevarer posten",
          notes = "Viser de gennemsnitlige marginale effekter for de logistiske regressioner estimeret i tabel X",
          type = "text", style = "commadefault",
          notes.align = "l")
)



#################################################################################
#######################                               ###########################
######################  OLS for stemmer og mandater   ###########################
######################                                ###########################
#################################################################################



########## Stemmer ##########
vote_ols1 <- lm_robust(vote_next ~ strat_lag03,
                      data = kunprime,
                      clusters = country_id,
                      se_type = "stata")
summary(vote_ols1)
## Laver objekt til stargazer
star_vote_ols1 <- lm(vote_next ~ strat_lag03, data = kunprime)

### Nuværende stemmeandel, og tid
vote_ols2 <- lm_robust(vote_next ~ strat_lag03+vote_share+
                       factor(decade), 
                       data = kunprime,
                       clusters = country_id,
                       se_type = "stata")
summary(vote_ols2)
star_vote_ols2 <- lm(vote_next ~ strat_lag03+vote_share+factor(decade),data = kunprime)


## økonomi
vote_ols3 <- lm_robust(vote_next ~ strat_lag03+vote_share+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade), 
                       data = kunprime,
                       clusters = country_id,
                       se_type = "stata")
summary(vote_ols3)
star_vote_ols3 <- lm(vote_next ~ strat_lag03+vote_share+
                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)



########## Mandater ############

seats_ols1 <- lm_robust(seats_next ~ strat_lag03,
                        data = kunprime,
                        clusters = country_id,
                        se_type = "stata")
summary(seats_ols1)
star_seats_ols1 <- lm(seats_next ~ strat_lag03,data = kunprime)


#### nuværende mandater og tidsdummies

seats_ols2 <- lm_robust(seats_next ~ strat_lag03+seats_andel+
                          factor(decade),
                        data = kunprime,
                        clusters = country_id,
                        se_type = "stata")
summary(seats_ols2)
star_seats_ols2 <- lm(seats_next ~ strat_lag03+seats_andel+factor(decade),data = kunprime)

### Økonomi

seats_ols3 <- lm_robust(seats_next ~ strat_lag03+seats_andel+
                          gdp_growth_lag_next+
                          inflation_gdp_deflator_lag_next+
                          factor(decade),
                        data = kunprime,
                        clusters = country_id,
                        se_type = "stata")
summary(seats_ols3)
star_seats_ols3 <- lm(seats_next ~ strat_lag03+seats_andel+gdp_growth_lag_next+
                        inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)


#### Skriver til stargazer
write_clip(
stargazer(star_vote_ols1, star_vote_ols2, star_vote_ols3,
          star_seats_ols1, star_seats_ols2, star_seats_ols3,
          
          se = list(vote_ols1$std.error, vote_ols2$std.error,
                    vote_ols3$std.error,
                    seats_ols1$std.error, seats_ols2$std.error,
                    seats_ols3$std.error),
          
          omit = "decade", omit.labels = "årti-dummies", omit.yes.no = c("Nej", "Ja"),
          notes = "Lande-robuste standardfejl i parentes.", omit.stat = c("ser","f"),
          order = c("strat_lag03", "vote_share", "seats_andel"),
          
          covariate.labels = c("Strategisk timet valg", "Nuværende stemmeandel",
                               "Nuværende mandatandel", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "Konstant"),
          dep.var.caption = "Afhængig variable:", notes.align = "l",
          dep.var.labels = c("Stemmeandel", "Mandatandel"),
          
          type = "html", style = "commadefault")
)



#################################################################################
#######################                               ###########################
######################    First stage regressioner    ###########################
######################                                ###########################
#################################################################################

rm(list=ls()[! ls() %in% c("kunprime","moveme")])

first_stage_ols1 <- lm_robust(strategisk ~ tav_disspm+
                                   factor(decade), 
                                 data = kunprime,
                                 clusters = country_id,
                                 se_type = "stata")
summary(first_stage_ols1)
star_first_stage_ols <- lm(strategisk ~ tav_disspm+factor(decade), data = kunprime)



first_stage_logit1 <- glm.cluster(strategisk ~ tav_disspm+
                                           factor(decade),
                                         data = kunprime,
                                         cluster = "country_id",
                                         family = "binomial")
summary(first_stage_logit1)
first_stage_logit1_se <- sqrt(diag(first_stage_logit1$vcov))
## OR
first_stage_logit1_OR <- exp(coef(first_stage_logit1))
first_stage_logit1_OR
## Margins
first_stage_marginal1 <- glm(strategisk ~ tav_disspm+factor(decade), data = kunprime,family = "binomial")
AME_first_stage1 <- summary(margins(first_stage_marginal1))
AME_first_stage1



### Stemmer
first_stage_ols_vote <- lm_robust(strat_lag03 ~ tav_disspm+vote_share+factor(decade),
                                  data = kunprime,
                                  clusters = country_id,
                                  se_type = "stata")
summary(first_stage_ols_vote)
star_first_stage_ols_vote <- lm(strat_lag03 ~ tav_disspm+vote_share+factor(decade), data = kunprime)

##logit
first_stage_logit_vote <- glm.cluster(strat_lag03 ~ tav_disspm+vote_share+factor(decade),
                                     data = kunprime,
                                     cluster = "country_id",
                                     family = "binomial")
summary(first_stage_logit_vote)
first_stage_logit_vote_se <- sqrt(diag(first_stage_logit_vote$vcov))
## Margins
first_stage_marginal_vote <- glm(strat_lag03 ~ tav_disspm+vote_share+factor(decade),
                                 data = kunprime,family = "binomial")
AME_first_stage_vote <- summary(margins(first_stage_marginal_vote))
AME_first_stage_vote

### Mandater
first_stage_ols_seats <- lm_robust(strat_lag03 ~ tav_disspm+seats_andel+
                                     factor(decade), 
                                   data = kunprime,
                                   clusters = country_id,
                                   se_type = "stata")
summary(first_stage_ols_seats)
star_first_stage_ols_seats <- lm(strat_lag03 ~ tav_disspm+seats_andel+factor(decade), data = kunprime)

## Logit mandater
first_stage_logit_seats <- glm.cluster(strat_lag03 ~ tav_disspm+seats_andel+
                                         factor(decade),
                                       data = kunprime,
                                       cluster = "country_id",
                                       family = "binomial")
summary(first_stage_logit_seats)
first_stage_logit_seats_se <- sqrt(diag(first_stage_logit_seats$vcov))
## Margins
first_stage_marginal_seats <- glm(strat_lag03 ~ tav_disspm+seats_andel+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_seats <- summary(margins(first_stage_marginal_seats))
AME_first_stage_seats



### Fulde first stage ###############################################################
first_stage_pmsurv_ols_full <- lm_robust(strategisk ~ tav_disspm+
                                       gdp_growth_lag+inflation_gdp_deflator_lag+
                                       factor(decade),
                                     data = kunprime,
                                     clusters = country_id,
                                     se_type = "stata")
summary(first_stage_pmsurv_ols_full)
star_first_stage_pmsurv_ols_full <- lm(strategisk ~ tav_disspm+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),data = kunprime)

## Logit
first_stage_logit_pmsurv_full <- glm.cluster(strategisk ~ tav_disspm+
                                              gdp_growth_lag+inflation_gdp_deflator_lag+
                                              factor(decade),
                                            data = kunprime,
                                            cluster = "country_id",
                                            family = "binomial")
summary(first_stage_logit_pmsurv_full)
first_stage_logit_pmsurv_full_se <- sqrt(diag(first_stage_logit_pmsurv_full$vcov))
## Margins
first_stage_pmsurv_full_marginal <- glm(strategisk~tav_disspm+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_pmsurv_full <- summary(margins(first_stage_pmsurv_full_marginal))
AME_first_stage_pmsurv_full


### First stage stemmer full ##################################################################
first_stage_ols_votes_full <- lm_robust(strat_lag03~tav_disspm+vote_share+
                                          gdp_growth_lag_next+
                                          inflation_gdp_deflator_lag_next+
                                          factor(decade),
                                        data = kunprime,
                                        clusters = country_id,
                                        se_type = "stata")
summary(first_stage_ols_votes_full)
star_first_stage_ols_votes_full <- lm(strat_lag03~tav_disspm+vote_share+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),data = kunprime)


### Logit
first_stage_logit_votes_full <- glm.cluster(strat_lag03~tav_disspm+vote_share+
                                                    gdp_growth_lag_next+
                                                    inflation_gdp_deflator_lag_next+
                                                    factor(decade),
                                                  data = kunprime,
                                                  cluster = "country_id",
                                                  family = "binomial")
summary(first_stage_logit_votes_full)
first_stage_logit_votes_full_se <- sqrt(diag(first_stage_logit_votes_full$vcov))
## Margins
first_stage_votes_full_marginal <- glm(strat_lag03~tav_disspm+vote_share+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_votes_full <- summary(margins(first_stage_votes_full_marginal))
AME_first_stage_votes_full


### first stage mandater full ####################################################################
first_stage_ols_seats_full <- lm_robust(strat_lag03~tav_disspm+seats_andel+
                                          gdp_growth_lag_next+
                                          inflation_gdp_deflator_lag_next+
                                          factor(decade),
                                        data = kunprime,
                                        clusters = country_id,
                                        se_type = "stata")
summary(first_stage_ols_seats_full)
star_first_stage_ols_seats_full <- lm(strat_lag03~tav_disspm+seats_andel+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)

## Logit
first_stage_logit_seats_full <- glm.cluster(strat_lag03~tav_disspm+seats_andel+
                                              gdp_growth_lag_next+
                                              inflation_gdp_deflator_lag_next+
                                              factor(decade),
                                            data = kunprime,
                                            cluster = "country_id",
                                            family = "binomial")
summary(first_stage_logit_seats_full)
first_stage_logit_seats_full_se <- sqrt(diag(first_stage_logit_seats_full$vcov))

## Margins
first_stage_seats_full_marginal <- glm(strat_lag03~tav_disspm+seats_andel+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_seats_full <- summary(margins(first_stage_seats_full_marginal))
AME_first_stage_seats_full

AME_first_stage_vote

####################################################################################
#####################                                  #############################
#####################     STARGAZER FIRST STAGE        #############################
#####################                                  #############################
####################################################################################

?stargazer

### Laver to paneler. Et med OLS og et med logistiske

## OLS
write_clip(
stargazer(star_first_stage_ols, star_first_stage_ols_vote, star_first_stage_ols_seats,
          star_first_stage_pmsurv_ols_full, star_first_stage_ols_votes_full, star_first_stage_ols_seats_full,
          
          se = list(first_stage_ols1$std.error, first_stage_ols_vote$std.error, 
                    first_stage_ols_seats$std.error,
                    first_stage_pmsurv_ols_full$std.error, first_stage_ols_votes_full$std.error,
                    first_stage_ols_seats_full$std.error),
          
          omit = "decade", omit.labels = "årti-dummies", omit.yes.no = c("Ja", "Nej"),
          notes = c("Lande-robuste standardfejl i parentes.", "PM: premierminister"), 
          omit.stat = c("ser", "f"),
          
          covariate.labels = c("PM opløsningsmagt", "Nuværende stemmeandel",
                               "Nuværende mandatandel", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "Konstant"),
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels.include = FALSE,
          column.labels = "Strategisk timet parlamentsvalg",
          column.separate = 6,
          notes.align = "l",
          
          type = "html", style = "commadefault")
)

## Logistisk
write_clip(
stargazer(first_stage_marginal1, first_stage_marginal_vote, 
          first_stage_marginal_seats,
          first_stage_pmsurv_full_marginal, first_stage_votes_full_marginal, 
          first_stage_seats_full_marginal,
          
          se = list(first_stage_logit1_se, first_stage_logit_vote_se,
                    first_stage_logit_seats_se,
                    first_stage_logit_pmsurv_full_se, first_stage_logit_votes_full_se,
                    first_stage_logit_seats_full_se),
          omit = "decade", omit.labels = "årti-dummies", omit.yes.no = c("Ja", "Nej"),
          notes = c("Koefficienterne vises i log-odds", "Lande-robuste standardfejl i parentes.", "PM: premierminister"), 
          omit.stat = c("ser", "f"),
          
          covariate.labels = c("PM opløsningsmagt", "Nuværende stemmeandel",
                               "Nuværende mandatandel", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "Konstant"),
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels.include = FALSE,
          column.labels = "Strategisk timet parlamentsvalg",
          column.separate = 6,
          notes.align = "l",
          
          type = "html", style = "commadefault")
)
          

#### Laver stargazer tabel med de gennemsnitlige marginale effekter
### OBS standardfejl skal tilføjes manuelt.
write_clip(
stargazer(first_stage_marginal1, first_stage_marginal_vote, 
          first_stage_marginal_seats,
          first_stage_pmsurv_full_marginal, first_stage_votes_full_marginal, 
          first_stage_seats_full_marginal,
         
          coef = list(AME_first_stage1$AME, AME_first_stage_vote$AME,
                      AME_first_stage_seats$AME, 
                      AME_first_stage_pmsurv_full$AME, AME_first_stage_votes_full$AME,
                      AME_first_stage_seats_full$AME),
          
          keep = c("tav_disspm", "vote_share", "seats_andel"),
          
          omit = c("decade", "gdp_growth_lag"),
          omit.labels = c("årti-dummies", "Kontrol økonomi"), 
          omit.yes.no = c("Ja", "Nej"),
          notes = c("Viser de gennemsnitlige marginale effekter for de logistiske regressioner estimeret i tabel X",
                    "PM: premierminister"), 
          omit.stat = c("ser", "f"),
          
          covariate.labels = c("PM opløsningsmagt", "Nuværende stemmeandel", "Nuværende mandatandel"),
          
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels.include = FALSE,
          column.labels = "Strategisk timet parlamentsvalg",
          column.separate = 6,
          notes.align = "l",
          
          type = "html", style = "commadefault")
)



####################################################################################
#####################                                  #############################
#####################     FINAL IV-Modeller            #############################
#####################                                  #############################
####################################################################################
rm(list=ls()[! ls() %in% c("kunprime","moveme")])

########################### PM_surv ##############################################
IV_pmsurv1 <- iv_robust(PMsurvive ~ strategisk |
                           tav_disspm,
                         data = kunprime, clusters = country_id,
                         se_type = "stata", diagnostics = TRUE)
summary(IV_pmsurv1)
star_IV_pmsurv1 <- lm(PMsurvive ~ strategisk, data = kunprime)
### Udregner F
IV_pmsurv1_f <- IV_pmsurv1$diagnostic_first_stage_fstatistic
IV_pmsurv1_f$stars <- ifelse(IV_pmsurv1_f[4]<0.01,"***",
                            ifelse(IV_pmsurv1_f[4]<0.05,"**",
                                   ifelse(IV_pmsurv1_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_pmsurv1_Wu <- IV_pmsurv1$diagnostic_endogeneity_test
IV_pmsurv1_Wu$stars <- ifelse(IV_pmsurv1_Wu[4]<0.01,"***",
                              ifelse(IV_pmsurv1_Wu[4]<0.05,"**",
                                     ifelse(IV_pmsurv1_Wu[4]<0.1,"*","")))



## 2
IV_pmsurv2 <- iv_robust(PMsurvive ~ strategisk+factor(decade) |
                          tav_disspm+factor(decade),
                        data = kunprime, clusters = country_id,
                        se_type = "stata", diagnostics = TRUE)
summary(IV_pmsurv2)
star_IV_pmsurv2 <- lm(PMsurvive ~ strategisk+factor(decade), data = kunprime)
### Udregner F
IV_pmsurv2_f <- IV_pmsurv2$diagnostic_first_stage_fstatistic
IV_pmsurv2_f$stars <- ifelse(IV_pmsurv2_f[4]<0.01,"***",
                             ifelse(IV_pmsurv2_f[4]<0.05,"**",
                                    ifelse(IV_pmsurv2_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_pmsurv2_Wu <- IV_pmsurv2$diagnostic_endogeneity_test
IV_pmsurv2_Wu$stars <- ifelse(IV_pmsurv2_Wu[4]<0.01,"***",
                              ifelse(IV_pmsurv2_Wu[4]<0.05,"**",
                                     ifelse(IV_pmsurv2_Wu[4]<0.1,"*","")))


## 3
IV_pmsurv3 <- iv_robust(PMsurvive ~ strategisk+
                          gdp_growth_lag+
                          inflation_gdp_deflator_lag+
                          factor(decade) |
                          tav_disspm+
                          gdp_growth_lag+
                          inflation_gdp_deflator_lag+
                          factor(decade),
                        data = kunprime, clusters = country_id,
                        se_type = "stata", diagnostics = TRUE)
summary(IV_pmsurv3)
star_IV_pmsurv3 <- lm(PMsurvive ~ strategisk+gdp_growth_lag+
                        inflation_gdp_deflator_lag+factor(decade),
                      data = kunprime)
### Udregner F
IV_pmsurv3_f <- IV_pmsurv3$diagnostic_first_stage_fstatistic
IV_pmsurv3_f$stars <- ifelse(IV_pmsurv3_f[4]<0.01,"***",
                             ifelse(IV_pmsurv3_f[4]<0.05,"**",
                                    ifelse(IV_pmsurv3_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_pmsurv3_Wu <- IV_pmsurv3$diagnostic_endogeneity_test
IV_pmsurv3_Wu$stars <- ifelse(IV_pmsurv3_Wu[4]<0.01,"***",
                              ifelse(IV_pmsurv3_Wu[4]<0.05,"**",
                                     ifelse(IV_pmsurv3_Wu[4]<0.1,"*","")))


###################  IV VOTES  ###############################

## Nummer 1
IV_votes1 <- iv_robust(vote_next ~ strat_lag03 |
                         tav_disspm,
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes1)

star_IV_votes1 <- lm(vote_next ~ strat_lag03, data = kunprime)

## Udregner F
IV_votes1_f <- IV_votes1$diagnostic_first_stage_fstatistic
IV_votes1_f$stars <- ifelse(IV_votes1_f[4]<0.01,"***",
                             ifelse(IV_votes1_f[4]<0.05,"**",
                                    ifelse(IV_votes1_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes1_Wu <- IV_votes1$diagnostic_endogeneity_test
IV_votes1_Wu$stars <- ifelse(IV_votes1_Wu[4]<0.01,"***",
                              ifelse(IV_votes1_Wu[4]<0.05,"**",
                                     ifelse(IV_votes1_Wu[4]<0.1,"*","")))


## Nummer 2
IV_votes2 <- iv_robust(vote_next ~ strat_lag03+
                         factor(decade) |
                         tav_disspm+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes2)

star_IV_votes2 <- lm(vote_next ~ strat_lag03+factor(decade), data = kunprime)

## Udregner F
IV_votes2_f <- IV_votes2$diagnostic_first_stage_fstatistic
IV_votes2_f$stars <- ifelse(IV_votes2_f[4]<0.01,"***",
                            ifelse(IV_votes2_f[4]<0.05,"**",
                                   ifelse(IV_votes2_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes2_Wu <- IV_votes2$diagnostic_endogeneity_test
IV_votes2_Wu$stars <- ifelse(IV_votes2_Wu[4]<0.01,"***",
                             ifelse(IV_votes2_Wu[4]<0.05,"**",
                                    ifelse(IV_votes2_Wu[4]<0.1,"*","")))


## Nummer 3
IV_votes3 <- iv_robust(vote_next ~ strat_lag03+
                         vote_share+
                         factor(decade) |
                         tav_disspm+
                         vote_share+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes3)

star_IV_votes3 <- lm(vote_next ~ strat_lag03+vote_share+factor(decade), data = kunprime)

## Udregner F
IV_votes3_f <- IV_votes3$diagnostic_first_stage_fstatistic
IV_votes3_f$stars <- ifelse(IV_votes3_f[4]<0.01,"***",
                            ifelse(IV_votes3_f[4]<0.05,"**",
                                   ifelse(IV_votes3_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes3_Wu <- IV_votes3$diagnostic_endogeneity_test
IV_votes3_Wu$stars <- ifelse(IV_votes3_Wu[4]<0.01,"***",
                             ifelse(IV_votes3_Wu[4]<0.05,"**",
                                    ifelse(IV_votes3_Wu[4]<0.1,"*","")))

## Nummer 4
IV_votes4 <- iv_robust(vote_next ~ strat_lag03+
                         vote_share+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade) |
                         tav_disspm+
                         vote_share+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes4)

star_IV_votes4 <- lm(vote_next ~ strat_lag03+vote_share+
                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                       factor(decade), data = kunprime)

## Udregner F
IV_votes4_f <- IV_votes4$diagnostic_first_stage_fstatistic
IV_votes4_f$stars <- ifelse(IV_votes4_f[4]<0.01,"***",
                            ifelse(IV_votes4_f[4]<0.05,"**",
                                   ifelse(IV_votes4_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes4_Wu <- IV_votes4$diagnostic_endogeneity_test
IV_votes4_Wu$stars <- ifelse(IV_votes4_Wu[4]<0.01,"***",
                             ifelse(IV_votes4_Wu[4]<0.05,"**",
                                    ifelse(IV_votes4_Wu[4]<0.1,"*","")))


################### IV MANDATER #####################################

## Nummer 1
IV_seats1 <- iv_robust(seats_next ~ strat_lag03 |
                         tav_disspm,
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats1)

star_IV_seats1 <- lm(seats_next ~ strat_lag03, data = kunprime)

## Udregner F
IV_seats1_f <- IV_seats1$diagnostic_first_stage_fstatistic
IV_seats1_f$stars <- ifelse(IV_seats1_f[4]<0.01,"***",
                            ifelse(IV_seats1_f[4]<0.05,"**",
                                   ifelse(IV_seats1_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats1_Wu <- IV_seats1$diagnostic_endogeneity_test
IV_seats1_Wu$stars <- ifelse(IV_seats1_Wu[4]<0.01,"***",
                             ifelse(IV_seats1_Wu[4]<0.05,"**",
                                    ifelse(IV_seats1_Wu[4]<0.1,"*","")))

## Nummer 2
IV_seats2 <- iv_robust(seats_next ~ strat_lag03+
                       factor(decade) |
                       tav_disspm+
                       factor(decade),
                    data = kunprime, clusters = country_id,
                    se_type = "stata", diagnostics = TRUE)
summary(IV_seats2)

star_IV_seats2 <- lm(seats_next ~ strat_lag03+factor(decade), data = kunprime)

## Udregner F
IV_seats2_f <- IV_seats2$diagnostic_first_stage_fstatistic
IV_seats2_f$stars <- ifelse(IV_seats2_f[4]<0.01,"***",
                            ifelse(IV_seats2_f[4]<0.05,"**",
                                   ifelse(IV_seats2_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats2_Wu <- IV_seats2$diagnostic_endogeneity_test
IV_seats2_Wu$stars <- ifelse(IV_seats2_Wu[4]<0.01,"***",
                             ifelse(IV_seats2_Wu[4]<0.05,"**",
                                    ifelse(IV_seats2_Wu[4]<0.1,"*","")))


## Nummer 3
IV_seats3 <- iv_robust(seats_next ~ strat_lag03+seats_andel+
                         factor(decade) |
                         tav_disspm+seats_andel+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats3)

star_IV_seats3 <- lm(seats_next ~ strat_lag03+seats_andel+factor(decade),
                     data = kunprime)

## Udregner F
IV_seats3_f <- IV_seats3$diagnostic_first_stage_fstatistic
IV_seats3_f$stars <- ifelse(IV_seats3_f[4]<0.01,"***",
                            ifelse(IV_seats3_f[4]<0.05,"**",
                                   ifelse(IV_seats3_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats3_Wu <- IV_seats3$diagnostic_endogeneity_test
IV_seats3_Wu$stars <- ifelse(IV_seats3_Wu[4]<0.01,"***",
                             ifelse(IV_seats3_Wu[4]<0.05,"**",
                                    ifelse(IV_seats3_Wu[4]<0.1,"*","")))


## Nummer 4
IV_seats4 <- iv_robust(seats_next ~ strat_lag03+seats_andel+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade) |
                         tav_disspm+seats_andel+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats4)

star_IV_seats4 <- lm(seats_next ~ strat_lag03+seats_andel+gdp_growth_lag_next+
                       inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)

## Udregner F
IV_seats4_f <- IV_seats4$diagnostic_first_stage_fstatistic
IV_seats4_f$stars <- ifelse(IV_seats4_f[4]<0.01,"***",
                            ifelse(IV_seats4_f[4]<0.05,"**",
                                   ifelse(IV_seats4_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats4_Wu <- IV_seats4$diagnostic_endogeneity_test
IV_seats4_Wu$stars <- ifelse(IV_seats4_Wu[4]<0.01,"***",
                             ifelse(IV_seats4_Wu[4]<0.05,"**",
                                    ifelse(IV_seats4_Wu[4]<0.1,"*","")))



####################################################################################
#####################                                  #############################
#####################     STARGAZER IV-MODELLER        #############################
#####################                                  #############################
####################################################################################


################################## PM SURVIVE #####################################
write_clip(
stargazer(star_IV_pmsurv1, star_IV_pmsurv2, star_IV_pmsurv3,
          # Indsætter koefficienter
          coef = list(IV_pmsurv1$coefficients, IV_pmsurv2$coefficients,
                      IV_pmsurv3$coefficients),
          # Standardfejl
          se = list(IV_pmsurv1$std.error, IV_pmsurv2$std.error,
                    IV_pmsurv3$std.error),
          
          omit = "decade", omit.labels = "årti-dummies",
          omit.yes.no = c("Nej", "Ja"), omit.stat = c("ser", "f", "rsq", "adj.rsq"),
          
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels = "premierministerparti bevarer posten",
          column.labels = "(IV: Strategisk valg = PM opløsningsmagt)",
          column.separate = 4,
          
          add.lines = list(c("Fist stage F-test",
                             paste0(round(IV_pmsurv1_f$value,3), IV_pmsurv1_f$stars),
                             paste0(round(IV_pmsurv2_f$value,3), IV_pmsurv2_f$stars),
                             paste0(round(IV_pmsurv3_f$value,3), IV_pmsurv3_f$stars)),
                           
                           c("Durbin-Wu-Hausman test (p-værdi)", 
                             paste0(round(IV_pmsurv1_Wu$p.value,3), IV_pmsurv1_Wu$stars),
                             paste0(round(IV_pmsurv2_Wu$p.value,3), IV_pmsurv2_Wu$stars),
                             paste0(round(IV_pmsurv3_Wu$p.value,3), IV_pmsurv3_Wu$stars))),
          
          covariate.labels = c("Strategisk timet valg", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "Konstant"),
          notes = c("Lande-robuste standardfejl i parentes.", "Antal lande: 35"), notes.align = "l",
          
          type = "text", style = "commadefault")
)


################################## STEMMEANDEL #####################################
write_clip(
stargazer(star_IV_votes1, star_IV_votes2, star_IV_votes3, star_IV_votes4,
          # Indsætter koefficienter
          coef = list(IV_votes1$coefficients, IV_votes2$coefficients,
                      IV_votes3$coefficients, IV_votes4$coefficients),
          # Standardfejl
          se = list(IV_votes1$std.error, IV_votes2$std.error,
                    IV_votes3$std.error, IV_votes4$std.error),
          
          omit = "decade", omit.labels = "årti-dummies",
          omit.yes.no = c("Nej", "Ja"), omit.stat = c("ser", "f", "rsq", "adj.rsq"),
          
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels = "Stemmeandel",
          column.labels = "(IV: Strategisk valg = PM opløsningsmagt)",
          column.separate = 5,
          
          add.lines = list(c("Fist stage F-test",
                             paste0(round(IV_votes1_f$value,3), IV_votes1_f$stars),
                             paste0(round(IV_votes2_f$value,3), IV_votes2_f$stars),
                             paste0(round(IV_votes3_f$value,3), IV_votes3_f$stars),
                             paste0(round(IV_votes4_f$value,3), IV_votes4_f$stars)),
                           
                           c("Durbin-Wu-Hausman test (p-værdi)", 
                             paste0(round(IV_votes1_Wu$p.value,3), IV_votes1_Wu$stars),
                             paste0(round(IV_votes2_Wu$p.value,3), IV_votes2_Wu$stars),
                             paste0(round(IV_votes3_Wu$p.value,3), IV_votes3_Wu$stars),
                             paste0(round(IV_votes4_Wu$p.value,3), IV_votes4_Wu$stars))),
          
          covariate.labels = c("Strategisk timet valg", "Nuværende stemmeandel",
                               "BNP vækst (1 år lag)", "Inflation (1 år lag)", 
                               "Konstant"),
          notes = c("Lande-robuste standardfejl i parentes.", "Antal lande: 35"), 
          notes.align = "l",
          
          type = "html", style = "commadefault")
)


################################## MANDATANDEL #####################################
write_clip(
stargazer(star_IV_seats1, star_IV_seats2, star_IV_seats3, star_IV_seats4,
          # Indsætter koefficienter
          coef = list(IV_seats1$coefficients, IV_seats2$coefficients,
                      IV_seats3$coefficients, IV_seats4$coefficients),
          # Standardfejl
          se = list(IV_seats1$std.error, IV_seats2$std.error,
                    IV_seats3$std.error, IV_seats4$std.error),
          
          omit = "decade", omit.labels = "årti-dummies",
          omit.yes.no = c("Nej", "Ja"), omit.stat = c("ser", "f", "rsq", "adj.rsq"),
          
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels = "Andel af mandater",
          column.labels = "(IV: Strategisk valg = PM opløsningsmagt)",
          column.separate = 5,
          
          add.lines = list(c("Fist stage F-test",
                             paste0(round(IV_seats1_f$value,3), IV_seats1_f$stars),
                             paste0(round(IV_seats2_f$value,3), IV_seats2_f$stars),
                             paste0(round(IV_seats3_f$value,3), IV_seats3_f$stars),
                             paste0(round(IV_seats4_f$value,3), IV_seats4_f$stars)),
                           
                           c("Durbin-Wu-Hausman test (p-værdi)", 
                             paste0(round(IV_seats1_Wu$p.value,3), IV_seats1_Wu$stars),
                             paste0(round(IV_seats2_Wu$p.value,3), IV_seats2_Wu$stars),
                             paste0(round(IV_seats3_Wu$p.value,3), IV_seats3_Wu$stars),
                             paste0(round(IV_seats4_Wu$p.value,3), IV_seats4_Wu$stars))),
          
          covariate.labels = c("Strategisk timet valg", "Nuværende mandantandel",
                               "BNP vækst (1 år lag)", "Inflation (1 år lag)", 
                               "Konstant"),
          notes = c("Lande-robuste standardfejl i parentes.", "Antal lande: 35"), 
          notes.align = "l",
          
          type = "html", style = "commadefault")
)

####################################################################################
#####################                                  #############################
#####################  TEST AF EKSKLUSIONSKRITERIET    #############################
#####################                                  #############################
####################################################################################
rm(list=ls()[! ls() %in% c("kunprime","moveme")])

################### FOR PMSURV ###################################################

## First stage
eksklu_first_st_pmsurv_logit <- glm.cluster(strategisk ~ tav_disspm+
                                      gdp_growth_lag+inflation_gdp_deflator_lag+
                                      factor(decade),
                                      data = subset(kunprime, !(is.na(kunprime$PMsurvive))), 
                                      family = "binomial",
                                    cluster = "country_id")
summary(eksklu_first_st_pmsurv_logit)
##Standardfejl
eksklu_first_st_pmsurv_logit_se <- sqrt(diag(eksklu_first_st_pmsurv_logit$vcov))

## Til stargazer
eksklu_first_st_pmsurv_marginal <- glm(strategisk ~ tav_disspm+gdp_growth_lag+
                                           inflation_gdp_deflator_lag+factor(decade),
                                         data = subset(kunprime, !(is.na(kunprime$PMsurvive))), 
                                       family = "binomial")

## Margins
AME_eksklu_first_st_pmsurv <- summary(margins(eksklu_first_st_pmsurv_marginal))
AME_eksklu_first_st_pmsurv



eksklu_first_st_pmsurv_ols <- lm_robust(strategisk ~ tav_disspm+
                                              gdp_growth_lag+inflation_gdp_deflator_lag+
                                              factor(decade), data = subset(kunprime, !(is.na(kunprime$PMsurvive))),
                                        clusters = country_id, se_type = "stata")
summary(eksklu_first_st_pmsurv_ols)
## star
star_eksklu_first_st_pmsurv_ols <- lm(strategisk ~ tav_disspm+gdp_growth_lag+
                                        inflation_gdp_deflator_lag+factor(decade),
                                      data = subset(kunprime, !(is.na(kunprime$PMsurvive))))

######### Second stage

eksklu_sencond_st_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk+tav_disspm+
                                                gdp_growth_lag+inflation_gdp_deflator_lag+
                                                factor(decade), data = kunprime,
                                              family = "binomial", cluster = "country_id")
summary(eksklu_sencond_st_pmsurv_logit)
eksklu_sencond_st_pmsurv_logit_se <- sqrt(diag(eksklu_sencond_st_pmsurv_logit$vcov))
## Til stargazer
eksklu_second_st_pmsurv_marginal <- glm(PMsurvive ~ strategisk+tav_disspm+
                                          gdp_growth_lag+inflation_gdp_deflator_lag+
                                          factor(decade), data = kunprime, family = "binomial")
AME_eksklu_second_st_pmsurv <- summary(margins(eksklu_second_st_pmsurv_marginal))
AME_eksklu_second_st_pmsurv


eksklu_second_st_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk+tav_disspm+
                                           gdp_growth_lag+inflation_gdp_deflator_lag+
                                           factor(decade), data = kunprime,
                                         clusters = country_id, se_type = "stata")
summary(eksklu_second_st_pmsurv_ols)


################## FOR STEMMEANDEL ################################################
eksklu_first_st_votes_logit <- glm.cluster(strat_lag03 ~ tav_disspm+vote_share+
                                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                             factor(decade),
                                           data = subset(kunprime, !(is.na(kunprime$vote_next))), family = "binomial",
                                           cluster = "country_id")
summary(eksklu_first_st_votes_logit)
eksklu_first_st_votes_logit_se <- sqrt(diag(eksklu_first_st_votes_logit$vcov))
## Til star
eksklu_first_st_votes_marginal <- glm(strat_lag03 ~ tav_disspm+vote_share+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade),
                                      data = subset(kunprime, !(is.na(kunprime$vote_next))), family = "binomial")
AME_eksklu_first_st_votes <- summary(margins(eksklu_first_st_votes_marginal))
AME_eksklu_first_st_votes

## OLS
eksklu_first_st_votes_ols <- lm_robust(strat_lag03 ~ tav_disspm+vote_share+
                                         gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                         factor(decade), data = subset(kunprime, !(is.na(kunprime$vote_next))),
                                       clusters = country_id, se_type = "stata")
summary(eksklu_first_st_votes_ols)
## Til stargazer
star_eksklu_first_st_votes_ols <- lm(strat_lag03 ~ tav_disspm+vote_share+
                                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                       factor(decade), data = subset(kunprime, !(is.na(kunprime$vote_next))))


eksklu_second_st_votes_ols <- lm_robust(vote_next ~ strat_lag03+tav_disspm+vote_share+
                                          gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                          factor(decade), data = kunprime,
                                        clusters = country_id, se_type = "stata")
summary(eksklu_second_st_votes_ols)
## Til stargazer
star_eksklu_second_st_votes_ols <- lm(vote_next ~ strat_lag03+tav_disspm+vote_share+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade), data = kunprime)


################ FOR MANDATANDEL ################################################

eksklu_first_st_seats_logit <- glm.cluster(strat_lag03 ~ tav_disspm+seats_andel+
                                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                             factor(decade),
                                           data = subset(kunprime, !(is.na(kunprime$seats_next))), family = "binomial",
                                           cluster = "country_id")
summary(eksklu_first_st_seats_logit)
eksklu_first_st_seats_logit_se <- sqrt(diag(eksklu_first_st_seats_logit$vcov))
## Til star
eksklu_first_st_seats_marginal <- glm(strat_lag03 ~ tav_disspm+seats_andel+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade),
                                      data = subset(kunprime, !(is.na(kunprime$seats_next))), family = "binomial")
AME_eksklu_first_st_seats <- summary(margins(eksklu_first_st_seats_marginal))
AME_eksklu_first_st_seats


eksklu_first_st_seats_ols <- lm_robust(strat_lag03 ~ tav_disspm+seats_andel+
                                         gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                         factor(decade), data = subset(kunprime, !(is.na(kunprime$seats_next))),
                                       clusters = country_id, se_type = "stata")
summary(eksklu_first_st_seats_ols)
star_eksklu_first_st_seats_ols <- lm(strat_lag03 ~ tav_disspm+seats_andel+
                                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                       factor(decade), data = subset(kunprime, !(is.na(kunprime$seats_next))))

eksklu_second_st_seats_ols <- lm_robust(seats_next ~ strat_lag03+tav_disspm+seats_andel+
                                          gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                          factor(decade), data = kunprime,
                                        clusters = country_id, se_type = "stata")
summary(eksklu_second_st_seats_ols)
star_eksklu_second_st_seats_ols <- lm(seats_next ~ strat_lag03+tav_disspm+seats_andel+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade), data = kunprime)



#######################################################################################
##################                                        #############################
##################  STARGAZER FOR EKSKLUSIONSKRITERIET    #############################
##################                                        #############################
#######################################################################################
write_clip(
stargazer(eksklu_first_st_pmsurv_marginal, eksklu_second_st_pmsurv_marginal,
          eksklu_first_st_votes_marginal, star_eksklu_first_st_votes_ols,
          star_eksklu_second_st_votes_ols,
          eksklu_first_st_seats_marginal, star_eksklu_first_st_seats_ols,
          star_eksklu_second_st_seats_ols,
          
          se = list(eksklu_first_st_pmsurv_logit_se, eksklu_sencond_st_pmsurv_logit_se,
                      eksklu_first_st_votes_logit_se, eksklu_first_st_votes_ols$std.error,
                      eksklu_second_st_votes_ols$std.error,
                      eksklu_first_st_seats_logit_se, eksklu_first_st_seats_ols$std.error,
                      eksklu_second_st_seats_ols$std.error),
          
          order = c("strategisk", "strat_lag03", "tav_disspm",
                    "vote_share", "seats_andel"),
          covariate.labels = c("Strategisk timet valg", "Strategisk timet valg",
                               "PM opløsningsmagt", "Nuværende stemmeandel",
                               "Nuværende mandatandel", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "BNP vækst (1 år lag)",
                               "Inlation (1 år lag)", "Konstant"),
          
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels = c("Strategisk valg", "PM overlever",
                             "Strategisk valg", "Stemmeandel",
                             "Strategisk valg", "Mandatandel"),
          column.labels = c("premierministerparti bevarer posten",
                            "Stemmeandel", "Andel af mandater"),
          column.separate = c(2,3,3),
          
          table.layout = "=!lcd-m#-t-o-s=!n",
          
          omit = "decade", omit.labels = "årti-dummies", 
          omit.yes.no = c("Ja", "Nej"),
          omit.stat = c("ser", "f"),
          
          notes = c("Lande-robuste standardfejl i parentes.",
                    "PM: premierminister"),
          notes.align = "l",

          type = "html", style = "commadefault")
)



#######################################################################################
##################                                        #############################
##################    ROBUSTHEDSTEST OLS OG EKSTRA IVs    #############################
##################                                        #############################
#######################################################################################

rm(list=ls()[! ls() %in% c("kunprime","moveme")])

#### Laver modeller med ekstra kontrol variable ##########

#### Lave kort deskriptiv statistik af dem, så de ikke forpester den
## første tabel med deskriptiv statistik.

# 1: valgsystem
# 2: regeringspartier/karakteristika majoritet

## OBS som bilagstabel skal regeringskarakteristika køres med de to variable hver 
## for sig, da der givet vis er høj grad af multikollinearitet mellem dem

# 3: valgperiode

## Har tre kontroller, valgsystem, antal partier i regering og længde af valgperioden.


kunprime <- kunprime %>% 
  mutate(regeringspartier=if_else(regeringspartier>1,as.integer(0),regeringspartier))



#################   premierministerPARTIET OVERLEVER    #########################

valgsystem_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk+valgsystem_majo+
                                          gdp_growth_lag+inflation_gdp_deflator_lag+
                                          factor(decade),
                                        data = kunprime, cluster = "country_id",
                                        family = "binomial")
summary(valgsystem_pmsurv_logit)
valgsystem_pmsurv_logit_se <- sqrt(diag(valgsystem_pmsurv_logit$vcov))
## margins
valgsystem_pmsurv_margianl <- glm(PMsurvive ~ strategisk+valgsystem_majo+
                                            gdp_growth_lag+inflation_gdp_deflator_lag+
                                            factor(decade),
                                          data = kunprime, family = "binomial")
AME_valgsystem_pmsurv <- summary(margins(valgsystem_pmsurv_margianl))
AME_valgsystem_pmsurv


## OLS
valgsystem_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk+valgsystem_majo+
                                     gdp_growth_lag+inflation_gdp_deflator_lag+
                                     factor(decade),
                                   data = kunprime, clusters = country_id,
                                   se_type = "stata")
summary(valgsystem_pmsurv_ols)
star_valgsystem_pmsurv_ols <- lm(PMsurvive ~ strategisk+valgsystem_majo+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade), data = kunprime)


### Valgsystem IV
IV_valgsystem_pmsurv <- iv_robust(PMsurvive ~ strategisk+valgsystem_majo+
                                    gdp_growth_lag+inflation_gdp_deflator_lag+
                                    factor(decade) |
                                    tav_disspm+valgsystem_majo+
                                    gdp_growth_lag+inflation_gdp_deflator_lag+
                                    factor(decade),
                                  data = kunprime, clusters = country_id,
                                  se_type = "stata", diagnostics = TRUE)
summary(IV_valgsystem_pmsurv)
star_valgsystem_pmsurv_IV <- ivreg(PMsurvive ~ strategisk+valgsystem_majo+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade) |
                                    tav_disspm+valgsystem_majo+
                                    gdp_growth_lag+inflation_gdp_deflator_lag+
                                    factor(decade), data = kunprime)
### Udregner F
IV_valgsystem_pmsurv_f <- IV_valgsystem_pmsurv$diagnostic_first_stage_fstatistic
IV_valgsystem_pmsurv_f$stars <- ifelse(IV_valgsystem_pmsurv_f[4]<0.01,"***",
                             ifelse(IV_valgsystem_pmsurv_f[4]<0.05,"**",
                                    ifelse(IV_valgsystem_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valgsystem_pmsurv_Wu <- IV_valgsystem_pmsurv$diagnostic_endogeneity_test
IV_valgsystem_pmsurv_Wu$stars <- ifelse(IV_valgsystem_pmsurv_Wu[4]<0.01,"***",
                              ifelse(IV_valgsystem_pmsurv_Wu[4]<0.05,"**",
                                     ifelse(IV_valgsystem_pmsurv_Wu[4]<0.1,"*","")))



######################### Regeringskarateristika logit
regering_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk+regeringspartier+majoritet+
                                       gdp_growth_lag+inflation_gdp_deflator_lag+
                                       factor(decade),
                                     data = kunprime, cluster = "country_id",
                                     family = "binomial")
summary(regering_pmsurv_logit)
regering_pmsurv_logit_se <- sqrt(diag(regering_pmsurv_logit$vcov))
##Margins
regering_pmsurv_marginal <- glm(PMsurvive ~ strategisk+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade), data = kunprime, family = "binomial")
AME_regering_pmsurv <- summary(margins(regering_pmsurv_marginal))
AME_regering_pmsurv

## OLS
regering_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk+regeringspartier+majoritet+
                                    gdp_growth_lag+inflation_gdp_deflator_lag+
                                    factor(decade),
                                  data = kunprime, clusters = country_id,
                                 se_type = "stata")
summary(regering_pmsurv_ols)
star_regering_pmsurv_ols <- lm(PMsurvive ~ strategisk+regeringspartier+majoritet+
                                 gdp_growth_lag+inflation_gdp_deflator_lag+
                                 factor(decade), data = kunprime)


### IV regering
IV_regering_pmsurv <- iv_robust(PMsurvive ~ strategisk+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade) |
                                  tav_disspm+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata", diagnostics = TRUE)
summary(IV_regering_pmsurv)
star_regering_pmsurv_IV <- ivreg(PMsurvive ~ strategisk+regeringspartier+majoritet+
                                gdp_growth_lag+inflation_gdp_deflator_lag+
                                factor(decade) |
                                  tav_disspm+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade), data = kunprime)
### Udregner F
IV_regering_pmsurv_f <- IV_regering_pmsurv$diagnostic_first_stage_fstatistic
IV_regering_pmsurv_f$stars <- ifelse(IV_regering_pmsurv_f[4]<0.01,"***",
                                       ifelse(IV_regering_pmsurv_f[4]<0.05,"**",
                                              ifelse(IV_regering_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_regering_pmsurv_Wu <- IV_regering_pmsurv$diagnostic_endogeneity_test
IV_regering_pmsurv_Wu$stars <- ifelse(IV_regering_pmsurv_Wu[4]<0.01,"***",
                                        ifelse(IV_regering_pmsurv_Wu[4]<0.05,"**",
                                               ifelse(IV_regering_pmsurv_Wu[4]<0.1,"*","")))


## 3 års periode logit
valg3aar_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk+tre_aar+
                                       gdp_growth_lag+inflation_gdp_deflator_lag+
                                       factor(decade),
                                     data = kunprime, cluster = "country_id",
                                     family = "binomial")
summary(valg3aar_pmsurv_logit)
valg3aar_pmsurv_logit_se <- sqrt(diag(valg3aar_pmsurv_logit$vcov))
## Margins
valg3aar_pmsurv_marginal <- glm(PMsurvive ~ strategisk+tre_aar+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade), data = kunprime, family = "binomial")
AME_valg3aar_pmsurv <- summary(margins(valg3aar_pmsurv_marginal))
AME_valg3aar_pmsurv


## OLS
valg3aar_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk+tre_aar+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata")
summary(valg3aar_pmsurv_ols)
star_valg3aar_pmsurv_ols <- lm(PMsurvive ~ strategisk+tre_aar+
                                 gdp_growth_lag+inflation_gdp_deflator_lag+
                                 factor(decade), data = kunprime)

## IV 3 år valgperiode
IV_valg3aar_pmsurv <- iv_robust(PMsurvive ~ strategisk+tre_aar+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade) |
                                  tav_disspm+tre_aar+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata", diagnostics = TRUE)
summary(IV_valg3aar_pmsurv)
star_valg3aar_pmsurv_IV <- ivreg(PMsurvive ~ strategisk+tre_aar+
                                gdp_growth_lag+inflation_gdp_deflator_lag+
                                factor(decade) |
                                  tav_disspm+tre_aar+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade), data = kunprime)
### Udregner F
IV_valg3aar_pmsurv_f <- IV_valg3aar_pmsurv$diagnostic_first_stage_fstatistic
IV_valg3aar_pmsurv_f$stars <- ifelse(IV_valg3aar_pmsurv_f[4]<0.01,"***",
                                     ifelse(IV_valg3aar_pmsurv_f[4]<0.05,"**",
                                            ifelse(IV_valg3aar_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valg3aar_pmsurv_Wu <- IV_valg3aar_pmsurv$diagnostic_endogeneity_test
IV_valg3aar_pmsurv_Wu$stars <- ifelse(IV_valg3aar_pmsurv_Wu[4]<0.01,"***",
                                      ifelse(IV_valg3aar_pmsurv_Wu[4]<0.05,"**",
                                             ifelse(IV_valg3aar_pmsurv_Wu[4]<0.1,"*","")))





########################   FOR STEMMEANDEL   ####################################


## Valgsystem
valgsystem_vote_ols <- lm_robust(vote_next ~ strat_lag03+vote_share+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata")
summary(valgsystem_vote_ols)
star_valgsystem_vote_ols <- lm(vote_next ~ strat_lag03+vote_share+valgsystem_majo+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade), data = kunprime)


## IV
IV_valgsystem_vote <- iv_robust(vote_next ~ strat_lag03+vote_share+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade) |
                                  tav_disspm+vote_share+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata", diagnostics = TRUE)
summary(IV_valgsystem_vote)
star_valgsystem_vote_IV <- ivreg(vote_next ~ strat_lag03+vote_share+valgsystem_majo+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                  tav_disspm+vote_share+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade), data = kunprime)
### Udregner F
IV_valgsystem_vote_f <- IV_valgsystem_vote$diagnostic_first_stage_fstatistic
IV_valgsystem_vote_f$stars <- ifelse(IV_valgsystem_vote_f[4]<0.01,"***",
                                     ifelse(IV_valgsystem_vote_f[4]<0.05,"**",
                                            ifelse(IV_valgsystem_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valgsystem_vote_Wu <- IV_valgsystem_vote$diagnostic_endogeneity_test
IV_valgsystem_vote_Wu$stars <- ifelse(IV_valgsystem_vote_Wu[4]<0.01,"***",
                                      ifelse(IV_valgsystem_vote_Wu[4]<0.05,"**",
                                             ifelse(IV_valgsystem_vote_Wu[4]<0.1,"*","")))



#### Regeringskarakteristika
regering_vote_ols <- lm_robust(vote_next ~ strat_lag03+vote_share+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata")
summary(regering_vote_ols)
star_regering_vote_ols <- lm(vote_next ~ strat_lag03+vote_share+regeringspartier+
                               majoritet+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade), data = kunprime)

## IV
IV_regering_vote <- iv_robust(vote_next ~ strat_lag03+vote_share+regeringspartier+
                                majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+vote_share+regeringspartier+
                                majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade),
                              data = kunprime, clusters = country_id,
                              se_type = "stata", diagnostics = TRUE)
summary(IV_regering_vote)
star_regering_vote_IV <- ivreg(vote_next ~ strat_lag03+vote_share+regeringspartier+
                              majoritet+
                              gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                              factor(decade) |
                                tav_disspm+vote_share+regeringspartier+
                                majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade), data = kunprime)

### Udregner F
IV_regering_vote_f <- IV_regering_vote$diagnostic_first_stage_fstatistic
IV_regering_vote_f$stars <- ifelse(IV_regering_vote_f[4]<0.01,"***",
                                     ifelse(IV_regering_vote_f[4]<0.05,"**",
                                            ifelse(IV_regering_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_regering_vote_Wu <- IV_regering_vote$diagnostic_endogeneity_test
IV_regering_vote_Wu$stars <- ifelse(IV_regering_vote_Wu[4]<0.01,"***",
                                      ifelse(IV_regering_vote_Wu[4]<0.05,"**",
                                             ifelse(IV_regering_vote_Wu[4]<0.1,"*","")))





#### 3 års valgperiode 
valg3aar_vote_ols <- lm_robust(vote_next ~ strat_lag03+vote_share+tre_aar+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata")
summary(valg3aar_vote_ols)
star_valg3aar_vote_ols <- lm(vote_next ~ strat_lag03+vote_share+tre_aar+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade), data = kunprime)

## IV
IV_valg3aar_vote <- iv_robust(vote_next ~ strat_lag03+vote_share+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+vote_share+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade),
                              data = kunprime, clusters = country_id,
                              se_type = "stata", diagnostics = TRUE)
summary(IV_valg3aar_vote)
star_valg3aar_vote_IV <- ivreg(vote_next ~ strat_lag03+vote_share+tre_aar+
                              gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+vote_share+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade), data = kunprime)
### Udregner F
IV_valg3aar_vote_f <- IV_valg3aar_vote$diagnostic_first_stage_fstatistic
IV_valg3aar_vote_f$stars <- ifelse(IV_valg3aar_vote_f[4]<0.01,"***",
                                   ifelse(IV_valg3aar_vote_f[4]<0.05,"**",
                                          ifelse(IV_valg3aar_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valg3aar_vote_Wu <- IV_valg3aar_vote$diagnostic_endogeneity_test
IV_valg3aar_vote_Wu$stars <- ifelse(IV_valg3aar_vote_Wu[4]<0.01,"***",
                                    ifelse(IV_valg3aar_vote_Wu[4]<0.05,"**",
                                           ifelse(IV_valg3aar_vote_Wu[4]<0.1,"*","")))




########################   FOR MANDATANDEL   ####################################



## Valgsystem
valgsystem_seats_ols <- lm_robust(seats_next ~ strat_lag03+seats_andel+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata")
summary(valgsystem_seats_ols)
star_valgsystem_seats_ols <- lm(seats_next ~ strat_lag03+seats_andel+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade), data = kunprime)


## IV
IV_valgsystem_seats <- iv_robust(seats_next ~ strat_lag03+seats_andel+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade) |
                                  tav_disspm+seats_andel+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata", diagnostics = TRUE)
summary(IV_valgsystem_seats)
star_valgsystem_seats_IV <-  ivreg(seats_next ~ strat_lag03+seats_andel+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade) |
                                    tav_disspm+seats_andel+valgsystem_majo+
                                    gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                    factor(decade), data = kunprime)
### Udregner F
IV_valgsystem_seats_f <- IV_valgsystem_seats$diagnostic_first_stage_fstatistic
IV_valgsystem_seats_f$stars <- ifelse(IV_valgsystem_seats_f[4]<0.01,"***",
                                   ifelse(IV_valgsystem_seats_f[4]<0.05,"**",
                                          ifelse(IV_valgsystem_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valgsystem_seats_Wu <- IV_valgsystem_seats$diagnostic_endogeneity_test
IV_valgsystem_seats_Wu$stars <- ifelse(IV_valgsystem_seats_Wu[4]<0.01,"***",
                                    ifelse(IV_valgsystem_seats_Wu[4]<0.05,"**",
                                           ifelse(IV_valgsystem_seats_Wu[4]<0.1,"*","")))


#### Regeringskarakteristika
regering_seats_ols <- lm_robust(seats_next ~ strat_lag03+seats_andel+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata")
summary(regering_seats_ols)
star_regering_seats_ols <- lm(seats_next ~ strat_lag03+seats_andel+regeringspartier+majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade), data = kunprime)

## IV
IV_regering_seats <- iv_robust(seats_next ~ strat_lag03+seats_andel+regeringspartier+
                                majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+seats_andel+regeringspartier+
                                majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade),
                              data = kunprime, clusters = country_id,
                              se_type = "stata", diagnostics = TRUE)
summary(IV_regering_seats)
star_regering_seats_IV <- ivreg(seats_next ~ strat_lag03+seats_andel+regeringspartier+majoritet+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade) |
                                 tav_disspm+seats_andel+regeringspartier+majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade), data = kunprime)

### Udregner F
IV_regering_seats_f <- IV_regering_seats$diagnostic_first_stage_fstatistic
IV_regering_seats_f$stars <- ifelse(IV_regering_seats_f[4]<0.01,"***",
                                      ifelse(IV_regering_seats_f[4]<0.05,"**",
                                             ifelse(IV_regering_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_regering_seats_Wu <- IV_regering_seats$diagnostic_endogeneity_test
IV_regering_seats_Wu$stars <- ifelse(IV_regering_seats_Wu[4]<0.01,"***",
                                       ifelse(IV_regering_seats_Wu[4]<0.05,"**",
                                              ifelse(IV_regering_seats_Wu[4]<0.1,"*","")))



#### 3 års valgperiode 
valg3aar_seats_ols <- lm_robust(seats_next ~ strat_lag03+seats_andel+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata")
summary(valg3aar_seats_ols)
star_valg3aar_seats_ols <- lm(seats_next ~ strat_lag03+seats_andel+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade), data = kunprime)

## IV
IV_valg3aar_seats <- iv_robust(seats_next ~ strat_lag03+seats_andel+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+seats_andel+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade),
                              data = kunprime, clusters = country_id,
                              se_type = "stata", diagnostics = TRUE)
summary(IV_valg3aar_seats)
star_valg3aar_seats_IV <- ivreg(seats_next ~ strat_lag03+seats_andel+tre_aar+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade) |
                                 tav_disspm+seats_andel+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade), data = kunprime)
### Udregner F
IV_valg3aar_seats_f <- IV_valg3aar_seats$diagnostic_first_stage_fstatistic
IV_valg3aar_seats_f$stars <- ifelse(IV_valg3aar_seats_f[4]<0.01,"***",
                                    ifelse(IV_valg3aar_seats_f[4]<0.05,"**",
                                           ifelse(IV_valg3aar_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valg3aar_seats_Wu <- IV_valg3aar_seats$diagnostic_endogeneity_test
IV_valg3aar_seats_Wu$stars <- ifelse(IV_valg3aar_seats_Wu[4]<0.01,"***",
                                     ifelse(IV_valg3aar_seats_Wu[4]<0.05,"**",
                                            ifelse(IV_valg3aar_seats_Wu[4]<0.1,"*","")))




#######################################################################################
##################                                        #############################
##################        STARGAZER EKSTRA OLS OG IV      #############################
##################                                        #############################
#######################################################################################


################### TABEL FOR PMSURVIVE ############################################
write_clip(
stargazer(valgsystem_pmsurv_margianl, star_valgsystem_pmsurv_ols, star_valgsystem_pmsurv_IV,
          regering_pmsurv_marginal, star_regering_pmsurv_ols, star_regering_pmsurv_IV,
          valg3aar_pmsurv_marginal, star_valg3aar_pmsurv_ols, star_valg3aar_pmsurv_IV,
          
          coef = list(NULL, NULL, IV_valgsystem_pmsurv$coefficients,
                      NULL, NULL, IV_regering_pmsurv$coefficients,
                      NULL, NULL, IV_valg3aar_pmsurv$coefficients),
          se = list(valgsystem_pmsurv_logit_se, valgsystem_pmsurv_ols$std.error,
                    IV_valgsystem_pmsurv$std.error,
                    regering_pmsurv_logit_se, regering_pmsurv_ols$std.error,
                    IV_regering_pmsurv$std.error,
                    valg3aar_pmsurv_logit_se, valg3aar_pmsurv_ols$std.error,
                    IV_valg3aar_pmsurv$std.error),
          
          omit = "decade", omit.labels = "årti-dummies",
          omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
          
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels = "premierministerparti bevarer posten",
          column.labels = "(IV: Strategisk valg = premierministerens opløsningsmagt)",
          column.separate = 9,
          
          table.layout = "=!ldc-m#-t-o-sa=!n",
          
          add.lines = list(c("First stage F-test",
                             "", "", 
                             paste0(round(IV_valgsystem_pmsurv_f$value,3), IV_valgsystem_pmsurv_f$stars),
                             "","",
                             paste0(round(IV_regering_pmsurv_f$value,3), IV_regering_pmsurv_f$stars),
                             "","",
                             paste0(round(IV_valg3aar_pmsurv_f$value,3), IV_valg3aar_pmsurv_f$stars)),
                           
                           c("Durbin-Wu-Hausman test (p-værdi)",
                             "","",
                             paste0(round(IV_valgsystem_pmsurv_Wu$p.value,3), IV_valgsystem_pmsurv_Wu$stars),
                             "","",
                             paste0(round(IV_regering_pmsurv_Wu$p.value,3), IV_regering_pmsurv_Wu$stars),
                             "","",
                             paste0(round(IV_valg3aar_pmsurv_Wu$p.value,3), IV_valg3aar_pmsurv_Wu$stars))),
          covariate.labels = c("Strategisk timet valg", "Flertalsvalg", "Antal regeringspartier",
                               "Majoritetsregering", "3 års valgperiode", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "Konstant"),
          
          notes = c("Lande-robuste standardfejl i parentes.",
                    "Koefficienterne for de logistiske regressioner vises i log-odds",
                    "Model 3, 6 og 9 anvender IV-estimation.",
                    "Antal lande: 35"),
          notes.align = "l",
          
          type = "text", style = "commadefault")
)


################### TABEL FOR STEMMEANDEL ############################################

write_clip(
stargazer(star_valgsystem_vote_ols, star_valgsystem_vote_IV,
          star_regering_vote_ols, star_regering_vote_IV,
          star_valg3aar_vote_ols, star_valg3aar_vote_IV,
          
          coef = list(NULL, IV_valgsystem_vote$coefficients,
                      NULL, IV_regering_vote$coefficients,
                      NULL, IV_valg3aar_vote$coefficients),
          
          se = list(valgsystem_vote_ols$std.error, IV_valgsystem_vote$std.error,
                    regering_vote_ols$std.error, IV_regering_vote$std.error,
                    valg3aar_vote_ols$std.error, IV_valg3aar_vote$std.error),
          
          add.lines = list(c("First stage F-test", "", 
                             paste0(round(IV_valgsystem_vote_f$value,3), IV_valgsystem_vote_f$stars), "",
                             paste0(round(IV_regering_vote_f$value,3), IV_regering_vote_f$stars), "",
                             paste0(round(IV_valg3aar_vote_f$value,3), IV_valg3aar_vote_f$stars)),
                           
                           c("Durbin-Wu-Hausman test (p-værdi)", "",
                             paste0(round(IV_valgsystem_vote_Wu$p.value,3), IV_valgsystem_vote_Wu$stars), "",
                             paste0(round(IV_regering_vote_Wu$p.value,3), IV_regering_vote_Wu$stars), "",
                             paste0(round(IV_valg3aar_vote_Wu$p.value,3), IV_valg3aar_vote_Wu$stars))),
          
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels = "Stemmeandel",
          column.labels = "(IV: Strategisk valg = premierministerens opløsningsmagt)",
          column.separate = 6,
          
          covariate.labels = c("Strategisk timet valg", "Nuværende stemmeandel",
                               "Flertalsvalg", "Antal regeringspartier",
                               "Majoritetsregering", "3 års valgperiode", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "Konstant"),
          
          omit = "decade", omit.labels = "årti-dummies",
          omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
          
          notes = c("Lande-robuste standardfejl i parentes.",
                    "Model 2, 4 og 6 anvender IV-estimation.",
                    "Antal lande: 35"), notes.align = "l",
          
          table.layout = "=!ldc-m#-t-o-sa=!n",
          type = "text", style = "commadefault")
)


###################### TABEL FOR MANDATANDEL ############################################

write_clip(
stargazer(star_valgsystem_seats_ols, star_valgsystem_seats_IV,
          star_regering_seats_ols, star_regering_seats_IV,
          star_valg3aar_seats_ols, star_valg3aar_seats_IV,
          
          coef = list(NULL, IV_valgsystem_seats$coefficients,
                      NULL, IV_regering_seats$coefficients,
                      NULL, IV_valg3aar_seats$coefficients),
          
          se = list(valgsystem_seats_ols$std.error, IV_valgsystem_seats$std.error,
                    regering_seats_ols$std.error, IV_regering_seats$std.error,
                    valg3aar_seats_ols$std.error, IV_valg3aar_seats$std.error),
          
          add.lines = list(c("First stage F-test", "", 
                             paste0(round(IV_valgsystem_seats_f$value,3), IV_valgsystem_seats_f$stars), "",
                             paste0(round(IV_regering_seats_f$value,3), IV_regering_seats_f$stars), "",
                             paste0(round(IV_valg3aar_seats_f$value,3), IV_valg3aar_seats_f$stars)),
                           
                           c("Durbin-Wu-Hausman test (p-værdi)", "",
                             paste0(round(IV_valgsystem_seats_Wu$p.value,3), IV_valgsystem_seats_Wu$stars), "",
                             paste0(round(IV_regering_seats_Wu$p.value,3), IV_regering_seats_Wu$stars), "",
                             paste0(round(IV_valg3aar_seats_Wu$p.value,3), IV_valg3aar_seats_Wu$stars))),
          
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels = "Andel af mandater",
          column.labels = "(IV: Strategisk valg = premierministerens opløsningsmagt)",
          column.separate = 6,
          
          covariate.labels = c("Strategisk timet valg", "Nuværende mandatandel",
                               "Flertalsvalg", "Antal regeringspartier",
                               "Majoritetsregering", "3 års valgperiode", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "Konstant"),
          
          omit = "decade", omit.labels = "årti-dummies",
          omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
          
          notes = c("Lande-robuste standardfejl i parentes.",
                    "Model 2, 4 og 6 anvender IV-estimation.",
                    "Antal lande: 35"), notes.align = "l",
          
          table.layout = "=!ldc-m#-t-o-sa=!n",
          type = "text", style = "commadefault")
)


###################### TABEL TIL BILAG FOR PMSURV AME OG OLS ######################################


write_clip(
stargazer(valgsystem_pmsurv_margianl, star_valgsystem_pmsurv_ols,
          regering_pmsurv_marginal, star_regering_pmsurv_ols,
          valg3aar_pmsurv_marginal, star_valg3aar_pmsurv_ols,
          
          coef = list(AME_valgsystem_pmsurv$AME, NULL,
                      AME_regering_pmsurv$AME, NULL,
                      AME_valg3aar_pmsurv$AME, NULL),
          
          se = list(NA, valgsystem_pmsurv_ols$std.error,
                    NA, regering_pmsurv_ols$std.error,
                    NA, valg3aar_pmsurv_ols$std.error),
          p = list(AME_valgsystem_pmsurv$p, NULL,
                   AME_regering_pmsurv$p, NULL,
                   AME_valg3aar_pmsurv$p, NULL),
          
          dep.var.caption = "Afhængig variabel:",
          dep.var.labels = "premierministerparti bevarer posten",
          
          covariate.labels = c("Strategisk timet valg", "Flertalsvalg", "Antal regeringspartier",
                               "Majoritetsregering", "3 års valgperiode", "BNP vækst (1 år lag)",
                               "Inflation (1 år lag)", "Konstant"),
          
          omit = "decade", omit.labels = "årti-dummies", 
          omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
          notes = c("Viser de gennemsnitlige marginale effekter for",
                    "de logistiske regressioner estimeret i tabel X.",
                    "OLS regressioner er medtaget til sammenligning",
                    "Antal lande: 35"),
          notes.align = "l",
          
          type = "text", style = "commadefault")
)

######################                                                             #########################
###################### ROBUSTHEDSTEST MED ALLE ALTERNATIVE FORKLARINGER INKLUDERET #########################
######################                                                             #########################



##### PM survive ##################

OLS_alt_pmsurv <- lm_robust(PMsurvive~strategisk+valgsystem_majo+
                       regeringspartier+majoritet+tre_aar+
                       gdp_growth_lag+inflation_gdp_deflator_lag+
                       factor(decade), 
                     data = kunprime, clusters = country_id,
                     se_type = "stata")
summary(OLS_alt_pmsurv)
star_OLS_alt_pmsurv <- lm(PMsurvive~strategisk+valgsystem_majo+
                            regeringspartier+majoritet+tre_aar+
                            gdp_growth_lag+inflation_gdp_deflator_lag+
                            factor(decade), 
                          data = kunprime)

logit_alt_pmsurv <- glm.cluster(PMsurvive~strategisk+valgsystem_majo+
                          regeringspartier+majoritet+tre_aar+
                          gdp_growth_lag+inflation_gdp_deflator_lag+
                          factor(decade), 
                        data = kunprime, family = "binomial",
                        cluster = "country_id")
summary(logit_alt_pmsurv)
logit_alt_pmsurv_se <- sqrt(diag(logit_alt_pmsurv$vcov))
star_logit_alt_pmsurv <- glm(PMsurvive~strategisk+valgsystem_majo+
                               regeringspartier+majoritet+tre_aar+
                               gdp_growth_lag+inflation_gdp_deflator_lag+
                               factor(decade), 
                             data = kunprime, family = "binomial")


IV_alt_pmsurv <- iv_robust(PMsurvive~strategisk+valgsystem_majo+
                             regeringspartier+majoritet+tre_aar+
                             gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade) |
                             tav_disspm+valgsystem_majo+
                             regeringspartier+majoritet+tre_aar+
                             gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),
                          data = kunprime, clusters = country_id,
                          se_type = "stata", diagnostics = TRUE)
summary(IV_alt_pmsurv)
star_IV_alt_pmsurv <- ivreg(PMsurvive~strategisk+valgsystem_majo+
                              regeringspartier+majoritet+tre_aar+
                              gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade) |
                              tav_disspm+valgsystem_majo+
                              regeringspartier+majoritet+tre_aar+
                              gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),
                            data = kunprime)
### Udregner F
IV_alt_pmsurv_f <- IV_alt_pmsurv$diagnostic_first_stage_fstatistic
IV_alt_pmsurv_f$stars <- ifelse(IV_alt_pmsurv_f[4]<0.01,"***",
                                    ifelse(IV_alt_pmsurv_f[4]<0.05,"**",
                                           ifelse(IV_alt_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_alt_pmsurv_Wu <- IV_alt_pmsurv$diagnostic_endogeneity_test
IV_alt_pmsurv_Wu$stars <- ifelse(IV_alt_pmsurv_Wu[4]<0.01,"***",
                                     ifelse(IV_alt_pmsurv_Wu[4]<0.05,"**",
                                            ifelse(IV_alt_pmsurv_Wu[4]<0.1,"*","")))



########## STEMMEANDEL ############

OLS_alt_vote <- lm_robust(vote_next ~ strat_lag03+vote_share+tre_aar+majoritet+regeringspartier+
                            valgsystem_majo+
                            gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                            factor(decade),
                          data = kunprime, clusters = country_id, se_type = "stata")
summary(OLS_alt_vote)
star_OLS_alt_vote <- lm(vote_next ~ strat_lag03+vote_share+tre_aar+majoritet+regeringspartier+
                          valgsystem_majo+
                          gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                          factor(decade),
                        data = kunprime)

IV_alt_vote <- iv_robust(vote_next ~ strat_lag03+vote_share+tre_aar+majoritet+regeringspartier+
                           valgsystem_majo+
                           gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                           factor(decade) |
                           tav_disspm+vote_share+tre_aar+majoritet+regeringspartier+
                           valgsystem_majo+
                           gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                           factor(decade),
                         data = kunprime, clusters = country_id,
                         se_type = "stata", diagnostics = TRUE)
summary(IV_alt_vote)
star_IV_alt_vote <- ivreg(vote_next ~ strat_lag03+vote_share+tre_aar+majoritet+regeringspartier+
                            valgsystem_majo+
                            gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                            factor(decade) |
                            tav_disspm+vote_share+tre_aar+majoritet+regeringspartier+
                            valgsystem_majo+
                            gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                            factor(decade),
                          data = kunprime)
### Udregner F
IV_alt_vote_f <- IV_alt_vote$diagnostic_first_stage_fstatistic
IV_alt_vote_f$stars <- ifelse(IV_alt_vote_f[4]<0.01,"***",
                                    ifelse(IV_alt_vote_f[4]<0.05,"**",
                                           ifelse(IV_alt_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_alt_vote_Wu <- IV_alt_vote$diagnostic_endogeneity_test
IV_alt_vote_Wu$stars <- ifelse(IV_alt_vote_Wu[4]<0.01,"***",
                                     ifelse(IV_alt_vote_Wu[4]<0.05,"**",
                                            ifelse(IV_alt_vote_Wu[4]<0.1,"*","")))

########## MANDATANDEL ##########################
OLS_alt_seats <- lm_robust(seats_next ~ strat_lag03+seats_andel+tre_aar+majoritet+regeringspartier+
                            valgsystem_majo+
                            gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                            factor(decade),
                          data = kunprime, clusters = country_id, se_type = "stata")
summary(OLS_alt_seats)
star_OLS_alt_seats <- lm(seats_next ~ strat_lag03+seats_andel+tre_aar+majoritet+regeringspartier+
                          valgsystem_majo+
                          gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                          factor(decade),
                        data = kunprime)

IV_alt_seats <- iv_robust(seats_next ~ strat_lag03+seats_andel+tre_aar+majoritet+regeringspartier+
                           valgsystem_majo+
                           gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                           factor(decade) |
                           tav_disspm+seats_andel+tre_aar+majoritet+regeringspartier+
                           valgsystem_majo+
                           gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                           factor(decade),
                         data = kunprime, clusters = country_id,
                         se_type = "stata", diagnostics = TRUE)
summary(IV_alt_seats)
star_IV_alt_seats <- ivreg(seats_next ~ strat_lag03+seats_andel+tre_aar+majoritet+regeringspartier+
                             valgsystem_majo+
                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                             factor(decade) |
                             tav_disspm+seats_andel+tre_aar+majoritet+regeringspartier+
                             valgsystem_majo+
                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                             factor(decade),
                           data = kunprime)
### Udregner F
IV_alt_seats_f <- IV_alt_seats$diagnostic_first_stage_fstatistic
IV_alt_seats_f$stars <- ifelse(IV_alt_seats_f[4]<0.01,"***",
                              ifelse(IV_alt_seats_f[4]<0.05,"**",
                                     ifelse(IV_alt_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_alt_seats_Wu <- IV_alt_seats$diagnostic_endogeneity_test
IV_alt_seats_Wu$stars <- ifelse(IV_alt_seats_Wu[4]<0.01,"***",
                               ifelse(IV_alt_seats_Wu[4]<0.05,"**",
                                      ifelse(IV_alt_seats_Wu[4]<0.1,"*","")))


#############################################################################################
#######################                                       ###############################
#######################       STARGAZER ROBUST ALT            ###############################
#######################                                       ###############################
#############################################################################################


write_clip(
  stargazer(star_logit_alt_pmsurv, star_OLS_alt_pmsurv, star_IV_alt_pmsurv,
            star_OLS_alt_vote, star_IV_alt_vote,
            star_OLS_alt_seats, star_IV_alt_seats,
            
            coef = list(NULL, NULL, IV_alt_pmsurv$coefficients,
                        NULL, IV_alt_vote$coefficients,
                        NULL, IV_alt_seats$coefficients),
            
            se = list(logit_alt_pmsurv_se, OLS_alt_pmsurv$std.error, IV_alt_pmsurv$std.error,
                      OLS_alt_vote$std.error, IV_alt_vote$std.error,
                      OLS_alt_seats$std.error, IV_alt_seats$std.error),
            
            add.lines = list(c("First stage F-test", "","", 
                               paste0(round(IV_alt_pmsurv_f$value,3), IV_alt_pmsurv_f$stars), "",
                               paste0(round(IV_alt_vote_f$value,3), IV_alt_vote_f$stars), "",
                               paste0(round(IV_alt_seats_f$value,3), IV_alt_seats_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", "","",
                               paste0(round(IV_alt_pmsurv_Wu$p.value,3), IV_alt_pmsurv_Wu$stars), "",
                               paste0(round(IV_alt_vote_Wu$p.value,3), IV_alt_vote_Wu$stars), "",
                               paste0(round(IV_alt_seats_Wu$p.value,3), IV_alt_seats_Wu$stars))),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = c("Premierministerparti bevarer posten", "Stemmeandel", "Andel af mandater"),
            column.labels = "(IV: Strategisk valg = premierministerens opløsningsmagt)",
            column.separate = 7,
            
            covariate.labels = c("Strategisk timet valg", "Strategisk timet valg",
                                 "Nuværende stemmeandel", "Nuværende mandatandel",
                                 "Flertalsvalg", "BNP-vækst (1 år lag)", "Inflation (1 år lag)",
                                 "Et-parti regering",
                                 "Majoritetsregering", "3 års valgperiode", "BNP vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
             
            omit = "decade", omit.labels = "årti-dummies",
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            
            notes = c("Lande-robuste standardfejl i parentes.",
                      "Model 3, 5 og 7 anvender IV-estimation.",
                      "Antal lande: 35"), notes.align = "l",
            
            table.layout = "=!ldc-m#-t-o-sa=!n",
            type = "html", style = "commadefault")
)





#############################################################################################
##############                                                  #############################
##############  TEST FOR REGERINGSKARAKTERISTIKA HVER FOR SIG   #############################
##############                                                  #############################
#############################################################################################

##################################      ANTAL AF REGERINGSPARTIER      ######################
rm(list=ls()[! ls() %in% c("kunprime","moveme")])

#################################          PM SURVIVE          ##########################

######################### Regeringskarateristika logit
partier_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk+regeringspartier+
                                      gdp_growth_lag+inflation_gdp_deflator_lag+
                                      factor(decade),
                                    data = kunprime, cluster = "country_id",
                                    family = "binomial")
summary(partier_pmsurv_logit)
partier_pmsurv_logit_se <- sqrt(diag(partier_pmsurv_logit$vcov))
##Margins
partier_pmsurv_marginal <- glm(PMsurvive ~ strategisk+regeringspartier+
                                 gdp_growth_lag+inflation_gdp_deflator_lag+
                                 factor(decade), data = kunprime, family = "binomial")
AME_partier_pmsurv <- summary(margins(partier_pmsurv_marginal))
AME_partier_pmsurv

## OLS
partier_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk+regeringspartier+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata")
summary(partier_pmsurv_ols)
star_partier_pmsurv_ols <- lm(PMsurvive ~ strategisk+regeringspartier+
                                gdp_growth_lag+inflation_gdp_deflator_lag+
                                factor(decade), data = kunprime)


### IV regering
IV_partier_pmsurv <- iv_robust(PMsurvive ~ strategisk+regeringspartier+
                                 gdp_growth_lag+inflation_gdp_deflator_lag+
                                 factor(decade) |
                                 tav_disspm+regeringspartier+majoritet+
                                 gdp_growth_lag+inflation_gdp_deflator_lag+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata", diagnostics = TRUE)
summary(IV_partier_pmsurv)
star_partier_pmsurv_IV <- ivreg(PMsurvive ~ strategisk+regeringspartier+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade) |
                                  tav_disspm+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade), data = kunprime)
### Udregner F
IV_partier_pmsurv_f <- IV_partier_pmsurv$diagnostic_first_stage_fstatistic
IV_partier_pmsurv_f$stars <- ifelse(IV_partier_pmsurv_f[4]<0.01,"***",
                                    ifelse(IV_partier_pmsurv_f[4]<0.05,"**",
                                           ifelse(IV_partier_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_partier_pmsurv_Wu <- IV_partier_pmsurv$diagnostic_endogeneity_test
IV_partier_pmsurv_Wu$stars <- ifelse(IV_partier_pmsurv_Wu[4]<0.01,"***",
                                     ifelse(IV_partier_pmsurv_Wu[4]<0.05,"**",
                                            ifelse(IV_partier_pmsurv_Wu[4]<0.1,"*","")))






##################################   STEMMEANDEL    ######################################

#### Regeringskarakteristika
partier_vote_ols <- lm_robust(vote_next ~ strat_lag03+vote_share+regeringspartier+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade),
                              data = kunprime, clusters = country_id,
                              se_type = "stata")
summary(partier_vote_ols)
star_partier_vote_ols <- lm(vote_next ~ strat_lag03+vote_share+regeringspartier+
                              gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                              factor(decade), data = kunprime)

## IV
IV_partier_vote <- iv_robust(vote_next ~ strat_lag03+vote_share+regeringspartier+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade) |
                               tav_disspm+vote_share+regeringspartier+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade),
                             data = kunprime, clusters = country_id,
                             se_type = "stata", diagnostics = TRUE)
summary(IV_partier_vote)
star_partier_vote_IV <- ivreg(vote_next ~ strat_lag03+vote_share+regeringspartier+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+vote_share+regeringspartier+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade), data = kunprime)

### Udregner F
IV_partier_vote_f <- IV_partier_vote$diagnostic_first_stage_fstatistic
IV_partier_vote_f$stars <- ifelse(IV_partier_vote_f[4]<0.01,"***",
                                  ifelse(IV_partier_vote_f[4]<0.05,"**",
                                         ifelse(IV_partier_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_partier_vote_Wu <- IV_partier_vote$diagnostic_endogeneity_test
IV_partier_vote_Wu$stars <- ifelse(IV_partier_vote_Wu[4]<0.01,"***",
                                   ifelse(IV_partier_vote_Wu[4]<0.05,"**",
                                          ifelse(IV_partier_vote_Wu[4]<0.1,"*","")))




######################################## MANDATANDEL ##########################################

#### Regeringskarakteristika
partier_seats_ols <- lm_robust(seats_next ~ strat_lag03+seats_andel+regeringspartier+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata")
summary(partier_seats_ols)
star_partier_seats_ols <- lm(seats_next ~ strat_lag03+seats_andel+regeringspartier+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade), data = kunprime)

## IV
IV_partier_seats <- iv_robust(seats_next ~ strat_lag03+seats_andel+regeringspartier+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+seats_andel+regeringspartier+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade),
                              data = kunprime, clusters = country_id,
                              se_type = "stata", diagnostics = TRUE)
summary(IV_partier_seats)
star_partier_seats_IV <- ivreg(seats_next ~ strat_lag03+seats_andel+regeringspartier+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade) |
                                 tav_disspm+seats_andel+regeringspartier+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade), data = kunprime)

### Udregner F
IV_partier_seats_f <- IV_partier_seats$diagnostic_first_stage_fstatistic
IV_partier_seats_f$stars <- ifelse(IV_partier_seats_f[4]<0.01,"***",
                                   ifelse(IV_partier_seats_f[4]<0.05,"**",
                                          ifelse(IV_partier_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_partier_seats_Wu <- IV_partier_seats$diagnostic_endogeneity_test
IV_partier_seats_Wu$stars <- ifelse(IV_partier_seats_Wu[4]<0.01,"***",
                                    ifelse(IV_partier_seats_Wu[4]<0.05,"**",
                                           ifelse(IV_partier_seats_Wu[4]<0.1,"*","")))



##################################      MAJORITETSREGERING      ######################




#################################          PM SURVIVE          ##########################

######################### Regeringskarateristika logit
majo_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk+majoritet+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade),
                                 data = kunprime, cluster = "country_id",
                                 family = "binomial")
summary(majo_pmsurv_logit)
majo_pmsurv_logit_se <- sqrt(diag(majo_pmsurv_logit$vcov))
##Margins
majo_pmsurv_marginal <- glm(PMsurvive ~ strategisk+majoritet+
                              gdp_growth_lag+inflation_gdp_deflator_lag+
                              factor(decade), data = kunprime, family = "binomial")
AME_majo_pmsurv <- summary(margins(majo_pmsurv_marginal))
AME_majo_pmsurv

## OLS
majo_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk+majoritet+
                               gdp_growth_lag+inflation_gdp_deflator_lag+
                               factor(decade),
                             data = kunprime, clusters = country_id,
                             se_type = "stata")
summary(majo_pmsurv_ols)
star_majo_pmsurv_ols <- lm(PMsurvive ~ strategisk+majoritet+
                             gdp_growth_lag+inflation_gdp_deflator_lag+
                             factor(decade), data = kunprime)


### IV regering
IV_majo_pmsurv <- iv_robust(PMsurvive ~ strategisk+majoritet+
                              gdp_growth_lag+inflation_gdp_deflator_lag+
                              factor(decade) |
                              tav_disspm+majoritet+majoritet+
                              gdp_growth_lag+inflation_gdp_deflator_lag+
                              factor(decade),
                            data = kunprime, clusters = country_id,
                            se_type = "stata", diagnostics = TRUE)
summary(IV_majo_pmsurv)
star_majo_pmsurv_IV <- ivreg(PMsurvive ~ strategisk+majoritet+
                               gdp_growth_lag+inflation_gdp_deflator_lag+
                               factor(decade) |
                               tav_disspm+majoritet+majoritet+
                               gdp_growth_lag+inflation_gdp_deflator_lag+
                               factor(decade), data = kunprime)
### Udregner F
IV_majo_pmsurv_f <- IV_majo_pmsurv$diagnostic_first_stage_fstatistic
IV_majo_pmsurv_f$stars <- ifelse(IV_majo_pmsurv_f[4]<0.01,"***",
                                 ifelse(IV_majo_pmsurv_f[4]<0.05,"**",
                                        ifelse(IV_majo_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_majo_pmsurv_Wu <- IV_majo_pmsurv$diagnostic_endogeneity_test
IV_majo_pmsurv_Wu$stars <- ifelse(IV_majo_pmsurv_Wu[4]<0.01,"***",
                                  ifelse(IV_majo_pmsurv_Wu[4]<0.05,"**",
                                         ifelse(IV_majo_pmsurv_Wu[4]<0.1,"*","")))






##################################   STEMMEANDEL    ######################################

#### Regeringskarakteristika
majo_vote_ols <- lm_robust(vote_next ~ strat_lag03+vote_share+majoritet+
                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                             factor(decade),
                           data = kunprime, clusters = country_id,
                           se_type = "stata")
summary(majo_vote_ols)
star_majo_vote_ols <- lm(vote_next ~ strat_lag03+vote_share+majoritet+
                           gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                           factor(decade), data = kunprime)

## IV
IV_majo_vote <- iv_robust(vote_next ~ strat_lag03+vote_share+majoritet+
                            gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                            factor(decade) |
                            tav_disspm+vote_share+majoritet+
                            gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                            factor(decade),
                          data = kunprime, clusters = country_id,
                          se_type = "stata", diagnostics = TRUE)
summary(IV_majo_vote)
star_majo_vote_IV <- ivreg(vote_next ~ strat_lag03+vote_share+majoritet+
                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                             factor(decade) |
                             tav_disspm+vote_share+majoritet+
                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                             factor(decade), data = kunprime)

### Udregner F
IV_majo_vote_f <- IV_majo_vote$diagnostic_first_stage_fstatistic
IV_majo_vote_f$stars <- ifelse(IV_majo_vote_f[4]<0.01,"***",
                               ifelse(IV_majo_vote_f[4]<0.05,"**",
                                      ifelse(IV_majo_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_majo_vote_Wu <- IV_majo_vote$diagnostic_endogeneity_test
IV_majo_vote_Wu$stars <- ifelse(IV_majo_vote_Wu[4]<0.01,"***",
                                ifelse(IV_majo_vote_Wu[4]<0.05,"**",
                                       ifelse(IV_majo_vote_Wu[4]<0.1,"*","")))



######################################## MANDATANDEL ##########################################

#### Regeringskarakteristika
majo_seats_ols <- lm_robust(seats_next ~ strat_lag03+seats_andel+majoritet+
                              gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                              factor(decade),
                            data = kunprime, clusters = country_id,
                            se_type = "stata")
summary(majo_seats_ols)
star_majo_seats_ols <- lm(seats_next ~ strat_lag03+seats_andel+majoritet+
                            gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                            factor(decade), data = kunprime)

## IV
IV_majo_seats <- iv_robust(seats_next ~ strat_lag03+seats_andel+majoritet+
                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                             factor(decade) |
                             tav_disspm+seats_andel+majoritet+
                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                             factor(decade),
                           data = kunprime, clusters = country_id,
                           se_type = "stata", diagnostics = TRUE)
summary(IV_majo_seats)
star_majo_seats_IV <- ivreg(seats_next ~ strat_lag03+seats_andel+majoritet+
                              gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                              factor(decade) |
                              tav_disspm+seats_andel+majoritet+
                              gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                              factor(decade), data = kunprime)

### Udregner F
IV_majo_seats_f <- IV_majo_seats$diagnostic_first_stage_fstatistic
IV_majo_seats_f$stars <- ifelse(IV_majo_seats_f[4]<0.01,"***",
                                ifelse(IV_majo_seats_f[4]<0.05,"**",
                                       ifelse(IV_majo_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_majo_seats_Wu <- IV_majo_seats$diagnostic_endogeneity_test
IV_majo_seats_Wu$stars <- ifelse(IV_majo_seats_Wu[4]<0.01,"***",
                                 ifelse(IV_majo_seats_Wu[4]<0.05,"**",
                                        ifelse(IV_majo_seats_Wu[4]<0.1,"*","")))



#############################################################################################
##############                                                  #############################
##############  STARGAZER REGERINGSKARAKTERISTIKA HVER FOR SIG  #############################
##############                                                  #############################
#############################################################################################

write_clip(
  stargazer(partier_pmsurv_marginal, star_partier_pmsurv_ols, star_partier_pmsurv_IV,
            majo_pmsurv_marginal, star_majo_pmsurv_ols, star_majo_pmsurv_IV,
            
            coef = list(NULL, NULL, IV_partier_pmsurv$coefficients,
                        NULL, NULL, IV_majo_pmsurv$coefficients),
            
            se = list(partier_pmsurv_logit_se, partier_pmsurv_ols$std.error,
                      IV_partier_pmsurv$std.error,
                      majo_pmsurv_logit_se, majo_pmsurv_ols$std.error,
                      IV_majo_pmsurv$std.error),
            
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "premierministerparti bevarer posten",
            column.labels = "(IV: Strategisk valg = premierministerens opløsningsmagt)",
            column.separate = 6,
            
            table.layout = "=!ldc-m#-t-o-sa=!n",
            
            add.lines = list(c("First stage F-test",
                               "", "", 
                               paste0(round(IV_partier_pmsurv_f$value,3), IV_partier_pmsurv_f$stars),
                               "","",
                               paste0(round(IV_majo_pmsurv_f$value,3), IV_majo_pmsurv_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)",
                               "","",
                               paste0(round(IV_partier_pmsurv_Wu$p.value,3), IV_partier_pmsurv_Wu$stars),
                               "","",
                               paste0(round(IV_majo_pmsurv_Wu$p.value,3), IV_majo_pmsurv_Wu$stars))),
            
            covariate.labels = c("Strategisk timet valg", "Et-parti regering",
                                 "Majoritetsregering", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "Koefficienterne for de logistiske regressioner vises i log-odds",
                      "Model 3 og 6 anvender IV-estimation.",
                      "Antal lande: 35"),
            notes.align = "l",
            
            type = "html", style = "commadefault")
)

#################### STEMMEANDEL #####################################################


write_clip(
  stargazer(star_partier_vote_ols, star_partier_vote_IV,
            star_majo_vote_ols, star_majo_vote_IV,
            
            
            coef = list(NULL, IV_partier_vote$coefficients,
                        NULL, IV_majo_vote$coefficients),
            
            se = list(partier_vote_ols$std.error, IV_partier_vote$std.error,
                      majo_vote_ols$std.error, IV_majo_vote$std.error),
            
            add.lines = list(c("First stage F-test", "", 
                               paste0(round(IV_partier_vote_f$value,3), IV_partier_vote_f$stars), "",
                               paste0(round(IV_majo_vote_f$value,3), IV_majo_vote_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", "",
                               paste0(round(IV_partier_vote_Wu$p.value,3), IV_partier_vote_Wu$stars), "",
                               paste0(round(IV_majo_vote_Wu$p.value,3), IV_majo_vote_Wu$stars))),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "Stemmeandel",
            column.labels = "(IV: Strategisk valg = premierministerens opløsningsmagt)",
            column.separate = 4,
            
            covariate.labels = c("Strategisk timet valg", "Nuværende stemmeandel",
                                 "Et-parti regering",
                                 "Majoritetsregering", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "Model 2 og 4 anvender IV-estimation.",
                      "Antal lande: 35"), notes.align = "l",
            
            table.layout = "=!ldc-m#-t-o-sa=!n",
            type = "html", style = "commadefault")
)



#################### MANDATANDEL #####################################################


write_clip(
  stargazer(star_partier_seats_ols, star_partier_seats_IV,
            star_majo_seats_ols, star_majo_seats_IV,
            
            
            coef = list(NULL, IV_partier_seats$coefficients,
                        NULL, IV_majo_seats$coefficients),
            
            se = list(partier_seats_ols$std.error, IV_partier_seats$std.error,
                      majo_seats_ols$std.error, IV_majo_seats$std.error),
            
            add.lines = list(c("First stage F-test", "", 
                               paste0(round(IV_partier_seats_f$value,3), IV_partier_seats_f$stars), "",
                               paste0(round(IV_majo_seats_f$value,3), IV_majo_seats_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", "",
                               paste0(round(IV_partier_seats_Wu$p.value,3), IV_partier_seats_Wu$stars), "",
                               paste0(round(IV_majo_seats_Wu$p.value,3), IV_majo_seats_Wu$stars))),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "Andel af mandater",
            column.labels = "(IV: Strategisk valg = premierministerens opløsningsmagt)",
            column.separate = 4,
            
            covariate.labels = c("Strategisk timet valg", "Nuværende mandatandel",
                                 "Et-parti regering",
                                 "Majoritetsregering", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "Model 2 og 4 anvender IV-estimation.",
                      "Antal lande: 35"), notes.align = "l",
            
            table.layout = "=!ldc-m#-t-o-sa=!n",
            type = "html", style = "commadefault")
)



########################################################################
############                                         ###################
############  KØRER ALLE MODELLER MED STRATEGISK 4   ###################
############                                         ###################
########################################################################


rm(list=ls()[! ls() %in% c("kunprime","moveme")])
#### PM Survive

PMsurv_ols1 <- lm_robust(PMsurvive~strategisk04,
                         data = kunprime,
                         clusters = country_id,
                         se_type = "stata")
summary(PMsurv_ols1)
## LM-objekt til stargazer
star_PMsurv_ols1 <-lm(PMsurvive~strategisk04, data = kunprime)


PMsurv_logit1 <- glm.cluster(PMsurvive ~ strategisk04,
                             data = kunprime,
                             cluster = "country_id",
                             family = "binomial")
summary(PMsurv_logit1)
# Gemmer standardfejl for logit 1
PMsurv_logit1_se <- sqrt(diag(PMsurv_logit1$vcov))

### Udregner Odds ratio
PMsurv_logit1_OR <- exp(cbind(coef(PMsurv_logit1), confint(PMsurv_logit1)))
PMsurv_logit1_OR

### Udregner margins for GLM modellen, margins-funktionen kan ikke spise
## glm.cluster objekter, så den må lige laves med en almindelige glm.
PMsurv_logit_marginal1 <- glm(PMsurvive ~ strategisk04,data = kunprime,family = "binomial")
summary(PMsurv_logit_marginal1)

AME_pmsurv <- summary(margins(PMsurv_logit_marginal1))
mean(AME_pmsurv$dydx_strategisk04)
### OLS og GLM (AME) - estimatet er næsten ens.

### Tiljøjer variable, Årti-dummies
PMsurv_ols2 <- lm_robust(PMsurvive ~ strategisk04+
                           factor(decade),
                         data = kunprime,
                         clusters = country_id,
                         se_type = "stata")
summary(PMsurv_ols2)
summary(PMsurv_ols2$N)
star_PMsurv_ols2 <- lm(PMsurvive ~ strategisk04+factor(decade),data = kunprime)

PMsurv_logit2 <- glm.cluster(PMsurvive ~ strategisk04+
                               factor(decade),
                             data = kunprime,
                             cluster = "country_id",
                             family = "binomial")
summary(PMsurv_logit2)
## Gemmer robuste standardfejl
PMsurv_logit2_se <- sqrt(diag(PMsurv_logit2$vcov))
## OR
PMsurv_OR_logit2 <- exp(coef(PMsurv_logit2))
PMsurv_OR_logit2

### Udregner margins for logit 2
PMsurv_logit_marginal2 <- glm(PMsurvive ~ strategisk04+factor(decade), data = kunprime, family = "binomial")
summary(PMsurv_logit_marginal2)
AME_pmsurv2 <- summary(margins(PMsurv_logit_marginal2))
AME_pmsurv2

### Tilføjer økonomi
PMsurv_ols3 <- lm_robust(PMsurvive ~ strategisk04+
                           gdp_growth_lag+
                           inflation_gdp_deflator_lag+
                           factor(decade),
                         data = kunprime,
                         clusters = country_id,
                         se_type = "stata")
summary(PMsurv_ols3)
summary(PMsurv_ols3$N)
## Går fra 361 til 306 (305 med inflation) observationer.
star_PMsurv_ols3 <- lm(PMsurvive ~ strategisk04+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),data = kunprime)



PMsurv_logit3 <- glm.cluster(PMsurvive ~ strategisk04+
                               gdp_growth_lag+
                               inflation_gdp_deflator_lag+
                               factor(decade),
                             data = kunprime,
                             cluster = "country_id",
                             family = "binomial")
summary(PMsurv_logit3)
## Gemmer robuste standardfejl
PMsurv_logit3_se <- sqrt(diag(PMsurv_logit3$vcov))
## OR
PMsurv_OR_logit3 <- exp(coef(PMsurv_logit3))
PMsurv_OR_logit3
## Margins for logit 3
PMsurv_logit_marginal3 <- glm(PMsurvive~strategisk04+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade), data = kunprime, family = "binomial")
summary(PMsurv_logit_marginal3)
AME_pmsurv3 <- summary(margins(PMsurv_logit_marginal3))
AME_pmsurv3



?stargazer

setwd("/Users/ethranholm/OneDrive/R/Speciale output")

#### Kører data ud i stargazer

write_clip(
  stargazer(star_PMsurv_ols1, PMsurv_logit_marginal1, 
            star_PMsurv_ols2, PMsurv_logit_marginal2,
            star_PMsurv_ols3, PMsurv_logit_marginal3,
            
            se = list(PMsurv_ols1$std.error, PMsurv_logit1_se,
                      PMsurv_ols2$std.error, PMsurv_logit2_se,
                      PMsurv_ols3$std.error, PMsurv_logit3_se),
            
            covariate.labels = c("strategisk04 timet valg", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            dep.var.caption = "Afhængig variabel:", dep.var.labels = "premierministerparti bevarer posten",
            
            omit = "decade", omit.labels ="Årti-dummies", omit.yes.no = c("Nej", "Ja"),
            notes = c("Lande-clustered robuste standardfejl i parentes.", "For de logistiske regressioner vises log-odds."),
            omit.stat = c("ser","f"),
            notes.align = "l",
            
            type = "html", style = "commadefault")
)




### Tabel med marginale effekter. OBS STANDARDFEJL TILFØJES MANUELT.
write_clip(
  stargazer(PMsurv_logit_marginal1, PMsurv_logit_marginal2, 
            PMsurv_logit_marginal3,
            
            coef = list(AME_pmsurv$AME, AME_pmsurv2$AME, AME_pmsurv3$AME),
            se = list(AME_pmsurv$p, AME_pmsurv2$p, AME_pmsurv3$p),
            
            keep = "strategisk04",
            omit = c("decade", "gdp_growth_lag"), 
            omit.labels = c("Årti-dummies", "Kontrol økonomi"),
            omit.yes.no = c("Nej", "Ja"),
            covariate.labels = "strategisk04 timet valg",
            dep.var.caption = "Afhængig variabel:", dep.var.labels = "premierministerparti bevarer posten",
            notes = "Viser de gennemsnitlige marginale effekter for de logistiske regressioner estimeret i tabel X",
            type = "html", style = "commadefault",
            notes.align = "l")
)



#################################################################################
#######################                               ###########################
######################  OLS for stemmer og mandater   ###########################
######################                                ###########################
#################################################################################



########## Stemmer ##########
vote_ols1 <- lm_robust(vote_next ~ strat_lag04,
                       data = kunprime,
                       clusters = country_id,
                       se_type = "stata")
summary(vote_ols1)
## Laver objekt til stargazer
star_vote_ols1 <- lm(vote_next ~ strat_lag04, data = kunprime)

### Nuværende stemmeandel, og tid
vote_ols2 <- lm_robust(vote_next ~ strat_lag04+vote_share+
                         factor(decade), 
                       data = kunprime,
                       clusters = country_id,
                       se_type = "stata")
summary(vote_ols2)
star_vote_ols2 <- lm(vote_next ~ strat_lag04+vote_share+factor(decade),data = kunprime)


## økonomi
vote_ols3 <- lm_robust(vote_next ~ strat_lag04+vote_share+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade), 
                       data = kunprime,
                       clusters = country_id,
                       se_type = "stata")
summary(vote_ols3)
star_vote_ols3 <- lm(vote_next ~ strat_lag04+vote_share+
                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)



########## Mandater ############

seats_ols1 <- lm_robust(seats_next ~ strat_lag04,
                        data = kunprime,
                        clusters = country_id,
                        se_type = "stata")
summary(seats_ols1)
star_seats_ols1 <- lm(seats_next ~ strat_lag04,data = kunprime)


#### nuværende mandater og tidsdummies

seats_ols2 <- lm_robust(seats_next ~ strat_lag04+seats_andel+
                          factor(decade),
                        data = kunprime,
                        clusters = country_id,
                        se_type = "stata")
summary(seats_ols2)
star_seats_ols2 <- lm(seats_next ~ strat_lag04+seats_andel+factor(decade),data = kunprime)

### Økonomi

seats_ols3 <- lm_robust(seats_next ~ strat_lag04+seats_andel+
                          gdp_growth_lag_next+
                          inflation_gdp_deflator_lag_next+
                          factor(decade),
                        data = kunprime,
                        clusters = country_id,
                        se_type = "stata")
summary(seats_ols3)
star_seats_ols3 <- lm(seats_next ~ strat_lag04+seats_andel+gdp_growth_lag_next+
                        inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)


#### Skriver til stargazer
write_clip(
  stargazer(star_vote_ols1, star_vote_ols2, star_vote_ols3,
            star_seats_ols1, star_seats_ols2, star_seats_ols3,
            
            se = list(vote_ols1$std.error, vote_ols2$std.error,
                      vote_ols3$std.error,
                      seats_ols1$std.error, seats_ols2$std.error,
                      seats_ols3$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies", omit.yes.no = c("Nej", "Ja"),
            notes = "Lande-clustered robuste standardfejl i parentes.", omit.stat = c("ser","f"),
            order = c("strat_lag04", "vote_share", "seats_andel"),
            
            covariate.labels = c("strategisk04 timet valg", "Nuværende stemmeandel",
                                 "Nuværende mandatandel", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            dep.var.caption = "Afhængig variable:", notes.align = "l",
            dep.var.labels = c("Stemmeandel", "Mandatandel"),
            
            type = "html", style = "commadefault")
)


#################################################################################
#######################                               ###########################
######################    First stage regressioner    ###########################
######################                                ###########################
#################################################################################

rm(list=ls()[! ls() %in% c("kunprime","moveme")])

first_stage_ols1 <- lm_robust(strategisk04 ~ tav_disspm+
                                factor(decade), 
                              data = kunprime,
                              clusters = country_id,
                              se_type = "stata")
summary(first_stage_ols1)
star_first_stage_ols <- lm(strategisk04 ~ tav_disspm+factor(decade), data = kunprime)



first_stage_logit1 <- glm.cluster(strategisk04 ~ tav_disspm+
                                    factor(decade),
                                  data = kunprime,
                                  cluster = "country_id",
                                  family = "binomial")
summary(first_stage_logit1)
first_stage_logit1_se <- sqrt(diag(first_stage_logit1$vcov))
## OR
first_stage_logit1_OR <- exp(coef(first_stage_logit1))
first_stage_logit1_OR
## Margins
first_stage_marginal1 <- glm(strategisk04 ~ tav_disspm+factor(decade), data = kunprime,family = "binomial")
AME_first_stage1 <- summary(margins(first_stage_marginal1))
AME_first_stage1



### Stemmer
first_stage_ols_vote <- lm_robust(strat_lag04 ~ tav_disspm+vote_share+factor(decade),
                                  data = kunprime,
                                  clusters = country_id,
                                  se_type = "stata")
summary(first_stage_ols_vote)
star_first_stage_ols_vote <- lm(strat_lag04 ~ tav_disspm+vote_share+factor(decade), data = kunprime)

##logit
first_stage_logit_vote <- glm.cluster(strat_lag04 ~ tav_disspm+vote_share+factor(decade),
                                      data = kunprime,
                                      cluster = "country_id",
                                      family = "binomial")
summary(first_stage_logit_vote)
first_stage_logit_vote_se <- sqrt(diag(first_stage_logit_vote$vcov))
## Margins
first_stage_marginal_vote <- glm(strat_lag04 ~ tav_disspm+vote_share+factor(decade),
                                 data = kunprime,family = "binomial")
AME_first_stage_vote <- summary(margins(first_stage_marginal_vote))
AME_first_stage_vote

### Mandater
first_stage_ols_seats <- lm_robust(strat_lag04 ~ tav_disspm+seats_andel+
                                     factor(decade), 
                                   data = kunprime,
                                   clusters = country_id,
                                   se_type = "stata")
summary(first_stage_ols_seats)
star_first_stage_ols_seats <- lm(strat_lag04 ~ tav_disspm+seats_andel+factor(decade), data = kunprime)

## Logit mandater
first_stage_logit_seats <- glm.cluster(strat_lag04 ~ tav_disspm+seats_andel+
                                         factor(decade),
                                       data = kunprime,
                                       cluster = "country_id",
                                       family = "binomial")
summary(first_stage_logit_seats)
first_stage_logit_seats_se <- sqrt(diag(first_stage_logit_seats$vcov))
## Margins
first_stage_marginal_seats <- glm(strat_lag04 ~ tav_disspm+seats_andel+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_seats <- summary(margins(first_stage_marginal_seats))
AME_first_stage_seats



### Fulde first stage ###############################################################
first_stage_pmsurv_ols_full <- lm_robust(strategisk04 ~ tav_disspm+
                                           gdp_growth_lag+inflation_gdp_deflator_lag+
                                           factor(decade),
                                         data = kunprime,
                                         clusters = country_id,
                                         se_type = "stata")
summary(first_stage_pmsurv_ols_full)
star_first_stage_pmsurv_ols_full <- lm(strategisk04 ~ tav_disspm+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),data = kunprime)

## Logit
first_stage_logit_pmsurv_full <- glm.cluster(strategisk04 ~ tav_disspm+
                                               gdp_growth_lag+inflation_gdp_deflator_lag+
                                               factor(decade),
                                             data = kunprime,
                                             cluster = "country_id",
                                             family = "binomial")
summary(first_stage_logit_pmsurv_full)
first_stage_logit_pmsurv_full_se <- sqrt(diag(first_stage_logit_pmsurv_full$vcov))
## Margins
first_stage_pmsurv_full_marginal <- glm(strategisk04~tav_disspm+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_pmsurv_full <- summary(margins(first_stage_pmsurv_full_marginal))
AME_first_stage_pmsurv_full


### First stage stemmer full ##################################################################
first_stage_ols_votes_full <- lm_robust(strat_lag04~tav_disspm+vote_share+
                                          gdp_growth_lag_next+
                                          inflation_gdp_deflator_lag_next+
                                          factor(decade),
                                        data = kunprime,
                                        clusters = country_id,
                                        se_type = "stata")
summary(first_stage_ols_votes_full)
star_first_stage_ols_votes_full <- lm(strat_lag04~tav_disspm+vote_share+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),data = kunprime)


### Logit
first_stage_logit_votes_full <- glm.cluster(strat_lag04~tav_disspm+vote_share+
                                              gdp_growth_lag_next+
                                              inflation_gdp_deflator_lag_next+
                                              factor(decade),
                                            data = kunprime,
                                            cluster = "country_id",
                                            family = "binomial")
summary(first_stage_logit_votes_full)
first_stage_logit_votes_full_se <- sqrt(diag(first_stage_logit_votes_full$vcov))
## Margins
first_stage_votes_full_marginal <- glm(strat_lag04~tav_disspm+vote_share+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_votes_full <- summary(margins(first_stage_votes_full_marginal))
AME_first_stage_votes_full


### first stage mandater full ####################################################################
first_stage_ols_seats_full <- lm_robust(strat_lag04~tav_disspm+seats_andel+
                                          gdp_growth_lag_next+
                                          inflation_gdp_deflator_lag_next+
                                          factor(decade),
                                        data = kunprime,
                                        clusters = country_id,
                                        se_type = "stata")
summary(first_stage_ols_seats_full)
star_first_stage_ols_seats_full <- lm(strat_lag04~tav_disspm+seats_andel+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)

## Logit
first_stage_logit_seats_full <- glm.cluster(strat_lag04~tav_disspm+seats_andel+
                                              gdp_growth_lag_next+
                                              inflation_gdp_deflator_lag_next+
                                              factor(decade),
                                            data = kunprime,
                                            cluster = "country_id",
                                            family = "binomial")
summary(first_stage_logit_seats_full)
first_stage_logit_seats_full_se <- sqrt(diag(first_stage_logit_seats_full$vcov))

## Margins
first_stage_seats_full_marginal <- glm(strat_lag04~tav_disspm+seats_andel+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_seats_full <- summary(margins(first_stage_seats_full_marginal))
AME_first_stage_seats_full






####################################################################################
#####################                                  #############################
#####################     STARGAZER FIRST STAGE        #############################
#####################                                  #############################
####################################################################################



### Laver to paneler. Et med OLS og et med logistiske

## OLS
write_clip(
  stargazer(star_first_stage_ols, star_first_stage_ols_vote, star_first_stage_ols_seats,
            star_first_stage_pmsurv_ols_full, star_first_stage_ols_votes_full, star_first_stage_ols_seats_full,
            
            se = list(first_stage_ols1$std.error, first_stage_ols_vote$std.error, 
                      first_stage_ols_seats$std.error,
                      first_stage_pmsurv_ols_full$std.error, first_stage_ols_votes_full$std.error,
                      first_stage_ols_seats_full$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies", omit.yes.no = c("Ja", "Nej"),
            notes = c("Lande-clustered robuste standardfejl i parentes.", "PM: premierminister"), 
            omit.stat = c("ser", "f"),
            
            covariate.labels = c("PM opløsningsmagt", "Nuværende stemmeandel",
                                 "Nuværende mandatandel", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels.include = FALSE,
            column.labels = "strategisk04 timet parlamentsvalg",
            column.separate = 6,
            notes.align = "l",
            
            type = "html", style = "commadefault")
)


## Logistisk
write_clip(
  stargazer(first_stage_marginal1, first_stage_marginal_vote, 
            first_stage_marginal_seats,
            first_stage_pmsurv_full_marginal, first_stage_votes_full_marginal, 
            first_stage_seats_full_marginal,
            
            se = list(first_stage_logit1_se, first_stage_logit_vote_se,
                      first_stage_logit_seats_se,
                      first_stage_logit_pmsurv_full_se, first_stage_logit_votes_full_se,
                      first_stage_logit_seats_full_se),
            omit = "decade", omit.labels = "Årti-dummies", omit.yes.no = c("Ja", "Nej"),
            notes = c("Koefficienterne vises i log-odds", "Lande-clustered robuste standardfejl i parentes.", "PM: premierminister"), 
            omit.stat = c("ser", "f"),
            
            covariate.labels = c("PM opløsningsmagt", "Nuværende stemmeandel",
                                 "Nuværende mandatandel", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels.include = FALSE,
            column.labels = "strategisk04 timet parlamentsvalg",
            column.separate = 6,
            notes.align = "l",
            
            type = "html", style = "commadefault")
)


#### Laver stargazer tabel med de gennemsnitlige marginale effekter
### OBS standardfejl skal tilføjes manuelt.

write_clip(
  stargazer(first_stage_marginal1, first_stage_marginal_vote, 
            first_stage_marginal_seats,
            first_stage_pmsurv_full_marginal, first_stage_votes_full_marginal, 
            first_stage_seats_full_marginal,
            
            coef = list(AME_first_stage1$AME, AME_first_stage_vote$AME,
                        AME_first_stage_seats$AME, 
                        AME_first_stage_pmsurv_full$AME, AME_first_stage_votes_full$AME,
                        AME_first_stage_seats_full$AME),
            
            keep = c("tav_disspm", "vote_share", "seats_andel"),
            
            omit = c("decade", "gdp_growth_lag"),
            omit.labels = c("Årti-dummies", "Kontrol økonomi"), 
            omit.yes.no = c("Ja", "Nej"),
            notes = c("Viser de gennemsnitlige marginale effekter for de logistiske regressioner estimeret i tabel X",
                      "PM: premierminister"), 
            omit.stat = c("ser", "f"),
            
            covariate.labels = c("PM opløsningsmagt", "Nuværende stemmeandel", "Nuværende mandatandel"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels.include = FALSE,
            column.labels = "strategisk04 timet parlamentsvalg",
            column.separate = 6,
            notes.align = "l",
            
            type = "html", style = "commadefault")
)




####################################################################################
#####################                                  #############################
#####################     FINAL IV-Modeller            #############################
#####################                                  #############################
####################################################################################
rm(list=ls()[! ls() %in% c("kunprime","moveme")])

########################### PM_surv ##############################################
IV_pmsurv1 <- iv_robust(PMsurvive ~ strategisk04 |
                          tav_disspm,
                        data = kunprime, clusters = country_id,
                        se_type = "stata", diagnostics = TRUE)
summary(IV_pmsurv1)
star_IV_pmsurv1 <- lm(PMsurvive ~ strategisk04, data = kunprime)
### Udregner F
IV_pmsurv1_f <- IV_pmsurv1$diagnostic_first_stage_fstatistic
IV_pmsurv1_f$stars <- ifelse(IV_pmsurv1_f[4]<0.01,"***",
                             ifelse(IV_pmsurv1_f[4]<0.05,"**",
                                    ifelse(IV_pmsurv1_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_pmsurv1_Wu <- IV_pmsurv1$diagnostic_endogeneity_test
IV_pmsurv1_Wu$stars <- ifelse(IV_pmsurv1_Wu[4]<0.01,"***",
                              ifelse(IV_pmsurv1_Wu[4]<0.05,"**",
                                     ifelse(IV_pmsurv1_Wu[4]<0.1,"*","")))



## 2
IV_pmsurv2 <- iv_robust(PMsurvive ~ strategisk04+factor(decade) |
                          tav_disspm+factor(decade),
                        data = kunprime, clusters = country_id,
                        se_type = "stata", diagnostics = TRUE)
summary(IV_pmsurv2)
star_IV_pmsurv2 <- lm(PMsurvive ~ strategisk04+factor(decade), data = kunprime)
### Udregner F
IV_pmsurv2_f <- IV_pmsurv2$diagnostic_first_stage_fstatistic
IV_pmsurv2_f$stars <- ifelse(IV_pmsurv2_f[4]<0.01,"***",
                             ifelse(IV_pmsurv2_f[4]<0.05,"**",
                                    ifelse(IV_pmsurv2_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_pmsurv2_Wu <- IV_pmsurv2$diagnostic_endogeneity_test
IV_pmsurv2_Wu$stars <- ifelse(IV_pmsurv2_Wu[4]<0.01,"***",
                              ifelse(IV_pmsurv2_Wu[4]<0.05,"**",
                                     ifelse(IV_pmsurv2_Wu[4]<0.1,"*","")))


## 3
IV_pmsurv3 <- iv_robust(PMsurvive ~ strategisk04+
                          gdp_growth_lag+
                          inflation_gdp_deflator_lag+
                          factor(decade) |
                          tav_disspm+
                          gdp_growth_lag+
                          inflation_gdp_deflator_lag+
                          factor(decade),
                        data = kunprime, clusters = country_id,
                        se_type = "stata", diagnostics = TRUE)
summary(IV_pmsurv3)
star_IV_pmsurv3 <- lm(PMsurvive ~ strategisk04+gdp_growth_lag+
                        inflation_gdp_deflator_lag+factor(decade),
                      data = kunprime)
### Udregner F
IV_pmsurv3_f <- IV_pmsurv3$diagnostic_first_stage_fstatistic
IV_pmsurv3_f$stars <- ifelse(IV_pmsurv3_f[4]<0.01,"***",
                             ifelse(IV_pmsurv3_f[4]<0.05,"**",
                                    ifelse(IV_pmsurv3_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_pmsurv3_Wu <- IV_pmsurv3$diagnostic_endogeneity_test
IV_pmsurv3_Wu$stars <- ifelse(IV_pmsurv3_Wu[4]<0.01,"***",
                              ifelse(IV_pmsurv3_Wu[4]<0.05,"**",
                                     ifelse(IV_pmsurv3_Wu[4]<0.1,"*","")))



###################  IV VOTES  ###############################

## Nummer 1
IV_votes1 <- iv_robust(vote_next ~ strat_lag04 |
                         tav_disspm,
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes1)

star_IV_votes1 <- lm(vote_next ~ strat_lag04, data = kunprime)

## Udregner F
IV_votes1_f <- IV_votes1$diagnostic_first_stage_fstatistic
IV_votes1_f$stars <- ifelse(IV_votes1_f[4]<0.01,"***",
                            ifelse(IV_votes1_f[4]<0.05,"**",
                                   ifelse(IV_votes1_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes1_Wu <- IV_votes1$diagnostic_endogeneity_test
IV_votes1_Wu$stars <- ifelse(IV_votes1_Wu[4]<0.01,"***",
                             ifelse(IV_votes1_Wu[4]<0.05,"**",
                                    ifelse(IV_votes1_Wu[4]<0.1,"*","")))


## Nummer 2
IV_votes2 <- iv_robust(vote_next ~ strat_lag04+
                         factor(decade) |
                         tav_disspm+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes2)

star_IV_votes2 <- lm(vote_next ~ strat_lag04+factor(decade), data = kunprime)

## Udregner F
IV_votes2_f <- IV_votes2$diagnostic_first_stage_fstatistic
IV_votes2_f$stars <- ifelse(IV_votes2_f[4]<0.01,"***",
                            ifelse(IV_votes2_f[4]<0.05,"**",
                                   ifelse(IV_votes2_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes2_Wu <- IV_votes2$diagnostic_endogeneity_test
IV_votes2_Wu$stars <- ifelse(IV_votes2_Wu[4]<0.01,"***",
                             ifelse(IV_votes2_Wu[4]<0.05,"**",
                                    ifelse(IV_votes2_Wu[4]<0.1,"*","")))


## Nummer 3
IV_votes3 <- iv_robust(vote_next ~ strat_lag04+
                         vote_share+
                         factor(decade) |
                         tav_disspm+
                         vote_share+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes3)

star_IV_votes3 <- lm(vote_next ~ strat_lag04+vote_share+factor(decade), data = kunprime)

## Udregner F
IV_votes3_f <- IV_votes3$diagnostic_first_stage_fstatistic
IV_votes3_f$stars <- ifelse(IV_votes3_f[4]<0.01,"***",
                            ifelse(IV_votes3_f[4]<0.05,"**",
                                   ifelse(IV_votes3_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes3_Wu <- IV_votes3$diagnostic_endogeneity_test
IV_votes3_Wu$stars <- ifelse(IV_votes3_Wu[4]<0.01,"***",
                             ifelse(IV_votes3_Wu[4]<0.05,"**",
                                    ifelse(IV_votes3_Wu[4]<0.1,"*","")))

## Nummer 4
IV_votes4 <- iv_robust(vote_next ~ strat_lag04+
                         vote_share+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade) |
                         tav_disspm+
                         vote_share+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes4)

star_IV_votes4 <- lm(vote_next ~ strat_lag04+vote_share+
                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                       factor(decade), data = kunprime)

## Udregner F
IV_votes4_f <- IV_votes4$diagnostic_first_stage_fstatistic
IV_votes4_f$stars <- ifelse(IV_votes4_f[4]<0.01,"***",
                            ifelse(IV_votes4_f[4]<0.05,"**",
                                   ifelse(IV_votes4_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes4_Wu <- IV_votes4$diagnostic_endogeneity_test
IV_votes4_Wu$stars <- ifelse(IV_votes4_Wu[4]<0.01,"***",
                             ifelse(IV_votes4_Wu[4]<0.05,"**",
                                    ifelse(IV_votes4_Wu[4]<0.1,"*","")))


################### IV MANDATER #####################################

## Nummer 1
IV_seats1 <- iv_robust(seats_next ~ strat_lag04 |
                         tav_disspm,
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats1)

star_IV_seats1 <- lm(seats_next ~ strat_lag04, data = kunprime)

## Udregner F
IV_seats1_f <- IV_seats1$diagnostic_first_stage_fstatistic
IV_seats1_f$stars <- ifelse(IV_seats1_f[4]<0.01,"***",
                            ifelse(IV_seats1_f[4]<0.05,"**",
                                   ifelse(IV_seats1_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats1_Wu <- IV_seats1$diagnostic_endogeneity_test
IV_seats1_Wu$stars <- ifelse(IV_seats1_Wu[4]<0.01,"***",
                             ifelse(IV_seats1_Wu[4]<0.05,"**",
                                    ifelse(IV_seats1_Wu[4]<0.1,"*","")))

## Nummer 2
IV_seats2 <- iv_robust(seats_next ~ strat_lag04+
                         factor(decade) |
                         tav_disspm+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats2)

star_IV_seats2 <- lm(seats_next ~ strat_lag04+factor(decade), data = kunprime)

## Udregner F
IV_seats2_f <- IV_seats2$diagnostic_first_stage_fstatistic
IV_seats2_f$stars <- ifelse(IV_seats2_f[4]<0.01,"***",
                            ifelse(IV_seats2_f[4]<0.05,"**",
                                   ifelse(IV_seats2_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats2_Wu <- IV_seats2$diagnostic_endogeneity_test
IV_seats2_Wu$stars <- ifelse(IV_seats2_Wu[4]<0.01,"***",
                             ifelse(IV_seats2_Wu[4]<0.05,"**",
                                    ifelse(IV_seats2_Wu[4]<0.1,"*","")))


## Nummer 3
IV_seats3 <- iv_robust(seats_next ~ strat_lag04+seats_andel+
                         factor(decade) |
                         tav_disspm+seats_andel+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats3)

star_IV_seats3 <- lm(seats_next ~ strat_lag04+seats_andel+factor(decade),
                     data = kunprime)

## Udregner F
IV_seats3_f <- IV_seats3$diagnostic_first_stage_fstatistic
IV_seats3_f$stars <- ifelse(IV_seats3_f[4]<0.01,"***",
                            ifelse(IV_seats3_f[4]<0.05,"**",
                                   ifelse(IV_seats3_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats3_Wu <- IV_seats3$diagnostic_endogeneity_test
IV_seats3_Wu$stars <- ifelse(IV_seats3_Wu[4]<0.01,"***",
                             ifelse(IV_seats3_Wu[4]<0.05,"**",
                                    ifelse(IV_seats3_Wu[4]<0.1,"*","")))


## Nummer 4
IV_seats4 <- iv_robust(seats_next ~ strat_lag04+seats_andel+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade) |
                         tav_disspm+seats_andel+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats4)

star_IV_seats4 <- lm(seats_next ~ strat_lag04+seats_andel+gdp_growth_lag_next+
                       inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)

## Udregner F
IV_seats4_f <- IV_seats4$diagnostic_first_stage_fstatistic
IV_seats4_f$stars <- ifelse(IV_seats4_f[4]<0.01,"***",
                            ifelse(IV_seats4_f[4]<0.05,"**",
                                   ifelse(IV_seats4_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats4_Wu <- IV_seats4$diagnostic_endogeneity_test
IV_seats4_Wu$stars <- ifelse(IV_seats4_Wu[4]<0.01,"***",
                             ifelse(IV_seats4_Wu[4]<0.05,"**",
                                    ifelse(IV_seats4_Wu[4]<0.1,"*","")))



####################################################################################
#####################                                  #############################
#####################     STARGAZER IV-MODELLER        #############################
#####################                                  #############################
####################################################################################


################################## PM SURVIVE #####################################

write_clip(
  stargazer(star_IV_pmsurv1, star_IV_pmsurv2, star_IV_pmsurv3,
            # Indsætter koefficienter
            coef = list(IV_pmsurv1$coefficients, IV_pmsurv2$coefficients,
                        IV_pmsurv3$coefficients),
            # Standardfejl
            se = list(IV_pmsurv1$std.error, IV_pmsurv2$std.error,
                      IV_pmsurv3$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Nej", "Ja"), omit.stat = c("ser", "f", "rsq", "adj.rsq"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "premierministerparti bevarer posten",
            column.labels = "(IV: strategisk04 valg = PM opløsningsmagt)",
            column.separate = 4,
            
            add.lines = list(c("Fist stage F-test",
                               paste0(round(IV_pmsurv1_f$value,3), IV_pmsurv1_f$stars),
                               paste0(round(IV_pmsurv2_f$value,3), IV_pmsurv2_f$stars),
                               paste0(round(IV_pmsurv3_f$value,3), IV_pmsurv3_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", 
                               paste0(round(IV_pmsurv1_Wu$p.value,3), IV_pmsurv1_Wu$stars),
                               paste0(round(IV_pmsurv2_Wu$p.value,3), IV_pmsurv2_Wu$stars),
                               paste0(round(IV_pmsurv3_Wu$p.value,3), IV_pmsurv3_Wu$stars))),
            
            covariate.labels = c("strategisk04 timet valg", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            notes = c("Lande-clustered robuste standardfejl i parentes.", "Antal lande: 35"), notes.align = "l",
            
            type = "html", style = "commadefault")
)


################################## STEMMEANDEL #####################################

write_clip(
  stargazer(star_IV_votes1, star_IV_votes2, star_IV_votes3, star_IV_votes4,
            # Indsætter koefficienter
            coef = list(IV_votes1$coefficients, IV_votes2$coefficients,
                        IV_votes3$coefficients, IV_votes4$coefficients),
            # Standardfejl
            se = list(IV_votes1$std.error, IV_votes2$std.error,
                      IV_votes3$std.error, IV_votes4$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Nej", "Ja"), omit.stat = c("ser", "f", "rsq", "adj.rsq"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "Stemmeandel",
            column.labels = "(IV: strategisk04 valg = PM opløsningsmagt)",
            column.separate = 5,
            
            add.lines = list(c("Fist stage F-test",
                               paste0(round(IV_votes1_f$value,3), IV_votes1_f$stars),
                               paste0(round(IV_votes2_f$value,3), IV_votes2_f$stars),
                               paste0(round(IV_votes3_f$value,3), IV_votes3_f$stars),
                               paste0(round(IV_votes4_f$value,3), IV_votes4_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", 
                               paste0(round(IV_votes1_Wu$p.value,3), IV_votes1_Wu$stars),
                               paste0(round(IV_votes2_Wu$p.value,3), IV_votes2_Wu$stars),
                               paste0(round(IV_votes3_Wu$p.value,3), IV_votes3_Wu$stars),
                               paste0(round(IV_votes4_Wu$p.value,3), IV_votes4_Wu$stars))),
            
            covariate.labels = c("strategisk04 timet valg", "Nuværende stemmeandel",
                                 "BNP-vækst (1 år lag)", "Inflation (1 år lag)", 
                                 "Konstant"),
            notes = c("Lande-clustered robuste standardfejl i parentes.", "Antal lande: 35"), 
            notes.align = "l",
            
            type = "html", style = "commadefault")
)


################################## MANDATANDEL #####################################

write_clip(
  stargazer(star_IV_seats1, star_IV_seats2, star_IV_seats3, star_IV_seats4,
            # Indsætter koefficienter
            coef = list(IV_seats1$coefficients, IV_seats2$coefficients,
                        IV_seats3$coefficients, IV_seats4$coefficients),
            # Standardfejl
            se = list(IV_seats1$std.error, IV_seats2$std.error,
                      IV_seats3$std.error, IV_seats4$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Nej", "Ja"), omit.stat = c("ser", "f", "rsq", "adj.rsq"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "Andel af mandater",
            column.labels = "(IV: strategisk04 valg = PM opløsningsmagt)",
            column.separate = 5,
            
            add.lines = list(c("Fist stage F-test",
                               paste0(round(IV_seats1_f$value,3), IV_seats1_f$stars),
                               paste0(round(IV_seats2_f$value,3), IV_seats2_f$stars),
                               paste0(round(IV_seats3_f$value,3), IV_seats3_f$stars),
                               paste0(round(IV_seats4_f$value,3), IV_seats4_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", 
                               paste0(round(IV_seats1_Wu$p.value,3), IV_seats1_Wu$stars),
                               paste0(round(IV_seats2_Wu$p.value,3), IV_seats2_Wu$stars),
                               paste0(round(IV_seats3_Wu$p.value,3), IV_seats3_Wu$stars),
                               paste0(round(IV_seats4_Wu$p.value,3), IV_seats4_Wu$stars))),
            
            covariate.labels = c("strategisk04 timet valg", "Nuværende mandantandel",
                                 "BNP-vækst (1 år lag)", "Inflation (1 år lag)", 
                                 "Konstant"),
            notes = c("Lande-clustered robuste standardfejl i parentes.", "Antal lande: 35"), 
            notes.align = "l",
            
            type = "html", style = "commadefault")
)

####################################################################################
#####################                                  #############################
#####################  TEST AF EKSKLUSIONSKRITERIET    #############################
#####################                                  #############################
####################################################################################
rm(list=ls()[! ls() %in% c("kunprime","moveme")])

################### FOR PMSURV ###################################################

## First stage
eksklu_first_st_pmsurv_logit <- glm.cluster(strategisk04 ~ tav_disspm+
                                              gdp_growth_lag+inflation_gdp_deflator_lag+
                                              factor(decade),
                                            data = subset(kunprime, !(is.na(kunprime$PMsurvive))), 
                                            family = "binomial",
                                            cluster = "country_id")
summary(eksklu_first_st_pmsurv_logit)
##Standardfejl
eksklu_first_st_pmsurv_logit_se <- sqrt(diag(eksklu_first_st_pmsurv_logit$vcov))

## Til stargazer
eksklu_first_st_pmsurv_marginal <- glm(strategisk04 ~ tav_disspm+gdp_growth_lag+
                                         inflation_gdp_deflator_lag+factor(decade),
                                       data = subset(kunprime, !(is.na(kunprime$PMsurvive))), 
                                       family = "binomial")

## Margins
AME_eksklu_first_st_pmsurv <- summary(margins(eksklu_first_st_pmsurv_marginal))
AME_eksklu_first_st_pmsurv



eksklu_first_st_pmsurv_ols <- lm_robust(strategisk04 ~ tav_disspm+
                                          gdp_growth_lag+inflation_gdp_deflator_lag+
                                          factor(decade), data = subset(kunprime, !(is.na(kunprime$PMsurvive))),
                                        clusters = country_id, se_type = "stata")
summary(eksklu_first_st_pmsurv_ols)
## star
star_eksklu_first_st_pmsurv_ols <- lm(strategisk04 ~ tav_disspm+gdp_growth_lag+
                                        inflation_gdp_deflator_lag+factor(decade),
                                      data = subset(kunprime, !(is.na(kunprime$PMsurvive))))

######### Second stage

eksklu_sencond_st_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk04+tav_disspm+
                                                gdp_growth_lag+inflation_gdp_deflator_lag+
                                                factor(decade), data = kunprime,
                                              family = "binomial", cluster = "country_id")
summary(eksklu_sencond_st_pmsurv_logit)
eksklu_sencond_st_pmsurv_logit_se <- sqrt(diag(eksklu_sencond_st_pmsurv_logit$vcov))
## Til stargazer
eksklu_second_st_pmsurv_marginal <- glm(PMsurvive ~ strategisk04+tav_disspm+
                                          gdp_growth_lag+inflation_gdp_deflator_lag+
                                          factor(decade), data = kunprime, family = "binomial")
AME_eksklu_second_st_pmsurv <- summary(margins(eksklu_second_st_pmsurv_marginal))
AME_eksklu_second_st_pmsurv


eksklu_second_st_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk04+tav_disspm+
                                           gdp_growth_lag+inflation_gdp_deflator_lag+
                                           factor(decade), data = kunprime,
                                         clusters = country_id, se_type = "stata")
summary(eksklu_second_st_pmsurv_ols)


################## FOR STEMMEANDEL ################################################
eksklu_first_st_votes_logit <- glm.cluster(strat_lag04 ~ tav_disspm+vote_share+
                                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                             factor(decade),
                                           data = subset(kunprime, !(is.na(kunprime$vote_next))), family = "binomial",
                                           cluster = "country_id")
summary(eksklu_first_st_votes_logit)
eksklu_first_st_votes_logit_se <- sqrt(diag(eksklu_first_st_votes_logit$vcov))
## Til star
eksklu_first_st_votes_marginal <- glm(strat_lag04 ~ tav_disspm+vote_share+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade),
                                      data = subset(kunprime, !(is.na(kunprime$vote_next))), family = "binomial")
AME_eksklu_first_st_votes <- summary(margins(eksklu_first_st_votes_marginal))
AME_eksklu_first_st_votes

## OLS
eksklu_first_st_votes_ols <- lm_robust(strat_lag04 ~ tav_disspm+vote_share+
                                         gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                         factor(decade), data = subset(kunprime, !(is.na(kunprime$vote_next))),
                                       clusters = country_id, se_type = "stata")
summary(eksklu_first_st_votes_ols)
## Til stargazer
star_eksklu_first_st_votes_ols <- lm(strat_lag04 ~ tav_disspm+vote_share+
                                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                       factor(decade), data = subset(kunprime, !(is.na(kunprime$vote_next))))


eksklu_second_st_votes_ols <- lm_robust(vote_next ~ strat_lag04+tav_disspm+vote_share+
                                          gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                          factor(decade), data = kunprime,
                                        clusters = country_id, se_type = "stata")
summary(eksklu_second_st_votes_ols)
## Til stargazer
star_eksklu_second_st_votes_ols <- lm(vote_next ~ strat_lag04+tav_disspm+vote_share+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade), data = kunprime)


################ FOR MANDATANDEL ################################################

eksklu_first_st_seats_logit <- glm.cluster(strat_lag04 ~ tav_disspm+seats_andel+
                                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                             factor(decade),
                                           data = subset(kunprime, !(is.na(kunprime$seats_next))), family = "binomial",
                                           cluster = "country_id")
summary(eksklu_first_st_seats_logit)
eksklu_first_st_seats_logit_se <- sqrt(diag(eksklu_first_st_seats_logit$vcov))
## Til star
eksklu_first_st_seats_marginal <- glm(strat_lag04 ~ tav_disspm+seats_andel+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade),
                                      data = subset(kunprime, !(is.na(kunprime$seats_next))), family = "binomial")
AME_eksklu_first_st_seats <- summary(margins(eksklu_first_st_seats_marginal))
AME_eksklu_first_st_seats


eksklu_first_st_seats_ols <- lm_robust(strat_lag04 ~ tav_disspm+seats_andel+
                                         gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                         factor(decade), data = subset(kunprime, !(is.na(kunprime$seats_next))),
                                       clusters = country_id, se_type = "stata")
summary(eksklu_first_st_seats_ols)
star_eksklu_first_st_seats_ols <- lm(strat_lag04 ~ tav_disspm+seats_andel+
                                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                       factor(decade), data = subset(kunprime, !(is.na(kunprime$seats_next))))

eksklu_second_st_seats_ols <- lm_robust(seats_next ~ strat_lag04+tav_disspm+seats_andel+
                                          gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                          factor(decade), data = kunprime,
                                        clusters = country_id, se_type = "stata")
summary(eksklu_second_st_seats_ols)
star_eksklu_second_st_seats_ols <- lm(seats_next ~ strat_lag04+tav_disspm+seats_andel+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade), data = kunprime)



#######################################################################################
##################                                        #############################
##################  STARGAZER FOR EKSKLUSIONSKRITERIET    #############################
##################                                        #############################
#######################################################################################

write_clip(
  stargazer(eksklu_first_st_pmsurv_marginal, eksklu_second_st_pmsurv_marginal,
            eksklu_first_st_votes_marginal, star_eksklu_first_st_votes_ols,
            star_eksklu_second_st_votes_ols,
            eksklu_first_st_seats_marginal, star_eksklu_first_st_seats_ols,
            star_eksklu_second_st_seats_ols,
            
            se = list(eksklu_first_st_pmsurv_logit_se, eksklu_sencond_st_pmsurv_logit_se,
                      eksklu_first_st_votes_logit_se, eksklu_first_st_votes_ols$std.error,
                      eksklu_second_st_votes_ols$std.error,
                      eksklu_first_st_seats_logit_se, eksklu_first_st_seats_ols$std.error,
                      eksklu_second_st_seats_ols$std.error),
            
            order = c("strategisk04", "strat_lag04", "tav_disspm",
                      "vote_share", "seats_andel"),
            covariate.labels = c("strategisk04 timet valg", "strategisk04 timet valg",
                                 "PM opløsningsmagt", "Nuværende stemmeandel",
                                 "Nuværende mandatandel", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "BNP-vækst (1 år lag)",
                                 "Inlation (1 år lag)", "Konstant"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = c("strategisk04 valg", "PM overlever",
                               "strategisk04 valg", "Stemmeandel",
                               "strategisk04 valg", "Mandatandel"),
            column.labels = c("premierministerparti bevarer posten",
                              "Stemmeandel", "Andel af mandater"),
            column.separate = c(2,3,3),
            
            table.layout = "=!lcd-m#-t-o-s=!n",
            
            omit = "decade", omit.labels = "Årti-dummies", 
            omit.yes.no = c("Ja", "Nej"),
            omit.stat = c("ser", "f"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "PM: premierminister"),
            notes.align = "l",
            
            type = "html", style = "commadefault")
)



#######################################################################################
##################                                        #############################
##################    ROBUSTHEDSTEST OLS OG EKSTRA IVs    #############################
##################                                        #############################
#######################################################################################

rm(list=ls()[! ls() %in% c("kunprime","moveme")])

#### Laver modeller med ekstra kontrol variable ##########

#### Lave kort deskriptiv statistik af dem, så de ikke forpester den
## første tabel med deskriptiv statistik.

# 1: valgsystem
# 2: regeringspartier/karakteristika majoritet

## OBS som bilagstabel skal regeringskarakteristika køres med de to variable hver 
## for sig, da der givet vis er høj grad af multikollinearitet mellem dem

# 3: valgperiode

## Har tre kontroller, valgsystem, antal partier i regering og længde af valgperioden.


#################   premierministerPARTIET OVERLEVER    #########################

valgsystem_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk04+valgsystem_majo+
                                         gdp_growth_lag+inflation_gdp_deflator_lag+
                                         factor(decade),
                                       data = kunprime, cluster = "country_id",
                                       family = "binomial")
summary(valgsystem_pmsurv_logit)
valgsystem_pmsurv_logit_se <- sqrt(diag(valgsystem_pmsurv_logit$vcov))
## margins
valgsystem_pmsurv_margianl <- glm(PMsurvive ~ strategisk04+valgsystem_majo+
                                    gdp_growth_lag+inflation_gdp_deflator_lag+
                                    factor(decade),
                                  data = kunprime, family = "binomial")
AME_valgsystem_pmsurv <- summary(margins(valgsystem_pmsurv_margianl))
AME_valgsystem_pmsurv


## OLS
valgsystem_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk04+valgsystem_majo+
                                     gdp_growth_lag+inflation_gdp_deflator_lag+
                                     factor(decade),
                                   data = kunprime, clusters = country_id,
                                   se_type = "stata")
summary(valgsystem_pmsurv_ols)
star_valgsystem_pmsurv_ols <- lm(PMsurvive ~ strategisk04+valgsystem_majo+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade), data = kunprime)


### Valgsystem IV
IV_valgsystem_pmsurv <- iv_robust(PMsurvive ~ strategisk04+valgsystem_majo+
                                    gdp_growth_lag+inflation_gdp_deflator_lag+
                                    factor(decade) |
                                    tav_disspm+valgsystem_majo+
                                    gdp_growth_lag+inflation_gdp_deflator_lag+
                                    factor(decade),
                                  data = kunprime, clusters = country_id,
                                  se_type = "stata", diagnostics = TRUE)
summary(IV_valgsystem_pmsurv)
star_valgsystem_pmsurv_IV <- ivreg(PMsurvive ~ strategisk04+valgsystem_majo+
                                     gdp_growth_lag+inflation_gdp_deflator_lag+
                                     factor(decade) |
                                     tav_disspm+valgsystem_majo+
                                     gdp_growth_lag+inflation_gdp_deflator_lag+
                                     factor(decade), data = kunprime)
### Udregner F
IV_valgsystem_pmsurv_f <- IV_valgsystem_pmsurv$diagnostic_first_stage_fstatistic
IV_valgsystem_pmsurv_f$stars <- ifelse(IV_valgsystem_pmsurv_f[4]<0.01,"***",
                                       ifelse(IV_valgsystem_pmsurv_f[4]<0.05,"**",
                                              ifelse(IV_valgsystem_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valgsystem_pmsurv_Wu <- IV_valgsystem_pmsurv$diagnostic_endogeneity_test
IV_valgsystem_pmsurv_Wu$stars <- ifelse(IV_valgsystem_pmsurv_Wu[4]<0.01,"***",
                                        ifelse(IV_valgsystem_pmsurv_Wu[4]<0.05,"**",
                                               ifelse(IV_valgsystem_pmsurv_Wu[4]<0.1,"*","")))



######################### Regeringskarateristika logit
regering_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk04+regeringspartier+majoritet+
                                       gdp_growth_lag+inflation_gdp_deflator_lag+
                                       factor(decade),
                                     data = kunprime, cluster = "country_id",
                                     family = "binomial")
summary(regering_pmsurv_logit)
regering_pmsurv_logit_se <- sqrt(diag(regering_pmsurv_logit$vcov))
##Margins
regering_pmsurv_marginal <- glm(PMsurvive ~ strategisk04+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade), data = kunprime, family = "binomial")
AME_regering_pmsurv <- summary(margins(regering_pmsurv_marginal))
AME_regering_pmsurv

## OLS
regering_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk04+regeringspartier+majoritet+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata")
summary(regering_pmsurv_ols)
star_regering_pmsurv_ols <- lm(PMsurvive ~ strategisk04+regeringspartier+majoritet+
                                 gdp_growth_lag+inflation_gdp_deflator_lag+
                                 factor(decade), data = kunprime)


### IV regering
IV_regering_pmsurv <- iv_robust(PMsurvive ~ strategisk04+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade) |
                                  tav_disspm+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata", diagnostics = TRUE)
summary(IV_regering_pmsurv)
star_regering_pmsurv_IV <- ivreg(PMsurvive ~ strategisk04+regeringspartier+majoritet+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade) |
                                   tav_disspm+regeringspartier+majoritet+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade), data = kunprime)
### Udregner F
IV_regering_pmsurv_f <- IV_regering_pmsurv$diagnostic_first_stage_fstatistic
IV_regering_pmsurv_f$stars <- ifelse(IV_regering_pmsurv_f[4]<0.01,"***",
                                     ifelse(IV_regering_pmsurv_f[4]<0.05,"**",
                                            ifelse(IV_regering_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_regering_pmsurv_Wu <- IV_regering_pmsurv$diagnostic_endogeneity_test
IV_regering_pmsurv_Wu$stars <- ifelse(IV_regering_pmsurv_Wu[4]<0.01,"***",
                                      ifelse(IV_regering_pmsurv_Wu[4]<0.05,"**",
                                             ifelse(IV_regering_pmsurv_Wu[4]<0.1,"*","")))


## 3 års periode logit
valg3aar_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk04+tre_aar+
                                       gdp_growth_lag+inflation_gdp_deflator_lag+
                                       factor(decade),
                                     data = kunprime, cluster = "country_id",
                                     family = "binomial")
summary(valg3aar_pmsurv_logit)
valg3aar_pmsurv_logit_se <- sqrt(diag(valg3aar_pmsurv_logit$vcov))
## Margins
valg3aar_pmsurv_marginal <- glm(PMsurvive ~ strategisk04+tre_aar+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade), data = kunprime, family = "binomial")
AME_valg3aar_pmsurv <- summary(margins(valg3aar_pmsurv_marginal))
AME_valg3aar_pmsurv


## OLS
valg3aar_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk04+tre_aar+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata")
summary(valg3aar_pmsurv_ols)
star_valg3aar_pmsurv_ols <- lm(PMsurvive ~ strategisk04+tre_aar+
                                 gdp_growth_lag+inflation_gdp_deflator_lag+
                                 factor(decade), data = kunprime)

## IV 3 år valgperiode
IV_valg3aar_pmsurv <- iv_robust(PMsurvive ~ strategisk04+tre_aar+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade) |
                                  tav_disspm+tre_aar+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata", diagnostics = TRUE)
summary(IV_valg3aar_pmsurv)
star_valg3aar_pmsurv_IV <- ivreg(PMsurvive ~ strategisk04+tre_aar+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade) |
                                   tav_disspm+tre_aar+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade), data = kunprime)
### Udregner F
IV_valg3aar_pmsurv_f <- IV_valg3aar_pmsurv$diagnostic_first_stage_fstatistic
IV_valg3aar_pmsurv_f$stars <- ifelse(IV_valg3aar_pmsurv_f[4]<0.01,"***",
                                     ifelse(IV_valg3aar_pmsurv_f[4]<0.05,"**",
                                            ifelse(IV_valg3aar_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valg3aar_pmsurv_Wu <- IV_valg3aar_pmsurv$diagnostic_endogeneity_test
IV_valg3aar_pmsurv_Wu$stars <- ifelse(IV_valg3aar_pmsurv_Wu[4]<0.01,"***",
                                      ifelse(IV_valg3aar_pmsurv_Wu[4]<0.05,"**",
                                             ifelse(IV_valg3aar_pmsurv_Wu[4]<0.1,"*","")))

AME_valg3aar_pmsurv

########################   FOR STEMMEANDEL   ####################################


## Valgsystem
valgsystem_vote_ols <- lm_robust(vote_next ~ strat_lag04+vote_share+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata")
summary(valgsystem_vote_ols)
star_valgsystem_vote_ols <- lm(vote_next ~ strat_lag04+vote_share+valgsystem_majo+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade), data = kunprime)


## IV
IV_valgsystem_vote <- iv_robust(vote_next ~ strat_lag04+vote_share+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade) |
                                  tav_disspm+vote_share+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata", diagnostics = TRUE)
summary(IV_valgsystem_vote)
star_valgsystem_vote_IV <- ivreg(vote_next ~ strat_lag04+vote_share+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade) |
                                   tav_disspm+vote_share+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade), data = kunprime)
### Udregner F
IV_valgsystem_vote_f <- IV_valgsystem_vote$diagnostic_first_stage_fstatistic
IV_valgsystem_vote_f$stars <- ifelse(IV_valgsystem_vote_f[4]<0.01,"***",
                                     ifelse(IV_valgsystem_vote_f[4]<0.05,"**",
                                            ifelse(IV_valgsystem_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valgsystem_vote_Wu <- IV_valgsystem_vote$diagnostic_endogeneity_test
IV_valgsystem_vote_Wu$stars <- ifelse(IV_valgsystem_vote_Wu[4]<0.01,"***",
                                      ifelse(IV_valgsystem_vote_Wu[4]<0.05,"**",
                                             ifelse(IV_valgsystem_vote_Wu[4]<0.1,"*","")))



#### Regeringskarakteristika
regering_vote_ols <- lm_robust(vote_next ~ strat_lag04+vote_share+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata")
summary(regering_vote_ols)
star_regering_vote_ols <- lm(vote_next ~ strat_lag04+vote_share+regeringspartier+
                               majoritet+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade), data = kunprime)

## IV
IV_regering_vote <- iv_robust(vote_next ~ strat_lag04+vote_share+regeringspartier+
                                majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+vote_share+regeringspartier+
                                majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade),
                              data = kunprime, clusters = country_id,
                              se_type = "stata", diagnostics = TRUE)
summary(IV_regering_vote)
star_regering_vote_IV <- ivreg(vote_next ~ strat_lag04+vote_share+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade) |
                                 tav_disspm+vote_share+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade), data = kunprime)

### Udregner F
IV_regering_vote_f <- IV_regering_vote$diagnostic_first_stage_fstatistic
IV_regering_vote_f$stars <- ifelse(IV_regering_vote_f[4]<0.01,"***",
                                   ifelse(IV_regering_vote_f[4]<0.05,"**",
                                          ifelse(IV_regering_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_regering_vote_Wu <- IV_regering_vote$diagnostic_endogeneity_test
IV_regering_vote_Wu$stars <- ifelse(IV_regering_vote_Wu[4]<0.01,"***",
                                    ifelse(IV_regering_vote_Wu[4]<0.05,"**",
                                           ifelse(IV_regering_vote_Wu[4]<0.1,"*","")))





#### 3 års valgperiode 
valg3aar_vote_ols <- lm_robust(vote_next ~ strat_lag04+vote_share+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata")
summary(valg3aar_vote_ols)
star_valg3aar_vote_ols <- lm(vote_next ~ strat_lag04+vote_share+tre_aar+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade), data = kunprime)

## IV
IV_valg3aar_vote <- iv_robust(vote_next ~ strat_lag04+vote_share+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+vote_share+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade),
                              data = kunprime, clusters = country_id,
                              se_type = "stata", diagnostics = TRUE)
summary(IV_valg3aar_vote)
star_valg3aar_vote_IV <- ivreg(vote_next ~ strat_lag04+vote_share+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade) |
                                 tav_disspm+vote_share+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade), data = kunprime)
### Udregner F
IV_valg3aar_vote_f <- IV_valg3aar_vote$diagnostic_first_stage_fstatistic
IV_valg3aar_vote_f$stars <- ifelse(IV_valg3aar_vote_f[4]<0.01,"***",
                                   ifelse(IV_valg3aar_vote_f[4]<0.05,"**",
                                          ifelse(IV_valg3aar_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valg3aar_vote_Wu <- IV_valg3aar_vote$diagnostic_endogeneity_test
IV_valg3aar_vote_Wu$stars <- ifelse(IV_valg3aar_vote_Wu[4]<0.01,"***",
                                    ifelse(IV_valg3aar_vote_Wu[4]<0.05,"**",
                                           ifelse(IV_valg3aar_vote_Wu[4]<0.1,"*","")))




########################   FOR MANDATANDEL   ####################################



## Valgsystem
valgsystem_seats_ols <- lm_robust(seats_next ~ strat_lag04+seats_andel+valgsystem_majo+
                                    gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                    factor(decade),
                                  data = kunprime, clusters = country_id,
                                  se_type = "stata")
summary(valgsystem_seats_ols)
star_valgsystem_seats_ols <- lm(seats_next ~ strat_lag04+seats_andel+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade), data = kunprime)


## IV
IV_valgsystem_seats <- iv_robust(seats_next ~ strat_lag04+seats_andel+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade) |
                                   tav_disspm+seats_andel+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata", diagnostics = TRUE)
summary(IV_valgsystem_seats)
star_valgsystem_seats_IV <-  ivreg(seats_next ~ strat_lag04+seats_andel+valgsystem_majo+
                                     gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                     factor(decade) |
                                     tav_disspm+seats_andel+valgsystem_majo+
                                     gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                     factor(decade), data = kunprime)
### Udregner F
IV_valgsystem_seats_f <- IV_valgsystem_seats$diagnostic_first_stage_fstatistic
IV_valgsystem_seats_f$stars <- ifelse(IV_valgsystem_seats_f[4]<0.01,"***",
                                      ifelse(IV_valgsystem_seats_f[4]<0.05,"**",
                                             ifelse(IV_valgsystem_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valgsystem_seats_Wu <- IV_valgsystem_seats$diagnostic_endogeneity_test
IV_valgsystem_seats_Wu$stars <- ifelse(IV_valgsystem_seats_Wu[4]<0.01,"***",
                                       ifelse(IV_valgsystem_seats_Wu[4]<0.05,"**",
                                              ifelse(IV_valgsystem_seats_Wu[4]<0.1,"*","")))


#### Regeringskarakteristika
regering_seats_ols <- lm_robust(seats_next ~ strat_lag04+seats_andel+regeringspartier+
                                  majoritet+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata")
summary(regering_seats_ols)
star_regering_seats_ols <- lm(seats_next ~ strat_lag04+seats_andel+regeringspartier+majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade), data = kunprime)

## IV
IV_regering_seats <- iv_robust(seats_next ~ strat_lag04+seats_andel+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade) |
                                 tav_disspm+seats_andel+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata", diagnostics = TRUE)
summary(IV_regering_seats)
star_regering_seats_IV <- ivreg(seats_next ~ strat_lag04+seats_andel+regeringspartier+majoritet+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade) |
                                  tav_disspm+seats_andel+regeringspartier+majoritet+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade), data = kunprime)

### Udregner F
IV_regering_seats_f <- IV_regering_seats$diagnostic_first_stage_fstatistic
IV_regering_seats_f$stars <- ifelse(IV_regering_seats_f[4]<0.01,"***",
                                    ifelse(IV_regering_seats_f[4]<0.05,"**",
                                           ifelse(IV_regering_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_regering_seats_Wu <- IV_regering_seats$diagnostic_endogeneity_test
IV_regering_seats_Wu$stars <- ifelse(IV_regering_seats_Wu[4]<0.01,"***",
                                     ifelse(IV_regering_seats_Wu[4]<0.05,"**",
                                            ifelse(IV_regering_seats_Wu[4]<0.1,"*","")))



#### 3 års valgperiode 
valg3aar_seats_ols <- lm_robust(seats_next ~ strat_lag04+seats_andel+tre_aar+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata")
summary(valg3aar_seats_ols)
star_valg3aar_seats_ols <- lm(seats_next ~ strat_lag04+seats_andel+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade), data = kunprime)

## IV
IV_valg3aar_seats <- iv_robust(seats_next ~ strat_lag04+seats_andel+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade) |
                                 tav_disspm+seats_andel+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata", diagnostics = TRUE)
summary(IV_valg3aar_seats)
star_valg3aar_seats_IV <- ivreg(seats_next ~ strat_lag04+seats_andel+tre_aar+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade) |
                                  tav_disspm+seats_andel+tre_aar+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade), data = kunprime)
### Udregner F
IV_valg3aar_seats_f <- IV_valg3aar_seats$diagnostic_first_stage_fstatistic
IV_valg3aar_seats_f$stars <- ifelse(IV_valg3aar_seats_f[4]<0.01,"***",
                                    ifelse(IV_valg3aar_seats_f[4]<0.05,"**",
                                           ifelse(IV_valg3aar_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valg3aar_seats_Wu <- IV_valg3aar_seats$diagnostic_endogeneity_test
IV_valg3aar_seats_Wu$stars <- ifelse(IV_valg3aar_seats_Wu[4]<0.01,"***",
                                     ifelse(IV_valg3aar_seats_Wu[4]<0.05,"**",
                                            ifelse(IV_valg3aar_seats_Wu[4]<0.1,"*","")))




#######################################################################################
##################                                        #############################
##################        STARGAZER EKSTRA OLS OG IV      #############################
##################                                        #############################
#######################################################################################


################### TABEL FOR PMSURVIVE ############################################

write_clip(
  stargazer(valgsystem_pmsurv_margianl, star_valgsystem_pmsurv_ols, star_valgsystem_pmsurv_IV,
            regering_pmsurv_marginal, star_regering_pmsurv_ols, star_regering_pmsurv_IV,
            valg3aar_pmsurv_marginal, star_valg3aar_pmsurv_ols, star_valg3aar_pmsurv_IV,
            
            coef = list(NULL, NULL, IV_valgsystem_pmsurv$coefficients,
                        NULL, NULL, IV_regering_pmsurv$coefficients,
                        NULL, NULL, IV_valg3aar_pmsurv$coefficients),
            se = list(valgsystem_pmsurv_logit_se, valgsystem_pmsurv_ols$std.error,
                      IV_valgsystem_pmsurv$std.error,
                      regering_pmsurv_logit_se, regering_pmsurv_ols$std.error,
                      IV_regering_pmsurv$std.error,
                      valg3aar_pmsurv_logit_se, valg3aar_pmsurv_ols$std.error,
                      IV_valg3aar_pmsurv$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "premierministerparti bevarer posten",
            column.labels = "(IV: strategisk04 valg = premierministerens opløsningsmagt)",
            column.separate = 9,
            
            table.layout = "=!ldc-m#-t-o-sa=!n",
            
            add.lines = list(c("First stage F-test",
                               "", "", 
                               paste0(round(IV_valgsystem_pmsurv_f$value,3), IV_valgsystem_pmsurv_f$stars),
                               "","",
                               paste0(round(IV_regering_pmsurv_f$value,3), IV_regering_pmsurv_f$stars),
                               "","",
                               paste0(round(IV_valg3aar_pmsurv_f$value,3), IV_valg3aar_pmsurv_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)",
                               "","",
                               paste0(round(IV_valgsystem_pmsurv_Wu$p.value,3), IV_valgsystem_pmsurv_Wu$stars),
                               "","",
                               paste0(round(IV_regering_pmsurv_Wu$p.value,3), IV_regering_pmsurv_Wu$stars),
                               "","",
                               paste0(round(IV_valg3aar_pmsurv_Wu$p.value,3), IV_valg3aar_pmsurv_Wu$stars))),
            covariate.labels = c("strategisk04 timet valg", "Flertalsvalg", "Antal regeringspartier",
                                 "Majoritetsregering", "3 års valgperiode", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "Koefficienterne for de logistiske regressioner vises i log-odds",
                      "Model 3, 6 og 9 anvender IV-estimation.",
                      "Antal lande: 35"),
            notes.align = "l",
            
            type = "html", style = "commadefault")
)


################### TABEL FOR STEMMEANDEL ############################################

write_clip(
  stargazer(star_valgsystem_vote_ols, star_valgsystem_vote_IV,
            star_regering_vote_ols, star_regering_vote_IV,
            star_valg3aar_vote_ols, star_valg3aar_vote_IV,
            
            coef = list(NULL, IV_valgsystem_vote$coefficients,
                        NULL, IV_regering_vote$coefficients,
                        NULL, IV_valg3aar_vote$coefficients),
            
            se = list(valgsystem_vote_ols$std.error, IV_valgsystem_vote$std.error,
                      regering_vote_ols$std.error, IV_regering_vote$std.error,
                      valg3aar_vote_ols$std.error, IV_valg3aar_vote$std.error),
            
            add.lines = list(c("First stage F-test", "", 
                               paste0(round(IV_valgsystem_vote_f$value,3), IV_valgsystem_vote_f$stars), "",
                               paste0(round(IV_regering_vote_f$value,3), IV_regering_vote_f$stars), "",
                               paste0(round(IV_valg3aar_vote_f$value,3), IV_valg3aar_vote_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", "",
                               paste0(round(IV_valgsystem_vote_Wu$p.value,3), IV_valgsystem_vote_Wu$stars), "",
                               paste0(round(IV_regering_vote_Wu$p.value,3), IV_regering_vote_Wu$stars), "",
                               paste0(round(IV_valg3aar_vote_Wu$p.value,3), IV_valg3aar_vote_Wu$stars))),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "Stemmeandel",
            column.labels = "(IV: strategisk04 valg = premierministerens opløsningsmagt)",
            column.separate = 6,
            
            covariate.labels = c("strategisk04 timet valg", "Nuværende stemmeandel",
                                 "Flertalsvalg", "Antal regeringspartier",
                                 "Majoritetsregering", "3 års valgperiode", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "Model 2, 4 og 6 anvender IV-estimation.",
                      "Antal lande: 35"), notes.align = "l",
            
            table.layout = "=!ldc-m#-t-o-sa=!n",
            type = "html", style = "commadefault")
)


###################### TABEL FOR MANDATANDEL ############################################

write_clip(
  stargazer(star_valgsystem_seats_ols, star_valgsystem_seats_IV,
            star_regering_seats_ols, star_regering_seats_IV,
            star_valg3aar_seats_ols, star_valg3aar_seats_IV,
            
            coef = list(NULL, IV_valgsystem_seats$coefficients,
                        NULL, IV_regering_seats$coefficients,
                        NULL, IV_valg3aar_seats$coefficients),
            
            se = list(valgsystem_seats_ols$std.error, IV_valgsystem_seats$std.error,
                      regering_seats_ols$std.error, IV_regering_seats$std.error,
                      valg3aar_seats_ols$std.error, IV_valg3aar_seats$std.error),
            
            add.lines = list(c("First stage F-test", "", 
                               paste0(round(IV_valgsystem_seats_f$value,3), IV_valgsystem_seats_f$stars), "",
                               paste0(round(IV_regering_seats_f$value,3), IV_regering_seats_f$stars), "",
                               paste0(round(IV_valg3aar_seats_f$value,3), IV_valg3aar_seats_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", "",
                               paste0(round(IV_valgsystem_seats_Wu$p.value,3), IV_valgsystem_seats_Wu$stars), "",
                               paste0(round(IV_regering_seats_Wu$p.value,3), IV_regering_seats_Wu$stars), "",
                               paste0(round(IV_valg3aar_seats_Wu$p.value,3), IV_valg3aar_seats_Wu$stars))),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "Andel af mandater",
            column.labels = "(IV: strategisk04 valg = premierministerens opløsningsmagt)",
            column.separate = 6,
            
            covariate.labels = c("strategisk04 timet valg", "Nuværende mandatandel",
                                 "Flertalsvalg", "Antal regeringspartier",
                                 "Majoritetsregering", "3 års valgperiode", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "Model 2, 4 og 6 anvender IV-estimation.",
                      "Antal lande: 35"), notes.align = "l",
            
            table.layout = "=!ldc-m#-t-o-sa=!n",
            type = "html", style = "commadefault")
)


###################### TABEL TIL BILAG FOR PMSURV AME OG OLS ######################################

write_clip(
  stargazer(valgsystem_pmsurv_margianl, star_valgsystem_pmsurv_ols,
            regering_pmsurv_marginal, star_regering_pmsurv_ols,
            valg3aar_pmsurv_marginal, star_valg3aar_pmsurv_ols,
            
            coef = list(AME_valgsystem_pmsurv$AME, NULL,
                        AME_regering_pmsurv$AME, NULL,
                        AME_valg3aar_pmsurv$AME, NULL),
            
            se = list(NA, valgsystem_pmsurv_ols$std.error,
                      NA, regering_pmsurv_ols$std.error,
                      NA, valg3aar_pmsurv_ols$std.error),
            p = list(AME_valgsystem_pmsurv$p, NULL,
                     AME_regering_pmsurv$p, NULL,
                     AME_valg3aar_pmsurv$p, NULL),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "premierministerparti bevarer posten",
            
            covariate.labels = c("strategisk04 timet valg", "Flertalsvalg", "Antal regeringspartier",
                                 "Majoritetsregering", "3 års valgperiode", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            omit = "decade", omit.labels = "Årti-dummies", 
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            notes = c("Viser de gennemsnitlige marginale effekter for",
                      "de logistiske regressioner estimeret i tabel X.",
                      "OLS regressioner er medtaget til sammenligning",
                      "Antal lande: 35"),
            notes.align = "l",
            
            type = "html", style = "commadefault")
)


########################################################################
############                                         ###################
############  KØRER ALLE MODELLER MED STRATEGISK 6   ###################
############                                         ###################
########################################################################


rm(list=ls()[! ls() %in% c("kunprime","moveme")])
#### PM Survive

PMsurv_ols1 <- lm_robust(PMsurvive~strategisk06,
                         data = kunprime,
                         clusters = country_id,
                         se_type = "stata")
summary(PMsurv_ols1)
## LM-objekt til stargazer
star_PMsurv_ols1 <-lm(PMsurvive~strategisk06, data = kunprime)


PMsurv_logit1 <- glm.cluster(PMsurvive ~ strategisk06,
                             data = kunprime,
                             cluster = "country_id",
                             family = "binomial")
summary(PMsurv_logit1)
# Gemmer standardfejl for logit 1
PMsurv_logit1_se <- sqrt(diag(PMsurv_logit1$vcov))

### Udregner Odds ratio
PMsurv_logit1_OR <- exp(cbind(coef(PMsurv_logit1), confint(PMsurv_logit1)))
PMsurv_logit1_OR

### Udregner margins for GLM modellen, margins-funktionen kan ikke spise
## glm.cluster objekter, så den må lige laves med en almindelige glm.
PMsurv_logit_marginal1 <- glm(PMsurvive ~ strategisk06,data = kunprime,family = "binomial")
summary(PMsurv_logit_marginal1)

AME_pmsurv <- summary(margins(PMsurv_logit_marginal1))
mean(AME_pmsurv$dydx_strategisk06)
### OLS og GLM (AME) - estimatet er næsten ens.

### Tiljøjer variable, Årti-dummies
PMsurv_ols2 <- lm_robust(PMsurvive ~ strategisk06+
                           factor(decade),
                         data = kunprime,
                         clusters = country_id,
                         se_type = "stata")
summary(PMsurv_ols2)
summary(PMsurv_ols2$N)
star_PMsurv_ols2 <- lm(PMsurvive ~ strategisk06+factor(decade),data = kunprime)

PMsurv_logit2 <- glm.cluster(PMsurvive ~ strategisk06+
                               factor(decade),
                             data = kunprime,
                             cluster = "country_id",
                             family = "binomial")
summary(PMsurv_logit2)
## Gemmer robuste standardfejl
PMsurv_logit2_se <- sqrt(diag(PMsurv_logit2$vcov))
## OR
PMsurv_OR_logit2 <- exp(coef(PMsurv_logit2))
PMsurv_OR_logit2

### Udregner margins for logit 2
PMsurv_logit_marginal2 <- glm(PMsurvive ~ strategisk06+factor(decade), data = kunprime, family = "binomial")
summary(PMsurv_logit_marginal2)
AME_pmsurv2 <- summary(margins(PMsurv_logit_marginal2))
AME_pmsurv2

### Tilføjer økonomi
PMsurv_ols3 <- lm_robust(PMsurvive ~ strategisk06+
                           gdp_growth_lag+
                           inflation_gdp_deflator_lag+
                           factor(decade),
                         data = kunprime,
                         clusters = country_id,
                         se_type = "stata")
summary(PMsurv_ols3)
summary(PMsurv_ols3$N)
## Går fra 361 til 306 (305 med inflation) observationer.
star_PMsurv_ols3 <- lm(PMsurvive ~ strategisk06+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),data = kunprime)



PMsurv_logit3 <- glm.cluster(PMsurvive ~ strategisk06+
                               gdp_growth_lag+
                               inflation_gdp_deflator_lag+
                               factor(decade),
                             data = kunprime,
                             cluster = "country_id",
                             family = "binomial")
summary(PMsurv_logit3)
## Gemmer robuste standardfejl
PMsurv_logit3_se <- sqrt(diag(PMsurv_logit3$vcov))
## OR
PMsurv_OR_logit3 <- exp(coef(PMsurv_logit3))
PMsurv_OR_logit3
## Margins for logit 3
PMsurv_logit_marginal3 <- glm(PMsurvive~strategisk06+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade), data = kunprime, family = "binomial")
summary(PMsurv_logit_marginal3)
AME_pmsurv3 <- summary(margins(PMsurv_logit_marginal3))
AME_pmsurv3



?stargazer

setwd("/Users/ethranholm/OneDrive/R/Speciale output")

#### Kører data ud i stargazer

write_clip(
  stargazer(star_PMsurv_ols1, PMsurv_logit_marginal1, 
            star_PMsurv_ols2, PMsurv_logit_marginal2,
            star_PMsurv_ols3, PMsurv_logit_marginal3,
            
            se = list(PMsurv_ols1$std.error, PMsurv_logit1_se,
                      PMsurv_ols2$std.error, PMsurv_logit2_se,
                      PMsurv_ols3$std.error, PMsurv_logit3_se),
            
            covariate.labels = c("strategisk06 timet valg", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            dep.var.caption = "Afhængig variabel:", dep.var.labels = "premierministerparti bevarer posten",
            
            omit = "decade", omit.labels ="Årti-dummies", omit.yes.no = c("Nej", "Ja"),
            notes = c("Lande-clustered robuste standardfejl i parentes.", "For de logistiske regressioner vises log-odds."),
            omit.stat = c("ser","f"),
            notes.align = "l",
            
            type = "html", style = "commadefault")
)




### Tabel med marginale effekter. OBS STANDARDFEJL TILFØJES MANUELT.

write_clip(
  stargazer(PMsurv_logit_marginal1, PMsurv_logit_marginal2, 
            PMsurv_logit_marginal3,
            
            coef = list(AME_pmsurv$AME, AME_pmsurv2$AME, AME_pmsurv3$AME),
            se = list(AME_pmsurv$p, AME_pmsurv2$p, AME_pmsurv3$p),
            
            keep = "strategisk06",
            omit = c("decade", "gdp_growth_lag"), 
            omit.labels = c("Årti-dummies", "Kontrol økonomi"),
            omit.yes.no = c("Nej", "Ja"),
            covariate.labels = "strategisk06 timet valg",
            dep.var.caption = "Afhængig variabel:", dep.var.labels = "premierministerparti bevarer posten",
            notes = "Viser de gennemsnitlige marginale effekter for de logistiske regressioner estimeret i tabel X",
            type = "html", style = "commadefault",
            notes.align = "l")
)



#################################################################################
#######################                               ###########################
######################  OLS for stemmer og mandater   ###########################
######################                                ###########################
#################################################################################



########## Stemmer ##########
vote_ols1 <- lm_robust(vote_next ~ strat_lag06,
                       data = kunprime,
                       clusters = country_id,
                       se_type = "stata")
summary(vote_ols1)
## Laver objekt til stargazer
star_vote_ols1 <- lm(vote_next ~ strat_lag06, data = kunprime)

### Nuværende stemmeandel, og tid
vote_ols2 <- lm_robust(vote_next ~ strat_lag06+vote_share+
                         factor(decade), 
                       data = kunprime,
                       clusters = country_id,
                       se_type = "stata")
summary(vote_ols2)
star_vote_ols2 <- lm(vote_next ~ strat_lag06+vote_share+factor(decade),data = kunprime)


## økonomi
vote_ols3 <- lm_robust(vote_next ~ strat_lag06+vote_share+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade), 
                       data = kunprime,
                       clusters = country_id,
                       se_type = "stata")
summary(vote_ols3)
star_vote_ols3 <- lm(vote_next ~ strat_lag06+vote_share+
                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)



########## Mandater ############

seats_ols1 <- lm_robust(seats_next ~ strat_lag06,
                        data = kunprime,
                        clusters = country_id,
                        se_type = "stata")
summary(seats_ols1)
star_seats_ols1 <- lm(seats_next ~ strat_lag06,data = kunprime)


#### nuværende mandater og tidsdummies

seats_ols2 <- lm_robust(seats_next ~ strat_lag06+seats_andel+
                          factor(decade),
                        data = kunprime,
                        clusters = country_id,
                        se_type = "stata")
summary(seats_ols2)
star_seats_ols2 <- lm(seats_next ~ strat_lag06+seats_andel+factor(decade),data = kunprime)

### Økonomi

seats_ols3 <- lm_robust(seats_next ~ strat_lag06+seats_andel+
                          gdp_growth_lag_next+
                          inflation_gdp_deflator_lag_next+
                          factor(decade),
                        data = kunprime,
                        clusters = country_id,
                        se_type = "stata")
summary(seats_ols3)
star_seats_ols3 <- lm(seats_next ~ strat_lag06+seats_andel+gdp_growth_lag_next+
                        inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)


#### Skriver til stargazer
write_clip(
  stargazer(star_vote_ols1, star_vote_ols2, star_vote_ols3,
            star_seats_ols1, star_seats_ols2, star_seats_ols3,
            
            se = list(vote_ols1$std.error, vote_ols2$std.error,
                      vote_ols3$std.error,
                      seats_ols1$std.error, seats_ols2$std.error,
                      seats_ols3$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies", omit.yes.no = c("Nej", "Ja"),
            notes = "Lande-clustered robuste standardfejl i parentes.", omit.stat = c("ser","f"),
            order = c("strat_lag06", "vote_share", "seats_andel"),
            
            covariate.labels = c("strategisk06 timet valg", "Nuværende stemmeandel",
                                 "Nuværende mandatandel", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            dep.var.caption = "Afhængig variable:", notes.align = "l",
            dep.var.labels = c("Stemmeandel", "Mandatandel"),
            
            type = "html", style = "commadefault")
)


#################################################################################
#######################                               ###########################
######################    First stage regressioner    ###########################
######################                                ###########################
#################################################################################

rm(list=ls()[! ls() %in% c("kunprime","moveme")])

first_stage_ols1 <- lm_robust(strategisk06 ~ tav_disspm+
                                factor(decade), 
                              data = kunprime,
                              clusters = country_id,
                              se_type = "stata")
summary(first_stage_ols1)
star_first_stage_ols <- lm(strategisk06 ~ tav_disspm+factor(decade), data = kunprime)



first_stage_logit1 <- glm.cluster(strategisk06 ~ tav_disspm+
                                    factor(decade),
                                  data = kunprime,
                                  cluster = "country_id",
                                  family = "binomial")
summary(first_stage_logit1)
first_stage_logit1_se <- sqrt(diag(first_stage_logit1$vcov))
## OR
first_stage_logit1_OR <- exp(coef(first_stage_logit1))
first_stage_logit1_OR
## Margins
first_stage_marginal1 <- glm(strategisk06 ~ tav_disspm+factor(decade), data = kunprime,family = "binomial")
AME_first_stage1 <- summary(margins(first_stage_marginal1))
AME_first_stage1



### Stemmer
first_stage_ols_vote <- lm_robust(strat_lag06 ~ tav_disspm+vote_share+factor(decade),
                                  data = kunprime,
                                  clusters = country_id,
                                  se_type = "stata")
summary(first_stage_ols_vote)
star_first_stage_ols_vote <- lm(strat_lag06 ~ tav_disspm+vote_share+factor(decade), data = kunprime)

##logit
first_stage_logit_vote <- glm.cluster(strat_lag06 ~ tav_disspm+vote_share+factor(decade),
                                      data = kunprime,
                                      cluster = "country_id",
                                      family = "binomial")
summary(first_stage_logit_vote)
first_stage_logit_vote_se <- sqrt(diag(first_stage_logit_vote$vcov))
## Margins
first_stage_marginal_vote <- glm(strat_lag06 ~ tav_disspm+vote_share+factor(decade),
                                 data = kunprime,family = "binomial")
AME_first_stage_vote <- summary(margins(first_stage_marginal_vote))
AME_first_stage_vote

### Mandater
first_stage_ols_seats <- lm_robust(strat_lag06 ~ tav_disspm+seats_andel+
                                     factor(decade), 
                                   data = kunprime,
                                   clusters = country_id,
                                   se_type = "stata")
summary(first_stage_ols_seats)
star_first_stage_ols_seats <- lm(strat_lag06 ~ tav_disspm+seats_andel+factor(decade), data = kunprime)

## Logit mandater
first_stage_logit_seats <- glm.cluster(strat_lag06 ~ tav_disspm+seats_andel+
                                         factor(decade),
                                       data = kunprime,
                                       cluster = "country_id",
                                       family = "binomial")
summary(first_stage_logit_seats)
first_stage_logit_seats_se <- sqrt(diag(first_stage_logit_seats$vcov))
## Margins
first_stage_marginal_seats <- glm(strat_lag06 ~ tav_disspm+seats_andel+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_seats <- summary(margins(first_stage_marginal_seats))
AME_first_stage_seats



### Fulde first stage ###############################################################
first_stage_pmsurv_ols_full <- lm_robust(strategisk06 ~ tav_disspm+
                                           gdp_growth_lag+inflation_gdp_deflator_lag+
                                           factor(decade),
                                         data = kunprime,
                                         clusters = country_id,
                                         se_type = "stata")
summary(first_stage_pmsurv_ols_full)
star_first_stage_pmsurv_ols_full <- lm(strategisk06 ~ tav_disspm+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),data = kunprime)

## Logit
first_stage_logit_pmsurv_full <- glm.cluster(strategisk06 ~ tav_disspm+
                                               gdp_growth_lag+inflation_gdp_deflator_lag+
                                               factor(decade),
                                             data = kunprime,
                                             cluster = "country_id",
                                             family = "binomial")
summary(first_stage_logit_pmsurv_full)
first_stage_logit_pmsurv_full_se <- sqrt(diag(first_stage_logit_pmsurv_full$vcov))
## Margins
first_stage_pmsurv_full_marginal <- glm(strategisk06~tav_disspm+gdp_growth_lag+inflation_gdp_deflator_lag+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_pmsurv_full <- summary(margins(first_stage_pmsurv_full_marginal))
AME_first_stage_pmsurv_full


### First stage stemmer full ##################################################################
first_stage_ols_votes_full <- lm_robust(strat_lag06~tav_disspm+vote_share+
                                          gdp_growth_lag_next+
                                          inflation_gdp_deflator_lag_next+
                                          factor(decade),
                                        data = kunprime,
                                        clusters = country_id,
                                        se_type = "stata")
summary(first_stage_ols_votes_full)
star_first_stage_ols_votes_full <- lm(strat_lag06~tav_disspm+vote_share+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),data = kunprime)


### Logit
first_stage_logit_votes_full <- glm.cluster(strat_lag06~tav_disspm+vote_share+
                                              gdp_growth_lag_next+
                                              inflation_gdp_deflator_lag_next+
                                              factor(decade),
                                            data = kunprime,
                                            cluster = "country_id",
                                            family = "binomial")
summary(first_stage_logit_votes_full)
first_stage_logit_votes_full_se <- sqrt(diag(first_stage_logit_votes_full$vcov))
## Margins
first_stage_votes_full_marginal <- glm(strat_lag06~tav_disspm+vote_share+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_votes_full <- summary(margins(first_stage_votes_full_marginal))
AME_first_stage_votes_full


### first stage mandater full ####################################################################
first_stage_ols_seats_full <- lm_robust(strat_lag06~tav_disspm+seats_andel+
                                          gdp_growth_lag_next+
                                          inflation_gdp_deflator_lag_next+
                                          factor(decade),
                                        data = kunprime,
                                        clusters = country_id,
                                        se_type = "stata")
summary(first_stage_ols_seats_full)
star_first_stage_ols_seats_full <- lm(strat_lag06~tav_disspm+seats_andel+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)

## Logit
first_stage_logit_seats_full <- glm.cluster(strat_lag06~tav_disspm+seats_andel+
                                              gdp_growth_lag_next+
                                              inflation_gdp_deflator_lag_next+
                                              factor(decade),
                                            data = kunprime,
                                            cluster = "country_id",
                                            family = "binomial")
summary(first_stage_logit_seats_full)
first_stage_logit_seats_full_se <- sqrt(diag(first_stage_logit_seats_full$vcov))

## Margins
first_stage_seats_full_marginal <- glm(strat_lag06~tav_disspm+seats_andel+gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),data = kunprime,family = "binomial")
AME_first_stage_seats_full <- summary(margins(first_stage_seats_full_marginal))
AME_first_stage_seats_full



####################################################################################
#####################                                  #############################
#####################     STARGAZER FIRST STAGE        #############################
#####################                                  #############################
####################################################################################

?stargazer

### Laver to paneler. Et med OLS og et med logistiske

## OLS
write_clip(
  stargazer(star_first_stage_ols, star_first_stage_ols_vote, star_first_stage_ols_seats,
            star_first_stage_pmsurv_ols_full, star_first_stage_ols_votes_full, star_first_stage_ols_seats_full,
            
            se = list(first_stage_ols1$std.error, first_stage_ols_vote$std.error, 
                      first_stage_ols_seats$std.error,
                      first_stage_pmsurv_ols_full$std.error, first_stage_ols_votes_full$std.error,
                      first_stage_ols_seats_full$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies", omit.yes.no = c("Ja", "Nej"),
            notes = c("Lande-clustered robuste standardfejl i parentes.", "PM: premierminister"), 
            omit.stat = c("ser", "f"),
            
            covariate.labels = c("PM opløsningsmagt", "Nuværende stemmeandel",
                                 "Nuværende mandatandel", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels.include = FALSE,
            column.labels = "strategisk06 timet parlamentsvalg",
            column.separate = 6,
            notes.align = "l",
            
            type = "html", style = "commadefault")
)

## Logistisk
write_clip(
  stargazer(first_stage_marginal1, first_stage_marginal_vote, 
            first_stage_marginal_seats,
            first_stage_pmsurv_full_marginal, first_stage_votes_full_marginal, 
            first_stage_seats_full_marginal,
            
            se = list(first_stage_logit1_se, first_stage_logit_vote_se,
                      first_stage_logit_seats_se,
                      first_stage_logit_pmsurv_full_se, first_stage_logit_votes_full_se,
                      first_stage_logit_seats_full_se),
            omit = "decade", omit.labels = "Årti-dummies", omit.yes.no = c("Ja", "Nej"),
            notes = c("Koefficienterne vises i log-odds", "Lande-clustered robuste standardfejl i parentes.", "PM: premierminister"), 
            omit.stat = c("ser", "f"),
            
            covariate.labels = c("PM opløsningsmagt", "Nuværende stemmeandel",
                                 "Nuværende mandatandel", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels.include = FALSE,
            column.labels = "strategisk06 timet parlamentsvalg",
            column.separate = 6,
            notes.align = "l",
            
            type = "html", style = "commadefault")
)



#### Laver stargazer tabel med de gennemsnitlige marginale effekter
### OBS standardfejl skal tilføjes manuelt.

write_clip(
  stargazer(first_stage_marginal1, first_stage_marginal_vote, 
            first_stage_marginal_seats,
            first_stage_pmsurv_full_marginal, first_stage_votes_full_marginal, 
            first_stage_seats_full_marginal,
            
            coef = list(AME_first_stage1$AME, AME_first_stage_vote$AME,
                        AME_first_stage_seats$AME, 
                        AME_first_stage_pmsurv_full$AME, AME_first_stage_votes_full$AME,
                        AME_first_stage_seats_full$AME),
            
            keep = c("tav_disspm", "vote_share", "seats_andel"),
            
            omit = c("decade", "gdp_growth_lag"),
            omit.labels = c("Årti-dummies", "Kontrol økonomi"), 
            omit.yes.no = c("Ja", "Nej"),
            notes = c("Viser de gennemsnitlige marginale effekter for de logistiske regressioner estimeret i tabel X",
                      "PM: premierminister"), 
            omit.stat = c("ser", "f"),
            
            covariate.labels = c("PM opløsningsmagt", "Nuværende stemmeandel", "Nuværende mandatandel"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels.include = FALSE,
            column.labels = "strategisk06 timet parlamentsvalg",
            column.separate = 6,
            notes.align = "l",
            
            type = "html", style = "commadefault")
)




####################################################################################
#####################                                  #############################
#####################     FINAL IV-Modeller            #############################
#####################                                  #############################
####################################################################################
rm(list=ls()[! ls() %in% c("kunprime","moveme")])

########################### PM_surv ##############################################
IV_pmsurv1 <- iv_robust(PMsurvive ~ strategisk06 |
                          tav_disspm,
                        data = kunprime, clusters = country_id,
                        se_type = "stata", diagnostics = TRUE)
summary(IV_pmsurv1)
star_IV_pmsurv1 <- lm(PMsurvive ~ strategisk06, data = kunprime)
### Udregner F
IV_pmsurv1_f <- IV_pmsurv1$diagnostic_first_stage_fstatistic
IV_pmsurv1_f$stars <- ifelse(IV_pmsurv1_f[4]<0.01,"***",
                             ifelse(IV_pmsurv1_f[4]<0.05,"**",
                                    ifelse(IV_pmsurv1_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_pmsurv1_Wu <- IV_pmsurv1$diagnostic_endogeneity_test
IV_pmsurv1_Wu$stars <- ifelse(IV_pmsurv1_Wu[4]<0.01,"***",
                              ifelse(IV_pmsurv1_Wu[4]<0.05,"**",
                                     ifelse(IV_pmsurv1_Wu[4]<0.1,"*","")))



## 2
IV_pmsurv2 <- iv_robust(PMsurvive ~ strategisk06+factor(decade) |
                          tav_disspm+factor(decade),
                        data = kunprime, clusters = country_id,
                        se_type = "stata", diagnostics = TRUE)
summary(IV_pmsurv2)
star_IV_pmsurv2 <- lm(PMsurvive ~ strategisk06+factor(decade), data = kunprime)
### Udregner F
IV_pmsurv2_f <- IV_pmsurv2$diagnostic_first_stage_fstatistic
IV_pmsurv2_f$stars <- ifelse(IV_pmsurv2_f[4]<0.01,"***",
                             ifelse(IV_pmsurv2_f[4]<0.05,"**",
                                    ifelse(IV_pmsurv2_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_pmsurv2_Wu <- IV_pmsurv2$diagnostic_endogeneity_test
IV_pmsurv2_Wu$stars <- ifelse(IV_pmsurv2_Wu[4]<0.01,"***",
                              ifelse(IV_pmsurv2_Wu[4]<0.05,"**",
                                     ifelse(IV_pmsurv2_Wu[4]<0.1,"*","")))


## 3
IV_pmsurv3 <- iv_robust(PMsurvive ~ strategisk06+
                          gdp_growth_lag+
                          inflation_gdp_deflator_lag+
                          factor(decade) |
                          tav_disspm+
                          gdp_growth_lag+
                          inflation_gdp_deflator_lag+
                          factor(decade),
                        data = kunprime, clusters = country_id,
                        se_type = "stata", diagnostics = TRUE)
summary(IV_pmsurv3)
star_IV_pmsurv3 <- lm(PMsurvive ~ strategisk06+gdp_growth_lag+
                        inflation_gdp_deflator_lag+factor(decade),
                      data = kunprime)
### Udregner F
IV_pmsurv3_f <- IV_pmsurv3$diagnostic_first_stage_fstatistic
IV_pmsurv3_f$stars <- ifelse(IV_pmsurv3_f[4]<0.01,"***",
                             ifelse(IV_pmsurv3_f[4]<0.05,"**",
                                    ifelse(IV_pmsurv3_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_pmsurv3_Wu <- IV_pmsurv3$diagnostic_endogeneity_test
IV_pmsurv3_Wu$stars <- ifelse(IV_pmsurv3_Wu[4]<0.01,"***",
                              ifelse(IV_pmsurv3_Wu[4]<0.05,"**",
                                     ifelse(IV_pmsurv3_Wu[4]<0.1,"*","")))



###################  IV VOTES  ###############################

## Nummer 1
IV_votes1 <- iv_robust(vote_next ~ strat_lag06 |
                         tav_disspm,
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes1)

star_IV_votes1 <- lm(vote_next ~ strat_lag06, data = kunprime)

## Udregner F
IV_votes1_f <- IV_votes1$diagnostic_first_stage_fstatistic
IV_votes1_f$stars <- ifelse(IV_votes1_f[4]<0.01,"***",
                            ifelse(IV_votes1_f[4]<0.05,"**",
                                   ifelse(IV_votes1_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes1_Wu <- IV_votes1$diagnostic_endogeneity_test
IV_votes1_Wu$stars <- ifelse(IV_votes1_Wu[4]<0.01,"***",
                             ifelse(IV_votes1_Wu[4]<0.05,"**",
                                    ifelse(IV_votes1_Wu[4]<0.1,"*","")))


## Nummer 2
IV_votes2 <- iv_robust(vote_next ~ strat_lag06+
                         factor(decade) |
                         tav_disspm+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes2)

star_IV_votes2 <- lm(vote_next ~ strat_lag06+factor(decade), data = kunprime)

## Udregner F
IV_votes2_f <- IV_votes2$diagnostic_first_stage_fstatistic
IV_votes2_f$stars <- ifelse(IV_votes2_f[4]<0.01,"***",
                            ifelse(IV_votes2_f[4]<0.05,"**",
                                   ifelse(IV_votes2_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes2_Wu <- IV_votes2$diagnostic_endogeneity_test
IV_votes2_Wu$stars <- ifelse(IV_votes2_Wu[4]<0.01,"***",
                             ifelse(IV_votes2_Wu[4]<0.05,"**",
                                    ifelse(IV_votes2_Wu[4]<0.1,"*","")))


## Nummer 3
IV_votes3 <- iv_robust(vote_next ~ strat_lag06+
                         vote_share+
                         factor(decade) |
                         tav_disspm+
                         vote_share+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes3)

star_IV_votes3 <- lm(vote_next ~ strat_lag06+vote_share+factor(decade), data = kunprime)

## Udregner F
IV_votes3_f <- IV_votes3$diagnostic_first_stage_fstatistic
IV_votes3_f$stars <- ifelse(IV_votes3_f[4]<0.01,"***",
                            ifelse(IV_votes3_f[4]<0.05,"**",
                                   ifelse(IV_votes3_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes3_Wu <- IV_votes3$diagnostic_endogeneity_test
IV_votes3_Wu$stars <- ifelse(IV_votes3_Wu[4]<0.01,"***",
                             ifelse(IV_votes3_Wu[4]<0.05,"**",
                                    ifelse(IV_votes3_Wu[4]<0.1,"*","")))

## Nummer 4
IV_votes4 <- iv_robust(vote_next ~ strat_lag06+
                         vote_share+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade) |
                         tav_disspm+
                         vote_share+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_votes4)

star_IV_votes4 <- lm(vote_next ~ strat_lag06+vote_share+
                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                       factor(decade), data = kunprime)

## Udregner F
IV_votes4_f <- IV_votes4$diagnostic_first_stage_fstatistic
IV_votes4_f$stars <- ifelse(IV_votes4_f[4]<0.01,"***",
                            ifelse(IV_votes4_f[4]<0.05,"**",
                                   ifelse(IV_votes4_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_votes4_Wu <- IV_votes4$diagnostic_endogeneity_test
IV_votes4_Wu$stars <- ifelse(IV_votes4_Wu[4]<0.01,"***",
                             ifelse(IV_votes4_Wu[4]<0.05,"**",
                                    ifelse(IV_votes4_Wu[4]<0.1,"*","")))


################### IV MANDATER #####################################

## Nummer 1
IV_seats1 <- iv_robust(seats_next ~ strat_lag06 |
                         tav_disspm,
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats1)

star_IV_seats1 <- lm(seats_next ~ strat_lag06, data = kunprime)

## Udregner F
IV_seats1_f <- IV_seats1$diagnostic_first_stage_fstatistic
IV_seats1_f$stars <- ifelse(IV_seats1_f[4]<0.01,"***",
                            ifelse(IV_seats1_f[4]<0.05,"**",
                                   ifelse(IV_seats1_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats1_Wu <- IV_seats1$diagnostic_endogeneity_test
IV_seats1_Wu$stars <- ifelse(IV_seats1_Wu[4]<0.01,"***",
                             ifelse(IV_seats1_Wu[4]<0.05,"**",
                                    ifelse(IV_seats1_Wu[4]<0.1,"*","")))

## Nummer 2
IV_seats2 <- iv_robust(seats_next ~ strat_lag06+
                         factor(decade) |
                         tav_disspm+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats2)

star_IV_seats2 <- lm(seats_next ~ strat_lag06+factor(decade), data = kunprime)

## Udregner F
IV_seats2_f <- IV_seats2$diagnostic_first_stage_fstatistic
IV_seats2_f$stars <- ifelse(IV_seats2_f[4]<0.01,"***",
                            ifelse(IV_seats2_f[4]<0.05,"**",
                                   ifelse(IV_seats2_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats2_Wu <- IV_seats2$diagnostic_endogeneity_test
IV_seats2_Wu$stars <- ifelse(IV_seats2_Wu[4]<0.01,"***",
                             ifelse(IV_seats2_Wu[4]<0.05,"**",
                                    ifelse(IV_seats2_Wu[4]<0.1,"*","")))


## Nummer 3
IV_seats3 <- iv_robust(seats_next ~ strat_lag06+seats_andel+
                         factor(decade) |
                         tav_disspm+seats_andel+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats3)

star_IV_seats3 <- lm(seats_next ~ strat_lag06+seats_andel+factor(decade),
                     data = kunprime)

## Udregner F
IV_seats3_f <- IV_seats3$diagnostic_first_stage_fstatistic
IV_seats3_f$stars <- ifelse(IV_seats3_f[4]<0.01,"***",
                            ifelse(IV_seats3_f[4]<0.05,"**",
                                   ifelse(IV_seats3_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats3_Wu <- IV_seats3$diagnostic_endogeneity_test
IV_seats3_Wu$stars <- ifelse(IV_seats3_Wu[4]<0.01,"***",
                             ifelse(IV_seats3_Wu[4]<0.05,"**",
                                    ifelse(IV_seats3_Wu[4]<0.1,"*","")))


## Nummer 4
IV_seats4 <- iv_robust(seats_next ~ strat_lag06+seats_andel+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade) |
                         tav_disspm+seats_andel+
                         gdp_growth_lag_next+
                         inflation_gdp_deflator_lag_next+
                         factor(decade),
                       data = kunprime, clusters = country_id,
                       se_type = "stata", diagnostics = TRUE)
summary(IV_seats4)

star_IV_seats4 <- lm(seats_next ~ strat_lag06+seats_andel+gdp_growth_lag_next+
                       inflation_gdp_deflator_lag_next+factor(decade), data = kunprime)

## Udregner F
IV_seats4_f <- IV_seats4$diagnostic_first_stage_fstatistic
IV_seats4_f$stars <- ifelse(IV_seats4_f[4]<0.01,"***",
                            ifelse(IV_seats4_f[4]<0.05,"**",
                                   ifelse(IV_seats4_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_seats4_Wu <- IV_seats4$diagnostic_endogeneity_test
IV_seats4_Wu$stars <- ifelse(IV_seats4_Wu[4]<0.01,"***",
                             ifelse(IV_seats4_Wu[4]<0.05,"**",
                                    ifelse(IV_seats4_Wu[4]<0.1,"*","")))



####################################################################################
#####################                                  #############################
#####################     STARGAZER IV-MODELLER        #############################
#####################                                  #############################
####################################################################################


################################## PM SURVIVE #####################################

write_clip(
  stargazer(star_IV_pmsurv1, star_IV_pmsurv2, star_IV_pmsurv3,
            # Indsætter koefficienter
            coef = list(IV_pmsurv1$coefficients, IV_pmsurv2$coefficients,
                        IV_pmsurv3$coefficients),
            # Standardfejl
            se = list(IV_pmsurv1$std.error, IV_pmsurv2$std.error,
                      IV_pmsurv3$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Nej", "Ja"), omit.stat = c("ser", "f", "rsq", "adj.rsq"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "premierministerparti bevarer posten",
            column.labels = "(IV: strategisk06 valg = PM opløsningsmagt)",
            column.separate = 4,
            
            add.lines = list(c("Fist stage F-test",
                               paste0(round(IV_pmsurv1_f$value,3), IV_pmsurv1_f$stars),
                               paste0(round(IV_pmsurv2_f$value,3), IV_pmsurv2_f$stars),
                               paste0(round(IV_pmsurv3_f$value,3), IV_pmsurv3_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", 
                               paste0(round(IV_pmsurv1_Wu$p.value,3), IV_pmsurv1_Wu$stars),
                               paste0(round(IV_pmsurv2_Wu$p.value,3), IV_pmsurv2_Wu$stars),
                               paste0(round(IV_pmsurv3_Wu$p.value,3), IV_pmsurv3_Wu$stars))),
            
            covariate.labels = c("strategisk06 timet valg", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            notes = c("Lande-clustered robuste standardfejl i parentes.", "Antal lande: 35"), notes.align = "l",
            
            type = "html", style = "commadefault")
)


################################## STEMMEANDEL #####################################

write_clip(
  stargazer(star_IV_votes1, star_IV_votes2, star_IV_votes3, star_IV_votes4,
            # Indsætter koefficienter
            coef = list(IV_votes1$coefficients, IV_votes2$coefficients,
                        IV_votes3$coefficients, IV_votes4$coefficients),
            # Standardfejl
            se = list(IV_votes1$std.error, IV_votes2$std.error,
                      IV_votes3$std.error, IV_votes4$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Nej", "Ja"), omit.stat = c("ser", "f", "rsq", "adj.rsq"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "Stemmeandel",
            column.labels = "(IV: strategisk06 valg = PM opløsningsmagt)",
            column.separate = 5,
            
            add.lines = list(c("Fist stage F-test",
                               paste0(round(IV_votes1_f$value,3), IV_votes1_f$stars),
                               paste0(round(IV_votes2_f$value,3), IV_votes2_f$stars),
                               paste0(round(IV_votes3_f$value,3), IV_votes3_f$stars),
                               paste0(round(IV_votes4_f$value,3), IV_votes4_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", 
                               paste0(round(IV_votes1_Wu$p.value,3), IV_votes1_Wu$stars),
                               paste0(round(IV_votes2_Wu$p.value,3), IV_votes2_Wu$stars),
                               paste0(round(IV_votes3_Wu$p.value,3), IV_votes3_Wu$stars),
                               paste0(round(IV_votes4_Wu$p.value,3), IV_votes4_Wu$stars))),
            
            covariate.labels = c("strategisk06 timet valg", "Nuværende stemmeandel",
                                 "BNP-vækst (1 år lag)", "Inflation (1 år lag)", 
                                 "Konstant"),
            notes = c("Lande-clustered robuste standardfejl i parentes.", "Antal lande: 35"), 
            notes.align = "l",
            
            type = "html", style = "commadefault")
)


################################## MANDATANDEL #####################################

write_clip(
  stargazer(star_IV_seats1, star_IV_seats2, star_IV_seats3, star_IV_seats4,
            # Indsætter koefficienter
            coef = list(IV_seats1$coefficients, IV_seats2$coefficients,
                        IV_seats3$coefficients, IV_seats4$coefficients),
            # Standardfejl
            se = list(IV_seats1$std.error, IV_seats2$std.error,
                      IV_seats3$std.error, IV_seats4$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Nej", "Ja"), omit.stat = c("ser", "f", "rsq", "adj.rsq"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "Andel af mandater",
            column.labels = "(IV: strategisk06 valg = PM opløsningsmagt)",
            column.separate = 5,
            
            add.lines = list(c("Fist stage F-test",
                               paste0(round(IV_seats1_f$value,3), IV_seats1_f$stars),
                               paste0(round(IV_seats2_f$value,3), IV_seats2_f$stars),
                               paste0(round(IV_seats3_f$value,3), IV_seats3_f$stars),
                               paste0(round(IV_seats4_f$value,3), IV_seats4_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", 
                               paste0(round(IV_seats1_Wu$p.value,3), IV_seats1_Wu$stars),
                               paste0(round(IV_seats2_Wu$p.value,3), IV_seats2_Wu$stars),
                               paste0(round(IV_seats3_Wu$p.value,3), IV_seats3_Wu$stars),
                               paste0(round(IV_seats4_Wu$p.value,3), IV_seats4_Wu$stars))),
            
            covariate.labels = c("strategisk06 timet valg", "Nuværende mandantandel",
                                 "BNP-vækst (1 år lag)", "Inflation (1 år lag)", 
                                 "Konstant"),
            notes = c("Lande-clustered robuste standardfejl i parentes.", "Antal lande: 35"), 
            notes.align = "l",
            
            type = "html", style = "commadefault")
)

####################################################################################
#####################                                  #############################
#####################  TEST AF EKSKLUSIONSKRITERIET    #############################
#####################                                  #############################
####################################################################################
rm(list=ls()[! ls() %in% c("kunprime","moveme")])

################### FOR PMSURV ###################################################

## First stage
eksklu_first_st_pmsurv_logit <- glm.cluster(strategisk06 ~ tav_disspm+
                                              gdp_growth_lag+inflation_gdp_deflator_lag+
                                              factor(decade),
                                            data = subset(kunprime, !(is.na(kunprime$PMsurvive))), 
                                            family = "binomial",
                                            cluster = "country_id")
summary(eksklu_first_st_pmsurv_logit)
##Standardfejl
eksklu_first_st_pmsurv_logit_se <- sqrt(diag(eksklu_first_st_pmsurv_logit$vcov))

## Til stargazer
eksklu_first_st_pmsurv_marginal <- glm(strategisk06 ~ tav_disspm+gdp_growth_lag+
                                         inflation_gdp_deflator_lag+factor(decade),
                                       data = subset(kunprime, !(is.na(kunprime$PMsurvive))), 
                                       family = "binomial")

## Margins
AME_eksklu_first_st_pmsurv <- summary(margins(eksklu_first_st_pmsurv_marginal))
AME_eksklu_first_st_pmsurv



eksklu_first_st_pmsurv_ols <- lm_robust(strategisk06 ~ tav_disspm+
                                          gdp_growth_lag+inflation_gdp_deflator_lag+
                                          factor(decade), data = subset(kunprime, !(is.na(kunprime$PMsurvive))),
                                        clusters = country_id, se_type = "stata")
summary(eksklu_first_st_pmsurv_ols)
## star
star_eksklu_first_st_pmsurv_ols <- lm(strategisk06 ~ tav_disspm+gdp_growth_lag+
                                        inflation_gdp_deflator_lag+factor(decade),
                                      data = subset(kunprime, !(is.na(kunprime$PMsurvive))))

######### Second stage

eksklu_sencond_st_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk06+tav_disspm+
                                                gdp_growth_lag+inflation_gdp_deflator_lag+
                                                factor(decade), data = kunprime,
                                              family = "binomial", cluster = "country_id")
summary(eksklu_sencond_st_pmsurv_logit)
eksklu_sencond_st_pmsurv_logit_se <- sqrt(diag(eksklu_sencond_st_pmsurv_logit$vcov))
## Til stargazer
eksklu_second_st_pmsurv_marginal <- glm(PMsurvive ~ strategisk06+tav_disspm+
                                          gdp_growth_lag+inflation_gdp_deflator_lag+
                                          factor(decade), data = kunprime, family = "binomial")
AME_eksklu_second_st_pmsurv <- summary(margins(eksklu_second_st_pmsurv_marginal))
AME_eksklu_second_st_pmsurv


eksklu_second_st_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk06+tav_disspm+
                                           gdp_growth_lag+inflation_gdp_deflator_lag+
                                           factor(decade), data = kunprime,
                                         clusters = country_id, se_type = "stata")
summary(eksklu_second_st_pmsurv_ols)


################## FOR STEMMEANDEL ################################################
eksklu_first_st_votes_logit <- glm.cluster(strat_lag06 ~ tav_disspm+vote_share+
                                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                             factor(decade),
                                           data = subset(kunprime, !(is.na(kunprime$vote_next))), family = "binomial",
                                           cluster = "country_id")
summary(eksklu_first_st_votes_logit)
eksklu_first_st_votes_logit_se <- sqrt(diag(eksklu_first_st_votes_logit$vcov))
## Til star
eksklu_first_st_votes_marginal <- glm(strat_lag06 ~ tav_disspm+vote_share+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade),
                                      data = subset(kunprime, !(is.na(kunprime$vote_next))), family = "binomial")
AME_eksklu_first_st_votes <- summary(margins(eksklu_first_st_votes_marginal))
AME_eksklu_first_st_votes

## OLS
eksklu_first_st_votes_ols <- lm_robust(strat_lag06 ~ tav_disspm+vote_share+
                                         gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                         factor(decade), data = subset(kunprime, !(is.na(kunprime$vote_next))),
                                       clusters = country_id, se_type = "stata")
summary(eksklu_first_st_votes_ols)
## Til stargazer
star_eksklu_first_st_votes_ols <- lm(strat_lag06 ~ tav_disspm+vote_share+
                                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                       factor(decade), data = subset(kunprime, !(is.na(kunprime$vote_next))))


eksklu_second_st_votes_ols <- lm_robust(vote_next ~ strat_lag06+tav_disspm+vote_share+
                                          gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                          factor(decade), data = kunprime,
                                        clusters = country_id, se_type = "stata")
summary(eksklu_second_st_votes_ols)
## Til stargazer
star_eksklu_second_st_votes_ols <- lm(vote_next ~ strat_lag06+tav_disspm+vote_share+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade), data = kunprime)


################ FOR MANDATANDEL ################################################

eksklu_first_st_seats_logit <- glm.cluster(strat_lag06 ~ tav_disspm+seats_andel+
                                             gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                             factor(decade),
                                           data = subset(kunprime, !(is.na(kunprime$seats_next))), family = "binomial",
                                           cluster = "country_id")
summary(eksklu_first_st_seats_logit)
eksklu_first_st_seats_logit_se <- sqrt(diag(eksklu_first_st_seats_logit$vcov))
## Til star
eksklu_first_st_seats_marginal <- glm(strat_lag06 ~ tav_disspm+seats_andel+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade),
                                      data = subset(kunprime, !(is.na(kunprime$seats_next))), family = "binomial")
AME_eksklu_first_st_seats <- summary(margins(eksklu_first_st_seats_marginal))
AME_eksklu_first_st_seats


eksklu_first_st_seats_ols <- lm_robust(strat_lag06 ~ tav_disspm+seats_andel+
                                         gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                         factor(decade), data = subset(kunprime, !(is.na(kunprime$seats_next))),
                                       clusters = country_id, se_type = "stata")
summary(eksklu_first_st_seats_ols)
star_eksklu_first_st_seats_ols <- lm(strat_lag06 ~ tav_disspm+seats_andel+
                                       gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                       factor(decade), data = subset(kunprime, !(is.na(kunprime$seats_next))))

eksklu_second_st_seats_ols <- lm_robust(seats_next ~ strat_lag06+tav_disspm+seats_andel+
                                          gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                          factor(decade), data = kunprime,
                                        clusters = country_id, se_type = "stata")
summary(eksklu_second_st_seats_ols)
star_eksklu_second_st_seats_ols <- lm(seats_next ~ strat_lag06+tav_disspm+seats_andel+
                                        gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                        factor(decade), data = kunprime)



#######################################################################################
##################                                        #############################
##################  STARGAZER FOR EKSKLUSIONSKRITERIET    #############################
##################                                        #############################
#######################################################################################

write_clip(
  stargazer(eksklu_first_st_pmsurv_marginal, eksklu_second_st_pmsurv_marginal,
            eksklu_first_st_votes_marginal, star_eksklu_first_st_votes_ols,
            star_eksklu_second_st_votes_ols,
            eksklu_first_st_seats_marginal, star_eksklu_first_st_seats_ols,
            star_eksklu_second_st_seats_ols,
            
            se = list(eksklu_first_st_pmsurv_logit_se, eksklu_sencond_st_pmsurv_logit_se,
                      eksklu_first_st_votes_logit_se, eksklu_first_st_votes_ols$std.error,
                      eksklu_second_st_votes_ols$std.error,
                      eksklu_first_st_seats_logit_se, eksklu_first_st_seats_ols$std.error,
                      eksklu_second_st_seats_ols$std.error),
            
            order = c("strategisk06", "strat_lag06", "tav_disspm",
                      "vote_share", "seats_andel"),
            covariate.labels = c("strategisk06 timet valg", "strategisk06 timet valg",
                                 "PM opløsningsmagt", "Nuværende stemmeandel",
                                 "Nuværende mandatandel", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "BNP-vækst (1 år lag)",
                                 "Inlation (1 år lag)", "Konstant"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = c("strategisk06 valg", "PM overlever",
                               "strategisk06 valg", "Stemmeandel",
                               "strategisk06 valg", "Mandatandel"),
            column.labels = c("premierministerparti bevarer posten",
                              "Stemmeandel", "Andel af mandater"),
            column.separate = c(2,3,3),
            
            table.layout = "=!lcd-m#-t-o-s=!n",
            
            omit = "decade", omit.labels = "Årti-dummies", 
            omit.yes.no = c("Ja", "Nej"),
            omit.stat = c("ser", "f"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "PM: premierminister"),
            notes.align = "l",
            
            type = "html", style = "commadefault")
)



#######################################################################################
##################                                        #############################
##################    ROBUSTHEDSTEST OLS OG EKSTRA IVs    #############################
##################                                        #############################
#######################################################################################

rm(list=ls()[! ls() %in% c("kunprime","moveme")])

#### Laver modeller med ekstra kontrol variable ##########

#### Lave kort deskriptiv statistik af dem, så de ikke forpester den
## første tabel med deskriptiv statistik.

# 1: valgsystem
# 2: regeringspartier/karakteristika majoritet

## OBS som bilagstabel skal regeringskarakteristika køres med de to variable hver 
## for sig, da der givet vis er høj grad af multikollinearitet mellem dem

# 3: valgperiode

## Har tre kontroller, valgsystem, antal partier i regering og længde af valgperioden.


#################   premierministerPARTIET OVERLEVER    #########################

valgsystem_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk06+valgsystem_majo+
                                         gdp_growth_lag+inflation_gdp_deflator_lag+
                                         factor(decade),
                                       data = kunprime, cluster = "country_id",
                                       family = "binomial")
summary(valgsystem_pmsurv_logit)
valgsystem_pmsurv_logit_se <- sqrt(diag(valgsystem_pmsurv_logit$vcov))
## margins
valgsystem_pmsurv_margianl <- glm(PMsurvive ~ strategisk06+valgsystem_majo+
                                    gdp_growth_lag+inflation_gdp_deflator_lag+
                                    factor(decade),
                                  data = kunprime, family = "binomial")
AME_valgsystem_pmsurv <- summary(margins(valgsystem_pmsurv_margianl))
AME_valgsystem_pmsurv


## OLS
valgsystem_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk06+valgsystem_majo+
                                     gdp_growth_lag+inflation_gdp_deflator_lag+
                                     factor(decade),
                                   data = kunprime, clusters = country_id,
                                   se_type = "stata")
summary(valgsystem_pmsurv_ols)
star_valgsystem_pmsurv_ols <- lm(PMsurvive ~ strategisk06+valgsystem_majo+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade), data = kunprime)


### Valgsystem IV
IV_valgsystem_pmsurv <- iv_robust(PMsurvive ~ strategisk06+valgsystem_majo+
                                    gdp_growth_lag+inflation_gdp_deflator_lag+
                                    factor(decade) |
                                    tav_disspm+valgsystem_majo+
                                    gdp_growth_lag+inflation_gdp_deflator_lag+
                                    factor(decade),
                                  data = kunprime, clusters = country_id,
                                  se_type = "stata", diagnostics = TRUE)
summary(IV_valgsystem_pmsurv)
star_valgsystem_pmsurv_IV <- ivreg(PMsurvive ~ strategisk06+valgsystem_majo+
                                     gdp_growth_lag+inflation_gdp_deflator_lag+
                                     factor(decade) |
                                     tav_disspm+valgsystem_majo+
                                     gdp_growth_lag+inflation_gdp_deflator_lag+
                                     factor(decade), data = kunprime)
### Udregner F
IV_valgsystem_pmsurv_f <- IV_valgsystem_pmsurv$diagnostic_first_stage_fstatistic
IV_valgsystem_pmsurv_f$stars <- ifelse(IV_valgsystem_pmsurv_f[4]<0.01,"***",
                                       ifelse(IV_valgsystem_pmsurv_f[4]<0.05,"**",
                                              ifelse(IV_valgsystem_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valgsystem_pmsurv_Wu <- IV_valgsystem_pmsurv$diagnostic_endogeneity_test
IV_valgsystem_pmsurv_Wu$stars <- ifelse(IV_valgsystem_pmsurv_Wu[4]<0.01,"***",
                                        ifelse(IV_valgsystem_pmsurv_Wu[4]<0.05,"**",
                                               ifelse(IV_valgsystem_pmsurv_Wu[4]<0.1,"*","")))



######################### Regeringskarateristika logit
regering_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk06+regeringspartier+majoritet+
                                       gdp_growth_lag+inflation_gdp_deflator_lag+
                                       factor(decade),
                                     data = kunprime, cluster = "country_id",
                                     family = "binomial")
summary(regering_pmsurv_logit)
regering_pmsurv_logit_se <- sqrt(diag(regering_pmsurv_logit$vcov))
##Margins
regering_pmsurv_marginal <- glm(PMsurvive ~ strategisk06+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade), data = kunprime, family = "binomial")
AME_regering_pmsurv <- summary(margins(regering_pmsurv_marginal))
AME_regering_pmsurv

## OLS
regering_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk06+regeringspartier+majoritet+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata")
summary(regering_pmsurv_ols)
star_regering_pmsurv_ols <- lm(PMsurvive ~ strategisk06+regeringspartier+majoritet+
                                 gdp_growth_lag+inflation_gdp_deflator_lag+
                                 factor(decade), data = kunprime)


### IV regering
IV_regering_pmsurv <- iv_robust(PMsurvive ~ strategisk06+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade) |
                                  tav_disspm+regeringspartier+majoritet+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata", diagnostics = TRUE)
summary(IV_regering_pmsurv)
star_regering_pmsurv_IV <- ivreg(PMsurvive ~ strategisk06+regeringspartier+majoritet+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade) |
                                   tav_disspm+regeringspartier+majoritet+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade), data = kunprime)
### Udregner F
IV_regering_pmsurv_f <- IV_regering_pmsurv$diagnostic_first_stage_fstatistic
IV_regering_pmsurv_f$stars <- ifelse(IV_regering_pmsurv_f[4]<0.01,"***",
                                     ifelse(IV_regering_pmsurv_f[4]<0.05,"**",
                                            ifelse(IV_regering_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_regering_pmsurv_Wu <- IV_regering_pmsurv$diagnostic_endogeneity_test
IV_regering_pmsurv_Wu$stars <- ifelse(IV_regering_pmsurv_Wu[4]<0.01,"***",
                                      ifelse(IV_regering_pmsurv_Wu[4]<0.05,"**",
                                             ifelse(IV_regering_pmsurv_Wu[4]<0.1,"*","")))


## 3 års periode logit
valg3aar_pmsurv_logit <- glm.cluster(PMsurvive ~ strategisk06+tre_aar+
                                       gdp_growth_lag+inflation_gdp_deflator_lag+
                                       factor(decade),
                                     data = kunprime, cluster = "country_id",
                                     family = "binomial")
summary(valg3aar_pmsurv_logit)
valg3aar_pmsurv_logit_se <- sqrt(diag(valg3aar_pmsurv_logit$vcov))
## Margins
valg3aar_pmsurv_marginal <- glm(PMsurvive ~ strategisk06+tre_aar+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade), data = kunprime, family = "binomial")
AME_valg3aar_pmsurv <- summary(margins(valg3aar_pmsurv_marginal))
AME_valg3aar_pmsurv


## OLS
valg3aar_pmsurv_ols <- lm_robust(PMsurvive ~ strategisk06+tre_aar+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata")
summary(valg3aar_pmsurv_ols)
star_valg3aar_pmsurv_ols <- lm(PMsurvive ~ strategisk06+tre_aar+
                                 gdp_growth_lag+inflation_gdp_deflator_lag+
                                 factor(decade), data = kunprime)

## IV 3 år valgperiode
IV_valg3aar_pmsurv <- iv_robust(PMsurvive ~ strategisk06+tre_aar+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade) |
                                  tav_disspm+tre_aar+
                                  gdp_growth_lag+inflation_gdp_deflator_lag+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata", diagnostics = TRUE)
summary(IV_valg3aar_pmsurv)
star_valg3aar_pmsurv_IV <- ivreg(PMsurvive ~ strategisk06+tre_aar+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade) |
                                   tav_disspm+tre_aar+
                                   gdp_growth_lag+inflation_gdp_deflator_lag+
                                   factor(decade), data = kunprime)
### Udregner F
IV_valg3aar_pmsurv_f <- IV_valg3aar_pmsurv$diagnostic_first_stage_fstatistic
IV_valg3aar_pmsurv_f$stars <- ifelse(IV_valg3aar_pmsurv_f[4]<0.01,"***",
                                     ifelse(IV_valg3aar_pmsurv_f[4]<0.05,"**",
                                            ifelse(IV_valg3aar_pmsurv_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valg3aar_pmsurv_Wu <- IV_valg3aar_pmsurv$diagnostic_endogeneity_test
IV_valg3aar_pmsurv_Wu$stars <- ifelse(IV_valg3aar_pmsurv_Wu[4]<0.01,"***",
                                      ifelse(IV_valg3aar_pmsurv_Wu[4]<0.05,"**",
                                             ifelse(IV_valg3aar_pmsurv_Wu[4]<0.1,"*","")))



########################   FOR STEMMEANDEL   ####################################


## Valgsystem
valgsystem_vote_ols <- lm_robust(vote_next ~ strat_lag06+vote_share+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata")
summary(valgsystem_vote_ols)
star_valgsystem_vote_ols <- lm(vote_next ~ strat_lag06+vote_share+valgsystem_majo+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade), data = kunprime)


## IV
IV_valgsystem_vote <- iv_robust(vote_next ~ strat_lag06+vote_share+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade) |
                                  tav_disspm+vote_share+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata", diagnostics = TRUE)
summary(IV_valgsystem_vote)
star_valgsystem_vote_IV <- ivreg(vote_next ~ strat_lag06+vote_share+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade) |
                                   tav_disspm+vote_share+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade), data = kunprime)
### Udregner F
IV_valgsystem_vote_f <- IV_valgsystem_vote$diagnostic_first_stage_fstatistic
IV_valgsystem_vote_f$stars <- ifelse(IV_valgsystem_vote_f[4]<0.01,"***",
                                     ifelse(IV_valgsystem_vote_f[4]<0.05,"**",
                                            ifelse(IV_valgsystem_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valgsystem_vote_Wu <- IV_valgsystem_vote$diagnostic_endogeneity_test
IV_valgsystem_vote_Wu$stars <- ifelse(IV_valgsystem_vote_Wu[4]<0.01,"***",
                                      ifelse(IV_valgsystem_vote_Wu[4]<0.05,"**",
                                             ifelse(IV_valgsystem_vote_Wu[4]<0.1,"*","")))



#### Regeringskarakteristika
regering_vote_ols <- lm_robust(vote_next ~ strat_lag06+vote_share+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata")
summary(regering_vote_ols)
star_regering_vote_ols <- lm(vote_next ~ strat_lag06+vote_share+regeringspartier+
                               majoritet+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade), data = kunprime)

## IV
IV_regering_vote <- iv_robust(vote_next ~ strat_lag06+vote_share+regeringspartier+
                                majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+vote_share+regeringspartier+
                                majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade),
                              data = kunprime, clusters = country_id,
                              se_type = "stata", diagnostics = TRUE)
summary(IV_regering_vote)
star_regering_vote_IV <- ivreg(vote_next ~ strat_lag06+vote_share+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade) |
                                 tav_disspm+vote_share+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade), data = kunprime)

### Udregner F
IV_regering_vote_f <- IV_regering_vote$diagnostic_first_stage_fstatistic
IV_regering_vote_f$stars <- ifelse(IV_regering_vote_f[4]<0.01,"***",
                                   ifelse(IV_regering_vote_f[4]<0.05,"**",
                                          ifelse(IV_regering_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_regering_vote_Wu <- IV_regering_vote$diagnostic_endogeneity_test
IV_regering_vote_Wu$stars <- ifelse(IV_regering_vote_Wu[4]<0.01,"***",
                                    ifelse(IV_regering_vote_Wu[4]<0.05,"**",
                                           ifelse(IV_regering_vote_Wu[4]<0.1,"*","")))





#### 3 års valgperiode 
valg3aar_vote_ols <- lm_robust(vote_next ~ strat_lag06+vote_share+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata")
summary(valg3aar_vote_ols)
star_valg3aar_vote_ols <- lm(vote_next ~ strat_lag06+vote_share+tre_aar+
                               gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                               factor(decade), data = kunprime)

## IV
IV_valg3aar_vote <- iv_robust(vote_next ~ strat_lag06+vote_share+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade) |
                                tav_disspm+vote_share+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade),
                              data = kunprime, clusters = country_id,
                              se_type = "stata", diagnostics = TRUE)
summary(IV_valg3aar_vote)
star_valg3aar_vote_IV <- ivreg(vote_next ~ strat_lag06+vote_share+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade) |
                                 tav_disspm+vote_share+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade), data = kunprime)
### Udregner F
IV_valg3aar_vote_f <- IV_valg3aar_vote$diagnostic_first_stage_fstatistic
IV_valg3aar_vote_f$stars <- ifelse(IV_valg3aar_vote_f[4]<0.01,"***",
                                   ifelse(IV_valg3aar_vote_f[4]<0.05,"**",
                                          ifelse(IV_valg3aar_vote_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valg3aar_vote_Wu <- IV_valg3aar_vote$diagnostic_endogeneity_test
IV_valg3aar_vote_Wu$stars <- ifelse(IV_valg3aar_vote_Wu[4]<0.01,"***",
                                    ifelse(IV_valg3aar_vote_Wu[4]<0.05,"**",
                                           ifelse(IV_valg3aar_vote_Wu[4]<0.1,"*","")))




########################   FOR MANDATANDEL   ####################################



## Valgsystem
valgsystem_seats_ols <- lm_robust(seats_next ~ strat_lag06+seats_andel+valgsystem_majo+
                                    gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                    factor(decade),
                                  data = kunprime, clusters = country_id,
                                  se_type = "stata")
summary(valgsystem_seats_ols)
star_valgsystem_seats_ols <- lm(seats_next ~ strat_lag06+seats_andel+valgsystem_majo+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade), data = kunprime)


## IV
IV_valgsystem_seats <- iv_robust(seats_next ~ strat_lag06+seats_andel+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade) |
                                   tav_disspm+seats_andel+valgsystem_majo+
                                   gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                   factor(decade),
                                 data = kunprime, clusters = country_id,
                                 se_type = "stata", diagnostics = TRUE)
summary(IV_valgsystem_seats)
star_valgsystem_seats_IV <-  ivreg(seats_next ~ strat_lag06+seats_andel+valgsystem_majo+
                                     gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                     factor(decade) |
                                     tav_disspm+seats_andel+valgsystem_majo+
                                     gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                     factor(decade), data = kunprime)
### Udregner F
IV_valgsystem_seats_f <- IV_valgsystem_seats$diagnostic_first_stage_fstatistic
IV_valgsystem_seats_f$stars <- ifelse(IV_valgsystem_seats_f[4]<0.01,"***",
                                      ifelse(IV_valgsystem_seats_f[4]<0.05,"**",
                                             ifelse(IV_valgsystem_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valgsystem_seats_Wu <- IV_valgsystem_seats$diagnostic_endogeneity_test
IV_valgsystem_seats_Wu$stars <- ifelse(IV_valgsystem_seats_Wu[4]<0.01,"***",
                                       ifelse(IV_valgsystem_seats_Wu[4]<0.05,"**",
                                              ifelse(IV_valgsystem_seats_Wu[4]<0.1,"*","")))


#### Regeringskarakteristika
regering_seats_ols <- lm_robust(seats_next ~ strat_lag06+seats_andel+regeringspartier+
                                  majoritet+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata")
summary(regering_seats_ols)
star_regering_seats_ols <- lm(seats_next ~ strat_lag06+seats_andel+regeringspartier+majoritet+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade), data = kunprime)

## IV
IV_regering_seats <- iv_robust(seats_next ~ strat_lag06+seats_andel+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade) |
                                 tav_disspm+seats_andel+regeringspartier+
                                 majoritet+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata", diagnostics = TRUE)
summary(IV_regering_seats)
star_regering_seats_IV <- ivreg(seats_next ~ strat_lag06+seats_andel+regeringspartier+majoritet+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade) |
                                  tav_disspm+seats_andel+regeringspartier+majoritet+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade), data = kunprime)

### Udregner F
IV_regering_seats_f <- IV_regering_seats$diagnostic_first_stage_fstatistic
IV_regering_seats_f$stars <- ifelse(IV_regering_seats_f[4]<0.01,"***",
                                    ifelse(IV_regering_seats_f[4]<0.05,"**",
                                           ifelse(IV_regering_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_regering_seats_Wu <- IV_regering_seats$diagnostic_endogeneity_test
IV_regering_seats_Wu$stars <- ifelse(IV_regering_seats_Wu[4]<0.01,"***",
                                     ifelse(IV_regering_seats_Wu[4]<0.05,"**",
                                            ifelse(IV_regering_seats_Wu[4]<0.1,"*","")))



#### 3 års valgperiode 
valg3aar_seats_ols <- lm_robust(seats_next ~ strat_lag06+seats_andel+tre_aar+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade),
                                data = kunprime, clusters = country_id,
                                se_type = "stata")
summary(valg3aar_seats_ols)
star_valg3aar_seats_ols <- lm(seats_next ~ strat_lag06+seats_andel+tre_aar+
                                gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                factor(decade), data = kunprime)

## IV
IV_valg3aar_seats <- iv_robust(seats_next ~ strat_lag06+seats_andel+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade) |
                                 tav_disspm+seats_andel+tre_aar+
                                 gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                 factor(decade),
                               data = kunprime, clusters = country_id,
                               se_type = "stata", diagnostics = TRUE)
summary(IV_valg3aar_seats)
star_valg3aar_seats_IV <- ivreg(seats_next ~ strat_lag06+seats_andel+tre_aar+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade) |
                                  tav_disspm+seats_andel+tre_aar+
                                  gdp_growth_lag_next+inflation_gdp_deflator_lag_next+
                                  factor(decade), data = kunprime)
### Udregner F
IV_valg3aar_seats_f <- IV_valg3aar_seats$diagnostic_first_stage_fstatistic
IV_valg3aar_seats_f$stars <- ifelse(IV_valg3aar_seats_f[4]<0.01,"***",
                                    ifelse(IV_valg3aar_seats_f[4]<0.05,"**",
                                           ifelse(IV_valg3aar_seats_f[4]<0.1,"*","")))
## Durbin-Wu-Hausman test
IV_valg3aar_seats_Wu <- IV_valg3aar_seats$diagnostic_endogeneity_test
IV_valg3aar_seats_Wu$stars <- ifelse(IV_valg3aar_seats_Wu[4]<0.01,"***",
                                     ifelse(IV_valg3aar_seats_Wu[4]<0.05,"**",
                                            ifelse(IV_valg3aar_seats_Wu[4]<0.1,"*","")))




#######################################################################################
##################                                        #############################
##################        STARGAZER EKSTRA OLS OG IV      #############################
##################                                        #############################
#######################################################################################


################### TABEL FOR PMSURVIVE ############################################

write_clip(
  stargazer(valgsystem_pmsurv_margianl, star_valgsystem_pmsurv_ols, star_valgsystem_pmsurv_IV,
            regering_pmsurv_marginal, star_regering_pmsurv_ols, star_regering_pmsurv_IV,
            valg3aar_pmsurv_marginal, star_valg3aar_pmsurv_ols, star_valg3aar_pmsurv_IV,
            
            coef = list(NULL, NULL, IV_valgsystem_pmsurv$coefficients,
                        NULL, NULL, IV_regering_pmsurv$coefficients,
                        NULL, NULL, IV_valg3aar_pmsurv$coefficients),
            se = list(valgsystem_pmsurv_logit_se, valgsystem_pmsurv_ols$std.error,
                      IV_valgsystem_pmsurv$std.error,
                      regering_pmsurv_logit_se, regering_pmsurv_ols$std.error,
                      IV_regering_pmsurv$std.error,
                      valg3aar_pmsurv_logit_se, valg3aar_pmsurv_ols$std.error,
                      IV_valg3aar_pmsurv$std.error),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "premierministerparti bevarer posten",
            column.labels = "(IV: strategisk06 valg = premierministerens opløsningsmagt)",
            column.separate = 9,
            
            table.layout = "=!ldc-m#-t-o-sa=!n",
            
            add.lines = list(c("First stage F-test",
                               "", "", 
                               paste0(round(IV_valgsystem_pmsurv_f$value,3), IV_valgsystem_pmsurv_f$stars),
                               "","",
                               paste0(round(IV_regering_pmsurv_f$value,3), IV_regering_pmsurv_f$stars),
                               "","",
                               paste0(round(IV_valg3aar_pmsurv_f$value,3), IV_valg3aar_pmsurv_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)",
                               "","",
                               paste0(round(IV_valgsystem_pmsurv_Wu$p.value,3), IV_valgsystem_pmsurv_Wu$stars),
                               "","",
                               paste0(round(IV_regering_pmsurv_Wu$p.value,3), IV_regering_pmsurv_Wu$stars),
                               "","",
                               paste0(round(IV_valg3aar_pmsurv_Wu$p.value,3), IV_valg3aar_pmsurv_Wu$stars))),
            covariate.labels = c("strategisk06 timet valg", "Flertalsvalg", "Antal regeringspartier",
                                 "Majoritetsregering", "3 års valgperiode", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "Koefficienterne for de logistiske regressioner vises i log-odds",
                      "Model 3, 6 og 9 anvender IV-estimation.",
                      "Antal lande: 35"),
            notes.align = "l",
            
            type = "html", style = "commadefault")
)


################### TABEL FOR STEMMEANDEL ############################################

write_clip(
  stargazer(star_valgsystem_vote_ols, star_valgsystem_vote_IV,
            star_regering_vote_ols, star_regering_vote_IV,
            star_valg3aar_vote_ols, star_valg3aar_vote_IV,
            
            coef = list(NULL, IV_valgsystem_vote$coefficients,
                        NULL, IV_regering_vote$coefficients,
                        NULL, IV_valg3aar_vote$coefficients),
            
            se = list(valgsystem_vote_ols$std.error, IV_valgsystem_vote$std.error,
                      regering_vote_ols$std.error, IV_regering_vote$std.error,
                      valg3aar_vote_ols$std.error, IV_valg3aar_vote$std.error),
            
            add.lines = list(c("First stage F-test", "", 
                               paste0(round(IV_valgsystem_vote_f$value,3), IV_valgsystem_vote_f$stars), "",
                               paste0(round(IV_regering_vote_f$value,3), IV_regering_vote_f$stars), "",
                               paste0(round(IV_valg3aar_vote_f$value,3), IV_valg3aar_vote_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", "",
                               paste0(round(IV_valgsystem_vote_Wu$p.value,3), IV_valgsystem_vote_Wu$stars), "",
                               paste0(round(IV_regering_vote_Wu$p.value,3), IV_regering_vote_Wu$stars), "",
                               paste0(round(IV_valg3aar_vote_Wu$p.value,3), IV_valg3aar_vote_Wu$stars))),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "Stemmeandel",
            column.labels = "(IV: strategisk06 valg = premierministerens opløsningsmagt)",
            column.separate = 6,
            
            covariate.labels = c("strategisk06 timet valg", "Nuværende stemmeandel",
                                 "Flertalsvalg", "Antal regeringspartier",
                                 "Majoritetsregering", "3 års valgperiode", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "Model 2, 4 og 6 anvender IV-estimation.",
                      "Antal lande: 35"), notes.align = "l",
            
            table.layout = "=!ldc-m#-t-o-sa=!n",
            type = "html", style = "commadefault")
)


###################### TABEL FOR MANDATANDEL ############################################

write_clip(
  stargazer(star_valgsystem_seats_ols, star_valgsystem_seats_IV,
            star_regering_seats_ols, star_regering_seats_IV,
            star_valg3aar_seats_ols, star_valg3aar_seats_IV,
            
            coef = list(NULL, IV_valgsystem_seats$coefficients,
                        NULL, IV_regering_seats$coefficients,
                        NULL, IV_valg3aar_seats$coefficients),
            
            se = list(valgsystem_seats_ols$std.error, IV_valgsystem_seats$std.error,
                      regering_seats_ols$std.error, IV_regering_seats$std.error,
                      valg3aar_seats_ols$std.error, IV_valg3aar_seats$std.error),
            
            add.lines = list(c("First stage F-test", "", 
                               paste0(round(IV_valgsystem_seats_f$value,3), IV_valgsystem_seats_f$stars), "",
                               paste0(round(IV_regering_seats_f$value,3), IV_regering_seats_f$stars), "",
                               paste0(round(IV_valg3aar_seats_f$value,3), IV_valg3aar_seats_f$stars)),
                             
                             c("Durbin-Wu-Hausman test (p-værdi)", "",
                               paste0(round(IV_valgsystem_seats_Wu$p.value,3), IV_valgsystem_seats_Wu$stars), "",
                               paste0(round(IV_regering_seats_Wu$p.value,3), IV_regering_seats_Wu$stars), "",
                               paste0(round(IV_valg3aar_seats_Wu$p.value,3), IV_valg3aar_seats_Wu$stars))),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "Andel af mandater",
            column.labels = "(IV: strategisk06 valg = premierministerens opløsningsmagt)",
            column.separate = 6,
            
            covariate.labels = c("strategisk06 timet valg", "Nuværende mandatandel",
                                 "Flertalsvalg", "Antal regeringspartier",
                                 "Majoritetsregering", "3 års valgperiode", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            omit = "decade", omit.labels = "Årti-dummies",
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            
            notes = c("Lande-clustered robuste standardfejl i parentes.",
                      "Model 2, 4 og 6 anvender IV-estimation.",
                      "Antal lande: 35"), notes.align = "l",
            
            table.layout = "=!ldc-m#-t-o-sa=!n",
            type = "html", style = "commadefault")
)


###################### TABEL TIL BILAG FOR PMSURV AME OG OLS ######################################

write_clip(
  stargazer(valgsystem_pmsurv_margianl, star_valgsystem_pmsurv_ols,
            regering_pmsurv_marginal, star_regering_pmsurv_ols,
            valg3aar_pmsurv_marginal, star_valg3aar_pmsurv_ols,
            
            coef = list(AME_valgsystem_pmsurv$AME, NULL,
                        AME_regering_pmsurv$AME, NULL,
                        AME_valg3aar_pmsurv$AME, NULL),
            
            se = list(NA, valgsystem_pmsurv_ols$std.error,
                      NA, regering_pmsurv_ols$std.error,
                      NA, valg3aar_pmsurv_ols$std.error),
            p = list(AME_valgsystem_pmsurv$p, NULL,
                     AME_regering_pmsurv$p, NULL,
                     AME_valg3aar_pmsurv$p, NULL),
            
            dep.var.caption = "Afhængig variabel:",
            dep.var.labels = "premierministerparti bevarer posten",
            
            covariate.labels = c("strategisk06 timet valg", "Flertalsvalg", "Antal regeringspartier",
                                 "Majoritetsregering", "3 års valgperiode", "BNP-vækst (1 år lag)",
                                 "Inflation (1 år lag)", "Konstant"),
            
            omit = "decade", omit.labels = "Årti-dummies", 
            omit.yes.no = c("Ja", "Nej"), omit.stat = c("ser", "f"),
            notes = c("Viser de gennemsnitlige marginale effekter for",
                      "de logistiske regressioner estimeret i tabel X.",
                      "OLS regressioner er medtaget til sammenligning",
                      "Antal lande: 35"),
            notes.align = "l",
            
            type = "html", style = "commadefault")
)

### Slut.