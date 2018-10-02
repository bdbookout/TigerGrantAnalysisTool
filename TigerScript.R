library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(rebus)
library(forcats)
library(gridExtra)
library(maps)
library(mapdata)
library(ggmap)
library(scales)
library(sp)
library(rgdal)
library(RColorBrewer)
library(colorspace)
library(ggvis)
library(plotly)
library(lubridate)
library(urbnmapr)



TigerGrants <- read_csv("/Users/bari/R-files/projects/TigerResearch/Tigermaster.csv")
statelatlon <- read_csv("/Users/bari/R-files/projects/TigerResearch/statelatlong.csv")
House <- read_csv("/Users/bari/R-files/projects/TigerResearch/house_members_103-115.csv")
Senate <- read_csv("/Users/bari/R-files/projects/TigerResearch/senators_103-115.csv")
CongressDuration <- read_csv("/Users/bari/R-files/projects/TigerResearch/Congress_Duration.csv")
rawtiger <- read_csv("https://www.transportation.gov/sites/dot.gov/files/docs/tiger_allUpdate.csv") %>% 
write_csv("/Users/bari/R-files/projects/TigerResearch/rawtiger.csv")
worldcities <- read_csv("/Users/bari/R-files/projects/TigerResearch/worldcities.csv")
StatePopulation <- read_csv("/Users/bari/R-files/projects/TigerResearch/StatePopulationDataEstCensus.csv")
CensusRegions <- read_csv("/Users/bari/R-files/projects/TigerResearch/CensusRegion.csv")
RawTiger2016 <- read_csv("/Users/bari/R-files/projects/TigerResearch/AllTIGER2016.csv")

#TIGER GRANT CLEANING SCRIPT
#CleanRawTiger2016 data and split lat lon for cross referencing
colnames(RawTiger2016) <- c("Admin", "Urban_Rural", "Project_Name", "Applicant", "Date", "Category", "Type", "Description", 
  "Amount", "Loc_Prec", "Coords")

RawTiger2016$Coords <- str_replace(RawTiger2016$Coords, fixed("("), "")
RawTiger2016$Coords <- str_replace(RawTiger2016$Coords, fixed(")"), "")
head(RawTiger2016[ , "Coords"])

RTS <- str_split(RawTiger2016$Coords, ", ", simplify = T)

RawTiger2016$Lat <- RTS[ , 1]
RawTiger2016$Long <- RTS[ , 2]

RawTiger2016C <- RawTiger2016 %>% mutate(Date = mdy(Date, tz = ""), 
                                         Year = year(Date),
                                         Lat = as.numeric(Lat),
                                         Long = as.numeric(Long)) 
  
RawTiger2016 %>% write_csv("/Users/bari/R-files/projects/TigerResearch/RawTiger2016.csv")




#cleaning Population Data
StatePopulation$State <- str_remove(StatePopulation$State, char_class("."))
StatePopulationLong <- StatePopulation %>% gather(Year, Population, -1) %>% mutate(Year = as.numeric(Year))
glimpse(StatePopulationLong)
StSp <- str_split(CensusRegions$State, OPEN_PAREN, n = 2, simplify = T)
CensusRegions$State <- str_trim(StSp[ , 1])
StatePopulationLong1 <- StatePopulationLong %>% left_join(CensusRegions, by = "State")

CensusRegions %>% left_join(statelatlon, by = "State") %>% 
  write_csv("/Users/bari/R files/CensusTradeData/RegionsLookup.csv")
#Convert State Population to 
patt1 <- capture(START %R% char_class("A-Z") %R% char_class("A-Z") %R% or(":", ""))
patt2 <- capture(or(patt1, "") %R% or(" ", "")) %R% capture(zero_or_more(WRD) %R% zero_or_more(SPC) %R% zero_or_more(WRD))
TG <- str_match(TigerGrants$State, patt2)
TigerGrants$StateAbb <- TG[ , 3]
TigerGrants$NewState <- TG[ , 4]
TigerGrants$StateAbb <- str_remove(TigerGrants$StateAbb, ":")
TigerGrants$TIGER_Grant_Award <- str_remove(TigerGrants$TIGER_Grant_Award,  char_class("$"))
TigerGrants$TIGER_Grant_Award <- str_replace_all(TigerGrants$TIGER_Grant_Award, pattern = fixed(","), "" )
TigerGrants$TIGER_Grant_Award <- str_replace_all(TigerGrants$TIGER_Grant_Award, pattern = fixed("*"), "" )
TGsplit <- str_split(TigerGrants$TIGER_Grant_Award, "\\n", n = 2, simplify = T)
TigerGrants$TIGER_Grant_Award <- TGsplit[ , 1]
TigerGrants1 <- TigerGrants %>% select(Grant_Code, Project_Name, Year, TIGER_Grant_Award, Urban_Rural, StateAbb, NewState) %>%
  mutate(State = NewState) %>% select(Grant_Code, Project_Name, Year, TIGER_Grant_Award, Urban_Rural, StateAbb, State)
TigerGrants1$State <- str_replace(TigerGrants1$State, "Illinois\nIN", "Illinois")

TigerGrantsClean <- TigerGrants1 %>% left_join(statelatlon, by = "StateAbb") %>% 
  left_join(statelatlon, by = c("State.x"= "State")) %>% 
  mutate(StateAbb.x = case_when(is.na(StateAbb.x) ~ StateAbb.y,
                                TRUE ~ StateAbb.x),
         State.x = case_when(is.na(State.x) ~ State.y,
                             State.x == "" ~ State.y,
                             TRUE ~ State.x),
         Latitude.x = case_when(is.na(Latitude.x) ~ Latitude.y,
                                TRUE ~ Latitude.x),
         Longitude.x = case_when(is.na(Longitude.x) ~ Longitude.y,
                                 TRUE ~ Longitude.x),
         TIGER_Grant_Award = as.numeric(TIGER_Grant_Award)) %>% 
  select(Grant_Code, Project_Name, Year, TIGER_Grant_Award, Urban_Rural, StateAbb = StateAbb.x, State = State.x,
         Latitude = Latitude.x, Longitude = Longitude.x) %>% write_csv("/Users/bari/R-files/projects/TigerResearch/TigerGrantsClean.csv")


#cleaning Script for Congress Duration
CongressDuration1 <- CongressDuration %>% fill(c(Congress, Start, End))
ConNum <- START %R% one_or_more(DGT)
CongressDuration1$CongNum <- str_match(CongressDuration1$Congress, ConNum)
dates <- str_match(CongressDuration1$Session_dates, one_or_more(WRD) %R% SPC %R% one_or_more(DGT) %R% char_class(",") %R% SPC %R% one_or_more(DGT))
CongressDuration1$SessionYear <- str_sub(dates, -4, -1)
CongressLookup <- CongressDuration1 %>% select(Congress = CongNum, Year = SessionYear) %>% mutate(Congress = as.numeric(Congress), Year = as.numeric(Year))
glimpse(CongressLookup)
H1 <- House %>% group_by(cong, state_po, party_txt) %>% summarise(Party = n()) %>% ungroup() %>% spread(party_txt, Party) %>% filter(cong >= 109) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>% select(Congress = cong, StateAbb = state_po, HD = D, HR = R) %>% 
  mutate(HMaj = case_when(HD - HR > 0 ~ "D",
                          TRUE ~ "R"))

S1 <- Senate %>% group_by(Congress, State, Party) %>% summarise(PNum = n()) %>% ungroup() %>% spread(Party, PNum) %>% 
  filter(Congress >= 109) %>% mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
  select(Congress, StateAbb = State, SD = D, SR = R) %>% 
  mutate(SMaj = case_when(SD - SR > 0 ~ "D",
                          TRUE ~ "R"))

AllCongress <- H1 %>% left_join(S1, by = c("Congress", "StateAbb"))
save(AllCongress, file = "~/R-files/projects/TigerResearch/TigerGrant/Data/AllCongress.RData")
glimpse(AllCongress)
TGC <- TigerGrantsClean %>% left_join(CongressLookup, by = "Year") %>% 
  write_csv("/Users/bari/R-files/projects/TigerResearch/TigerGrantsData1.csv")
save(TGC, file = "~/R-files/projects/TigerResearch/TigerGrant/Data/TGC.RData")
 
TGProjDet <- TigerGrantsClean %>% left_join(CongressLookup, by = "Year") %>% 
  left_join(AllCongress, by = c("Congress", "StateAbb")) %>% 
  select(Year, State, StateAbb, Project_Name, Urban_Rural, Congress, HD, HR, HMaj, SD, SR, SMaj) %>%
  arrange(Year, State)

save(TGProjDet, file = "~/R-files/projects/TigerResearch/TigerGrant/Data/TGProjDet.RData")


TGCC <- TGC %>% group_by(Year, State, StateAbb, Congress) %>%
  summarize(Award = sum(TIGER_Grant_Award), Projects = n()) %>% 
  left_join(AllCongress, by = c("Congress", "StateAbb")) %>%
  left_join(StatePopulationLong1, by = c("Year", "State")) %>%
  mutate(AwardPopRatio = Award / Population) %>% left_join(statelatlon, by = c("State", "StateAbb")) %>%
  write_csv("/Users/bari/R-files/projects/TigerResearch/TigerGrantsData2.csv")

save(TGCC, file = "~/R-files/projects/TigerResearch/TigerGrant/Data/TGCC.RData")

TGCC %>% ggplot(aes(Year, AwardPopRatio, color = Region)) +
  geom_point(position = "jitter")


##Analysis of awards by year by region
TGR <- TGCC %>% filter(Year == 2017) %>% group_by(Region) %>% summarise(Dollars = sum(Award), Projects = sum(Projects), 
                                               Majority = case_when(sum(HD + SD) - sum(HR + SR) > 0 ~ "D",
                                                                    TRUE ~ "R")) %>%
                                                
  ggplot(aes(Region, Dollars / 1000000, fill = Majority)) +
  geom_col() +
  geom_text(aes(x = Region, y = Dollars / 1000000 + 5, 
                label = paste(dollar(round(Dollars / 1000000)), "M", sep = ""))) +
  scale_fill_manual(values = partycolours) +
  scale_x_discrete(expand = c(0,0)) +
  labs(title = "TIGER Awards by Region", subtitle = "Congressional Majority", 
       y = NULL, x = NULL) +
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())
  


TGCC$ID <- 1:nrow(TGCC)

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- TGCC[TGCC$ID == x$ID, ]
  paste0(names(row), ": ", format(row), collapse = "<br />")
}

TGCC %>% group_by(Region) %>% ggvis(~Region, ~Award) %>% 
  layer_points(fill = ~Region) %>%
  add_tooltip(all_values, "hover")

head(TGCC)
partycolours <- c("#F53513EF", "#1B81DBFE")

StateAnalysis <- TGC %>% group_by(State, StateAbb) %>% 
  left_join(AllCongress, by = c("Congress", "StateAbb")) %>%
  summarize(TotalAwards = n(), 
            TotalDollars = sum(TIGER_Grant_Award),
            TotDem = sum(HD, na.rm = T) + sum(SD, na.rm = T),
            TotRep = sum(HR, na.rm = T) + sum(SR, na.rm = T),
            Majority = case_when(TotDem - TotRep > 0 ~ "D",
                                 TRUE ~ "R")) %>% 
  arrange(desc(TotalDollars), TotalAwards) %>% ungroup() %>% 
  mutate(region = tolower(State)) %>% left_join(statelatlon, by = c("State", "StateAbb"))

#number of party by region over 10 years
TGCC %>% group_by(Region) %>% summarize(Dem = sum(HD, na.rm = T) + sum(SD, na.rm = T), 
                                        Rep = sum(HR, na.rm = T) + sum(SR, na.rm = T)) %>% 
  gather(Party, Number, -1) %>% 
  ggplot(aes(Region, Number, color = Party)) + 
  geom_point(aes(size = Number)) +
  scale_color_manual(values = partycolours)
#Annual amount awarded
AnSummary <- TGCC %>% group_by(Year) %>% summarize(Dollars = sum(Award), Projects = sum(Projects), 
                                      HD = sum(HD, na.rm = T), HR = sum(HR, na.rm = T),
                                      Majority = case_when(HD - HR > 0 ~ "D",
                                                           TRUE ~ "R"))
AnSummary %>% ggplot() + 
  geom_col(aes(x = factor(Year), y = Dollars / 1000000, fill = Dollars)) +
  geom_text(aes(x = factor(Year), y = Dollars / 1000000 + 30, 
                label = paste(dollar(round(Dollars / 1000000)), "M", sep = ""))) + 
  scale_y_continuous(labels = dollar, expand = c(0, 0), limits = c(0, 1600)) + 
  labs(title = "Total TIGER Dollars by Year", y = "$Millions", x = NULL) +
  theme_light() + 
  theme(panel.grid.major.x = element_blank())



##Barchart showing $ awarded each year, divided by majority party of House reps in each state
TGCC %>% group_by(Year, HMaj) %>% summarise(Award = sum(Award), Project = sum(Projects)) %>% ungroup() %>% 
  filter(!is.na(HMaj)) %>% 
  ggplot(aes(x = factor(Year), fill = HMaj)) +
  geom_col(aes(y = Award), position = "dodge") + 
  geom_text(aes(y = Award + 10005500, label = paste(dollar(round(Award / 1000000)), "M", sep = "")), 
            position = position_dodge(0.9), size = 3) + 
  labs(title = "TIGER Grant Awards", subtitle = "by Majority Party of House Represenatatives by State", 
       x = NULL, y = "Amount Awarded") +
  scale_y_continuous(labels = dollar_format(), expand = c(0,0), limits = c(0, 1225000000)) + 
  scale_fill_manual(values = c("#F53513EF", "#1B81DBFE", "dark gray")) +
  theme_light() + 
  theme(panel.grid.major.x = element_blank())


StateDetail <- TGC %>% group_by(State, StateAbb, Year) %>% 
  left_join(AllCongress, by = c("Congress", "StateAbb")) %>%
  summarize(TotalAwards = n(), 
            TotalDollars = sum(TIGER_Grant_Award),
            TotDem = sum(HD, na.rm = T) + sum(SD, na.rm = T),
            TotRep = sum(HR, na.rm = T) + sum(SR, na.rm = T),
            Majority = case_when(TotDem - TotRep > 0 ~ "D",
                                 TRUE ~ "R")) %>% 
  arrange(desc(TotalDollars), TotalAwards) %>% ungroup() %>% 
  mutate(region = tolower(State)) %>% 
  filter(Year == 2009) 

  StateSummary <- TGC %>% group_by(State, StateAbb, Year) %>% 
  left_join(AllCongress, by = c("Congress", "StateAbb")) %>%
  summarize(TotalAwards = n(), 
            TotalDollars = sum(TIGER_Grant_Award),
            TotDem = sum(HD, na.rm = T) + sum(SD, na.rm = T),
            TotRep = sum(HR, na.rm = T) + sum(SR, na.rm = T),
            Majority = case_when(TotDem - TotRep > 0 ~ "D",
                                 TRUE ~ "R")) %>% 
  arrange(desc(TotalDollars), TotalAwards) %>% ungroup() %>% 
  mutate(region = tolower(State)) %>% left_join(statelatlon, by = c("State", "StateAbb"))  %>% 
  group_by(State, StateAbb) %>% 
  summarise(TotalAwards = sum(TotalAwards),
            TotalDollars = sum(TotalDollars),
            Majority = case_when(sum(TotDem) - sum(TotRep) > 0 ~ "Dem",
                                 TRUE ~ "Rep")) %>% ungroup()
save(StateSummary, file = "~/R-files/projects/TigerResearch/TigerGrant/Data/StateSummary.RData")

USA <- map_data("usa")
States <- map_data("state")
Counties <- map_data("county")
head(worldcities)
StateCapitals <- worldcities %>% 
  filter(country == "United States", capital %in% c("admin", "primary")) 
StatesD <- States %>% left_join(StateAnalysis, by = "region") 
glimpse(StatesD)
partycolours <- c("#F53513EF", "#1B81DBFE")

#dollars by state
StateDetail %>% mutate(State = fct_reorder(as_factor(State), 
                                                  TotalDollars, .fun = sum, desc = T)) %>% 
  ggplot(aes(State, TotalDollars)) + 
  geom_bar(stat = "identity", aes(fill = Majority)) + 
  scale_fill_manual(values = partycolours) +
  scale_y_continuous(labels = dollar, expand = c(0,0)) +
  coord_flip() + 
  labs(title = "TIGER Grant Dollars 2007-2017", x = NULL, y = "Total Dollars Awarded")

#projects by state
SP <- StateAnalysis %>% mutate(State = as_factor(State), State2 = fct_reorder(State, TotalAwards, .fun = sum, desc = T)) %>% 
  ggplot(aes(State2, TotalAwards)) + 
  geom_bar(stat = "identity", aes(fill = Majority)) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = partycolours) +
  coord_flip() +
  labs(title = "Tiger Grant Projects 2007-2017", x = NULL, y = "Total Projects Awarded Grants")

#delegates by state
grid.arrange(SD, SP)



#maps
#
USA <- map_data("usa")
Counties <- map_data("county")
States <- map_data("state")

States %>% ggplot() +
  geom_polygon(aes(long, lat, group = group, order = order)) +
  geom_point(data = RawTiger2016C, aes(Long, Lat, color = Year))

states %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)  +
  theme_nothing()

territories <- get_urbn_map(map = "territories")
labels <- get_urbn_labels(map = "territories")

states <- get_urbn_map(map = "states")

StateMap <- states %>% left_join(StateAnalysis, by = c("state_name" = "State", 
                                                        "state_abbv" = "StateAbb"))

SUD <-  StateMap %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = TotalDollars),
               color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_light() + 
  scale_fill_continuous(labels = dollar, guide = guide_legend("Total Dollars")) + 
  guides(fill = guide_legend("Total Dollars")) +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()) 

SUD + 
  geom_label(data = StateAnalysis, aes(Longitude, Latitude, 
                                       label = TotalAwards, 
                                       color = Majority), size = 1.75) + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_color_manual(values = c("#F53513EF", "#1B81DBFE")) +                                                                               
  labs(title = "TIGER Projects 2007-2017", subtitle = "with number of projects in each state", 
       caption = "color scale represents dollars awarded\nhouse member majority identified by label color",
       color = "Majority Party")

  


states %>% filter(state_abbv == "HI")

StateAnalysis <- TGC %>% group_by(State, StateAbb) %>% 
  left_join(AllCongress, by = c("Congress", "StateAbb")) %>%
  summarize(TotalAwards = n(), 
            TotalDollars = sum(TIGER_Grant_Award),
            TotDem = sum(HD, na.rm = T) + sum(SD, na.rm = T),
            TotRep = sum(HR, na.rm = T) + sum(SR, na.rm = T),
            Majority = case_when(TotDem - TotRep > 0 ~ "D",
                                 TRUE ~ "R")) %>% 
  arrange(desc(TotalDollars), TotalAwards) %>% ungroup() %>% 
  mutate(region = tolower(State)) %>% left_join(statelatlon, by = c("State", "StateAbb"))

head(Counties)
State48 <- StateAnalysis %>% filter(!(StateAbb %in% c("AK", "HI")))

save(StateAnalysis, file = "~/R-files/projects/TigerResearch/TigerGrant/Data/StateAnalysis.RData")


gradientcol  <- brewer.pal(n = 9, "Greys")


 gg3 <- ggplot(data = StatesD) +
   geom_polygon(aes(long, lat, fill = TotalDollars, group = group), color = "white", show.legend = T) +
   coord_fixed(1.3) + 
   theme_light() + 
   scale_fill_continuous(labels = dollar, guide = guide_legend("Total Dollars")) + 
   guides(fill = guide_legend("Total Dollars")) +
   theme(axis.line = element_blank(),
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         panel.border = element_blank(),
         panel.grid = element_blank(),
         axis.title = element_blank()) 
 

gg3 + 
  geom_label(data = State48, aes(Longitude, Latitude, label = TotalAwards, color = Majority, 
                              face = "bold")) +
  scale_color_manual(values = c("#F53513EF", "#1B81DBFE") 
                     ) +
  labs(title = "TIGER Projects 2007-2017", subtitle = "number of projects in lower", 
       caption = "color scale represents dollars awarded\nhouse member majority identified by label color",
       color = "Majority Party")



