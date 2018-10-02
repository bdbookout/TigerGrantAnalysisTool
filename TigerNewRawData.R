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
library(tmap)
library(rgdal)
library(RColorBrewer)
library(colorspace)
library(ggvis)
library(plotly)
library(lubridate)
library(urbnmapr)
library(leaflet)
library(sf)

##READ IN AND CLEAN DATA
RawTiger2016 <- read_csv("/Users/bari/R-files/projects/TigerResearch/AllTIGER2016.csv")
head(RawTiger2016)
colnames(RawTiger2016) <- c("Admin", "Urban_Rural", "Project_Name", "Applicant", "Date", "Category", "Type", "Description", 
                            "Amount", "Loc_Prec", "Coords")
RawTiger2016$Coords <- str_replace(RawTiger2016$Coords, fixed("("), "")
RawTiger2016$Coords <- str_replace(RawTiger2016$Coords, fixed(")"), "")
head(RawTiger2016[ , "Coords"])

RTS <- str_split(RawTiger2016$Coords, ", ", simplify = T)

RawTiger2016$Lat <- RTS[ , 1]
RawTiger2016$Long <- RTS[ , 2]



TigerGeo2016 <- read_csv("/Users/bari/R-files/projects/TigerResearch/RawTiger2016geo.csv")
TigerGeo2016$Date <- mdy_hm(TigerGeo2016$Date, tz = "")
TigerGeo2016$ZIP <- as.numeric(TigerGeo2016$ZIP)
glimpse(TigerGeo2016)
TigerGeo2016 <- TigerGeo2016 %>% mutate(Year = year(Date), Type = tolower(Type)) 

TigerGeo2016$Type <- str_replace(TigerGeo2016$Type, START %R% "rail" %R% END, "freight rail")
TigerGeo2016$Type <- str_replace(TigerGeo2016$Type, START %R% "port" %R% END, "port/maritime")
TigerGeo2016$Type <- str_replace(TigerGeo2016$Type, START %R% "maritime" %R% END, "port/maritime")
unique(TigerGeo2016$Type)
save(TigerGeo2016, file = "~/R-files/projects/TigerResearch/TigerGrant/TigerGeo2016.RData")
library(forcats)
TypeCol <- c("#A2CD5A", "#548B54", "#009ACD", "#EEAD0E", "#EE2C2C", "#696969", "#4682B4")

TigerGeo2016
TigerGeo2016 %>% group_by(Year, Type) %>%
  summarize(Projects = n(), Amount = sum(Amount)) %>% ungroup() %>%
  mutate(Type= fct_reorder(as_factor(Type), Amount,.fun = sum, desc = T)) %>%
  ggplot(aes(factor(Year), Projects, color = Type)) +
  geom_col(aes(fill = Type), position = "dodge") +
  scale_fill_manual(values = TypeCol) +
  scale_color_manual(values = TypeCol) +
  scale_y_continuous(expand = c(0,0)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "variable", y = NULL)


TigerGeo2016 %>% group_by(Year, Type) %>%
  summarize(Projects = n(), Amount = sum(Amount)) %>% ungroup() %>%
  mutate(Type= fct_reorder(as_factor(Type), Amount,.fun = sum, desc = T)) %>%
  ggplot(aes(factor(Year), Amount, color = Type)) +
  geom_col(aes(fill = Type), position = "dodge") +
  scale_fill_manual(values = TypeCol) +
  scale_color_manual(values = TypeCol) +
  scale_y_continuous(expand = c(0,0), 
                     labels = dollar) +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) 

glimpse(TigerGeo2016)
ProjMapping <- TigerGeo2016 %>% filter(!StateAbb %in% c("GU", "VI", "PR", "AK", "HI"), !is.na(StateAbb)) %>% 
  select(Year, Long, Lat, Type, Project_Name, Amount, StateAbb)

unique(ProjMapping$StateAbb)
states <- get_urbn_map(map = "states")
States <- map_data("state")
TypeCol <- c("#A2CD5A", "#548B54", "#009ACD", "#EEAD0E", "#EE2C2C", "#696969", "#4682B4")
ProjData <- ProjMapping %>% filter(Year == 2009)
States %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group), 
               color = "#ffffff", size = 0.05) +
  geom_point(data = ProjData, aes(Long, Lat, color = Type), alpha = 0.75, size = 7) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_color_manual(values = TypeCol) +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()) + 
  labs(title = paste("Capital Projects in", "input$year"), 
       subtitle = "lower 48 states",
       caption = "detailed information on Data page")



summary <- TigerGeo2016 %>% filter(Year  == 2009) %>%
  group_by(Type) %>%  
  summarise(Projects = n(), 
            Dollars = sum(Amount))%>% ungroup()  

func <- function(x) {
  if(is.numeric(x)) {sum(x)} else { ""}
}
sumrow <- as_tibble(lapply(summary, func)) %>% mutate(Type = "TOTAL")

table <- bind_rows(summary, sumrow) %>% mutate(Dollars = dollar(Dollars))



TGDNew <- TigerGeo2016 %>% select(Year, StateAbb, Type, Amount, 
                                  Project_Name, Applicant, Admin, Urban_Rural) %>%
  mutate(Amount = dollar(Amount))

save(TGDNew, file = )

load("~/R-files/projects/TigerResearch/TigerGrant/Data/TGDNew.RData")

states <- get_urbn_map("states")


library(maps)
mapStates <- map("state", fill = F)
leaflet(data = mapStates) %>% addCircles(data = ProjData)
