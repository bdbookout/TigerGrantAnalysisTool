library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(ggmap)
library(scales)
library(RColorBrewer)
library(plotly)
library(lubridate)
library(urbnmapr)
library(shinythemes)

load("~/R-files/projects/TigerResearch/TigerGrant/Data/TGC.RData")
load("~/R-files/projects/TigerResearch/TigerGrant/Data/TGCC.RData")
load("~/R-files/projects/TigerResearch/TigerGrant/Data/AllCongress.RData")
load("~/R-files/projects/TigerResearch/TigerGrant/Data/TGD48.RData")
load("~/R-files/projects/TigerResearch/TigerGrant/Data/StateSummary.RData")
load("~/R-files/projects/TigerResearch/TigerGrant/Data/TGProjDet.RData")
load("~/R-files/projects/TigerResearch/TigerGrant/Data/StateAnalysis.RData")
load("~/R-files/projects/TigerResearch/TigerGrant/Data/TigerGeo2016.RData")
load("~/R-files/projects/TigerResearch/TigerGrant/Data/TGDNew.RData")

partycolours <- c("#F53513EF", "#1B81DBFE")
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
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
   
   # Application title
   titlePanel("TIGER Grant Analysis Tool"),
   
    tabsetPanel(
      tabPanel("TIGER Grant Summary",
               fluidRow(
                 column(3, 
                        br(),
                        br(),
                        br(),
                        br(),
                        h4("TIGER Discetionary Grant Program"),
                        br(),
                        p("The Transportation Investment Generating Economic Recovery (TIGER) discretionary
                          grant program was established in 2009 and as of 2018, will become the BUILD
                          grant program."),
                        p("The program has had broad eligibility for infrastructure projects that otherwise don't qualify for 
                          federal funds due to type of project or applicant.  It has been a way to provide dollars for road, rail, transit and port projects
                          that achieve critical national objectives."),
                        p("This analysis tool was created to explore the available TIGER Grant data and provide details about how the funds have been allocated.")),
                 br(),
                 column(9, plotOutput("TDMap", height = 600))
               ),
               
               fluidRow(
                 column(12, plotOutput("TDSummary", height = 300))
               ) 
               
         ),
   tabPanel("Party Analysis",
            fluidRow(
              column(6, 
                fluidRow(
                         column(9, 
                                br(),
                                h3("Grants by State Party Majority"),
                                p("Is there a visible trend in how funds are allocated? Scoll by year to examine.")),
                         column(3, 
                                wellPanel(
                                selectInput("Year1",
                                               "Select Year",
                                               c("2009-2017", 2009, 2010, 2011, 2012, 2013, 
                                                 2014, 2015, 2016, 2017), selected = 2017)))),
                fluidRow(
                  br(),
                  br(),
                  column(12, plotOutput("RegYr", height = 200))),
                fluidRow(
                  br(),
                  br(),
                column(12, plotOutput("AnParty", height = 400))
                )
                  
              ),
              column(6,
                     fluidRow(
                       column(12, plotOutput("statedet", height = 400))),
                     fluidRow(
                       column(12, plotOutput("stateprojdet", height = 400))
                     )
                     ))),
   tabPanel("Grant Details",
            br(),
            br(),
            fluidRow(
                     column(2,
                            wellPanel(
                            fluidRow(
                              selectInput("type", "Select", c("Projects", "Amount"))
                            ),
                            fluidRow(
                              selectInput("year2", 
                                          "Select Year", 
                                          c(2009, 2010, 2011, 2012, 2013, 
                                            2014, 2015, 2016, 2017))
                            ))),
                     column(6, plotOutput("typeplot", height = 300)),
                     column(4, tableOutput("dtproj"))
                     ),
            fluidRow(
                     column(12, plotOutput("projmap", height = 600))
                     )
            ),
   tabPanel("Data",
            fluidRow(
              br(),
              column(3, selectInput("seldataset", "Select Data Set", 
                                    c("Congressional Information", "Projects by Type"))),
              br(),
              column(3, downloadButton("dldata", label = "Download CSV"))
            ),
            fluidRow(
              br(),
              br(),
              column(12, dataTableOutput("projtable"))
            ))
 ))


server <- function(input, output) {
   
   output$TDSummary <- renderPlot({
     AnSummary <- TGCC %>% group_by(Year) %>% summarize(Dollars = sum(Award), Projects = sum(Projects), 
                                                        HD = sum(HD, na.rm = T), HR = sum(HR, na.rm = T),
                                                        Majority = case_when(HD - HR > 0 ~ "D",
                                                                             TRUE ~ "R"))
     AnSummary %>% ggplot() + 
       geom_col(aes(x = factor(Year), y = Dollars / 1000000, fill = Dollars)) +
       geom_text(aes(x = factor(Year), y = Dollars / 1000000 + 100, 
                     label = paste(dollar(round(Dollars / 1000000)), "M", sep = ""))) + 
       scale_y_continuous(labels = dollar, expand = c(0, 0), limits = c(0, 1700)) + 
       labs(title = "Total TIGER Dollars by Year", y = "$Millions", x = NULL) +
       theme_light() + 
       theme(panel.grid.major.x = element_blank())
        })
   
   output$TDMap <- renderPlot({

     SUD + 
       geom_label(data = StateAnalysis, aes(Longitude, Latitude, 
                                            label = TotalAwards, 
                                            color = Majority), size = 1.75) + 
       coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
       scale_color_manual(values = c("#F53513EF", "#1B81DBFE")) +                                                                               
       labs(title = "TIGER Projects 2007-2017", subtitle = "with number of projects in each state", 
            caption = "color scale represents dollars awarded\nhouse member majority identified by label color",
            color = "Majority Party")
     
   })
   output$AnParty <- renderPlot({
    TGCC %>% group_by(Year, HMaj) %>% summarise(Award = sum(Award), Project = sum(Projects)) %>% ungroup() %>% 
       filter(!is.na(HMaj)) %>% 
       ggplot(aes(x = factor(Year), fill = HMaj)) +
       geom_col(aes(y = Award), position = "dodge", show.legend = F) + 
       geom_text(aes(y = Award + 11000000, label = paste(dollar(round(Award / 1000000)), "M", sep = "")), 
                 position = position_dodge(0.9), size = 3) + 
       labs(title = "TIGER Grant Awards", subtitle = "by State Majority Party", 
            x = NULL, y = "Amount Awarded") +
       scale_y_continuous(labels = dollar_format(), expand = c(0,0), limits = c(0, 1225000000)) + 
       scale_fill_manual(values = c("#F53513EF", "#1B81DBFE", "dark gray")) +
       theme_light() + 
       theme(panel.grid.major.x = element_blank())
     
   })
   RegYrData <- reactive({
     if(input$Year1 != "2009-2017") {
       TGCC %>% filter(Year == input$Year1)
     } else {
       TGCC
     }
      })
   output$RegYr <- renderPlot({
     
     RegYrData() %>% group_by(Region) %>% summarise(Dollars = sum(Award), Projects = sum(Projects), 
                                        Majority = case_when(sum(HD + SD) - sum(HR + SR) > 0 ~ "D",
                                                             TRUE ~ "R")) %>%
       
       ggplot(aes(Region, Dollars / 1000000, fill = Majority)) +
       geom_col(show.legend = F) +
       geom_text(aes(x = Region, y = Dollars / 1000000 + 10, 
                     label = paste(dollar(round(Dollars / 1000000)), "M", sep = "")), size = 3) +
       scale_fill_manual(values = partycolours) +
       scale_x_discrete(expand = c(0,0)) +
       labs(title = "TIGER Awards by Region", subtitle = paste(input$Year1), 
            y = NULL, x = NULL) +
       theme_light() +
       theme(panel.grid = element_blank(),
             panel.border = element_blank(),
             axis.ticks = element_blank(),
             axis.text.y = element_blank())
   })
   StateDetData <- reactive({
     if(input$Year1 == "2009-2017") {
       StateSummary
     } else
       StateDetail <- TGC %>% group_by(State, StateAbb, Year) %>% 
         left_join(AllCongress, by = c("Congress", "StateAbb")) %>%
         summarize(TotalAwards = n(), 
                   TotalDollars = sum(TIGER_Grant_Award),
                   TotDem = sum(HD, na.rm = T) + sum(SD, na.rm = T),
                   TotRep = sum(HR, na.rm = T) + sum(SR, na.rm = T),
                   Majority = case_when(TotDem - TotRep > 0 ~ "D",
                                        TRUE ~ "R")) %>% 
         arrange(desc(TotalDollars), TotalAwards) %>% ungroup() %>% 
         filter(Year == input$Year1)
        
   })
   output$statedet <- renderPlot({
     StateDetData() %>% mutate(State = fct_reorder(as_factor(State), 
                                                   TotalDollars, .fun = sum, desc = T)) %>% 
       ggplot(aes(State, TotalDollars)) + 
       geom_bar(stat = "identity", aes(fill = Majority)) + 
       scale_fill_manual(values = partycolours) +
       scale_y_continuous(labels = dollar, expand = c(0,0)) +
       theme_light() +
       theme(panel.grid.major.y = element_blank(),
             panel.grid.minor.x = element_blank()) +
       coord_flip() + 
       labs(title = paste("TIGER Grant Dollars", input$Year1), x = NULL, y = "Total Dollars Awarded")
     
   })
   output$stateprojdet <- renderPlot({
     StateDetData() %>% mutate(State = fct_reorder(as_factor(State), 
                                                   TotalAwards, .fun = sum, desc = T)) %>% 
       ggplot(aes(State, TotalAwards)) + 
       geom_bar(stat = "identity", aes(fill = Majority)) + 
       scale_fill_manual(values = partycolours) +
       scale_y_continuous(expand = c(0,0)) +
       theme_light() +
       theme(panel.grid.major.y = element_blank(),
             panel.grid.minor.x = element_blank()) +
       coord_flip() + 
       labs(title = paste("TIGER Grant Projects", input$Year1), x = NULL, y = "Total Projects Awarded")
     
   })
   SelectType <- reactive({
     if(input$type == "Projects") {
       "Projects"
     } else { "Amount / 1000000"
     }
        })
   TypeLabels <- reactive({
     if(input$type == "Amount") {
       dollar
     } else { number }
   })
   output$typeplot <- renderPlot({
     TypeCol <- c("#A2CD5A", "#548B54", "#009ACD", "#EEAD0E", "#EE2C2C", "#696969", "#4682B4")
     TigerGeo2016 %>% group_by(Year, Type) %>%
       summarize(Projects = n(), Amount = sum(Amount)) %>% ungroup() %>%
       mutate(Type= fct_reorder(as_factor(Type), Amount,.fun = sum, desc = T)) %>%
       ggplot(aes_string("factor(Year)", SelectType(), color = "Type")) +
       geom_col(aes(fill = Type), position = "dodge") +
       scale_fill_manual(values = TypeCol) +
       scale_color_manual(values = TypeCol) +
       scale_y_continuous(expand = c(0,0), labels = TypeLabels()) +
       theme_light() +
       theme(panel.grid.major.x = element_blank()) +
       labs(title = paste("Grant type by", input$type), x = NULL, y = input$type)
     
   })
   output$projmap <- renderPlot({
     ProjMapping <- TigerGeo2016 %>% filter(!StateAbb %in% c("GU", "VI", "PR", "AK", "HI"), !is.na(StateAbb)) %>% 
       select(Year, Long, Lat, Type, Project_Name, Amount, StateAbb)
     
     States <- map_data("state")
     TypeCol <- c("#A2CD5A", "#548B54", "#009ACD", "#EEAD0E", "#EE2C2C", "#696969", "#4682B4")
     ProjData <- ProjMapping %>% filter(Year == input$year2)
     States %>% 
       ggplot() +
       geom_polygon(aes(long, lat, group = group), 
                    color = "#ffffff", size = 0.05) +
       geom_point(data = ProjData, aes(Long, Lat, color = Type), alpha = 0.75, size = 7) +
       geom_text(data = ProjData, aes(Long + 1.5, Lat, 
                                      label = paste(dollar(round(Amount/1000000)), "M", sep = ""))) +
       coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
       scale_color_manual(values = TypeCol) +
       theme_light() +
       theme(axis.line = element_blank(),
             axis.ticks = element_blank(),
             axis.text = element_blank(),
             panel.border = element_blank(),
             panel.grid = element_blank(),
             axis.title = element_blank()) + 
       labs(title = paste("Capital Projects in", input$year2), 
            subtitle = "lower 48 states",
            caption = "detailed information on Data page")
   })
   
   
   output$dtproj <- renderTable({
     summary <- TigerGeo2016 %>% filter(Year  == input$year2) %>%
       group_by(Type) %>%  
       summarise(Projects = n(), 
                 Dollars = sum(Amount))%>% ungroup()  
     func <- function(x) {
       if(is.numeric(x)) {sum(x)} else { ""}
     }
     sumrow <- as_tibble(lapply(summary, func)) %>% mutate(Type = "TOTAL")
     bind_rows(summary, sumrow) %>% mutate(Dollars = dollar(Dollars))
   })
   datatables <- reactive({
     if(input$seldataset == "Congressional Information") {
       TGProjDet
     } else { TGDNew }
   })
   output$projtable <- renderDataTable({
     datatables()
   })
   output$dldata <- downloadHandler(
     filename = function() {
       paste(input$seldataset, ".csv", sep = "")
       },
     content = function(file) {
       write.csv(datatables(), file, row.names = F)  
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

