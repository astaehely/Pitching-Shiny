############### CREATED AND CODED BY AARON STAEHELY (X: AARON_S12) FOR COASTAL CAROLINA BASEBALL ###############


library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(scales)
library(readr)
library(tidyverse)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(ggplot2)
library(GeomMLBStadiums)

pitch_colors = c("Fastball" = '#FA8072', 
                 "Sinker" = "#fdae61", 
                 "Slider" = "#A020F0", 
                 "Sweeper" = "magenta",
                 "Curveball" = '#2c7bb6', 
                 "ChangeUp" = '#90EE90',
                 "Splitter" = '#90EE32',
                 "Cutter" = "red")

data <-  read.csv("test.csv")
#data <- read_excel("/Users/aaronstaehely/Library/CloudStorage/OneDrive-CoastalCarolinaUniversity/Coastal Carolina Baseball Analytics/Trackman CSV's/Intrasquad/Pre-Season Spring/weeks1n2.xlsx")

data[data == ""] <- NA

data <- data %>%
  mutate(
    Outing = Date
  )

data$Date <- as.Date(data$Date, format = "%m/%d/%y")

data <- data %>%
  # FILTER
  filter(PitcherTeam %in% c('SOU_GAM'))
# CHANGE DATE WHEN NECESSARY
#Date == max(Date))


data <- data %>%
  filter(
    !is.na(RelSpeed)
  )

data <- data %>%
  filter(
    !is.na(Date)
  )

spraydata <- data %>%
  filter(
    PitchCall == 'InPlay'
  )

spraydata <- spraydata %>%
  mutate(hc_bearing = Bearing * (3.14159/180),
         hc_x = Distance * sin(hc_bearing),
         hc_y = Distance * cos(hc_bearing))



data <- data %>%
  filter(
    PitchCall != 'Undefined' | TaggedPitchType != 'Undefined'
  )%>%
  mutate(
    FBindicator = ifelse(TaggedPitchType == 'Fastball' | TaggedPitchType == 'Sinker', 1, 0),
    OSindicator = ifelse(TaggedPitchType %in% c("Slider", "Cutter", "Curveball", "ChangeUp"), 
                         1, 0),
    EarlyIndicator = ifelse(
      ((Balls == 0 & Strikes == 0 & PitchCall == "InPlay") |
         (Balls == 1 & Strikes == 0 & PitchCall == "InPlay") |
         (Balls == 0 & Strikes == 1 & PitchCall == "InPlay") |
         (Balls == 1 & Strikes == 1 & PitchCall == "InPlay")), 
      1, 0),
    AheadIndicator = ifelse(
      ((Balls == 0 & Strikes == 1) & (PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable",'FoulBall'))) |
        ((Balls == 1 & Strikes == 1) & (PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable",'FoulBall'))), 
      1, 0),
    StrikeZoneIndicator = ifelse(
      PlateLocSide >= -0.8333 & PlateLocSide <= 0.8333 & 
        PlateLocHeight >= 1.5 & PlateLocHeight <= 3.37467, 
      1, 0),
    EdgeHeightIndicator = ifelse(
      ((PlateLocHeight > 14/12 & PlateLocHeight < 22/12) |
         (PlateLocHeight > 38/12 & PlateLocHeight < 46/12)), 
      1, 0),
    EdgeZoneHtIndicator = ifelse(
      PlateLocHeight > 16/12 & PlateLocHeight < 45.2/12, 
      1, 0),
    EdgeZoneWIndicator = ifelse(
      PlateLocSide > -13.4/12 & PlateLocSide < 13.4/12, 
      1, 0),
    EdgeWidthIndicator = ifelse(
      ((PlateLocSide > -13.3/12 & PlateLocSide < -6.7/12) |
         (PlateLocSide < 13.3/12 & PlateLocSide > 6.7/12)), 
      1, 0),
    HeartIndicator = ifelse(
      PlateLocSide >= -0.5583 & PlateLocSide <= 0.5583 & 
        PlateLocHeight >= 1.83 & PlateLocHeight <= 3.5, 
      1, 0),
    StrikeIndicator = ifelse(
      PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBallNotFieldable",'FoulBall', "InPlay"), 
      1, 0),
    WhiffIndicator = ifelse(
      PitchCall == 'StrikeSwinging',1,0
    ),
    SwingIndicator = ifelse(
      PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable", 'FoulBall',"InPlay"), 
      1, 0),
    LHHindicator = ifelse(
      BatterSide == 'Left', 1,0
    ),
    RHHindicator = ifelse(
      BatterSide == 'Right', 1,0
    ),
    ABindicator = ifelse(
      PlayResult %in% c("Error", "FieldersChoice", "Out", "Single", "Double", "Triple", "HomeRun") | 
        KorBB == "Strikeout", 
      1, 0),
    HitIndicator = ifelse(
      PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 
      1, 0),
    FPindicator = ifelse(Balls == 0 & Strikes == 0, 1,0),
    PAindicator = ifelse(
      PitchCall %in% c("InPlay", "HitByPitch", "CatchersInterference") | 
        KorBB %in% c("Walk", "Strikeout"), 
      1, 0),
    LeadOffIndicator = ifelse(
      (PAofInning == 1 & (PlayResult != "Undefined" | KorBB != "Undefined")) | 
        PitchCall == "HitByPitch", 
      1, 0),
    HBPIndicator = ifelse(
      PitchCall == 'HitByPitch',1,0),
    WalkIndicator = ifelse(
      KorBB == 'Walk',1,0
    ),
    BIPind = ifelse(
      PitchCall == 'InPlay', 1, 0
    ),
    SolidContact = ifelse(
      (PitchCall == "In Play" & 
         ((ExitSpeed > 95 & Angle >= 0 & Angle <= 40) | 
            (ExitSpeed > 92 & Angle >= 8 & Angle <= 40))), 1, 0),
    HHindicator = ifelse(PitchCall=='InPlay' & ExitSpeed > 95,1,0),
    biphh = ifelse(PitchCall == 'InPlay' & ExitSpeed > 15,1,0)
  )

data <- data %>%
  mutate(
    FBstrikeind = ifelse(
      (PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall",'FoulBallNotFieldable', "InPlay")) & 
        (FBindicator == 1), 
      1, 0),
    OSstrikeind = ifelse(
      (PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall",'FoulBallNotFieldable', "InPlay")) & 
        (OSindicator == 1), 
      1, 0),
    EdgeIndicator = ifelse(
      (EdgeHeightIndicator == 1 & EdgeZoneWIndicator == 1) | 
        (EdgeWidthIndicator == 1 & EdgeZoneHtIndicator == 1), 
      1, 0),
    QualityPitchIndicator = ifelse(
      StrikeZoneIndicator == 1 | EdgeIndicator == 1, 
      1, 0),
    FPSindicator = ifelse(
      PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable",'FoulBall', "InPlay") &
        (FPindicator == 1),
      1, 0),
    OutIndicator = ifelse(
      (PlayResult %in% c("Out", "FieldersChoice") | KorBB == "Strikeout") & HBPIndicator == 0, 
      1, 0),
    LOOindicator = ifelse(
      LeadOffIndicator == 1 & OutIndicator == 1, 
      1, 0),
    Zwhiffind = ifelse(
      WhiffIndicator == 1 & StrikeZoneIndicator == 1, 1,0),
    Zswing = ifelse(
      StrikeZoneIndicator == 1 & SwingIndicator == 1, 1,0
    ),
    GBindicator = ifelse(TaggedHitType=='GroundBall',1,0),
    Chaseindicator = ifelse(SwingIndicator==1 & StrikeZoneIndicator == 0,1,0),
    OutofZone = ifelse(StrikeZoneIndicator == 0,1,0),
    OnBaseindicator = ifelse(
      PlayResult %in% c("Single", "Double", "Triple", "HomeRun") | 
        KorBB == "Walk" | 
        PitchCall == "HitByPitch", 
      1, 0),
    totalbases = ifelse(PlayResult == "Single", 1, 
                        ifelse(PlayResult == "Double", 2, 
                               ifelse(PlayResult == "Triple", 3, 
                                      ifelse(PlayResult == "HomeRun", 4, 0))))
  )

data <- data %>%
  mutate(PlayResult = ifelse(is.na(PlayResult) | PlayResult == "Undefined", PitchCall, PlayResult))

data$TaggedPitchType <- factor(data$TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball", "Slider", "ChangeUp", "Splitter", 'Knuckleball', 'Other'))

data2 <- data



ui <- fluidPage(
  tags$head(
    # Link to the custom font file in the www folder
    tags$style(
      HTML("
        @font-face {
          font-family: 'Avenir';  # Name for the font
          src: url('Avenir.ttc') format('truetype');  # Path to the font file in www
        }

        body {
          font-family: 'Avenir', sans-serif;  # Apply the custom font to the entire body
        }
        #name {
        font-size: 8px;
          font-weight: bold;
          text-align: right;
          margin-bottom: 5px;
        }
      ")
    )
  ),
  tags$div(id = "name", "Created by: Aaron Staehely"),
  tags$img(src = "Own It Bronze.png", height = "80px", width = "auto"),
  titlePanel("Pitcher Data"),
  tabsetPanel(
    tabPanel('Pitcher Arsenals',
             fluidRow(
               #style = 'width: 250px',
               column(2,selectInput("pitcher", "Pitcher", choices = unique(data$Pitcher))),
               column(3,dateRangeInput("dateRange", "Date Range", 
                                       start = min(data$Date), 
                                       end = max(data$Date))),
               column(2,pickerInput(
                 inputId = "BatterHand",
                 label = HTML("Batter Hand"),
                 choices = unique(data2$BatterSide),
                 selected = unique(data2$BatterSide),
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE
               )),
               column(2, sliderInput("veloRange", "Velocity Range", 
                                     min = min(data$RelSpeed, na.rm = TRUE), 
                                     max = max(data$RelSpeed, na.rm = TRUE), 
                                     value = c(min(data$RelSpeed, na.rm = TRUE), max(data$RelSpeed, na.rm = TRUE)),
                                     step = 0.5))
             ),
             fluidRow(
               mainPanel(
                 width = 12,
                 #style = 'width: calc(100% - 150px);',
                 h3("Overview"),
                 #div(style = "width: 100%; overflow: auto;", 
                 dataTableOutput('selected_row'),
                 column(6,
                        div(style = "text-align: center;", h3("Movement Plot")),
                        plotOutput("mvmt_plot")),
                 column(6,       
                        div(style = "text-align: center;", h3("Release Plot")),
                        plotOutput("relplot")),
                 h3("Pitch Data"),
                 tableOutput("movement_table"),
                 h3("vRHH"),
                 tableOutput("vrhh_table"),
                 h3("vLHH"),
                 tableOutput("vlhh_table"),
                 h3("By Date"),
                 pickerInput(
                   inputId = "pitchtype",
                   label = HTML("Pitch Type"),
                   choices = unique(data2$TaggedPitchType),
                   selected = unique(data2$TaggedPitchType),
                   options = list(`actions-box` = TRUE),
                   multiple = TRUE
                 ),
                 dataTableOutput("by_date_table"),
                 h3("Heatmaps"),
                 selectInput("heatmap_filter", "Select Filter for Heatmap:", 
                             choices = c("All Pitches", ">95 EV", "Whiffs"), 
                             selected = "All Pitches"),
                 fluidRow(column(12,plotOutput("heatmap"))
                 )
               ),
               
               
             )
    ),
    tabPanel('Pitching Leaderboard',
             fluidRow(
               #style = 'width: 250px',
               column(3,dateRangeInput("dateRange", "Date Range", 
                                       start = min(data$Date), 
                                       end = max(data$Date))),
               column(2,pickerInput(
                 inputId = "BatterHand",
                 label = HTML("Batter Hand"),
                 choices = unique(data2$BatterSide),
                 selected = unique(data2$BatterSide),
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE
               )),
               column(2,pickerInput(
                 inputId = "PitcherHand",
                 label = HTML("Pitcher Hand"),
                 choices = unique(data2$PitcherThrows),
                 selected = unique(data2$PitcherThrows),
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE
               ))
               
             ),
               mainPanel(
                 width = 11,
                 dataTableOutput("statistics_table")
               )
             ),
    tabPanel('vCCU Spray Charts',
             fluidPage(
               #column(3, selectInput("pitcher", "Pitcher", choices = unique(data$Pitcher))),
               column(2,pickerInput(
                 inputId = "BatterHand",
                 label = HTML("Batter Hand"),
                 choices = unique(data2$BatterSide),
                 selected = unique(data2$BatterSide),
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE
               )),
              
                         plotOutput('spraychart',width='100%', height='800px'))
             )
    )
  )



server <- function(input, output) {
  
  mound_curve <- data.frame(
    x = c(seq(-3, 3, length.out = 100), rev(seq(-3, 3, length.out = 100))),
    y = c(rep(0, 100), sqrt(9 - seq(-3, 3, length.out = 100)^2) * 0.15)  # Adjust multiplier for mound height
  )
  
  filtered_data <- reactive({
    data %>% filter(Pitcher == input$pitcher, Date >= as.Date(input$dateRange[1]) & 
                      Date <= as.Date(input$dateRange[2]),
                    BatterSide %in% c(input$BatterHand),
                    RelSpeed >= input$veloRange[1] & RelSpeed <= input$veloRange[2])
  })
  
  filtered_data_heatmap <- reactive({
    req(input$heatmap_filter, input$dateRange, input$BatterHand)
    
    data_filtered <- data %>% 
      filter(Pitcher == input$pitcher,Date >= as.Date(input$dateRange[1]) & Date <= as.Date(input$dateRange[2]),
             BatterSide %in% input$BatterHand,
             !is.na(PlateLocSide), !is.na(PlateLocHeight))
    
    # Apply custom filters based on the selected heatmap filter
    if (input$heatmap_filter == ">95 EV") {
      data_filtered <- data_filtered %>% filter(ExitSpeed >= 95)
    } else if (input$heatmap_filter == "Whiffs") {
      data_filtered <- data_filtered %>% filter(WhiffIndicator == 1)
    }
    
    return(data_filtered)
  })
  
  output$selected_row <- renderDataTable({
    req(input$pitcher)
    
    pitcher_row <- processed_data() %>% filter(Pitcher == input$pitcher)
    
    DT::datatable(
      pitcher_row,
      options = list(paging = FALSE, ordering = TRUE, searching = FALSE)
    ) %>%
      formatStyle(
        'Avg FB Velo',
        backgroundColor = styleInterval(brks, clrs)
      ) %>%
      formatStyle(
        'Max FB Velo',
        backgroundColor = styleInterval(brks2,clrs2)
      )%>%
      formatRound('Strike %', digits = 1 )%>%
      formatRound('FB Strike %', digits = 1) %>%
      formatRound('OS Strike %', digits = 1) %>%
      formatRound('E+A %', digits=1) %>%
      formatRound('1PK %', digits=1) %>%
      formatRound('HH %', digits=1)%>%
      formatRound('GB %', digits=1)%>%
      formatStyle(
        'Strike %',
        backgroundColor = styleInterval(brks3, clrs3)
      )%>%
      formatStyle(
        'FB Strike %',
        backgroundColor = styleInterval(brks4,clrs4)
      ) %>%
      formatStyle(
        'OS Strike %',
        backgroundColor = styleInterval(brks5,clrs5))%>%
      formatStyle(
        'E+A %',
        backgroundColor = styleInterval(brksepa,clrsepa)
      )%>%
      formatStyle(
        '1PK %',
        backgroundColor = styleInterval(brks6,clrs6)
      )%>%
      formatStyle(
        'Whiff %',
        backgroundColor = styleInterval(
          brks7, clrs7))%>%
      formatStyle(
        'IZ Whiff %',
        backgroundColor = styleInterval(
          brks8,clrs8
        )
      )%>%
      formatStyle(
        'Chase %',
        backgroundColor = styleInterval(
          brkschase,clrschase
        )
      )%>%
      formatStyle(
        'K %',
        backgroundColor = styleInterval(
          brks9,clrs9
        )
      )%>%
      formatStyle(
        'BB %',
        backgroundColor = styleInterval(
          brksbb, clrsbb
        )
      )%>%
      formatStyle(
        'AVG',
        backgroundColor = styleInterval(
          brksavg, clrsavg
        )
      )%>%
      formatStyle(
        'OPS',
        backgroundColor = styleInterval(
          brksops, clrsops
        )
      )%>%
      formatStyle(
        'HH %',
        backgroundColor = styleInterval(
          brkshh, clrshh
        )
      )%>%
      formatStyle(
        'GB %',
        backgroundColor = styleInterval(
          brksgb, clrsgb
        )
      )
    # Ensure the pitcher is selected
    
    # Filter the leaderboard data to find the selected pitcher row
    
    # Return the selected pitcher row (if exists)
  })
  
  
  output$mvmt_plot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(HorzBreak, InducedVertBreak)) +
      geom_point(aes(fill = TaggedPitchType), color = 'black',pch=21,na.rm = TRUE, alpha = .99, size = 2.5) +
      scale_fill_manual(values = pitch_colors) + # make a scatterplot, alpha controls the opacity of the plot
      scale_x_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5)) + # set limits of the scale and have ticks every 5
      scale_y_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5), position = 'left') + # color the points by the pitch colors set earlier
      geom_hline(yintercept = 0, linetype = 2) + # these two lines create dashed lines at the intercepts
      geom_vline(xintercept = 0, linetype = 2) + 
      theme_bw() + theme(legend.position = "right", axis.title = element_blank(),legend.text = element_text(size = 18),
                         legend.title = element_blank()) + # sets the plot theme, base_size should always be used! increases the size of text and other visual aids
      coord_fixed()
  })
  
  output$relplot <- renderPlot({
    filtered_data() %>%
      ggplot() +
      geom_point(aes(x = RelSide, y = RelHeight, fill = TaggedPitchType), color = 'black',pch=21,na.rm = TRUE, alpha = .99, size = 2.5) + 
      scale_fill_manual(values = pitch_colors) +
      scale_x_continuous(
        limits = c(-3,3)
      ) +
      scale_y_continuous(
        limits = c(0,7)
      ) + 
      geom_polygon(data = mound_curve, aes(x = x, y = y), fill = 'brown', color = 'black', inherit.aes = FALSE) +
      geom_polygon(data = data.frame(x = c(-1, 1, 1, -1), y = c(0.3, 0.3, 0.45, 0.45)),
                   aes(x = x, y = y), fill = 'white', color = 'black', inherit.aes = FALSE) +
      scale_color_manual(values = pitch_colors) +
      theme_void() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
                           plot.background = element_rect(color = "black", fill = NA, size = .5)) +
      theme(legend.position = "none", axis.title = element_blank())  +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
      ) + coord_fixed()
  })
  
  output$heatmap <- renderPlot({
    filtered_data_heatmap() %>%
      ggplot( 
      aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d(aes(fill = after_stat(nlevel)), geom = 'polygon', alpha = 0.8) +
      scale_fill_gradientn(colours = c('#87A1CE','white','red'))+
      #geom_point(aes(x=PlateLocSide,y=PlateLocHeight,fill = TaggedPitchType), color = 'black',pch=21,na.rm = TRUE, alpha = .99, size = 2) +
      xlim(-4,4) + ylim(-.25,6) + labs(color = "")+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .5, color = "black") + 
      #geom_rect(aes(xmin = -0.5583, xmax = 0.5583, ymin = 1.83, ymax = 3.17), alpha = 0, size = .5, color = "black", linetype = 'dotdash') +
      #geom_rect(aes(xmin = -1.108, xmax = 1.108, ymin = 1.167, ymax = 3.83), alpha = 0, size = .5, color = "black", linetype = 'dotdash') +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = .25, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = .25, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = .25, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = .25, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = .25, color = "black") +
      #stat_ellipse(aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType, fill = TaggedPitchType),
      #geom = 'polygon', level = 0.4, alpha = 0.1) +
      # , na.rm = TRUE)+
      theme_void() + 
      theme(legend.position = "none", axis.title = element_blank(),panel.border = element_rect(color = "black", fill = NA, size = .5))  +
      theme(strip.text = element_text(size = 20, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank()
            #remove y axis labels
      ) + coord_fixed() + facet_wrap(~TaggedPitchType, ncol=4)
  })
  
  output$movement_table <- renderTable({
    filtered_data() %>% 
      #filter(Date != 'Fall') %>%
      group_by('Pitch' = TaggedPitchType) %>% 
      summarise(
        'Total' = n(),
        'Avg Velo' = round(mean(RelSpeed, na.rm = TRUE), 1),
        'Max Velo' = round(max(RelSpeed, na.rm = TRUE), 1),
        'Avg Spin' = round(mean(SpinRate, na.rm = TRUE), 0),
        'Avg IVB' = round(mean(InducedVertBreak, na.rm = TRUE), 1),
        'Avg HB' = round(mean(HorzBreak, na.rm = TRUE), 1),
        'Avg Rel Ht (ft)' = round(mean(RelHeight, na.rm = TRUE), 2),
        'Avg Rel Side (ft)' = round(mean(RelSide, na.rm = TRUE), 2),
        'Avg Ext (ft)' = round(mean(Extension, na.rm = TRUE), 2)
      )
    
  })
  
  output$vrhh_table <- renderTable({
    filtered_data() %>%
      filter(BatterSide == "Right" ) %>%
      dplyr::group_by('Pitch' = TaggedPitchType) %>%
      summarise(
        '#' = n(),
        # Calculate vLHH and vRHH as percentages of total LHH and RHH pitcher
        #'%' = paste0(round(sum(RHHindicator & (TaggedPitchType == Pitch), na.rm = TRUE) /
        #                    total_RHH * 100, 1), '%'),
        'Strike %' = paste0(round(sum(StrikeIndicator, na.rm = TRUE)/n(),3)*100,'%'),
        'AVG' = round(sum(HitIndicator,na.rm=TRUE)/sum(ABindicator,na.rm=TRUE),3),
        'SLG' = round(sum(totalbases, na.rm=TRUE)/sum(ABindicator, na.rm=TRUE),3),
        'Whiff %' = paste0(round(sum(WhiffIndicator, na.rm = TRUE)/sum(SwingIndicator),3)*100,"%"),
        'Z-Whiff %' = paste0(round(sum(Zwhiffind, na.rm = TRUE)/sum(Zswing, na.rm = T),3)*100,"%"),
        'Chase %' = round(sum(Chaseindicator, na.rm=T)/sum(OutofZone, na.rm=T),3)*100,
        'Zone %' = paste0(round(sum(StrikeZoneIndicator, na.rm=TRUE)/n(),3)*100,'%'),
        'Comp. P %' = paste0(round(sum(QualityPitchIndicator, na.rm = TRUE)/n(),2)*100,'%'),
        'HH %' = round(sum(HHindicator, na.rm=T)/sum(biphh,na.rm=T),3)*100,
        'GB %' = round(sum(GBindicator, na.rm = T)/sum(BIPind,na.rm=T),3)*100
        #'Edge %' = paste0(round(sum(EdgeIndicator, na.rm = TRUE)/n(),3)*100,'%'),
        #'BIP' = sum(BIPind),
        #'SC' = sum(SolidContact) 
      ) 
  })
  
  # vLHH Table (Filtered for Left-Handed Hitters)
  output$vlhh_table <- renderTable({
    filtered_data() %>%
      filter(BatterSide == "Left") %>%
      dplyr::group_by('Pitch' = TaggedPitchType) %>%
      summarise(
        '#' = n(),
        # Calculate vLHH and vRHH as percentages of total LHH and RHH pitcher
        #'%' = paste0(round(sum(LHHindicator & (TaggedPitchType == Pitch), na.rm = TRUE) /
        #                    total_LHH * 100, 1), '%'),
        'Strike %' = paste0(round(sum(StrikeIndicator, na.rm = TRUE)/n(),3)*100,'%'),
        'AVG' = round(sum(HitIndicator,na.rm=TRUE)/sum(ABindicator,na.rm=TRUE),3),
        'SLG' = round(sum(totalbases, na.rm=TRUE)/sum(ABindicator, na.rm=TRUE),3),
        'Whiff %' = paste0(round(sum(WhiffIndicator, na.rm = TRUE)/sum(SwingIndicator),3)*100,"%"),
        'Z-Whiff %' = paste0(round(sum(Zwhiffind, na.rm = TRUE)/sum(Zswing, na.rm = T),3)*100,"%"),
        'Chase %' = round(sum(Chaseindicator, na.rm=T)/sum(OutofZone, na.rm=T),3)*100,
        'Zone %' = paste0(round(sum(StrikeZoneIndicator, na.rm=TRUE)/n(),3)*100,'%'),
        'Comp. P %' = paste0(round(sum(QualityPitchIndicator, na.rm = TRUE)/n(),2)*100,'%'),
        'HH %' = round(sum(HHindicator, na.rm=T)/sum(biphh,na.rm=T),3)*100,
        'GB %' = round(sum(GBindicator, na.rm = T)/sum(BIPind,na.rm=T),3)*100
        #'Edge %' = paste0(round(sum(EdgeIndicator, na.rm = TRUE)/n(),3)*100,'%'),
        #'BIP' = sum(BIPind),
        #'SC' = sum(SolidContact) 
      )
  })
  
  by_date_data <- reactive({
    #req(input$dateRange, input$BatterHand)
    by_date_data2 <- data %>% filter(Pitcher == input$pitcher,Date >= as.Date(input$dateRange[1]) & 
                                         Date <= as.Date(input$dateRange[2]),
                                       BatterSide %in% c(input$BatterHand),
                                       PitcherThrows %in% c(input$PitcherHand),
                                       RelSpeed >= input$veloRange[1] & RelSpeed <= input$veloRange[2],
                                     TaggedPitchType %in% c(input$pitchtype))
  
    by_date_summary <- by_date_data2 %>%
      dplyr::group_by('Pitch' = TaggedPitchType, Outing) %>%
      dplyr::summarise(
        'Total' = n(),
        'Avg Velo' = round(mean(RelSpeed, na.rm = TRUE),1),
        'Max Velo' = round(max(RelSpeed, na.rm = TRUE),1),
        'Avg Spin' = round(mean(SpinRate, na.rm = TRUE),0),
        #'Max Spin' = round(max(SpinRate, na.rm = TRUE),1),
        'Avg IVB' = round(mean(InducedVertBreak, na.rm = TRUE),1),
        'Avg HB' = round(mean(HorzBreak, na.rm = TRUE),1),
        'Avg Rel Ht (ft)' = round(mean(RelHeight, na.rm=T), 2),
        'Avg Rel Side (ft)' = round(mean(RelSide, na.rm=T),2),
        'Avg Ext (ft)' = round(mean(Extension, na.rm=T),2)
      )
  })
  
  # By Date Table (Grouped by Date)
  output$by_date_table <- renderDataTable({
    by_date_data() %>%
      DT::datatable(
        by_date_data(),
        options = list(paging = FALSE, ordering = TRUE, searching=FALSE)
      ) 
  })
  
  #data2$Date <- as.Date(data2$Date, format = "%Y-%m-%d")
  # Process data and calculate statistics
  processed_data <- reactive({
    #req(input$dateRange, input$BatterHand)
    filtered_data2 <- data2 %>% filter(Date >= as.Date(input$dateRange[1]) & 
                                         Date <= as.Date(input$dateRange[2]),
                                       BatterSide %in% c(input$BatterHand),
                                       PitcherThrows %in% c(input$PitcherHand),
                                       RelSpeed >= input$veloRange[1] & RelSpeed <= input$veloRange[2])
    
  
    
    # Calculate statistics for each pitcher
    summary_data <- filtered_data2 %>%
      dplyr::group_by(Pitcher) %>%
      dplyr::summarize(
        'BF' = n_distinct(Inning, Batter, PAofInning),
        'Avg FB Velo' = round(mean(RelSpeed[TaggedPitchType == "Fastball" | TaggedPitchType == 'Sinker'], na.rm = TRUE), 1),
        'Max FB Velo' = round(max(RelSpeed[TaggedPitchType == "Fastball" | TaggedPitchType == 'Sinker'], na.rm = TRUE), 1),
        'Strike %' = round(sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall",'FoulBallNotFieldable', "InPlay")) / n(),3)*100,
        'FB Strike %' = round(sum(FBstrikeind) / sum(FBindicator),3) * 100,
        'OS Strike %' = round(sum(OSstrikeind)/ sum(OSindicator),3)*100,
        'E+A %' = round((sum(EarlyIndicator,na.rm=TRUE)+sum(AheadIndicator,na.rm=TRUE))/sum(PAindicator,na.rm=TRUE),3)*100,
        '1PK %' = round(sum(FPSindicator)/sum(FPindicator),3)*100,
        'Whiff %' = round(sum(WhiffIndicator)/sum(SwingIndicator),3)*100,
        'IZ Whiff %' = round(sum(Zwhiffind, na.rm=T)/sum(Zswing,na.rm=T),3)*100,
        'Chase %' = round(sum(Chaseindicator, na.rm=T)/sum(OutofZone, na.rm=T),3)*100,
        'K %' = round(sum(KorBB =="Strikeout")/sum(PAindicator),3)*100,
        'BB %' = round(sum(WalkIndicator)/sum(PAindicator),3)*100,
        AVG = round(sum(HitIndicator)/sum(ABindicator),3),
        OPS = round((sum(OnBaseindicator)/sum(PAindicator))+(sum(totalbases)/sum(ABindicator)),3),
        'HH %' = round(sum(HHindicator, na.rm=T)/sum(biphh,na.rm=T),3)*100,
        'GB %' = round(sum(GBindicator, na.rm = T)/sum(BIPind,na.rm=T),3)*100
      )
  })
  
  
  brks <- seq(82, 98, 1)
  clrs <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brks) + 1)
  brks2 <- seq(85,102,1)
  clrs2 <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brks2) + 1)
  brks3 <- seq(40,75,1)
  clrs3 <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brks3) + 1)
  brks4 <- seq(46,76,1)
  clrs4 <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brks4) + 1)
  brks5 <- seq(40,74,1)
  clrs5 <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brks5) + 1)
  brksepa <- seq(40,90,1)
  clrsepa <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brksepa) + 1)
  brks6 <- seq(25,82,1)
  clrs6 <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brks6) + 1)
  brks7 <- seq(2,50,1)
  clrs7 <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brks7) + 1)
  brks8 <- seq(0,40,1)
  clrs8 <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brks8) + 1)
  brks9 <- seq(0,45,1)
  clrs9 <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brks9) + 1)
  brksbb <- seq(0,35,1)
  clrsbb <- colorRampPalette(c("#00840D","white","#E1463E" ))(length(brksbb) + 1)
  brksavg <- seq(.1,.450,.001)
  clrsavg <- colorRampPalette(c("#00840D","white","#E1463E" ))(length(brksavg) + 1)
  brksops <- seq(.389,1.368,.001)
  clrsops <- colorRampPalette(c("#00840D","white","#E1463E" ))(length(brksops) + 1)
  brkshh <- seq(8,54,1)
  clrshh <- colorRampPalette(c("#00840D","white","#E1463E" ))(length(brkshh) + 1)
  brksgb <- seq(0,100,1)
  clrsgb <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brksgb) + 1)
  brkschase <- seq(20,31,1)
  clrschase <- colorRampPalette(c("#E1463E","white", "#00840D"))(length(brkschase) + 1)
  
  
  # Render the statistics table
  output$statistics_table <- renderDataTable({
    DT::datatable(
      processed_data(),
      options = list(paging = FALSE, ordering = TRUE, searching=FALSE)
    ) %>%
      formatStyle(
        'Avg FB Velo',
        backgroundColor = styleInterval(brks, clrs)
      ) %>%
      formatStyle(
        'Max FB Velo',
        backgroundColor = styleInterval(brks2,clrs2)
      )%>%
      formatRound('Strike %', digits = 1 )%>%
      formatRound('FB Strike %', digits = 1) %>%
      formatRound('OS Strike %', digits = 1) %>%
      formatRound('E+A %', digits=1) %>%
      formatRound('1PK %', digits=1) %>%
      formatRound('HH %', digits=1)%>%
      formatStyle(
        'Strike %',
        backgroundColor = styleInterval(brks3, clrs3)
      )%>%
      formatStyle(
        'FB Strike %',
        backgroundColor = styleInterval(brks4,clrs4)
      ) %>%
      formatStyle(
        'OS Strike %',
        backgroundColor = styleInterval(brks5,clrs5))%>%
      formatStyle(
        'E+A %',
        backgroundColor = styleInterval(brksepa,clrsepa)
      )%>%
      formatStyle(
        '1PK %',
        backgroundColor = styleInterval(brks6,clrs6)
      )%>%
      formatStyle(
        'Whiff %',
        backgroundColor = styleInterval(
          brks7, clrs7))%>%
      formatStyle(
        'IZ Whiff %',
        backgroundColor = styleInterval(
          brks8,clrs8
        )
      )%>%
      formatStyle(
        'Chase %',
        backgroundColor = styleInterval(
          brkschase,clrschase
        )
      )%>%
      formatStyle(
        'K %',
        backgroundColor = styleInterval(
          brks9,clrs9
        )
      )%>%
      formatStyle(
        'BB %',
        backgroundColor = styleInterval(
          brksbb, clrsbb
        )
      )%>%
      formatStyle(
        'AVG',
        backgroundColor = styleInterval(
          brksavg, clrsavg
        )
      )%>%
      formatStyle(
        'OPS',
        backgroundColor = styleInterval(
          brksops, clrsops
        )
      )%>%
      formatStyle(
        'HH %',
        backgroundColor = styleInterval(
          brkshh, clrshh
        )
      )%>%
      formatStyle(
        'GB %',
        backgroundColor = styleInterval(
          brksgb, clrsgb
        )
      )
  })
  
  filtered_spray_data <- reactive({
    spraydata %>% filter( 
                    BatterSide %in% c(input$BatterHand))
  })
  
  output$spraychart <- renderPlot({
    
    line_length <- 150
    
    filtered_spray_data() %>%
    ggplot(aes(x = hc_x, y = hc_y, fill = TaggedHitType)) +
      #annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -75, ymax = Inf)+
      geom_point(size = 3, alpha = 0.8, shape = 21, color="black") +
      geom_segment(data = filter(filtered_spray_data(), TaggedHitType == 'GroundBall'), 
                   aes(x = 0, y = 0,xend = hc_x / sqrt(hc_x^2 + hc_y^2) * line_length, 
                       yend = hc_y / sqrt(hc_x^2 + hc_y^2) * line_length), 
                   color = "red") +  # Extend lines for GB
      #scale_fill_gradient(high = 'red', low = 'blue') +
      #scale_color_gradient(low = "red", high = "green") +
      geom_mlb_stadium(stadium_ids = 'cardinals', stadium_transform_coords = TRUE, stadium_segments = 'all', color = "black") +
      theme_void() + theme(legend.position = "bottom", axis.title = element_blank()) +
      theme(strip.text = element_text(size = 20, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(), 
            legend.text = element_text(size = 18),
            legend.title = element_blank()#remove y axis labels
      )  + facet_wrap(~Pitcher) + coord_cartesian()
  
      
  })
  
  
  
}

shinyApp(ui, server)
