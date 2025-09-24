library(shiny)
library(ggiraph)
library(patchwork)
library(shinydashboard)
library(ggplot2)
library(ggpubr)
library(plotly)
library(DT)
library(dplyr)
library(stringr)
library(tidyverse)
library(plotly)
library(gtsummary)
library(cards)
theme_gtsummary_compact() # reduce default padding and font size for a gt table
library(tern)
library(data.table)
library(arsenal)
library(gapminder)
library(gt)
data(gapminder)
library(haven)
library(gtools)
library(tidyr)
library("foreign")
library(writexl)

library(shinylive)
library(httpuv)


vsdata <- read.csv("https:\\github.com\\Rosh00L\\Rshinylive\\blob\\main\\Data\\advs.csv")
                    

vssub_=subset(vsdata, select=c(SUBJID,SEX,AGE,RACE,PARAM,PARAMCD,AVISIT,VISITNUM,ADY,ATPT,ATPTN,VSDTC,AVAL,TRT01A,RANDFL,SAFFL))

vssub <- vssub_ %>%
  mutate(ATPTN = ifelse(is.na(ATPTN), 0, ATPTN)) %>%
  mutate(ADY = ifelse(is.na(ADY), 0, ADY)) %>%
  mutate(AVISIT= ifelse(ATPTN== 1,"Baseline",AVISIT )) %>%
  mutate(ATPT= ifelse(ATPT=='Predose',"",ATPT))

data0 <- vssub %>%
  mutate(DayV_ =  ifelse(ADY < -1 ,-2,ifelse(ADY > 1 ,ADY,ifelse(ADY == -1 ,ADY,ifelse(AVISIT=='Baseline',0, gsub("[^0-9.-]", "", ATPT)))))) %>%
  
  mutate(DayV=as.numeric(DayV_))

#print (vssub)
data <- data0 %>%
  filter(PARAMCD != "INTP") %>%
  mutate(DayN= ifelse(ADY < 1 , ADY*24, ifelse(ADY > 1 , ADY*24, DayV))) %>%
  mutate(VisDay = (ifelse((ADY==1 & AVISIT !='Baseline'), paste(AVISIT, ATPT, sep=","), AVISIT)))

#print(data)

vs2  <- data |>
  filter(!AVISIT %in% "Screen Failure" & PARAMCD != "INTP") |>
  mutate(PARAMList = PARAM) |>
  mutate(VisDayList = VisDay) |>
  mutate(VISITNUM = ifelse(ATPTN > 0, VISITNUM+DayV, ifelse(ATPTN < 1, VISITNUM, VISITNUM ))) |>
  mutate(SEX = ifelse(SEX=="F","Female","Male")) |>
  mutate(
    AGEGR1 =
      case_when(
        between(AGE, 18, 30) ~ "18-30",
        between(AGE, 31, 40) ~ "31-40",
        between(AGE, 41, 55) ~ "41-55",
        between(AGE, 56, 64) ~ "56-64",
        AGE > 64 ~ ">=65"
      ) |>
      factor(levels = c("18-30", "31-40", "41-55","55-64", ">=65"))
  ) |>
  labelled::set_variable_labels(AGE = "Age (yr)",
                                AGEGR1 = "Age group",
                                SEX = "Sex",
                                RACE = "Race")

SUBJID_f <- c(11001,11002,11005,12001,12002,12003,13003,13004,13005,14001,14003,14005,15001,15002,15004)

mtcars <- vs2 %>%
  arrange(VISITNUM,DayN) %>%
  filter(SUBJID %in% SUBJID_f) %>%
  filter(SAFFL=="Y")

write_xlsx(mtcars, "https:\\github.com\\Rosh00L\\Rshinylive\\blob\\main\\Data\\mtcars.xlsx")

addmds<- mtcars %>%
  distinct(SUBJID, .keep_all= TRUE)

##################################################################################################
#~~~~~~~~~~~~~~~~~~~Dataset for VS summery table ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

dayliast <- factor(unique(mtcars$DayN))


# Define UI
ui <- fluidPage(
  titlePanel ("Vital Sings Data Dashboard"),
  br(),
  sidebarLayout(
    sidebarPanel(width = 2, style = "border-style: solid; border-color: black",
                 checkboxGroupInput("TRT","Treatment group:",
                                    choices = unique(mtcars$TRT01A),selected =unique(mtcars$TRT01A)),
                 
                 selectInput("y_axis", "Select Y Axis:",
                             choices = unique(mtcars$PARAM), selected = unique(mtcars$PARAM[2])),
                 
                 checkboxGroupInput("bySUBJID","Select Subject:",
                                    choices = unique(mtcars$SUBJID),selected =unique(mtcars$SUBJID)),
                 
                 radioButtons("plot_type1", "Variables to show:",
                              c("Vital sings Paramers by Treatmets" = "TRT01A",
                                "Vital sings Paramers by Gender" = "SEX",
                                "Vital sings Paramers by Subject" = "SUBJID")
                 ),
                 
                 checkboxGroupInput("Visits", "Treal Visits:",
                                    choices = unique(mtcars$VisDay),selected =unique(mtcars$VisDay)),
                 
                 checkboxInput("showTable", "Show Data Table", value = FALSE),
                 hr(),
                 
                 sliderInput(
                   inputId = "age_range",
                   label = "Age Range",
                   min = 5, max = 85,
                   value = c(5, 85), step = 5
                 )
    ),
    
    
    mainPanel(width = 10, #h1("Main Panel Header"),
              tabsetPanel(
                
                tabPanel("VS",
                         br(),
                         fluidRow(
                           # 1st column for map
                           column(6,  style = 'border: 1px solid lightgrey; border-radius: 25px',
                                  br(),
                                  # ntitle and info button
                                  div(HTML('<b> </b> '), style = 'display: inline-block;'),
                                  uiOutput('sales_map_button', style = 'display: inline-block;'),
                                  br(), br(),
                                  # map plot
                                  gt_output(outputId = "ADVS_table"),
                                  br()
                           ),
                           
                           column(6,  style = 'border: 1px solid lightgrey; border-radius: 25px',
                                  br(),
                                  # ntitle and info button
                                  div(HTML('<b>  </b> '), style = 'display: inline-block;'),
                                  uiOutput('sales_map_button', style = 'display: inline-block;'),
                                  br(), br(),
                                  # map plot
                                  plotOutput('line1', height = '350px'),
                                  br()
                           ),
                           column(6, style = 'border: 1px solid lightgrey; border-radius: 25px',
                                  br(),
                                  # ntitle and info button
                                  div(HTML('<b>  </b> '), style = 'display: inline-block;'),
                                  uiOutput('sales_map_button', style = 'display: inline-block;'),
                                  br(), br(),
                                  # map plot
                                  plotOutput('line2', height = '350px'),
                                  br()
                           ),
                           
                           column(6, style = 'border: 1px solid lightgrey; border-radius: 25px',
                                  br(),
                                  # sales trend title and info button
                                  div(HTML('<b>   </b> '), style = 'display: inline-block;'),
                                  uiOutput('sales_trend_button', style = 'display: inline-block;'),
                                  br(), br(),
                                  # trend plot
                                  plotOutput('plot1', height = '350px')
                           ),
                           column(6, style = 'border: 1px solid lightgrey; border-radius: 25px',
                                  # fluidRow for sales trend
                                  br(),
                                  # sales trend title and info button
                                  div(HTML('<b>   </b> '), style = 'display: inline-block;'),
                                  uiOutput('sales_trend_button', style = 'display: inline-block;'),
                                  br(), br(),
                                  # trend plot
                                  plotOutput('scatter', height = '350px')
                                  
                           ),
                         ),
                ),
                
                tabPanel("DEMO",
                         br(),
                         fluidRow(
                           # 1st column for map
                           column(6,  style = 'border: 1px solid lightgrey; border-radius: 25px',
                                  br(),
                                  # ntitle and info button
                                  div(HTML('<b>  </b> '), style = 'display: inline-block;'),
                                  uiOutput('sales_map_button', style = 'display: inline-block;'),
                                  br(), br(),
                                  # map plot
                                  gt_output(outputId = "my_gt_table"),
                                  br()
                           ),
                         ),
                ),
                
                
                tabPanel("Plot",
                         br(),
                         fluidRow(
                           # 1st column for map
                           # 2nd column for plots
                         ),
                         
                         br(),
                         
                         fluidRow(
                           # 1st column for map
                           # 2nd column for plots
                         ),
                ),
              ),
    )
  ))
# Define server logic
server <- function(input, output, session) {
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ line 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  filterDay <- reactive({
    Pdata1 <- mtcars%>%
      filter (VISITNUM != 10000) %>%
      filter (PARAM %in% c(input$y_axis))%>%
      filter (TRT01A %in% (input$TRT)) %>%
      filter (SUBJID %in% (input$bySUBJID)) %>%
      group_by(across(all_of(c("DayN"))))   %>%
      summarise_at(vars(AVAL), list(meanVal= mean))
    
  })
  mean_val1 <- reactive({mean(filterDay()$meanVal)})
  max_val1 <- reactive({max(filterDay()$meanVal)})
  
  #observe(print(mean_val1()))
  
  # Generate plot based on selected options
  #Scatter Plot
  output$line1 <- renderPlot({
    input$plot_type == "Line Plot"
    ggplot(filterDay(), aes_string(x = filterDay()$DayN, y =filterDay()$meanVal)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max_val1()+25)) +
      geom_line(aes(colour = filterDay()$TRT01A),size = 1.5) +
      scale_x_continuous(labels=as.character(filterDay()$DayN),breaks=filterDay()$DayN) +
      theme(axis.text.x = element_text(size = 10,angle = 45), axis.text.y = element_text(size = 15)) +
      labs(title =  paste("Figure 10.0.0.0.1 Overall Mean value in", sub(" *\\(.*", "",input$y_axis), "versus Nominal Time - Linear Scale - (Safety Set)"), x = "Visit Day" , y = input$y_axis) +
      geom_vline(xintercept = 0, linewidth = 0.5, linetype='dashed', color = 'navyblue')+
      annotate("text", x=1, y=89, label="Pre-dose", size=4, color="blue") +
      geom_hline(yintercept = mean_val1(), color = "red", linetype = "dashed", size = 0.5)+
      annotate("text", x=25, y=mean_val1()+1,  label="Average", size=4, color="red")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ line 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  filteredData <- reactive({
    Pdata2 <- mtcars%>%
      filter (VISITNUM != 10000) %>%
      filter (SUBJID %in% (input$bySUBJID)) %>%
      filter (PARAM == input$y_axis) %>%
      filter (TRT01A %in% (input$TRT)) %>%
      filter (VisDay %in% c(input$Visits)) %>%
      group_by(across(all_of(c(input$plot_type1,"DayN"))))   %>%
      summarise_at(vars(AVAL), list(meanVal2= mean)) %>%
      mutate(Gx= ifelse(input$plot_type1=="TRT01A", TRT01A, ifelse(input$plot_type1=="SEX", SEX, ifelse(input$plot_type1=="SUBJID", SUBJID))))
    
  })
  
  mean_val2 <- reactive({mean(filteredData()$meanVal2)})
  max_val2 <- reactive({max(filteredData()$meanVal2)})
  
  output$line2 <- renderPlot({
    
    if (input$plot_type1 == "TRT01A") {
      
      ggplot(filteredData(), aes_string(x = filteredData()$DayN, y =filteredData()$meanVal2), colour =factor(filteredData()$TRT01A)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max_val2()+15)) +
        geom_line(aes(colour = filteredData()$Gx),size = 1.5) +
        scale_x_continuous(labels=as.character(filteredData()$DayN),breaks=filteredData()$DayN) +
        theme(axis.text.x = element_text(size = 10,angle = 45), axis.text.y = element_text(size = 15)) +
        labs(title = paste("Figure 10.0.0.0.2 Mean", sub(" *\\(.*", "",input$y_axis),"value versus Nominal Time - By Treatment Group - Linear Scale - (Safety Set)"), x = "Visit Day", y = input$y_axis) +
        geom_vline(xintercept = 0, linewidth = 0.5, linetype='dashed', color = 'navyblue')+
        annotate("text", x=1.5, y=89, label="Pre-dose", size=4, color="blue") +
        geom_hline(yintercept = mean_val2(), color = "red", linetype = "dashed", size = 0.5)+
        annotate("text", x=29, y=mean_val2()+1,label="Average", size=4, color="red")+
        guides(colour = guide_legend(title="Treatment"))
      
    }
    
    else if ( input$plot_type1 == "SEX") {
      ggplot(filteredData(), aes_string(x = filteredData()$DayN, y =filteredData()$meanVal2)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max_val2()+15)) +
        geom_line(aes(colour = filteredData()$Gx),size = 1.5) +
        scale_x_continuous(labels=as.character(filteredData()$DayN),breaks=filteredData()$DayN) +
        theme(axis.text.x = element_text(size = 10,angle = 45), axis.text.y = element_text(size = 15)) +
        labs(title = paste("Figure 10.0.0.0.2 Mean", sub(" *\\(.*", "",input$y_axis),"value versus Nominal Time - By Gender Group - Linear Scale - (Safety Set)"), x = "Visit Day", y = input$y_axis) +
        geom_vline(xintercept = 0, linewidth = 0.5, linetype='dashed', color = 'navyblue')+
        annotate("text", x=1.5, y=89, label="Pre-dose", size=4, color="blue") +
        geom_hline(yintercept = mean_val2(), color = "red", linetype = "dashed", size = 0.5)+
        annotate("text", x=29, y=mean_val2()+1,label="Average", size=4, color="red")+
        guides(colour = guide_legend(title="Gender"))
      
      
    }
    
    else if ( input$plot_type1 == "SUBJID") {
      ggplot(filteredData(), aes_string(x = filteredData()$DayN, y =filteredData()$meanVal2, group=filteredData()$SUBJID, colour =factor(filteredData()$SUBJID))) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max_val2()+15)) +
        geom_line() +
        scale_x_continuous(labels=as.character(filteredData()$DayN),breaks=filteredData()$DayN) +
        theme(axis.text.x = element_text(size = 10,angle = 45), axis.text.y = element_text(size = 15)) +
        guides(color = guide_legend(title = " Subject"))+
        labs(title = paste("Figure 10.0.0.0.2", sub(" *\\(.*", "",input$y_axis),"value versus Nominal Time - By Subjct - Linear Scale - (Safety Set)"), x = "Visit Day", y = input$y_axis)
      
    }
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Scatter ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  filteredsc <- reactive({
    Psc <- mtcars %>%
      filter (VISITNUM != 10000) %>%
      filter (SUBJID %in% (input$bySUBJID)) %>%
      filter (PARAM %in% c(input$y_axis))%>%
      filter (TRT01A %in% (input$TRT)) %>%
      mutate(Gx= ifelse(input$plot_type1=="TRT01A", "TRT01A", ifelse(input$plot_type1=="SEX", "SEX", ifelse(input$plot_type1=="SUBJID", "SUBJID"))))
    
  })
  
  #observe(print(filteredsc()))
  mean_valsc <- reactive({mean(filteredsc()$AVAL)})
  max_valsc <- reactive({max(filteredsc()$AVAL)})
  
  ScVar <- reactive({input$plot_type1})
  
  
  output$scatter<- renderPlot({
    
    if ( input$plot_type1 == "SEX") {
      
      #scatter1
      ggplot(filteredsc(), aes(x= filteredsc()$DayN, y = filteredsc()$AVAL, colour = as.factor(filteredsc()$SEX))) +
        geom_point()  +
        scale_x_continuous(labels=as.character(filteredData()$DayN),breaks=filteredData()$DayN) +
        labs(title = paste("Figure 10.0.0.0.4", sub(" *\\(.*", "",input$y_axis), "Value Versus Nominal Time - By Gender- (Safety Set)"), x = "Visit Day", y =  input$y_axis, colour="Gender") +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max_valsc()+10))
      
      
    }
    
    else if ( input$plot_type1 == "TRT01A") {
      
      #scatter2
      ggplot(filteredsc(), aes(x= filteredsc()$DayN, y = filteredsc()$AVAL, colour = as.factor(filteredsc()$TRT01A))) +
        geom_point()  +
        scale_x_continuous(labels=as.character(filteredData()$DayN),breaks=filteredData()$DayN) +
        labs(title = paste("Figure 10.0.0.0.4", sub(" *\\(.*", "",input$y_axis), "Value Versus Nominal Time - By Treatment Group - (Safety Set)"), x = "Visit Day", y =  input$y_axis , colour="Treatment" ) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max_valsc()+10))
      #geom_point(stat="identity", position = "dodge")  +
      #labs(title = "What is your favourite animal?", subtitle = "Percent of respondents")
      
    }
    
    else if ( input$plot_type1 == "SUBJID") {
      #scatter3
      ggplot(filteredsc(), aes(x= filteredsc()$DayN, y = filteredsc()$AVAL, colour = as.factor(filteredsc()$SUBJID))) +
        geom_point()  +
        scale_x_continuous(labels=as.character(filteredData()$DayN),breaks=filteredData()$DayN) +
        labs(title = paste("Figure 10.0.0.0.4", sub(" *\\(.*", "",input$y_axis), "Value Versus Nominal Time - By Subject- (Safety Set)"), x = "Visit Day", y =  input$y_axis , colour="Subject" ) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max_valsc()+10))
      #geom_point(stat="identity", position = "dodge")  +
      #labs(title = "What is your favourite animal?", subtitle = "Percent of respondents")
    }
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Bar Chart ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  #mean_bval <- reactive({mean(filterGen()$meanbVal)})
  
  #observe(print(filterGen()))
  
  output$plot1 <- renderPlot({
    
    if ( input$plot_type1 == "TRT01A") {
      
      filterGenb1 <- reactive({
        Bardata <- mtcars%>%
          filter (VISITNUM != 10000) %>%
          filter (SUBJID %in% (input$bySUBJID)) %>%
          filter (PARAM == input$y_axis)%>%
          filter (TRT01A %in% (input$TRT)) %>%
          group_by(across(all_of(c(input$plot_type1,"AGEGR1")))) %>%
          summarise_at(vars(AVAL), list(meanbVal= mean))
      })
      
      observe(print(filterGenb1()))
      
      ggplot(filterGenb1(), aes(x=filterGenb1()$AGEGR1, y = filterGenb1()$meanbVal, fill = factor(filterGenb1()$TRT01A))) +
        geom_bar(stat="identity", position = "dodge")+
        scale_y_continuous(expand = c(0, 0))+
        labs(title =  paste("Figure 10.0.0.0.3 Age Group in", sub(" *\\(.*", "",input$y_axis), "versus Age Group  - By Treatment"), x = "Age Group", y =  input$y_axis )+
        labs(fill= "Treatment")
    }
    
    else  if ( input$plot_type1 == "SEX") {
      
      filterGen <- reactive({
        Bardata <- mtcars%>%
          filter (VISITNUM != 10000) %>%
          filter (SUBJID %in% (input$bySUBJID)) %>%
          filter (PARAM == input$y_axis)%>%
          filter (TRT01A %in% (input$TRT)) %>%
          group_by(across(all_of(c("TRT01A",input$plot_type1)))) %>%
          summarise_at(vars(AVAL), list(meanbVal= mean))
      })
      
      observe(print(filterGen()))
      
      ggplot(filterGen(), aes(x= filterGen()$TRT01A, y = filterGen()$meanbVal, fill =  factor(filterGen()$SEX))) +
        geom_bar(stat="identity", position = "dodge")  +
        scale_y_continuous(expand = c(0, 0)) +
        labs(title = paste("Figure 10.0.0.0.3 Age Group in", sub(" *\\(.*", "",input$y_axis), "versus Treatment - By Gender "), x = "Treatment", y = input$y_axis, fill= "Gender" )
      
    }
    else if ( input$plot_type1 == "SUBJID") {
      
      filterGen <- reactive({
        Bardata <- mtcars%>%
          filter (VISITNUM != 10000) %>%
          filter (SUBJID %in% (input$bySUBJID)) %>%
          filter (PARAM == input$y_axis)%>%
          filter (TRT01A %in% (input$TRT)) %>%
          group_by(across(all_of(c("TRT01A",input$plot_type1,"AGEGR1")))) %>%
          summarise_at(vars(AVAL), list(meanbVal= mean))
      })
      
      #observe(print(filterGen()))
      
      ggplot(filterGen(), aes(x=filterGen()$TRT01A, y = filterGen()$meanbVal,group=filterGen()$SUBJID, fill = factor(filterGen()$SUBJID))) +
        geom_bar(stat="identity", position = position_dodge(),width=0.75)  +
        #scale_y_continuous(expand = c(0, 0)) +
        labs(title = paste("Figure 10.0.0.0.3 Age Group in", sub(" *\\(.*", "",input$y_axis), "versus Treatment - By Suject"), x = "Treatment", y = input$y_axis, fill= "Subject"  )
      
    }
  })
  
  
  ####################ADSL  Summery table##########################################################
  
  addmds2 <- reactive({
    dmdata <- addmds %>%
      group_by(TRT01A) %>%
      filter (TRT01A %in% (input$TRT))
  })
  
  #observe(print(addmds2()))
  
  output$my_gt_table <-
    render_gt(
      addmds2() %>%
        # filter out patients outside of age range
        #dplyr::filter(
        # dplyr::between(AGE, input$age_range[1], input$age_range[2])
        #) %>%
        tbl_summary(
          by = TRT01A,
          type = list(AGE = "continuous2"),
          include = c(AGE,AGEGR1,SEX,RACE),
          statistic = all_continuous() ~ c("{N_nonmiss}","{mean} ({sd})","{median} ({p25}, {p75})", "{min}, {max}"),
          missing = "no"
        ) %>%
        add_overall()%>%
        italicize_levels() %>%
        add_stat_label() %>%
        # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
        as_gt() %>%
        tab_header(md("**Table 1. Summary of Demographics** - Safety Population"))
    )
  
  
  
  #################ADVS Summery table#################################################################################
  
  
  LB4 <- mtcars %>%
    select(c("SUBJID", "TRT01A", "VISITNUM", "VisDay","PARAM", "PARAMCD","AVAL")) %>%
    filter (!(VisDay %in% c('Screening','Unscheduled')))
  
  
  
  LB4$VAR= as.numeric(LB4$AVAL)
  
  stat1 <- LB4 %>%
    group_by(TRT01A,PARAMCD,PARAM,VisDay,VISITNUM) %>%
    summarise(
      n= N(VAR),
      Median=sprintf(median(VAR),fmt= '%#.1f'),
      Mean = sprintf(mean(VAR),fmt= '%#.1f'),
      SD= sprintf(sd(VAR),fmt= '%#.2f'),
      Max = sprintf(max(VAR),fmt= '%#.1f'),
      Min = sprintf(min(VAR),fmt= '%#.1f'),
      "  "="     "
    )
  
  
  stat1$VisDayList <- factor(stat1$VisDay)
  stat1$PARAMList <- factor(stat1$PARAM)
  stat1$VisDay <- (stat1$VisDay)
  stat1$PARAM <- (stat1$PARAM)
  
  tr <- unique(stat1$TRT01A)
  
  trf <- levels(factor(unique(stat1$TRT01A)))
  
  trfn <-rnorm(trf)
  
  stat2 <- stat1 %>%
    mutate(TRT01A= ifelse( TRT01A=='Placebo',"Tr6",
                           ifelse( TRT01A=='0.3 ug',"Tr1",
                                   ifelse( TRT01A=='1.0 ug',"Tr2",
                                           ifelse( TRT01A=='3.0 ug',"Tr3",
                                                   ifelse( TRT01A=='6.0 ug',"Tr4",
                                                           ifelse( TRT01A=='12.0 ug',"Tr5",""))))))) %>%
    mutate("Mean (SD)" = paste(Mean," (",SD,")", sep = " ")) %>%
    mutate("Min,  Max" =  paste(Min,", ",Max, sep = " "))%>%
    subset(select = -c(Mean,SD,Min, Max))
  
  #tr <- unique(stat2$TRT01A)
  
  #print(is.factor(stat1$VisDayList))
  
  data_long <- gather(stat2, Stats, statVal, n,`Mean (SD)`,Median,`Min,  Max`,"  ", factor_key=TRUE)
  
  arrange(data_long, TRT01A,PARAMCD,PARAM,VisDay)
  
  
  pw1 <- pivot_wider(
    data_long,
    names_from= TRT01A,
    values_from= statVal,
    #values_fill = 0,
    id_cols = everything(),
    names_sep = "_"
  )
  
  
  pw_ <- pw1 %>%
    arrange(PARAMCD,PARAM,VISITNUM,VisDay) %>%
    mutate(ordnum = row_number()) %>%
    mutate(PARAMList= ifelse( Stats != "n",PARAM, PARAM)) %>%
    mutate(VisDayList= ifelse( Stats != "n",VisDay, VisDay)) %>%
    mutate(DayList= ifelse( ordnum == 1 ,paste(" "," ",VisDay,sep = " "), " ")) %>%
    mutate(Tr1 = ifelse(ordnum < 5 &is.na(Tr1),"_", ifelse(ordnum > 4 &is.na(Tr1)," ", Tr1)))%>%
    mutate(Tr2 = ifelse(ordnum < 5 &is.na(Tr2),"_", ifelse(ordnum > 4 &is.na(Tr2)," ", Tr2)))%>%
    mutate(Tr3 = ifelse(ordnum < 5 &is.na(Tr3),"_", ifelse(ordnum > 4 &is.na(Tr3)," ", Tr3)))%>%
    mutate(Tr4 = ifelse(ordnum < 5 &is.na(Tr4),"_", ifelse(ordnum > 4 &is.na(Tr4)," ", Tr4)))%>%
    mutate(Tr5 = ifelse(ordnum < 5 &is.na(Tr5),"_", ifelse(ordnum > 4 &is.na(Tr5)," ", Tr5)))%>%
    mutate(Tr6 = ifelse(ordnum < 5 &is.na(Tr6),"_", ifelse(ordnum > 4 &is.na(Tr6)," ", Tr6)))%>%
    subset(select = -c(PARAMCD,PARAM,VisDay))
  
  
  pw5 <- reactive({
    pw4_ <- pw_ %>%
      group_by(PARAMList) %>% #PARAMList, VisDayList
      filter (PARAMList %in% (input$y_axis))  %>%
      filter (VisDayList %in% c(input$Visits))
  })
  
  
  
  agg <- aggregate( SUBJID ~ TRT01A+SUBJID, data = addmds, function(x) length(x))
  
  trtmc <- reactive({
    agg %>%
      mutate(TrtFq = paste(TRT01A,"<br>","(N= ", SUBJID,")", sep = " "))
  })
  
  #observe(print(pw5()))
  #observe(print(trtmc()))
  
  output$ADVS_table <-
    render_gt(
      
      gt(pw5()) %>%
        
        gt::tab_options(table.font.names = "Times New Roman", table.font.size = 14,
                        summary_row.padding = px(15),
                        data_row.padding = px(2),
                        row_group.padding = px(10),
                        
                        heading.align = 'center',
                        heading.background.color = 'lightgrey',
                        heading.title.font.size = px(20),
                        
                        column_labels.background.color = 'lightgrey',
                        column_labels.border.bottom.style = 'none',
                        column_labels.font.size = 14,
                        column_labels.font.weight= "bold"
        ) %>%
        
        #arrange(data_long, TRT01A,PARAMCD,PARAM,VisDay)
        
        #tab_stubhead(label = "VisDay") |>
        #cols_width(PARAMList  ~ px(1000))  |>
        cols_label(
          DayList = html("Parameter (Unit)"),
          #PARAMList = html("Parameter"),
          Stats = html("Statistic"),
          Tr1 = html(trtmc()$TrtFq[1]),
          Tr2 = html(trtmc()$TrtFq[2]),
          Tr3 = html(trtmc()$TrtFq[3]),
          Tr4 = html(trtmc()$TrtFq[4]),
          Tr5 = html(trtmc()$TrtFq[5]),
          Tr6 = html(trtmc()$TrtFq[6])
          
        ) |>
        
        tab_header(
          title = md("**Table 10.0.0.1 Vital Signs – Descriptive Statistics (Safety Set)**")
          
        ) |>
        
        cols_hide(columns = c(VISITNUM,ordnum,PARAMList,VisDayList)) |>
        
        tab_footnote(
          footnote = ".",
          locations = cells_body(columns = Stats, rows = 3:4)
        ) |>
        
        cols_move_to_start(
          columns = c(DayList, Stats,Tr1,Tr3,Tr4,Tr2,Tr5,Tr6) #PARAMList
        ) |>
        
        tab_spanner(
          label = "Treatment",
          columns = c(Tr1,Tr3,Tr4,Tr2,Tr5,Tr6)
        ) |>
        ####################################################################
      tab_style(
        style = cell_text(color = "black",size = "medium" , weight = "bold"),
        locations = cells_body(columns = DayList)
      ) |>
        
        cols_align(
          align = "left",
          columns = c("DayList")
        )|>
        
        tab_style(
          style = cell_text(color = "black",size = "medium" , weight = "bold"),
          locations = cells_body(columns = Stats)
        ) |>
        
        
        cols_align(
          align = "center",
          columns = c(Tr1,Tr3,Tr4,Tr2,Tr5)
        )|>
        
        
        #####################################################################
      #### Group variables  ######
      tab_style(
        style = list(
          cell_fill("lightgray"),
          cell_text(color = "black",size = "medium" , weight = "bold")
        ),
        locations = cells_row_groups()
      ) |>
        
        tab_style(
          style = cell_text(color = "lightgray", weight = "bold"),
          locations = cells_stub()
        )|>
        ####
        
        tab_style(
          style = cell_borders(sides = "bottom", color = "black", weight = px(3)),
          locations = cells_column_labels(everything())
        )|>
        
        
        
        
        #####################################################################
      
      tab_style(
        style = cell_text(weight = "bold",
                          size = px(18),
                          align = "center",
                          color = "black"),
        locations = list( cells_column_spanners(matches("Treatment")))
      )|>
        
        
        
        opt_horizontal_padding(scale = 3)
    )
  
  
  
  ##https://stackoverflow.com/questions/79168295/gtsummary-hierarchical-summary-table-for-multiple-measurements
  #################################################################################################
  # Show/hide data table based on checkbox
  output$dataTable <- renderDT({
    if (input$showTable) {
      filteredData()
    } else {
      NULL
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
