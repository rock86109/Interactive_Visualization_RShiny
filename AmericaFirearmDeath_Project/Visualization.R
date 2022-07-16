rm(list = ls())

# install.packages("dplyr")
# install.packages("shiny")
# install.packages("usmap")
# install.packages("plotly")
# install.packages("ggplot2")
# install.packages("shinythemes")

library(dplyr)
library(shiny)
library(usmap)
library(plotly)
library(ggplot2)
library(shinythemes)


########################## data exploration ##################################
dataset_A <- read.csv("USA_Today.csv")
dataset_B <- read.csv("Racial_Distribution_Rate.csv")
dataset_C <- read.csv("wiki_imputation.csv")


## Shiny
############################################################## ui ##############################################################
ui <- fluidPage(theme = shinytheme("cerulean"), # united
                tags$head(HTML("<title>FIT5147 DVP</title>")),
                titlePanel(h3("", style='border-bottom-style:solid;border-bottom-width:medium;')),
                titlePanel(h1("Exploration of possible factors related to gun and murder safety issue in the United States", 
                              style='border-bottom-style:solid;border-bottom-width:medium;',
                              align = "center")), 
                titlePanel(h3("FIT5147 Data Visualization | Name: SiaoHsuan, Jiang | Student ID: 33029229 | Tutor’s Name: Mohit Gupta | Tutorial Number: 1", 
                              style='border-bottom-style:solid;border-bottom-width:medium;', 
                              align = "center")),

                tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all;}"))),  # word wrap for text output! 
                
                fluidPage(
                  column(12,
                         titlePanel(h1("Layout", style="font-size:100%;font-weight:bold;", align="left")) 
                  ),
                  column(4, 
                         plotlyOutput("regression", height="500px"),
                         align="left"
                  ),
                  
                  column(4, 
                         style='border-left-width:20px;border-left-style:outset;border-left-color:white;',
                         plotOutput("scatter", height="500px"), 
                         align="center"
                         
                  ),
                  
                  column(4, 
                         style='border-left-width:20px;border-left-style:outset;border-left-color:white;',
                         plotlyOutput("hist", height="500px"), 
                         align="center"
                  )
                ),
                
                
                titlePanel(h3("", style='border-bottom-style:solid;border-bottom-width:medium;')), # middle line
                
                
                fluidPage(
                  column(5, 
                         radioButtons("Race", "Race:", c("Indian"="IndianRate", "White"="WhiteRate", "Black"="BlackRate", "Asian"="AsianRate", "Hawaiian"="HawaiianRate"), inline=T),
                         align="center"
                  ),
                  
                  column(3, 
                         style='border-left-width:20px;border-left-style:outset;border-left-color:white;',
                         selectInput("X_axes", "Possible Fators:", c("Gun Ownership"="Gun_Ownership...", "Poverty Rate"="Numeric_PovertyRate")),
                         align="center"
                         
                  ),
                  
                  column(3, 
                         style='border-left-width:20px;border-left-style:outset;border-left-color:white;',
                         selectInput("Y_axes", "Topic:", c("Firearm Death Rate"="Firearm_deaths_per_100000", "Murder and Nonnegligent Manslaughter Rate"="Murder_and_Nonnegligent_Manslaughter_Rate.per100.000.")),
                         align="center"
                  )
                ),
                
                
                titlePanel(h3("", style='border-bottom-style:solid;border-bottom-width:medium;')), # middle line
                
                
                fluidPage(
                  column(4, 
                         titlePanel(h1("Details", style="font-size:100%;font-weight:bold;", align="center")), # Description (center bold size)
                         verbatimTextOutput("detail"),
                         tags$head(tags$style("#detail{overflow-y:scroll;max-height:280px;}"))
                  ),

                  column(4,
                         titlePanel(h1("Focus/Zoom Choropleth Map", style="font-size:100%;font-weight:bold;", align="center")), # Description (center bold size)
                         plotlyOutput("mymap", height="280px"),
                  ),
                  
                  column(4, 
                         titlePanel(h1("Operation", style="font-size:100%;font-weight:bold;", align="center")), # Description (center bold size)
                         verbatimTextOutput("operation"),
                         tags$head(tags$style("#operation{overflow-y:scroll;max-height:280px;}")) # made the output box scrollable
                  )
                )
)


############################################################## server ##############################################################
ThreeData <- merge(x = dataset_A, y = dataset_B, by = "State", all = TRUE)
ThreeData <- merge(x = ThreeData, y = dataset_C, by = "State", all = TRUE)
Numeric_PovertyRate <- strsplit(ThreeData$Poverty_rate, "%") %>% unlist() %>% as.numeric()
ThreeData <- ThreeData %>% mutate(Numeric_PovertyRate = Numeric_PovertyRate)

oldname = c("Firearm_deaths_per_100000", "Murder_and_Nonnegligent_Manslaughter_Rate.per100.000.", "Numeric_PovertyRate", "Gun_Ownership...")
newname = c("FirearmDeathRate", "MurderAndNonnegligentManslaughterRate", "PovertyRate", "GunOwnership")


server <- function(input, output, session) {
  ThreeData_temp = reactive({
    ThreeData %>% mutate(y_temp=ThreeData[, input$Y_axes], x_temp=ThreeData[, input$X_axes], race_temp=ThreeData[, input$Race]) # input$Y = Firearm_deaths_per_100000, input$X = PovertyRate
  })

  
## XYname
  YXname = reactive({
    for (i in 1:length(oldname)){
      if (oldname[i] == input$Y_axes){
        y = newname[i]
      }
      if (oldname[i] == input$X_axes){
        x = newname[i]
      }
    }
    c(y, x)
  })
  

  ## Text Output
  output$detail <- renderPrint({
    t = "The visualization page is designed by using R shiny.\n
Used Library: dplyr, shiny, ggplot2, usmap, plotly, shinythemes.\n
Please open it in browser for better user experience.\n
Height of three layout plot is '500px'.\n
Height of text boxes and choropleth map plot is '350px'.\n
Page is using shinytheme 'cerulean'.\n
Only text scatter plot does not show hover tooltips.
If no bar is being selected, the map won’t show any value when being hovered.
"
    cat(paste0(t, collapse = " "))
  })
  
  output$operation <- renderPrint({
t = "You can hover on points, bars or states to see more details.
Once you click on any bar in the bar graph, all 4 graphs will change.
Once you double click( or double tap on your trackpad ) quickly on the white place in the bar graph, all 4 graphs will return to the initial status.

Setting pairs and findings:

//1
[Indian | Gun Ownership | Firearm Death Rate]
(Bar Plot)Bars on the left are having relatively darker colors which means states with the higher Gun Ownership Rates are having relatively higher Firearm Death Rates. 
(Text Scatter Plot)States with higher Indian Rates are having relatively higher Firearm Death Rates.

//2
[Asian | Gun Ownership | Firearm Death Rate]
(Text Scatter Plot)Most of states with red labels are at the bottom left which means they are having relatively lower Firearm Death Rates. 

//3
[Asian | Poverty Rate | Firearm Death Rate]
(Bar Plot)The higher PovertyRate are having relatively higher Firearm Death Rates.

//4
[Black | Gun Ownership/Poverty Rate | Murder and Nonnegligent Manslaughter Rate]
(Regression Plot/Hover)Hypothesis of slope = 0 is rejected by the p-value under the assumption of alpha = 0.5. 

//5
[White | Gun Ownership/Poverty Rate | Murder and Nonnegligent Manslaughter Rate]
(Regression Plot/Hover)Hypothesis of slope = 0 is rejected by the p-value under the assumption of alpha = 0.5. 

"
cat(paste0(t, collapse = " "))
  })
  
  
  r_hist <- reactive({
    event_data("plotly_doubleclick", source = "hist_src")
  })
  r2 <- reactive({
    temp = ThreeData_temp() %>% arrange(desc(y_temp)) %>% mutate(State = factor(State, levels = unique(State)))
    click_state_hist = temp[event_data("plotly_click", source = "hist_src")$x, 'State']
    click_state_hist
  })
  
  
  
## Plot Output
  
  ## Regression Plot
  output$regression <- renderPlotly({
    
    reg_data <- ThreeData_temp() %>% mutate(hover_state_hist = "black")

    if (is.null(r2()) == FALSE) {
      reg_data[reg_data[,'State'] == r2(), 'hover_state_hist'] <- "red"
    }
  
    reg_res <- lm(y_temp~reg_data[, input$Race], data = reg_data)

    ggplotly(
      ggplot(data = reg_data, aes(reg_data[, input$Race], y_temp, text=paste("State: ", State, "<br>", input$Race, ":", reg_data[, input$Race], "<br>", YXname()[1], ":", y_temp))) +
        geom_point(colour = reg_data[, 'hover_state_hist']) +
        ggtitle(paste(input$Race, " and ", YXname()[1], sep=" ")) + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        xlab(input$Race)+
        ylab(YXname()[1]) + 
        theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) + 
        geom_smooth(method = "lm", formula = y~x, show.legend = TRUE, se=FALSE)
      , tooltip = "text") %>%
    add_trace(x =reg_data[, input$Race], y = fitted(reg_res), mode = 'lines', name = "Regression", 
              hovertemplate = paste("Intersept :", round(reg_res$coefficients[1], 4),
                           "<br>", "Slope :" , round(reg_res$coefficients[2], 4),
                           "<br>", "R-squared :", round(summary(reg_res)$r.squared, 4),
                           "<br>", "p-value: ", round(summary(reg_res)$coefficients[4], 4)), 
              hoveron = "lines")
  })
  
  ## Scatter Plot 
  output$scatter <- renderPlot({
    scatter_df <- ThreeData_temp() %>% 
      arrange(desc(race_temp)) %>%
      mutate(max_flag = c(rep("10 Highest Rate States", 10), rep("Others", 40))) %>%
      mutate(hover_state_hist = "black")
    
    if (is.null(r2()) == FALSE) {
      scatter_df[scatter_df[,'State'] == r2(), 'max_flag'] <- "Selected State"
    }
    
    k <- ggplot(data = scatter_df, aes(x_temp , y_temp)) +
      geom_point() +
      ggtitle(paste(YXname()[1], " and ", YXname()[2],"(Ten States With Highest ", input$Race,")", sep = "", collapse = NULL)) +
      geom_label(aes(label=State, fill = factor(max_flag), size = scatter_df[,input$Race]), color="black") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab(YXname()[2]) +
      ylab(YXname()[1]) +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) + 
      theme(legend.title.align = 0.5, legend.position = c(0.85, 0.2))
    k$labels$fill = "10 Highest States"
    k$labels$size = input$Race
    k
  })
  
  ## Hist Plot 
  output$hist <- renderPlotly({
    hist_df <- ThreeData_temp() %>% 
      arrange(desc(y_temp)) %>%
      mutate(State = factor(State, levels = unique(State))) %>%
      mutate(click_state_hist = "black")

    if (is.null(r2()) == FALSE) {
      hist_df[hist_df[,'State'] == r2(), 'click_state_hist'] <- "red"
    }
    
    k <- ggplot(hist_df, aes(x=State, y=y_temp, fill = x_temp, 
                             text = paste(" State: ", State,
                                          "<br>", YXname()[1], ": ", y_temp,
                                          "<br>", YXname()[2], ": ", x_temp)
                             )) + 
      geom_bar(stat = "identity", position=position_dodge(), color=hist_df[, 'click_state_hist']) +
      ggtitle(paste(YXname()[1], "in Each State", sep = " ", collapse = NULL)) +
      ylab(YXname()[1]) +
      scale_fill_continuous(low="#FFFFFF", high="#003060") +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
      theme(legend.title.align = 0.5)
    k$labels$fill=YXname()[2]
    
    k <- ggplotly(k, tooltip = "text", source = "hist_src")
    k %>% layout(xaxis = list(tickangle=45, tickfont = list(size=11)))

  })
  
  ## Map
  output$mymap <- renderPlotly({
    map_df <- ThreeData_temp()
    

    df <- data.frame(map_df)
    colnames(df)[which(names(df) == "State")] <- "state"
    colnames(df)[which(names(df) == "x_temp")] <- YXname()[2]
    
    p <- plot_usmap(data = df, values = YXname()[2], color = "black", include = r2()) + 
      # ggtitle(paste(YXname()[2]," Choropleth Map", sep = "", collapse = NULL)) +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
      scale_fill_continuous(low = "#ADADAD", high = "#BB3D00")
    t <- ThreeData_temp()[ThreeData_temp()[,'State']==r2(),]
    p <- style(p, text = paste("State: ", t[,"State"], "<br>",
                               "Population: ", t[,"Population"], "<br>",
                               "FirearmDeathRate: ", t[,"Firearm_deaths_per_100000"], "<br>",
                               "MurderAndNonnegligentManslaughterRate: ", t[,"Murder_and_Nonnegligent_Manslaughter_Rate.per100.000."], "<br>",
                               "PovertyRate: ", t[,"Poverty_rate"], "<br>",
                               "GunOwnership: ", t[,"Gun_Ownership..."], "<br>",
                               "WhiteRate: ", t[,"WhiteRate"], "<br>",
                               "BlackRate: ", t[,"BlackRate"], "<br>",
                               "IndianRate: ", t[,"IndianRate"], "<br>",
                               "AsianRate: ", t[,"AsianRate"], "<br>",
                               "HawaiianRate: ", t[,"HawaiianRate"]
                                ))

    ggplotly(p)
  })
}
############################################################## App ##############################################################
shinyApp(ui, server)

