library(shinydashboard)
library(rhandsontable)
library(qcc)
library(shiny)
library(ggpubr)
library(SixSigma)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(RPostgreSQL)
library(plotly)
library(ggplot2)
library(readxl)
library(assertthat)
library(xlsx)

DF <-
  data.frame(
    Value = 1:5,
    MT_Probable_Cause = "",
    MT_Possible_Cause = "",
    Machine_Probable_Cause = "",
    Machine_Possible_Cause = "",
    MP_Probable_Cause = "",
    MP_Possible_Cause = "",
    Method_Probable_Cause = "",
    Method_Possible_Cause = "",
    Measurement_Probable_Cause = "",
    Measurement_Possible_Cause = "",
    Env_Probable_Cause = "",
    Env_Possible_Cause = "",
    stringsAsFactors = FALSE
  )
DF1 <-
  data.frame(
    Value = 1:5,
    Defect_Category = "",
    Defect_Value = "",
    stringsAsFactors = FALSE
  )
DF2 <-
  data.frame(
    Reason = "",
    Sun = "",
    Mon = "",
    Tue = "",
    Wed = "",
    Thu = "",
    Fri = "",
    Sat = "",
    Total = "",
    stringsAsFactors = FALSE
  )

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "iviewsense.com"),
  dashboardSidebar(
    radioButtons(
      "dataFilter",
      "Data Filter",
      c("From Database" = "db",
        "File Upload" = "fi")
    ),
    conditionalPanel(condition = "input.dataFilter == 'fi'",
                     fileInput(
                       'file1',
                       'Choose CSV File',
                       accept = c(
                         'application/vnd.ms-excel',
                         'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                         '.xls',
                         '.xlsx',
                         '.csv'
                       )
                     )),
    conditionalPanel(
      condition = "input.dataFilter == 'db'",
      dateRangeInput(
        inputId = "date",
        strong("Date Range"),
        start = "2019-01-01",
        end = "2019-12-31",
        min = "2019-01-01",
        max = "2019-12-31"
      )
    ),
    
    selectInput(
      "type",
      "Chart Type",
      choices = c(
        "Select chart Type" = "",
        "Stratification" = "strat",
        "Control chart" = "ct",
        "Histogram" = "Hist",
        "Scatter Plot" = "scatter",
        "Pareto" = "pareto",
        "Cause & effect" = "cause",
        "Check sheet" = "chk"
      )
    ),
    conditionalPanel(condition = "input.type == 'ct'",
                     actionButton("submit", "View")),
    conditionalPanel(
      condition = "input.type == 'Hist'",
      actionButton("submit3", "View"),
      numericInput("lslv", "LSL", 0, min = -100, max = 500),
      numericInput("uslv", "USL", 0, min = -100, max = 500)
    ),
    conditionalPanel(condition = "input.type == 'scatter'",
                     actionButton("submit4", "View")),
    
    conditionalPanel(condition = "input.type == 'strat'",
                     actionButton("submit5", "View")),
    
    conditionalPanel(
      condition = "input.type == 'pareto'",
      radioButtons("useType1", "Use Data Types", c("TRUE", "FALSE")),
      actionButton("submit2", "View Pareto")
      
    ),
    conditionalPanel(
      condition = "input.type == 'cause'",
      radioButtons("useType", "Use Data Types", c("TRUE", "FALSE")),
      actionButton("submit1", "Fish bone")
      
    ),    
    uiOutput('columns'),
    uiOutput('twoColumns')
  ),
  dashboardBody(# Boxes need to be put in a row (or column)
    fluidRow(
      fluidRow(
        column(3, uiOutput("familyFilter")),
        column(3, uiOutput("customerFilter")),
        column(3, uiOutput("modelFilter")),
        column(3, uiOutput("resultFilter"))
      ),
      box(
        width = 12,
        status = "danger",
        collapsed = FALSE,
        solidHeader = TRUE,
        title = "Quality Control Charts",
        conditionalPanel(condition = "input.type == 'ct' & input.submit",
                         plotOutput("controlChartHist")),
        conditionalPanel(condition = "input.type == 'ct' & input.submit",
                         plotOutput("controlChartHist1")),
        conditionalPanel(condition = "input.type == 'scatter' & input.submit4",
                         plotlyOutput("scatterPlotly")),
        conditionalPanel(condition = "input.type == 'strat'",
                         plotlyOutput("stratPlotly")),        
        conditionalPanel(condition = "input.type == 'Hist' & input.submit3",
                         plotOutput("histG")),
        conditionalPanel(
          condition = "input.type == 'pareto'",
          rHandsontableOutput("hot1"),
          plotOutput("paretoC")
        ),
        
        conditionalPanel(
          condition = "input.type == 'chk'",
          rHandsontableOutput("hot2")
          
        ),
        conditionalPanel(
          condition = "input.type == 'cause'",
          rHandsontableOutput("hot"),
          plotOutput("CauseCT"),
          plotOutput("CauseCT1")
        )
      )
    ))
)


server = function(input, output, session) {
  values <- reactiveValues()
  
  passData <- reactive({
    con <-
      dbConnect(
        dbDriver("PostgreSQL"),
        dbname = "7QcT",
        host = "localhost",
        port = 5432,
        user = "postgres",
        password = "postgres"
      )
    on.exit(dbDisconnect(con))
    
    qry <-
      paste("SELECT * FROM qcranepy WHERE date >= '",
            input$date[1],
            "' AND date <= '",
            input$date[2],
            "'")
    
    subset_Table <- DBI::dbGetQuery(con, qry)
    
    subset_Table <- as.data.frame(subset_Table)
    
    subset_Table
    
    
  })
  
  
  ## Handsontablek
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF,
                    useTypes = as.logical(input$useType),
                    stretchH = "all")
  })
  
  observe({
    if (!is.null(input$hot2)) {
      DF2 <- hot_to_r(input$hot2)
    } else {
      if (is.null(values[["DF2"]]))
        DF2 <- DF2
      else
        DF2 <- values[["DF2"]]
    }
    values[["DF2"]] <- DF2
  })
  
  output$hot2 <- renderRHandsontable({
    DF2 <- values[["DF2"]]
    if (!is.null(DF2))
      rhandsontable(DF2)
  })
  
  observe({
    if (!is.null(input$hot1)) {
      DF1 = hot_to_r(input$hot1)
    } else {
      if (is.null(values[["DF1"]]))
        DF1 <- DF1
      else
        DF1 <- values[["DF1"]]
    }
    values[["DF1"]] <- DF1
  })
  
  output$hot1 <- renderRHandsontable({
    DF1 <- values[["DF1"]]
    if (!is.null(DF1))
      rhandsontable(DF1,
                    useTypes = as.logical(input$useType1),
                    stretchH = "all")
  })
  
  observe({
    input$cols
    updateNumericInput(session, "lslv", value = 0)
    updateNumericInput(session, "uslv", value = 0)
  })
  
  observe({
    input$dataFilter
    updateSelectInput(session, "type", label = "Select chart Type", selected = "")
  })
  
  ## Save
  observeEvent(input$submit1, {
    finalDF <- isolate(values[["DF"]])
    saveRDS(finalDF, file = file.path(getwd(), sprintf("%s.rds", "table")))
    output$CauseCT <- renderPlot({
      # Cause and Effect Diagram
      ##Create effect as string
      effect <- "Low Quality product"
      ##Create vector of causes
      causes.gr <-
        c("Measurement",
          "Material",
          "Methods",
          "Environment",
          "Manpower",
          "Machines")
      # Create indiviual cause as vector of list
      causes <- vector(mode = "list", length = length(causes.gr))
      causes[1] <- list(finalDF$Measurement_Probable_Cause)
      causes[2] <- list(finalDF$MT_Probable_Cause)
      causes[3] <- list(finalDF$Method_Probable_Cause)
      causes[4] <- list(finalDF$Env_Probable_Cause)
      causes[5] <- list(finalDF$MP_Probable_Cause)
      causes[6] <- list(finalDF$Machine_Probable_Cause)
      # Create Cause and Effect Diagram
      ss.ceDiag(
        effect,
        causes.gr,
        causes,
        sub = "Fish Bone Diagram Example for probable cause",
        ss.col = c(
          "#666666",
          "#BBBBBB",
          "#CCCCCC",
          "#DDDDDD",
          "#EEEEEE",
          "#f1f1f1"
        )
      )
      
      
    })
    output$CauseCT1 <- renderPlot({
      # Cause and Effect Diagram
      ##Create effect as string
      effect <- "Low Quality product"
      ##Create vector of causes
      causes.gr <-
        c("Measurement",
          "Material",
          "Methods",
          "Environment",
          "Manpower",
          "Machines")
      # Create indiviual cause as vector of list
      causes <- vector(mode = "list", length = length(causes.gr))
      causes[1] <- list(finalDF$Measurement_Possible_Cause)
      causes[2] <- list(finalDF$MT_Possible_Cause)
      causes[3] <- list(finalDF$Method_Possible_Cause)
      causes[4] <- list(finalDF$Env_Possible_Cause)
      causes[5] <- list(finalDF$MP_Possible_Cause)
      causes[6] <- list(finalDF$Machine_Possible_Cause)
      # Create Cause and Effect Diagram
      ss.ceDiag(effect, causes.gr, causes, sub = "Fish Bone Diagram Example for possible cause")
      
    })
  })
  
  observeEvent(input$submit2, {
    finalDF1 <- isolate(values[["DF1"]])
    defect <- as.numeric(finalDF1$Defect_Value)
    names(defect) <- finalDF1$Defect_Category
    output$paretoC <- renderPlot({
      pareto.chart(defect, ylab = "Defect frequency", col = heat.colors(length(defect)))
    })
  })
  
  output$columns <- renderUI({
    if (input$dataFilter == "fi") {
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
        data = read.csv(inFile$datapath, header = T, encoding = "UTF-8")
        # names(data) <- gsub(x = names(data),
        #                     pattern = "(\\.)+",
        #                     replacement = "_")
        #names(data) <- gsub(" ","_", names(data))
        
        conditionalPanel(condition = "input.type != 'pareto' & input.type != 'cause' & input.type != 'strat' & input.type != 'chk' & input.type != ''",
                         selectInput("cols", "Input Column 1", names(data)))
      } else{
        data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE)
        conditionalPanel(condition = "input.type != 'pareto' & input.type != 'cause' & input.type != 'strat' & input.type != 'chk' & input.type != ''",
                         selectInput("cols", "Input Column 1", names(data)))
      }
    }
    else{
      data = as.data.frame(passData())
      conditionalPanel(condition = "input.type != 'pareto' & input.type != 'cause' & input.type != 'chk' & input.type != 'strat' & input.type != ''",
                       selectInput("cols", "Input Column 1", names(data)))
    }
    
  })
  
  
  output$twoColumns <- renderUI({
    if (input$dataFilter == "fi") {
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
        data = read.csv(inFile$datapath, header = T, encoding = "UTF-8")
        # names(data) <- gsub(x = names(data),
        # pattern = "(\\.)+",
        # replacement = "_")
        #names(data) <- gsub(" ","_", names(data))
        conditionalPanel(condition = "input.type == 'scatter'",
                         selectInput("cols2", "Input Column 2", names(data)))
      } else{
        data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE)
        conditionalPanel(condition = "input.type == 'scatter'",
                         selectInput("cols2", "Input Column 2", names(data)))
      }
      
    }
    else{
      data = as.data.frame(passData())
      conditionalPanel(condition = "input.type == 'scatter'",
                       selectInput("cols2", "Input Column 2", names(data)))
    }
    
  })
  
  observeEvent(input$submit, {
    output$controlChartHist <- renderPlot({
      if (input$dataFilter == "fi") {
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
          data <- read.csv(inFile$datapath, header = T, encoding="UTF-8")
          #names(data) <- gsub(" ","_", names(data))
          data <- subset(data, select = input$cols)
        }
        else{
          data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE)
          #colmn1 <- gsub(" ","_",input$cols)
          data <- subset(data, select = input$cols)
        }
        
        #colmn1 <- gsub(" ","_",input$cols)
        #data <- subset(data, select = colmn1)        
        
        qcc(data = data,
            # The dataaset
            type = "xbar.one",
            # The chart type (in this case it lets qcc know that n = 1)
            plot = TRUE)
      } else{
        data = as.data.frame(passData())
        data <-
          subset(data, select = input$cols)
        qcc.options(bg.margin = "azure2")
        xbar <- qcc(data = data,
                    # The dataaset
                    type = "xbar.one",
                    # The chart type (in this case it lets qcc know that n = 1)
                    plot = TRUE)
        
      }
      
    })
  })
  
  observeEvent(input$submit, {
    output$controlChartHist1 <- renderPlot({
      if (input$dataFilter == "fi") {
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
          data = read.csv(inFile$datapath, header = T, encoding="UTF-8")
          #names(data) <- gsub(" ","_", names(data))
          data <- subset(data, select = input$cols)
        }
        else{
          data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE)
          data <- subset(data, select = input$cols)
        }
        
        test <- data[, ]
        #print(data)
        MR <-
          matrix(cbind(test[1:length(test) - 1], test[2:length(test)]), ncol = 2)
        q2 <- qcc(MR, type = "R", plot = TRUE)
        
      }
      else{
        data <- passData()
        data <- subset(data, select = input$cols)
        
        test <- data[, ]
        test <- as.numeric(test)
        MR <-
          matrix(cbind(test[1:length(test) - 1], test[2:length(test)]), ncol = 2)
        q2 <- qcc(MR, type = "R", plot = TRUE)
      }
    })
  })
  
  observeEvent(input$submit4, {
    output$scatterPlotly <- renderPlotly({
      if (input$dataFilter == "fi") {
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
          data <- read.csv(inFile$datapath, header = T, encoding="UTF-8")
        }
        else{
          data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE)
        }
        
        data <- data.frame(data)
        
        x <- subset(data, select = input$cols)
        z <- subset(data, select = input$cols2)
        
        print(x)
        
        X <- as.numeric(unlist(x))
        Z <- as.numeric(unlist(z))
        
        sp <-
          ggplot(data, aes(x = X, y = Z)) + geom_point(
            show.legend = TRUE,
            shape = 23,
            fill = "blue",
            color = "darkred",
            size = 0.5
          ) + geom_smooth(
            method = lm,
            formula = y ~ x,
            size = 0.1,
            color = "blue",
            se = TRUE
          ) # Add linear regression line
        mytheme <- theme(axis.line = element_line(size = 1),
                         panel.background = element_rect(fill = "white"))
        sp + mytheme +
          labs(
            subtitle = "QC Variation",
            y = input$cols2,
            x = input$cols,
            title = "Scatterplot",
            caption = "7QC"
          )
      }
      else{
        SCT <- passData()
        df <- data.frame(SCT, check.names = TRUE)
        
        x <- subset(df, select = input$cols)
        
        z <- subset(df, select = input$cols2)
        X <- as.numeric(unlist(x))
        Z <- as.numeric(unlist(z))
        
        # Add a linear trend :
        tmp <-
          ggplot(df, aes(x = X, y = Z)) + geom_point(
            show.legend = TRUE,
            shape = 23,
            fill = "blue",
            color = "darkred",
            size = 0.5
          ) + geom_smooth(
            method = lm,
            formula = y ~ x,
            size = 0.1,
            color = "blue",
            se = TRUE
          ) +
          scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) # Add linear regression line
        mytheme <- theme(axis.line = element_line(size = 1),
                         panel.background = element_rect(fill = "white"))
        tmp + mytheme + labs(
          subtitle = "QC Variation",
          y = input$cols2,
          x = input$cols,
          title = "Scatterplot",
          caption = "7QC"
        )
        
      }
    })
  })
  
  observeEvent(input$submit5,{
    output$stratPlotly <- renderPlotly({
      if (input$dataFilter == "fi") {
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
          data <- read.csv(inFile$datapath, header = T, encoding="UTF-8")
        }
        else{
          data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE)
        }        
        data <- data.frame(data)
        p <- nrow(subset(data, ((data$NPD..bar. >= 0 & data$NPD..bar. <=2.5)) & ((data$Torque.Bal.1..Nm. >= 0 & data$Torque.Bal.1..Nm. <=0.8))
                         & ((data$Hysterisis.CW... >= 0 & data$Hysterisis.CW... <= 38)) & ((data$Leakage.CW..lpm. >= -0.2 & data$Leakage.CW..lpm. <=0.6))
                         & ((data$Leakage.CCW..lpm. >= -0.2 & data$Leakage.CCW..lpm. <=0.6)) & ((data$Blk.Passage.Pr..bar. >= 1 & data$Blk.Passage.Pr..bar. <= 6))
                         & ((data$Efficiency.CW..bar. >= 0 & data$Efficiency.CW..bar. <=85)) & ((data$Efficiency.CCW..bar. >= 0 & data$Efficiency.CCW..bar. <=85))
                         
        ))       
        
        r <- nrow(data) - p
        
        data <- data.frame(status= c("Passed", "Rejected"), values= c(p, r) )
        
        p <- plot_ly(data, labels = ~status, values = ~values, type = 'pie') %>%
          layout(title = 'Rane - Stratification of Gears',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
      }
      else{
        SCT <- passData()
        data <- data.frame(SCT, check.names = TRUE)
        p <- nrow(subset(data, ((data$npd_bar >= 0 & data$npd_bar <=2.5)) & ((data$torque_bal_1 >= 0 & data$torque_bal_1 <=0.8))
                         & ((data$hysterisis_cw >= 0 & data$hysterisis_cw <= 38)) & ((data$leakage_cw >= -0.2 & data$leakage_cw <=0.6))
                         & ((data$leakage_ccw >= -0.2 & data$leakage_ccw <=0.6)) & ((data$blk_passage_pr >= 1 & data$blk_passage_pr <= 6))
                         & ((data$efficiency_ccw >= 0 & data$efficiency_ccw <=85)) & ((data$efficiency_cw >= 0 & data$efficiency_ccw <=85))
        ))        
        
        r <- nrow(data) - p 
        
        data <- data.frame(status= c("Passed", "Rejected"), values= c(p, r) )
        
        p <- plot_ly(data, labels = ~status, values = ~values, type = 'pie') %>%
          layout(title = 'Rane - Stratification of Gears',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
      }
    })
  })
  
  
  observeEvent(input$submit3, {
    output$histG <- renderPlot({
      if (input$dataFilter == "fi") {
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
          data <- read.csv(inFile$datapath, header = T, encoding="UTF-8", stringsAsFactors = FALSE)
        }
        else{
          data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE, stringsAsFactors = FALSE)
        }
        plotData <- data %>% filter(Family %in% input$familyFilterInput) %>%
          filter(Cust %in% input$customerFilterInput) %>%
          filter(Model %in% input$modelFilterInput) %>%
          filter(Result %in% input$resultFilterInput) %>%
          subset(select = input$cols)
        q1 = qcc(plotData,
                 type = "xbar.one",
                 nsigmas = 3,
                 plot = FALSE)
        process.capability(q1, spec.limits = c(input$lslv, input$uslv))
      }
      else{
        data = as.data.frame(passData())
        plotData <- data %>% filter(Family %in% input$familyFilterInput) %>%
          filter(Cust %in% input$customerFilterInput) %>%
          filter(Model %in% input$modelFilterInput) %>%
          filter(Result %in% input$resultFilterInput) %>%
          subset(select = input$cols)
        q1 = qcc(plotData,
                 type = "xbar.one",
                 nsigmas = 3,
                 plot = FALSE)
        process.capability(q1, spec.limits = c(input$lslv, input$uslv))
      }
    })
  })

  output$familyFilter <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
      data <- read.csv(inFile$datapath, header = T, encoding="UTF-8", stringsAsFactors = FALSE)
    }
    else{
      data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE, stringsAsFactors = FALSE)
    }
    options <- unique(data$Family)
    pickerInput("familyFilterInput", "Family Filter", options, options, multiple = TRUE)
  })
  output$customerFilter <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
      data <- read.csv(inFile$datapath, header = T, encoding="UTF-8", stringsAsFactors = FALSE)
    }
    else{
      data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE, stringsAsFactors = FALSE)
    }
    options <- unique(data$Cust)
    pickerInput("customerFilterInput", "Cust Filter", options, options, multiple = TRUE)
  })
  output$modelFilter <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
      data <- read.csv(inFile$datapath, header = T, encoding="UTF-8", stringsAsFactors = FALSE)
    }
    else{
      data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE, stringsAsFactors = FALSE)
    }
    options <- unique(data$Model)
    pickerInput("modelFilterInput", "Model Filter", options, options, multiple = TRUE)
  })
  output$resultFilter <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if (see_if(has_extension(inFile$datapath, 'csv')) == TRUE) {
      data <- read.csv(inFile$datapath, header = T, encoding="UTF-8", stringsAsFactors = FALSE)
    }
    else{
      data <- read.xlsx(inFile$datapath, sheetIndex = 1, as.data.frame = TRUE, header=TRUE, stringsAsFactors = FALSE)
    }
    options <- unique(data$Result)
    pickerInput("resultFilterInput", "Result Filter", options, options, multiple = TRUE)
  })
  
}

shinyApp(ui, server)