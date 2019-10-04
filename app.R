source("global.R")

ui = tags$div(
	tags$head(
		tags$link(rel = "shortcut icon", type = "image/png", href = "iviewsense.png")
	),
	useShinyjs(),
	useShinyalert(),
	conditionalPanel(
		condition = "$('html').hasClass('shiny-busy')",
		tags$div(
			style = "position: fixed;top: 250px; left: 0px; width: 100%;
			padding: 5px 0px 5px 0px; text-align: center; font-weight: bold;
			font-size: 300%; color: #ffffff; background-color:'transparent'; z-index: 105;",
			tags$img(src = "loading_icon.svg", height = "200px", width = "200px")
		)
	),
	tags$style(
		paste0(
			".form-control {font-size: ", bodyFontSize, " !important;}",
			".shiny-input-container {font-size: ", bodyFontSize, " !important;}",
			".btn, .btn:link, .btn:visited {text-transform: uppercase;text-decoration: none;
				padding: 8px 32px; display: inline-block;
				transition: all .2s; position: relative; font-size: 12px;
				border: none; cursor: pointer;}
			.btn:hover {transform: translateY(-3px); box-shadow: 0 7.5px 15px rgba(0, 0, 0, 0.2);}
			.btn:hover::after {transform: scaleX(1.4) scaleY(1.6); opacity: 0;}
			.btn:active, .btn:focus {outline: none; transform: translateY(-1px); box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);}
			.p-bigger {font-size: 20px}"
		)
	),
	tags$div(
		id = "main_page_ui",
		bs4DashPage(
			title = "iViewSense | Analyze your Data",
			sidebar_collapsed = TRUE,
			navbar = bs4DashNavbar(
				status = "white"
			),
			sidebar = bs4DashSidebar(
				skin = "light",
				status = "primary",
				title = "iViewSense",
				brandColor = "blue",
				url = "https://www.iviewsense.org/",
				src = "iviewsense.png",
				elevation = 3,
				opacity = 0.9,
				bs4SidebarMenu(
					bs4SidebarMenuItem(
						text = "Data Source",
						tabName = "data_source",
						icon = "database"
					),
					bs4SidebarMenuItem(
						text = "Stratification",
						tabName = "stratification",
						icon = "layer-group"
					),
					bs4SidebarMenuItem(
						text = "Control chart",
						tabName = "control_chart",
						icon = "hourglass-half"
					),
					bs4SidebarMenuItem(
						text = "Histogram",
						tabName = "histogram",
						icon = "chart-bar"
					),
					bs4SidebarMenuItem(
						text = "Scatter plot",
						tabName = "scatter_plot",
						icon = "chart-line"
					),
					bs4SidebarMenuItem(
						text = "Pareto",
						tabName = "pareto",
						icon = "percentage"
					),
					bs4SidebarMenuItem(
						text = "Cause & effect",
						tabName = "cause_effect",
						icon = "balance-scale"
					),
					bs4SidebarMenuItem(
						text = "Check sheet",
						tabName = "check_sheet",
						icon = "check"
					)
				)
			),
			body = bs4DashBody(
				bs4TabItems(
					data_source_body,
					stratification_body,
					control_chart_body,
					histogram_body,
					scatter_plot_body,
					pareto_body,
					cause_effect_body,
					check_sheet_body
				)
			)
		)
	)
)

server = function(input, output, session) {
	# mainData <- read_xlsx("Geartek.xlsx", sheet = 1, col_names = TRUE)
	# mainData <- formatData(mainData)
	# minDate <- as.Date(min(mainData$Date_Time))
	# maxDate <- as.Date(max(mainData$Date_Time))
	# familyOptions <- unique(mainData$Family)
	# custOptions <- unique(mainData$Cust)
	# modelOptions <- unique(mainData$Model)
	# resultOptions <- unique(mainData$Result)
	# operatorOptions <- unique(mainData$Opr)
	# machineOptions <- unique(mainData$Machine)
	mainData <- data.frame()
	checkSheetData <- data.frame()
	checkSheetTableData <- data.frame()
	observeEvent(input$remote_or_local, {
		output$data_source_body_ui <- renderUI({
			if (input$remote_or_local %% 2 == 0) {
				ui <- fluidRow(
					column(
						12,
						fileInput(
							"data_source_input_file",
							"Upload your file or drag and drop it here",
							accept = c(".xlsx", ".csv")
						)
					),
					column(
						12, HTML(
							paste0(
								"Make sure that the uploaded file has the columns with these names<br>
								<b>", dataColumnNamesString, "</b>"
							)
						)
					)
				)
			} else {
				ui <- HTML("The data from MySQL database will be used for analysis!")
				mainData <<- selectDbQuery("SELECT * FROM testresults") %>% formatData()
				minDate <<- as.Date(min(mainData$Date_Time))
				maxDate <<- as.Date(max(mainData$Date_Time))
				familyOptions <<- unique(mainData$Family)
				custOptions <<- unique(mainData$Cust)
				modelOptions <<- unique(mainData$Model)
				resultOptions <<- unique(mainData$Result)
				operatorOptions <<- unique(mainData$Opr)
				machineOptions <<- unique(mainData$Machine)
				checkSheetData <<- getCheckSheetData()
				plots__trigger$trigger()
			}
			return(ui)
		})
	})
	observeEvent(input$data_source_input_file, {
		inFile <- input$data_source_input_file
		if (str_detect(inFile$datapath, "\\.xlsx")) {
			mainData <<- read_xlsx(inFile$datapath, sheet = 1, col_names = TRUE)
		} else if (str_detect(inFile$datapath, "\\.csv")) {
			mainData <<- read.csv(inFile$datapath,stringsAsFactors = FALSE, header = TRUE)
		} else {
			popUpWindow("This is an invalid file format, please upload a .xlsx or .csv file")
			return()
		}
		mainData <<- formatData(mainData)
		minDate <<- as.Date(min(mainData$Date_Time))
		maxDate <<- as.Date(max(mainData$Date_Time))
		familyOptions <<- unique(mainData$Family)
		custOptions <<- unique(mainData$Cust)
		modelOptions <<- unique(mainData$Model)
		resultOptions <<- unique(mainData$Result)
		operatorOptions <<- unique(mainData$Opr)
		machineOptions <<- unique(mainData$Machine)
		plots__trigger$trigger()
	})

	output$histogram_filters <- renderUI({
		plots__trigger$depend()
		plotVariables <- names(mainData)[11:51]
		fluidRow(
			column(
				2,
				dateInput(
					"histogram_filters_date_from", "From date",
					minDate, minDate, maxDate
				)
			),
			column(
				2,
				dateInput(
					"histogram_filters_date_to", "To date",
					maxDate, minDate, maxDate
				)
			),
			column(
				2,
				pickerInput(
					"histogram_filters_family", "Family",
					familyOptions, familyOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"histogram_filters_cust", "Customer",
					custOptions, custOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"histogram_filters_model", "Model",
					modelOptions, modelOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"histogram_filters_result", "Result",
					resultOptions, resultOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"histogram_filters_shift", "Shift",
					shiftNames, shiftNames, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"histogram_filters_machine", "Machine",
					machineOptions, machineOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"histogram_filters_operator", "Operator",
					operatorOptions, operatorOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4, offset = 1,
				pickerInput(
					"histogram_column", "Select the plot variable",
					plotVariables, "CW_1",
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(3, numericInput("histogram_lsl", "Enter the LSL", value = 0)),
			column(3, numericInput("histogram_usl", "Enter the USL", value = 0))
		)
	})
	observeEvent(c(
		input$histogram_column, input$histogram_filters_family,
		input$histogram_filters_cust, input$histogram_filters_model,
		input$histogram_filters_result, input$histogram_filters_date_from,
		input$histogram_filters_date_to, input$histogram_filters_shift,
		input$histogram_filters_operator, input$histogram_filters_machine), {
		plotData <- mainData %>%
			filter(
				Family %in% input$histogram_filters_family & Cust %in% input$histogram_filters_cust &
				Model %in% input$histogram_filters_model & Result %in% input$histogram_filters_result &
				Date >= input$histogram_filters_date_from & Date <= input$histogram_filters_date_to &
				shift %in% input$histogram_filters_shift & Opr %in% input$histogram_filters_operator &
				Machine %in% input$histogram_filters_machine
			)
		if (nrow(plotData) == 0) {
			output$histogram_plot <- renderPlot(textPlot())
			return(NULL)
		}
		plot_variable <- plotData[[input$histogram_column]]
		plot_variable <- plot_variable[!is.na(plot_variable)]
		lslValue <- round(mean(plot_variable) - 3 * sd(plot_variable), 2)
		uslValue <- round(mean(plot_variable) + 3 * sd(plot_variable), 2)
		updateNumericInput(
			session, "histogram_lsl", value = lslValue
			# label = HTML(paste0("Enter the LCL (mean - 3 x sd = ", lslValue, ")"))
		)
		updateNumericInput(
			session, "histogram_usl", value = uslValue
			# label = HTML(paste0("Enter the LCL (mean + 3 x sd = ", uslValue, ")"))
		)
		output$histogram_plot <- renderPlot({
			plots__trigger$depend()
			returnPlot <- tryCatch({
				process.capability(
					qcc(
						plot_variable,
						type = "xbar.one",
						nsigmas = 3,
						plot = FALSE
					),
					spec.limits = c(input$histogram_lsl, input$histogram_usl)
				)
			}, error = function(err) {
				returnPlot <- textPlot(paste0("There is no proper data for ", input$histogram_column))
			})
			return(returnPlot)
		})
	})

	output$scatter_plot_filters <- renderUI({
		plots__trigger$depend()
		numericPlotVariables <- names(select_if(mainData, is.numeric))
		factorPlotVariables <- names(select_if(mainData, is.character))
		fluidRow(
			column(
				2,
				dateInput(
					"scatter_plot_filters_date_from", "From date",
					minDate, minDate, maxDate
				)
			),
			column(
				2,
				dateInput(
					"scatter_plot_filters_date_to", "To date",
					maxDate, minDate, maxDate
				)
			),
			column(
				2,
				pickerInput(
					"scatter_plot_filters_family", "Family",
					familyOptions, familyOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"scatter_plot_filters_cust", "Customer",
					custOptions, custOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"scatter_plot_filters_model", "Model",
					modelOptions, modelOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"scatter_plot_filters_result", "Result",
					resultOptions, resultOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_filters_shift", "Shift",
					shiftNames, shiftNames, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_filters_machine", "Machine",
					machineOptions, machineOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_filters_operator", "Operator",
					operatorOptions, operatorOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_x_axis", "Select the X axis variable",
					numericPlotVariables, "CW_1",
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_y_axis", "Select the X axis variable",
					numericPlotVariables, "CCW_1",
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_color_axis", "Select the color axis variable",
					c("No color", factorPlotVariables), "No color",
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			)
		)
	})
	output$scatter_plot <- renderPlotly({
		plots__trigger$depend()
		req(input$scatter_plot_filters_family)
		req(input$scatter_plot_filters_cust)
		req(input$scatter_plot_filters_model)
		req(input$scatter_plot_filters_result)
		req(input$scatter_plot_filters_date_from)
		req(input$scatter_plot_filters_date_to)
		req(input$scatter_plot_filters_shift)
		req(input$scatter_plot_filters_operator)
		req(input$scatter_plot_filters_machine)
		plotData <- mainData %>%
			filter(
				Family %in% input$scatter_plot_filters_family & Cust %in% input$scatter_plot_filters_cust &
				Model %in% input$scatter_plot_filters_model & Result %in% input$scatter_plot_filters_result &
				Date >= input$scatter_plot_filters_date_from & Date <= input$scatter_plot_filters_date_to &
				shift %in% input$scatter_plot_filters_shift & Opr %in% input$scatter_plot_filters_operator &
				Machine %in% input$scatter_plot_filters_machine
			)
		if (nrow(plotData) == 0) return(ggplotly(textPlot()))
		if (input$scatter_plot_color_axis == "No color") {
			xyPlotData <- plotData %>% select(one_of(input$scatter_plot_x_axis, input$scatter_plot_y_axis))
			xyPlotData <- xyPlotData[complete.cases(xyPlotData), ]
			if (nrow(xyPlotData) == 0) return(ggplotly(textPlot()))
			xVariable <- xyPlotData[[input$scatter_plot_x_axis]]
			yVariable <- xyPlotData[[input$scatter_plot_y_axis]]
			plot <- plot_ly(
				data = xyPlotData, x = xVariable,
				y = yVariable, name = "X-Y Plot",
				type = "scatter", mode = "markers", size = 2
			) %>%
			add_lines(
				x= ~xVariable, name = "Trend",
				y= fitted(lm(yVariable~xVariable)),
				line = list(color = "#000000"), inherit = FALSE
			) %>%
			layout(
				xaxis = list(title = input$scatter_plot_x_axis),
				yaxis = list(title = input$scatter_plot_y_axis)
			)
		} else {
			xyColorPlotData <- plotData %>% select(one_of(input$scatter_plot_x_axis, input$scatter_plot_y_axis, input$scatter_plot_color_axis))
			xyColorPlotData <- xyColorPlotData[complete.cases(xyColorPlotData), ]
			if (nrow(xyColorPlotData) == 0) return(ggplotly(textPlot()))
			xVariable <- xyColorPlotData[[input$scatter_plot_x_axis]]
			yVariable <- xyColorPlotData[[input$scatter_plot_y_axis]]
			colorVariable <- xyColorPlotData[[input$scatter_plot_color_axis]]
			plot <- plot_ly(
				data = xyColorPlotData, x = xVariable,
				y = yVariable, color = colorVariable,
				type = "scatter", mode = "markers", size = 2
			) %>%
			add_lines(
				x= ~xVariable, name = "Trend",
				y= fitted(lm(yVariable~xVariable)),
				line = list(color = "#000000"), inherit = FALSE
			) %>%
			layout(
				xaxis = list(title = input$scatter_plot_x_axis),
				yaxis = list(title = input$scatter_plot_y_axis)
			)
		}
		return(plot)
	})

	output$control_chart_filters <- renderUI({
		plots__trigger$depend()
		familyOptions <- unique(mainData$Family)
		custOptions <- unique(mainData$Cust)
		modelOptions <- unique(mainData$Model)
		resultOptions <- unique(mainData$Result)
		plotVariables <- names(mainData)[11:51]
		fluidRow(
			column(
				2,
				dateInput(
					"control_chart_filters_date_from", "From date",
					minDate, minDate, maxDate
				)
			),
			column(
				2,
				dateInput(
					"control_chart_filters_date_to", "To date",
					maxDate, minDate, maxDate
				)
			),
			column(
				2,
				pickerInput(
					"control_chart_filters_family", "Family",
					familyOptions, familyOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"control_chart_filters_cust", "Customer",
					custOptions, custOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"control_chart_filters_model", "Model",
					modelOptions, modelOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"control_chart_filters_result", "Result",
					resultOptions, resultOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"control_chart_filters_shift", "Shift",
					shiftNames, shiftNames, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"control_chart_filters_machine", "Machine",
					machineOptions, machineOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"control_chart_filters_operator", "Operator",
					operatorOptions, operatorOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4, offset = 1,
				pickerInput(
					"control_chart_column", "Select the plot variable",
					plotVariables, "CW_1",
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(3, numericInput("control_chart_lcl", "Enter the LCL", value = 0)),
			column(3, numericInput("control_chart_ucl", "Enter the UCL", value = 0))
		)
	})
	observeEvent(c(
		input$control_chart_column, input$control_chart_filters_family,
		input$control_chart_filters_cust, input$control_chart_filters_model,
		input$control_chart_filters_result, input$control_chart_filters_date_from,
		input$control_chart_filters_date_to, input$control_chart_filters_shift,
		input$control_chart_filters_operator, input$control_chart_filters_machine), {
		plotData <- mainData %>%
			filter(
				Family %in% input$control_chart_filters_family & Cust %in% input$control_chart_filters_cust &
				Model %in% input$control_chart_filters_model & Result %in% input$control_chart_filters_result &
				Date >= input$control_chart_filters_date_from & Date <= input$control_chart_filters_date_to &
				shift %in% input$control_chart_filters_shift & Opr %in% input$control_chart_filters_operator &
				Machine %in% input$control_chart_filters_machine
			)
		if (nrow(plotData) == 0) {
			output$control_chart_plot_xbar_one <- renderPlot(textPlot())
			output$control_chart_plot_xbar_r <- renderPlot(textPlot())
			return(NULL)
		}
		plot_variable <- plotData[[input$control_chart_column]]
		plot_variable <- plot_variable[!is.na(plot_variable)]
		outPlot <- qcc(data = plot_variable, type = "xbar.one", plot = FALSE)
		lslValue <- round(outPlot$limits[1], 2)
		uslValue <- round(outPlot$limits[2], 2)
		updateNumericInput(
			session, "control_chart_lcl", value = lslValue
			# label = HTML(paste0("Enter the LCL (mean - 3 x sd = ", lslValue, ")"))
		)
		updateNumericInput(
			session, "control_chart_ucl", value = uslValue
			# label = HTML(paste0("Enter the UCL (mean + 3 x sd = ", uslValue, ")"))
		)
		output$control_chart_plot_xbar_one <- renderPlot({
			plots__trigger$depend()
			returnPlot <- tryCatch({
				qcc(
					data = plot_variable, type = "xbar.one",
					limits = c(input$control_chart_lcl, input$control_chart_ucl)
				)
			}, error = function(err) {
				returnPlot <- textPlot(paste0("There is no proper data for ", input$control_chart_column))
			})
			return(returnPlot)
		})
		matrixData <- matrix(cbind(plot_variable[1:length(plot_variable) - 1], plot_variable[2:length(plot_variable)]), ncol = 2)
		outPlot <- qcc(data = matrixData, type = "R", plot = FALSE)
		lslValue <- round(outPlot$limits[1], 2)
		uslValue <- round(outPlot$limits[2], 2)
		updateNumericInput(
			session, "control_chart_r_lcl", value = lslValue,
			label = HTML(paste0("Enter the LCL (mean - 3 x sd = ", lslValue, ")"))
		)
		updateNumericInput(
			session, "control_chart_r_ucl", value = uslValue,
			label = HTML(paste0("Enter the UCL (mean + 3 x sd = ", uslValue, ")"))
		)
		output$control_chart_plot_xbar_r <- renderPlot({
			plots__trigger$depend()
			returnPlot <- tryCatch({
				qcc(
					data = matrixData, type = "R",
					limits = c(input$control_chart_r_lcl, input$control_chart_r_ucl)
				)
			}, error = function(err) {
				returnPlot <- textPlot(paste0("There is no proper data for ", input$control_chart_column))
			})
			return(returnPlot)
		})
	})
	output$stratification_filters <- renderUI({
		plots__trigger$depend()
		plotVariables <- names(select_if(mainData, is.numeric))
		fluidRow(
			column(
				2,
				dateInput(
					"stratification_filters_date_from", "From date",
					minDate, minDate, maxDate
				)
			),
			column(
				2,
				dateInput(
					"stratification_filters_date_to", "To date",
					maxDate, minDate, maxDate
				)
			),
			column(
				2,
				pickerInput(
					"stratification_filters_family", "Family",
					familyOptions, familyOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"stratification_filters_cust", "Customer",
					custOptions, custOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"stratification_filters_model", "Model",
					modelOptions, modelOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				2,
				pickerInput(
					"stratification_filters_operator", "Operator",
					operatorOptions, operatorOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			)
		)
	})
	output$stratification_table <- function() {
		plots__trigger$depend()
		req(input$stratification_filters_date_from)
		req(input$stratification_filters_date_to)
		req(input$stratification_filters_family)
		req(input$stratification_filters_cust)
		req(input$stratification_filters_model)
		req(input$stratification_filters_operator)
		if (nrow(mainData) == 0) return(data.frame())
		filterData <- mainData %>%
			filter(
				Date >= input$stratification_filters_date_from & Date <= input$stratification_filters_date_to &
				Family %in% input$stratification_filters_family & Cust %in% input$stratification_filters_cust &
				Model %in% input$stratification_filters_model & Opr %in% input$stratification_filters_operator
			)
		filterData$hasPassed <- evaluateFailPass(filterData$Result)
		constFields <- filterData %>% select(Family, Cust) %>% distinct(Family, Cust)
		unformattedData <- filterData %>% group_by(Family, Cust, shift) %>%
			summarise(passCount = sum(hasPassed), failCount = n() - passCount) %>%
			ungroup() %>% select(Family, Cust, shift, passCount, failCount)
		filterData$DayOfWeek <- weekdays(filterData$Date)
		passCountData <- filterData %>% group_by(Family, Cust, shift, Model, Opr, DayOfWeek) %>%
			summarise(passCount = sum(hasPassed), failCount = n() - passCount) %>%
			ungroup() %>% select(Family, Cust, shift, passCount, Model, Opr, DayOfWeek, failCount)
		shiftPlotData <- passCountData %>% group_by(shift) %>%
			summarise(pass_percentage = round(sum(passCount) / (sum(passCount) + sum(failCount)) * 100, 1))
		familyPlotData <- passCountData %>% group_by(Family) %>%
			summarise(pass_percentage = round(sum(passCount) / (sum(passCount) + sum(failCount)) * 100, 1))
		customerPlotData <- passCountData %>% group_by(Cust) %>%
			summarise(pass_percentage = round(sum(passCount) / (sum(passCount) + sum(failCount)) * 100, 1))
		modelPlotData <- passCountData %>% group_by(Model) %>%
			summarise(pass_percentage = round(sum(passCount) / (sum(passCount) + sum(failCount)) * 100, 1))
		operatorPlotData <- passCountData %>% group_by(Opr) %>%
			summarise(pass_percentage = round(sum(passCount) / (sum(passCount) + sum(failCount)) * 100, 1))
		dayOfWeekPlotData <- passCountData %>% group_by(DayOfWeek) %>%
			summarise(pass_percentage = round(sum(passCount) / (sum(passCount) + sum(failCount)) * 100, 1))
		output$stratification_shift <- renderPlotly({
			req(input$stratification_filters_date_from)
			req(input$stratification_filters_date_to)
			req(input$stratification_filters_family)
			req(input$stratification_filters_cust)
			req(input$stratification_filters_model)
			req(input$stratification_filters_operator)
			if (nrow(shiftPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = shiftPlotData,
				x = ~pass_percentage,
				y = reorder(shiftPlotData$shift, shiftPlotData$pass_percentage),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s Pass percentage</i>: <b>%{x:.2f}%</b><extra></extra>"
				)
			) %>% layout(
				title = "% of Pass across different Shifts",
				xaxis = list(visible = FALSE)
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_family <- renderPlotly({
			req(input$stratification_filters_date_from)
			req(input$stratification_filters_date_to)
			req(input$stratification_filters_family)
			req(input$stratification_filters_cust)
			req(input$stratification_filters_model)
			req(input$stratification_filters_operator)
			if (nrow(familyPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = familyPlotData,
				x = ~pass_percentage,
				y = reorder(familyPlotData$Family, familyPlotData$pass_percentage),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s Pass percentage</i>: <b>%{x:.2f}%</b><extra></extra>"
				)
			) %>% layout(
				title = "% of Pass across different Families",
				xaxis = list(visible = FALSE)
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_customer <- renderPlotly({
			req(input$stratification_filters_date_from)
			req(input$stratification_filters_date_to)
			req(input$stratification_filters_family)
			req(input$stratification_filters_cust)
			req(input$stratification_filters_model)
			req(input$stratification_filters_operator)
			if (nrow(customerPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = customerPlotData,
				x = ~pass_percentage,
				y = reorder(customerPlotData$Cust, customerPlotData$pass_percentage),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s Pass percentage</i>: <b>%{x:.2f}%</b><extra></extra>"
				)
			) %>% layout(
				title = "% of Pass across different Customers",
				xaxis = list(visible = FALSE)
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_model <- renderPlotly({
			req(input$stratification_filters_date_from)
			req(input$stratification_filters_date_to)
			req(input$stratification_filters_family)
			req(input$stratification_filters_cust)
			req(input$stratification_filters_model)
			req(input$stratification_filters_operator)
			if (nrow(modelPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = modelPlotData,
				x = ~pass_percentage,
				y = reorder(modelPlotData$Model, modelPlotData$pass_percentage),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s Pass percentage</i>: <b>%{x:.2f}%</b><extra></extra>"
				)
			) %>% layout(
				title = "% of Pass across different Models",
				xaxis = list(visible = FALSE)
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_operator <- renderPlotly({
			req(input$stratification_filters_date_from)
			req(input$stratification_filters_date_to)
			req(input$stratification_filters_family)
			req(input$stratification_filters_cust)
			req(input$stratification_filters_model)
			req(input$stratification_filters_operator)
			if (nrow(operatorPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = operatorPlotData,
				x = ~pass_percentage,
				y = reorder(operatorPlotData$Opr, operatorPlotData$pass_percentage),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s Pass percentage</i>: <b>%{x:.2f}%</b><extra></extra>"
				)
			) %>% layout(
				title = "% of Pass across different Operators",
				xaxis = list(visible = FALSE)
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_day_of_week <- renderPlotly({
			req(input$stratification_filters_date_from)
			req(input$stratification_filters_date_to)
			req(input$stratification_filters_family)
			req(input$stratification_filters_cust)
			req(input$stratification_filters_model)
			req(input$stratification_filters_operator)
			if (nrow(dayOfWeekPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = dayOfWeekPlotData,
				x = ~pass_percentage,
				y = reorder(dayOfWeekPlotData$DayOfWeek, dayOfWeekPlotData$pass_percentage),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s Pass percentage</i>: <b>%{x:.2f}%</b><extra></extra>"
				)
			) %>% layout(
				title = "% of Pass across different days of the week"
			) %>% config(displayModeBar = FALSE)
		})
		shiftOneData <- constFields %>% left_join(unformattedData %>% filter(shift == shiftNames[1]), by = c("Family", "Cust")) %>%
			select(Family, Customer = Cust, Pass = passCount, Fail = failCount) %>% arrange(Family, Customer)
		shiftTwoData <- constFields %>% left_join(unformattedData %>% filter(shift == shiftNames[2]), by = c("Family", "Cust")) %>%
			arrange(Family, Cust) %>% select(Pass = passCount, Fail = failCount)
		shiftThreeData <- constFields %>% left_join(unformattedData %>% filter(shift == shiftNames[3]), by = c("Family", "Cust")) %>%
			arrange(Family, Cust) %>% select(Pass = passCount, Fail = failCount)
		tableData <- cbind(shiftOneData, shiftTwoData, shiftThreeData)
		kable(tableData, "html") %>%
			kable_styling("striped", full_width = F) %>%
			add_header_above(c(" " = 2, "Shift 1" = 2, "Shift 2" = 2, "Shift 3" = 2))
	}

	output$headerText <- renderText({
		plots__trigger$depend()
		paste0("Total Fails: ", nrow(checkSheetData))
	})
	output$check_sheet_table <- renderDT({
		plots__trigger$depend()
		if (nrow(checkSheetData) == 0) return(data.frame())
		checkSheetTableData <<- checkSheetData %>%
			select(Date, Shift, Model, "Defects Category" = defects_category, "Defects Qty" = defects_qty)
		datatable(
			checkSheetTableData,
			rownames = FALSE,
			editable = TRUE,
			class = "cell-border stripe",
			options = list(
				pageLength = 20
			)
		)
	})
	tableOutputProxy <- dataTableProxy("check_sheet_table")
	observeEvent(input$check_sheet_table_cell_edit, {
		info = input$check_sheet_table_cell_edit
		if (!info$value %in% defectsCategories) {
			popUpWindow(
				paste0(
					"<b>Please enter the Defects category from one of these values:</b><br><br>",
					paste(defectsCategories, collapse = ", ")
				)
			)
			replaceData(tableOutputProxy, checkSheetTableData, resetPaging = FALSE, rownames = FALSE)
			return()
		}
		if (info$col == 3) {
			row_id <- checkSheetData[info$row, "id"]
			updateDefectInDB(id = row_id, defect_cat = info$value)
			checkSheetData$defects_category[checkSheetData$id == row_id] <<- info$value
			checkSheetTableData[info$row, "Defects Category"] <<- info$value
			replaceData(tableOutputProxy, checkSheetTableData, resetPaging = FALSE, rownames = FALSE)
		} else {
			replaceData(tableOutputProxy, checkSheetTableData, resetPaging = FALSE, rownames = FALSE)
		}
	})
}

shinyApp(ui, server)