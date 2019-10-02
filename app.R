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
	# mainData$shift <- getShifts(mainData$DateTime)
	# mainData[is.na(mainData)] <- "NA"
	# mainData$Date <- as.Date(mainData$DateTime)
	# minDate <- as.Date(min(mainData$DateTime))
	# maxDate <- as.Date(max(mainData$DateTime))
	# familyOptions <- unique(mainData$Family)
	# custOptions <- unique(mainData$Cust)
	# modelOptions <- unique(mainData$Model)
	# resultOptions <- unique(mainData$Result)
	# operatorOptions <- unique(mainData$Opr)
	mainData <- data.frame()
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
							"Make sure that the uploaded file has the columns with these names<br>
							<b>'Family', 'Cust', 'Model' and 'Result'</b>"
						)
					)
				)
			} else {
				ui <- HTML("The data from PostgreSQL will be used for analysis!")
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
		mainData$shift <<- getShifts(mainData$DateTime)
		mainData[is.na(mainData)] <<- "NA"
		mainData$Date <<- as.Date(mainData$DateTime)
		minDate <<- as.Date(min(mainData$DateTime))
		maxDate <<- as.Date(max(mainData$DateTime))
		familyOptions <<- unique(mainData$Family)
		custOptions <<- unique(mainData$Cust)
		modelOptions <<- unique(mainData$Model)
		resultOptions <<- unique(mainData$Result)
		operatorOptions <<- unique(mainData$Opr)
		histogram__trigger$trigger()
	})

	output$histogram_filters <- renderUI({
		histogram__trigger$depend()
		plotVariables <- names(select_if(mainData, is.numeric))
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
					familyOptions, familyOptions, multiple = TRUE
				)
			),
			column(
				2,
				pickerInput(
					"histogram_filters_cust", "Customer",
					custOptions, custOptions, multiple = TRUE
				)
			),
			column(
				2,
				pickerInput(
					"histogram_filters_model", "Model",
					modelOptions, modelOptions, multiple = TRUE
				)
			),
			column(
				2,
				pickerInput(
					"histogram_filters_result", "Result",
					resultOptions, resultOptions, multiple = TRUE
				)
			),
			column(
				4,
				pickerInput(
					"histogram_filters_shift", "Shift",
					shiftNames, shiftNames, multiple = TRUE
				)
			),
			column(
				4,
				pickerInput(
					"histogram_filters_machine", "Machine",
					machinesList, machinesList, multiple = TRUE
				)
			),
			column(
				4,
				pickerInput(
					"histogram_filters_operator", "Operator",
					operatorOptions, operatorOptions, multiple = TRUE
				)
			),
			column(
				4, offset = 1,
				pickerInput(
					"histogram_column", "Select the plot variable",
					plotVariables, plotVariables[20]
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
		input$histogram_filters_operator), {
		plotData <- mainData %>%
			filter(
				Family %in% input$histogram_filters_family & Cust %in% input$histogram_filters_cust &
				Model %in% input$histogram_filters_model & Result %in% input$histogram_filters_result &
				Date >= input$histogram_filters_date_from & Date <= input$histogram_filters_date_to &
				shift %in% input$histogram_filters_shift & Opr %in% input$histogram_filters_operator
			)
		if (nrow(plotData) == 0) {
			output$histogram_plot <- renderPlot(textPlot())
			return(NULL)
		}
		plot_variable <- plotData[[input$histogram_column]]
		plot_variable <- plot_variable[!is.na(plot_variable)]
		lslValue <- mean(plot_variable) - 3 * sd(plot_variable)
		uslValue <- mean(plot_variable) + 3 * sd(plot_variable)
		updateNumericInput(session, "histogram_lsl", value = round(lslValue, 2))
		updateNumericInput(session, "histogram_usl", value = round(uslValue, 2))
		output$histogram_plot <- renderPlot({
			histogram__trigger$depend()
			process.capability(
				qcc(
					plot_variable,
					type = "xbar.one",
					nsigmas = 3,
					plot = FALSE
				),
				spec.limits = c(input$histogram_lsl, input$histogram_usl)
			)
		})
	})

	output$scatter_plot_filters <- renderUI({
		histogram__trigger$depend()
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
					familyOptions, familyOptions, multiple = TRUE
				)
			),
			column(
				2,
				pickerInput(
					"scatter_plot_filters_cust", "Customer",
					custOptions, custOptions, multiple = TRUE
				)
			),
			column(
				2,
				pickerInput(
					"scatter_plot_filters_model", "Model",
					modelOptions, modelOptions, multiple = TRUE
				)
			),
			column(
				2,
				pickerInput(
					"scatter_plot_filters_result", "Result",
					resultOptions, resultOptions, multiple = TRUE
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_filters_shift", "Shift",
					shiftNames, shiftNames, multiple = TRUE
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_filters_machine", "Machine",
					machinesList, machinesList, multiple = TRUE
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_filters_operator", "Operator",
					operatorOptions, operatorOptions, multiple = TRUE
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_x_axis", "Select the X axis variable",
					numericPlotVariables, numericPlotVariables[1]
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_y_axis", "Select the X axis variable",
					numericPlotVariables, numericPlotVariables[2]
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_color_axis", "Select the color axis variable",
					c("No color", factorPlotVariables), "No color"
				)
			)
		)
	})
	output$scatter_plot <- renderPlotly({
		histogram__trigger$depend()
		plotData <- mainData %>%
			filter(
				Family %in% input$scatter_plot_filters_family & Cust %in% input$scatter_plot_filters_cust &
				Model %in% input$scatter_plot_filters_model & Result %in% input$scatter_plot_filters_result &
				Date >= input$scatter_plot_filters_date_from & Date <= input$scatter_plot_filters_date_to &
				shift %in% input$scatter_plot_filters_shift & Opr %in% input$scatter_plot_filters_operator
			)
		if (nrow(plotData) == 0) {
			return(ggplotly(textPlot()))
		}
		if (input$scatter_plot_color_axis == "No color") {
			plot <- plot_ly(
				data = plotData, x = plotData[[input$scatter_plot_x_axis]],
				y = plotData[[input$scatter_plot_y_axis]],
				type = "scatter", mode = "markers", size = 5
			)
		} else {
			plot <- plot_ly(
				data = plotData, x = plotData[[input$scatter_plot_x_axis]],
				y = plotData[[input$scatter_plot_y_axis]], color = plotData[[input$scatter_plot_color_axis]],
				type = "scatter", mode = "markers", size = 5
			)
		}
		return(plot)
	})

	output$control_chart_filters <- renderUI({
		histogram__trigger$depend()
		familyOptions <- unique(mainData$Family)
		custOptions <- unique(mainData$Cust)
		modelOptions <- unique(mainData$Model)
		resultOptions <- unique(mainData$Result)
		plotVariables <- names(select_if(mainData, is.numeric))
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
					familyOptions, familyOptions, multiple = TRUE
				)
			),
			column(
				2,
				pickerInput(
					"control_chart_filters_cust", "Customer",
					custOptions, custOptions, multiple = TRUE
				)
			),
			column(
				2,
				pickerInput(
					"control_chart_filters_model", "Model",
					modelOptions, modelOptions, multiple = TRUE
				)
			),
			column(
				2,
				pickerInput(
					"control_chart_filters_result", "Result",
					resultOptions, resultOptions, multiple = TRUE
				)
			),
			column(
				4,
				pickerInput(
					"control_chart_filters_shift", "Shift",
					shiftNames, shiftNames, multiple = TRUE
				)
			),
			column(
				4,
				pickerInput(
					"control_chart_filters_machine", "Machine",
					machinesList, machinesList, multiple = TRUE
				)
			),
			column(
				4,
				pickerInput(
					"control_chart_filters_operator", "Operator",
					operatorOptions, operatorOptions, multiple = TRUE
				)
			),
			column(
				4, offset = 1,
				pickerInput(
					"control_chart_column", "Select the plot variable",
					plotVariables, plotVariables[20]
				)
			),
			column(3, numericInput("control_chart_lsl", "Enter the LSL", value = 0)),
			column(3, numericInput("control_chart_usl", "Enter the USL", value = 0))
		)
	})
	observeEvent(c(
		input$control_chart_column, input$control_chart_filters_family,
		input$control_chart_filters_cust, input$control_chart_filters_model,
		input$control_chart_filters_result, input$control_chart_filters_date_from,
		input$control_chart_filters_date_to, input$control_chart_filters_shift,
		input$control_chart_filters_operator), {
		plotData <- mainData %>%
			filter(
				Family %in% input$control_chart_filters_family & Cust %in% input$control_chart_filters_cust &
				Model %in% input$control_chart_filters_model & Result %in% input$control_chart_filters_result &
				Date >= input$control_chart_filters_date_from & Date <= input$control_chart_filters_date_to &
				shift %in% input$control_chart_filters_shift & Opr %in% input$control_chart_filters_operator
			)
		if (nrow(plotData) == 0) {
			output$control_chart_plot <- renderPlot(textPlot())
			return(NULL)
		}
		plot_variable <- plotData[[input$control_chart_column]]
		plot_variable <- plot_variable[!is.na(plot_variable)]
		lslValue <- mean(plot_variable) - 3 * sd(plot_variable)
		uslValue <- mean(plot_variable) + 3 * sd(plot_variable)
		updateNumericInput(session, "control_chart_lsl", value = round(lslValue, 2))
		updateNumericInput(session, "control_chart_usl", value = round(uslValue, 2))
		output$control_chart_plot_xbar_one <- renderPlot({
			histogram__trigger$depend()
			qcc(data = plot_variable, type = "xbar.one", limits = c(input$control_chart_lsl, input$control_chart_usl))
		})
	})
}

shinyApp(ui, server)