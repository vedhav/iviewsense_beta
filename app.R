source("global.R")

ui = tags$div(
	tags$head(
		tags$link(rel = "shortcut icon", type = "image/png", href = "iviewsense.png"),
		tags$base(target = "_blank")
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
					# bs4SidebarMenuItem(
					# 	text = "Summary",
					# 	tabName = "summary",
					# 	icon = "chart-pie"
					# ),
					bs4SidebarMenuItem(
						text = "Data Source",
						tabName = "data_source",
						icon = "database"
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
						text = "Stratification",
						tabName = "stratification",
						icon = "layer-group"
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
					)
				)
			),
			body = bs4DashBody(
				bs4TabItems(
					# summary_body,
					data_source_body,
					control_chart_body,
					histogram_body,
					scatter_plot_body,
					stratification_body,
					pareto_body,
					cause_effect_body
				)
			)
		)
	)
)

server = function(input, output, session) {
	mainData <- data.frame()
	checkSheetTableData <- data.frame()
	hasDbConnection <- FALSE
	causeEffectToken <- 0
	stratificationTable <- data.frame()
	checkSheetFilterData <- data.frame()
	checkSheetFilterDataDisplay <- data.frame()
	checkSheetCreateData <- data.frame()
	config_table <- data.frame()
	lsl_usl_data <- data.frame()
	lsl_usl_data_display <- data.frame()
	all_table_data <- tibble()
	currentTableName <- ""

	######################################## SUMMARY ########################################
	output$summary_body_ui <- renderUI({
		all_table_data <<- get_all_table_data()
		min_date <- min(all_table_data$Date)
		max_date <- max(all_table_data$Date)
		tags$div(
			fluidRow(
				column(
					2,
					dateInput(
						"summary_filters_date_from", "From date",
						max_date - 10, min_date, max_date
					)
				),
				column(
					2,
					dateInput(
						"summary_filters_date_to", "To date",
						max_date, min_date, max_date
					)
				),
				column(
					3, style = "margin-top: 25px;", allign = "left",
					switchInput(
						inputId = "summary_stats_type", size = "mini",
						onLabel = "Show percentage", offLabel = "Show numbers", value = FALSE
					)
				)
			),
			fluidRow(
				column(5, plotlyOutput("summary_overall_plot")),
				column(7, plotlyOutput("summary_number_of_fails_plot")),
				column(12, br()),
				column(6, plotlyOutput("summary_machine_plot", height = "500px")),
				column(6, plotlyOutput("summary_customer_plot", height = "500px")),
				column(12, br()),
				column(6, plotlyOutput("summary_family_plot", height = "1200px")),
				column(6, plotlyOutput("summary_model_plot", height = "1200px"))
			)
		)
	})
	output$summary_overall_plot <- renderPlotly({
		req(input$summary_filters_date_from)
		req(input$summary_filters_date_to)
		if (nrow(all_table_data) == 0) {
			return()
		}
		plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
			group_by(Result) %>% summarise(count = n())
		plotData$Result[plotData$Result == passName] <- "FTR"
		plotData$Result[plotData$Result == failName] <- "Fail"
		plot_ly(
			data = plotData, labels = ~Result, values = ~count,
			marker = list(colors = c("#edb009", "#5ac938")),
			type = 'pie', hole = 0.5
		) %>% layout(
			title = "Overall stats",
			legend = list(y = 0.5)
		)
	})
	output$summary_number_of_fails_plot <- renderPlotly({
		req(input$summary_filters_date_from)
		req(input$summary_filters_date_to)
		if (nrow(all_table_data) == 0) {
			return()
		}
		if (input$summary_stats_type) {
			plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
				group_by(Date, Result) %>% arrange(Date) %>% summarise(count = n())
			plotDataPass <- plotData %>% filter(Result == passName) %>% select(Date, pass = count)
			plotDataCount <- plotData %>% group_by(Date) %>% summarise(count = sum(count))
			plotData <- inner_join(plotDataPass, plotDataCount, by = "Date")
			plotData$FTR <- round(plotData$pass/plotData$count * 100, 1)
			plotData$Text <- paste0(round(plotData$FTR, 0), "%")
			plot <- plot_ly(
				data = plotData, x = ~Date, y = ~FTR, text = ~Text, textposition = 'auto',
				marker = list(color = "#5ac938"), type = "bar", name = "",
				hovertemplate = paste0("<i>Date: %{x}<br>FTR: %{text}</i>")
			) %>% layout(barmode = 'stack', title = "Daily status", legend = list(y = 0.5))
		} else {
			plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
				group_by(Date, Result) %>% arrange(Date) %>% summarise(count = n())
			plotData$Date <- format(plotData$Date, format = "%d-%b-%y")
			plotData$Result[plotData$Result == passName] <- "FTR"
			plotData$Result[plotData$Result == failName] <- "Fail"
			plotData$csum <- ave(plotData$count, plotData$Date, FUN = cumsum) - (mean(plotData$count)/4)
			plotData$csum <- if_else(plotData$csum < 10, 10, plotData$csum)
			plot <- plot_ly(
				data = plotData, x = ~Date, y = ~count, color = ~Result,
				colors = c("#edb009", "#5ac938"), type = "bar"
			) %>%
			add_annotations(
				y = plotData$csum, text = plotData$count, name = "",
				showarrow = FALSE, xanchor = "center"
			) %>%
			layout(barmode = 'stack', title = "Daily status", legend = list(y = 0.5))
		}
		return(plot)
	})
	output$summary_machine_plot <- renderPlotly({
		req(input$summary_filters_date_from)
		req(input$summary_filters_date_to)
		if (nrow(all_table_data) == 0) {
			return()
		}
		if (input$summary_stats_type) {
			plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
				group_by(Machine, Result) %>% summarise(count = n())
			plotDataPass <- plotData %>% filter(Result == passName) %>% select(Machine, pass = count)
			plotDataCount <- plotData %>% group_by(Machine) %>% summarise(count = sum(count))
			plotData <- inner_join(plotDataPass, plotDataCount, by = "Machine")
			plotData$FTR <- round(plotData$pass/plotData$count * 100)
			plotData$Text <- paste0(round(plotData$FTR, 0), "%")
			plot <- plot_ly(
				data = plotData, x = ~FTR, y = reorder(plotData$Machine, plotData$FTR),
				text = ~Text, textposition = 'auto',
				marker = list(color = "#5ac938"), type = "bar", name = "",
				hovertemplate = paste0("<i>Machine: %{y}<br>FTR: %{text}</i>")
			) %>% layout(barmode = 'stack', title = "Machine status", legend = list(y = 0.5))
		} else {
			plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
				group_by(Machine, Result) %>% summarise(count = n())
			plotData$Result[plotData$Result == passName] <- "FTR"
			plotData$Result[plotData$Result == failName] <- "Fail"
			plot <- plot_ly(
				data = plotData, x = ~count, y = reorder(plotData$Machine, plotData$count), color = ~Result,
				colors = c("#edb009", "#5ac938"), type = "bar"
			) %>% layout(barmode = 'stack', title = "Machine status", legend = list(y = 0.5))
		}
		return(plot)
	})
	output$summary_customer_plot <- renderPlotly({
		req(input$summary_filters_date_from)
		req(input$summary_filters_date_to)
		if (nrow(all_table_data) == 0) {
			return()
		}
		if (input$summary_stats_type) {
			plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
				group_by(Cust, Result) %>% summarise(count = n())
			plotDataPass <- plotData %>% filter(Result == passName) %>% select(Cust, pass = count)
			plotDataCount <- plotData %>% group_by(Cust) %>% summarise(count = sum(count))
			plotData <- inner_join(plotDataPass, plotDataCount, by = "Cust")
			plotData$FTR <- round(plotData$pass/plotData$count * 100)
			plotData$Text <- paste0(round(plotData$FTR, 0), "%")
			plot <- plot_ly(
				data = plotData, x = ~FTR, y = reorder(plotData$Cust, plotData$FTR),
				marker = list(color = "#5ac938"), type = "bar", name = "", text = ~Text, textposition = 'auto',
				hovertemplate = paste0("<i>Customer: %{y}<br>FTR: %{text}</i>")
			) %>% layout(barmode = 'stack', title = "Customer status", legend = list(y = 0.5))
		} else {
			plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
				group_by(Cust, Result) %>% summarise(count = n())
			plotData$Result[plotData$Result == passName] <- "FTR"
			plotData$Result[plotData$Result == failName] <- "Fail"
			plot <- plot_ly(
				data = plotData, x = ~count, y = reorder(plotData$Cust, plotData$count), color = ~Result,
				colors = c("#edb009", "#5ac938"), type = "bar"
			) %>% layout(barmode = 'stack', title = "Customer status", legend = list(y = 0.5))
		}
		return(plot)
	})
	output$summary_family_plot <- renderPlotly({
		req(input$summary_filters_date_from)
		req(input$summary_filters_date_to)
		if (nrow(all_table_data) == 0) {
			return()
		}
		if (input$summary_stats_type) {
			plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
				group_by(Family, Result) %>% summarise(count = n())
			plotDataPass <- plotData %>% filter(Result == passName) %>% select(Family, pass = count)
			plotDataCount <- plotData %>% group_by(Family) %>% summarise(count = sum(count))
			plotData <- inner_join(plotDataPass, plotDataCount, by = "Family")
			plotData$FTR <- round(plotData$pass/plotData$count * 100)
			plotData$Text <- paste0(round(plotData$FTR, 0), "%")
			plot <- plot_ly(
				data = plotData, x = ~FTR, y = reorder(plotData$Family, plotData$FTR),
				marker = list(color = "#5ac938"), type = "bar", name = "", text = ~Text, textposition = 'auto',
				hovertemplate = paste0("<i>Family: %{y}<br>FTR: %{text}</i>")
			) %>% layout(barmode = 'stack', title = "Family status", legend = list(y = 0.5))
		} else {
			plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
				group_by(Family, Result) %>% summarise(count = n())
			plotData$Result[plotData$Result == passName] <- "FTR"
			plotData$Result[plotData$Result == failName] <- "Fail"
			plot <- plot_ly(
				data = plotData, x = ~count, y = reorder(plotData$Family, plotData$count), color = ~Result,
				colors = c("#edb009", "#5ac938"), type = "bar"
			) %>% layout(barmode = 'stack', title = "Family status", legend = list(y = 0.5))
		}
		return(plot)
	})
	output$summary_model_plot <- renderPlotly({
		req(input$summary_filters_date_from)
		req(input$summary_filters_date_to)
		if (nrow(all_table_data) == 0) {
			return()
		}
		if (input$summary_stats_type) {
			plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
				group_by(Model, Result) %>% summarise(count = n())
			plotDataPass <- plotData %>% filter(Result == passName) %>% select(Model, pass = count)
			plotDataCount <- plotData %>% group_by(Model) %>% summarise(count = sum(count))
			plotData <- inner_join(plotDataPass, plotDataCount, by = "Model")
			plotData$FTR <- round(plotData$pass/plotData$count * 100)
			plotData$Text <- paste0(round(plotData$FTR, 0), "%")
			plot <- plot_ly(
				data = plotData, x = ~FTR, y = reorder(plotData$Model, plotData$FTR),
				marker = list(color = "#5ac938"), type = "bar", name = "", text = ~Text, textposition = 'auto',
				hovertemplate = paste0("<i>Model: %{y}<br>FTR: %{text}</i>")
			) %>% layout(barmode = 'stack', title = "Model status", legend = list(y = 0.5))
		} else {
			plotData <- all_table_data %>% filter(Date >= input$summary_filters_date_from & Date <= input$summary_filters_date_to) %>%
				group_by(Model, Result) %>% summarise(count = n())
			plotData$Result[plotData$Result == passName] <- "FTR"
			plotData$Result[plotData$Result == failName] <- "Fail"
			plot <- plot_ly(
				data = plotData, x = ~count, y = reorder(plotData$Model, plotData$count), color = ~Result,
				colors = c("#edb009", "#5ac938"), type = "bar"
			) %>% layout(barmode = 'stack', title = "Model status", legend = list(y = 0.5))
		}
		return(plot)
	})


	######################################## DATA SOURCE ########################################
	observeEvent(input$remote_or_local, {
		output$data_source_body_ui <- renderUI({
			if (input$remote_or_local %% 2 != 0) {
				ui <- fluidRow(
					column(
						12,
						fileInput(
							"data_source_input_file",
							"Upload your file or drag and drop it here",
							accept = ".csv"
						)
					),
					column(
						12, HTML(
							paste0(
								"Make sure that the first 9 columns contain these values in the same order<br>
								<b>", dataColumnNamesString, "</b><br>",
								"And the 'Date Time' Column is of this format '29-12-2019  18:52:27' (date-month-year hour:min:sec)"
							)
						)
					)
				)
			} else {
				config_table <<- selectDbQuery("SELECT * FROM config")
				config_machine_options <- unique(config_table$Machine)
				ui <- tags$div(
					fluidRow(
						column(3, pickerInput("config_machine", "Machine", config_machine_options, config_machine_options[1])),
						column(3, pickerInput("config_customer", "Customer", "", "")),
						column(3, pickerInput("config_family", "Family", "", "")),
						column(3, pickerInput("config_model", "Model", "", ""))
					),
					tags$div(
						id = 'admin_authentication_div',
						fluidRow(
							style = "margin-top: 20vh",
							column(3, ""),
							column(3, passwordInput('admin_password', '', placeholder = 'Enter admin password', width = "100%")),
							column(1, actionButton('login_button', 'Login', style = buttonStyle)),
							column(2, style = "margin-top: 23px", actionLink('password_change_button', 'Change password')),
							column(3, "")
						)
					),
					shinyjs::hidden(
						tags$div(
							id = 'admin_div',
							fluidRow(
								column(12, DTOutput("admin_editable_table"))
							)
						)
					)
				)
			}
			return(ui)
		})
	})
	observeEvent(input$password_change_button, {
		showModal(
			modalDialog(
				title = NULL, easyClose = TRUE, footer = NULL,
				fluidRow(
					align = "center",
					column(12, passwordInput("password_reset_old_password", "Enter the Old Password")),
					column(12, passwordInput("password_reset_new_password_1", "Enter a New Password")),
					column(12, passwordInput("password_reset_new_password_2", "Confirm a New Password")),
					column(12, actionButton("update_new_admin_password", "Update Password", style = buttonStyle))
				)
			)
		)
	})
	observeEvent(input$update_new_admin_password, {
		actual_admin_password <- selectDbQuery("SELECT * FROM config_table_master WHERE table_name = 'admin_password'")
		if (digest(input$password_reset_old_password) != actual_admin_password$column_name) {
			popUpWindow("The old password you entered is incorrect")
			return()
		}
		if (input$password_reset_new_password_1 != input$password_reset_new_password_2) {{
			popUpWindow("The new passwords do not match, please try again")
			return()
		}}
		updateAdminPassword(digest(input$password_reset_new_password_1))
		popUpWindow("The admin password has been changed")
	})
	observeEvent(c(input$config_machine, input$remote_or_local), {
		config_table_filtered <- config_table %>% filter(Machine == input$config_machine)
		config_customer_options <- unique(config_table_filtered$CUSTOMER)
		updatePickerInput(session, "config_customer", choices = config_customer_options, selected = config_customer_options[1])
	})
	observeEvent(input$config_customer, {
		config_table_filtered <- config_table %>% filter(Machine == input$config_machine) %>%
			filter(CUSTOMER == input$config_customer)
		config_family_options <- unique(config_table_filtered$FAMILY)
		updatePickerInput(session, "config_family", choices = config_family_options, selected = config_family_options[1])
	})
	observeEvent(input$config_family, {
		config_table_filtered <- config_table %>% filter(Machine == input$config_machine) %>%
			filter(CUSTOMER == input$config_customer) %>% filter(FAMILY == input$config_family)
		config_model_options <- unique(config_table_filtered$MODEL)
		updatePickerInput(session, "config_model", choices = config_model_options, selected = config_model_options[1])
	})
	observeEvent(input$config_model, {
		config_table_filtered <- config_table %>% filter(Machine == input$config_machine) %>%
			filter(CUSTOMER == input$config_customer) %>% filter(FAMILY == input$config_family) %>%
			filter(MODEL == input$config_model)
		if (nrow(config_table_filtered) != 0) {
			currentTableName <<- config_table_filtered$TABLE_NAME[1]
			print(paste0("Fetching data from ", config_table_filtered$TABLE_NAME[1]))
			lsl_usl_data <<- selectDbQuery("SELECT * FROM config_table_master WHERE table_name = ?", list(currentTableName))
			mainData <<- tryCatch({
				selectDbQuery(paste0("SELECT * FROM `", config_table_filtered$TABLE_NAME[1], "`")) %>% formatRemoteData()
			}, error = function(err) {
				popUpWindow("The data is not of the proper format, please select some other option.")
				return(data.frame())
			})
			if (nrow(mainData) == 0) {
				popUpWindow("The date format is not of the proper format, please select some other option.")
				return()
			}
			minDate <<- as.Date(min(mainData$Date_Time), tz = "")
			maxDate <<- as.Date(max(mainData$Date_Time), tz = "")
			familyOptions <<- unique(mainData$Family)
			custOptions <<- unique(mainData$Cust)
			modelOptions <<- unique(mainData$Model)
			resultOptions <<- unique(mainData$Result)
			operatorOptions <<- unique(mainData$Opr)
			machineOptions <<- unique(mainData$Machine)
			hasDbConnection <<- TRUE
			plots__trigger$trigger()
		}
		observeEvent(input$login_button, {
			# req(input$admin_password)
			req(input$config_machine)
			req(input$config_customer)
			req(input$config_family)
			req(input$config_model)
			lsl_usl_data <<- selectDbQuery("SELECT * FROM config_table_master WHERE table_name = ?", list(currentTableName))
			actual_admin_password <- selectDbQuery("SELECT * FROM config_table_master WHERE table_name = 'admin_password'")
			if (digest(input$admin_password) == actual_admin_password$column_name) {
				lsl_usl_data_display <<- lsl_usl_data %>% select(-table_name)
				output$admin_editable_table <- renderDT({
					datatable(
						lsl_usl_data_display,
						rownames = FALSE,
						editable = TRUE,
						class = "cell-border stripe",
						options = list(dom = 'tip', pageLength = -1)
					)
				})
				admin_editable_table_output_proxy <- dataTableProxy("admin_editable_table")
				observeEvent(input$admin_editable_table_cell_edit, {
					info = input$admin_editable_table_cell_edit
					this_id <- lsl_usl_data[info$row, "id"]
					this_column_name <- lsl_usl_data[info$row, "column_name"]
					update_column_name <- names(lsl_usl_data_display)[info$col+1]
					updateUnitsTable(id = this_id, column_name = update_column_name, value = info$value)
					lsl_usl_data[lsl_usl_data$id == this_id, update_column_name] <<- info$value
					lsl_usl_data_display[lsl_usl_data_display$id == this_id, update_column_name] <<- info$value
					# This might be optional
					# replaceData(admin_editable_table_output_proxy, lsl_usl_data_display, resetPaging = FALSE, rownames = FALSE)
				})
				shinyjs::hide("admin_authentication_div", anim = TRUE)
				shinyjs::show("admin_div", anim = TRUE)
			} else {
				popUpWindow("The password is incorrect")
			}
		})
	})
	observeEvent(input$data_source_input_file, {
		inFile <- input$data_source_input_file
		mainData <<- read.csv(inFile$datapath, stringsAsFactors = FALSE, header = TRUE) %>% formatLocalData()
		minDate <<- as.Date(min(mainData$Date_Time), tz = "")
		maxDate <<- as.Date(max(mainData$Date_Time), tz = "")
		familyOptions <<- unique(mainData$Family)
		custOptions <<- unique(mainData$Cust)
		modelOptions <<- unique(mainData$Model)
		resultOptions <<- unique(mainData$Result)
		operatorOptions <<- unique(mainData$Opr)
		machineOptions <<- unique(mainData$Machine)
		mainData$Defects_Qty <<- 1
		hasDbConnection <<- FALSE
		plots__trigger$trigger()
	})


	######################################## CONTROL CHART ########################################
	output$control_chart_filters <- renderUI({
		plots__trigger$depend()
		familyOptions <- unique(mainData$Family)
		custOptions <- unique(mainData$Cust)
		modelOptions <- unique(mainData$Model)
		resultOptions <- unique(mainData$Result)
		numericData <- select_if(mainData[,11:(ncol(mainData)-5)], is.numeric)
		plotVariables <- names(numericData)
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
					plotVariables, plotVariables[1],
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(3, numericInput("control_chart_lcl", "Enter the LCL", value = 0)),
			column(3, numericInput("control_chart_ucl", "Enter the UCL", value = 0)),
			column(
				1, style = "margin-top: 35px",
				prettySwitch(inputId = "control_chart_one_manual_automatic", label = "Actual",  status = "primary", slim = TRUE)
			)
		)
	})
	observeEvent(c(
		input$control_chart_column, input$control_chart_filters_family,
		input$control_chart_filters_cust, input$control_chart_filters_model,
		input$control_chart_filters_result, input$control_chart_filters_date_from,
		input$control_chart_filters_date_to, input$control_chart_filters_shift,
		input$control_chart_filters_operator, input$control_chart_filters_machine,
		input$control_chart_one_manual_automatic, input$control_chart_r_manual_automatic), {
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
		if (input$control_chart_one_manual_automatic) {
			lslValue <- lsl_usl_data$LSL[lsl_usl_data$column_name == input$control_chart_column]
			uslValue <- lsl_usl_data$USL[lsl_usl_data$column_name == input$control_chart_column]
		}
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
		tryCatch({
			matrixData <- matrix(cbind(plot_variable[1:length(plot_variable) - 1], plot_variable[2:length(plot_variable)]), ncol = 2)
			outPlot <- qcc(data = matrixData, type = "R", plot = FALSE)
			lslValue <- round(outPlot$limits[1], 2)
			uslValue <- round(outPlot$limits[2], 2)
			if (input$control_chart_r_manual_automatic) {
				lslValue <- lsl_usl_data$LSL[lsl_usl_data$column_name == input$control_chart_column]
				uslValue <- lsl_usl_data$USL[lsl_usl_data$column_name == input$control_chart_column]
			}
			updateNumericInput(
				session, "control_chart_r_lcl", value = lslValue
				# label = HTML(paste0("Enter the LCL (mean - 3 x sd = ", lslValue, ")"))
			)
			updateNumericInput(
				session, "control_chart_r_ucl", value = uslValue
				# label = HTML(paste0("Enter the UCL (mean + 3 x sd = ", uslValue, ")"))
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
		}, error = function(err) {
			return(textPlot(paste0("There is no proper data for ", input$control_chart_column)))
		})
	})

	######################################## HISTOGRAM ########################################
	output$histogram_filters <- renderUI({
		plots__trigger$depend()
		numericData <- select_if(mainData[,11:(ncol(mainData)-5)], is.numeric)
		plotVariables <- names(numericData)
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
				4,
				pickerInput(
					"histogram_column", "Select the plot variable",
					plotVariables, "CW_1",
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(3, numericInput("histogram_lsl", "Enter the LSL", value = 0)),
			column(3, numericInput("histogram_usl", "Enter the USL", value = 0)),
			column(
				1, style = "margin-top: 35px",
				prettySwitch(inputId = "histogram_manual_automatic", label = "Actual",  status = "primary", slim = TRUE)
			)
		)
	})
	observeEvent(c(
		input$histogram_column, input$histogram_filters_family,
		input$histogram_filters_cust, input$histogram_filters_model,
		input$histogram_filters_result, input$histogram_filters_date_from,
		input$histogram_filters_date_to, input$histogram_filters_shift,
		input$histogram_filters_operator, input$histogram_filters_machine,
		input$histogram_manual_automatic), {
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
		outPlot <- qcc(plot_variable, type = "xbar.one", plot = FALSE)
		lslValue <- round(outPlot$limits[1], 2)
		uslValue <- round(outPlot$limits[2], 2)
		if (input$histogram_manual_automatic) {
			lslValue <- lsl_usl_data$LSL[lsl_usl_data$column_name == input$histogram_column]
			uslValue <- lsl_usl_data$USL[lsl_usl_data$column_name == input$histogram_column]
		}
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

	######################################## SCATTER PLOT ########################################
	output$scatter_plot_filters <- renderUI({
		plots__trigger$depend()
		numericData <- select_if(mainData[,11:(ncol(mainData)-5)], is.numeric)
		numericPlotVariables <- names(numericData)
		factorPlotVariables <- names(select_if(mainData[,1:10], is.character))
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
					numericPlotVariables, numericPlotVariables[1],
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				4,
				pickerInput(
					"scatter_plot_y_axis", "Select the Y axis variable",
					numericPlotVariables, numericPlotVariables[2],
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
		req(input$scatter_plot_filters_family)
		plots__trigger$depend()
		if (nrow(mainData) == 0) return(ggplotly(textPlot()))
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
			if (ncol(xyPlotData) != 1) {
				xyPlotData <- xyPlotData[complete.cases(xyPlotData), ]
			}
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

	######################################## STRATIFICATION ########################################
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
		req(input$stratification_filters_family)
		plots__trigger$depend()
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
		familyPlotData <- passCountData %>% select(Family, Pass = passCount, Fail = failCount) %>%
			gather(key = "key", value = "value", 2:3) %>%
			group_by(Family, key) %>% summarise(value = sum(value))
		customerPlotData <- passCountData %>% select(Cust, Pass = passCount, Fail = failCount) %>%
			gather(key = "key", value = "value", 2:3) %>%
			group_by(Cust, key) %>% summarise(value = sum(value))
		modelPlotData <- passCountData %>% select(Model, Pass = passCount, Fail = failCount) %>%
			gather(key = "key", value = "value", 2:3) %>%
			group_by(Model, key) %>% summarise(value = sum(value))
		shiftPlotData <- passCountData %>% select(shift, Pass = passCount, Fail = failCount) %>%
			gather(key = "key", value = "value", 2:3) %>%
			group_by(shift, key) %>% summarise(value = sum(value))
		shiftPlotData <- inner_join(shiftPlotData, shifts_order, by = c("shift" = "shifts"))
		shiftPlotData <- shiftPlotData %>% arrange(order_value)
		dayOfWeekPlotData <- passCountData %>% select(DayOfWeek, Pass = passCount, Fail = failCount) %>%
			gather(key = "key", value = "value", 2:3) %>%
			group_by(DayOfWeek, key) %>% summarise(value = sum(value))
		operatorPlotData <- passCountData %>% select(Opr, Pass = passCount, Fail = failCount) %>%
			gather(key = "key", value = "value", 2:3) %>%
			group_by(Opr, key) %>% summarise(value = sum(value))
		dayOfWeekPlotData <- inner_join(dayOfWeekPlotData, day_of_the_weeks_order, by = c("DayOfWeek" = "dow"))
		dayOfWeekPlotData <- dayOfWeekPlotData %>% arrange(order_value)
		output$stratification_family <- renderPlotly({
			if (nrow(familyPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = familyPlotData,
				x = ~value, color = ~key, text = ~value, colors = c("#edb009", "#5ac938"),
				y = reorder(familyPlotData$Family, familyPlotData$value),
				height = 200, textposition = "auto",
				textfont = list(color = "#000000"),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s count</i>: <b>%{x}</b><extra></extra>"
				)
			) %>% layout(
				title = "Pass-Fail across <b>Family</b>",
				xaxis = list(title = ""), showlegend = FALSE
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_customer <- renderPlotly({
			if (nrow(customerPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = customerPlotData,
				x = ~value, color = ~key, text = ~value, colors = c("#edb009", "#5ac938"),
				y = reorder(customerPlotData$Cust, customerPlotData$value),
				height = 200, textposition = "auto",
				textfont = list(color = "#000000"),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s count</i>: <b>%{x}</b><extra></extra>"
				)
			) %>% layout(
				title = "Pass-Fail across <b>Customer</b>",
				xaxis = list(title = ""), showlegend = FALSE
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_model <- renderPlotly({
			if (nrow(modelPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = modelPlotData,
				x = ~value, color = ~key, text = ~value, colors = c("#edb009", "#5ac938"),
				y = reorder(modelPlotData$Model, modelPlotData$value),
				height = 200, textposition = "auto",
				textfont = list(color = "#000000"),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s count</i>: <b>%{x}</b><extra></extra>"
				)
			) %>% layout(
				title = "Pass-Fail across <b>Model</b>",
				xaxis = list(title = ""), showlegend = FALSE
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_shift <- renderPlotly({
			if (nrow(shiftPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = shiftPlotData,
				x = ~value, color = ~key, text = ~value, colors = c("#edb009", "#5ac938"),
				y = reorder(shiftPlotData$shift, -shiftPlotData$order_value),
				height = 350, textposition = "auto",
				textfont = list(color = "#000000"),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s count</i>: <b>%{x}</b><extra></extra>"
				)
			) %>% layout(
				title = "Pass-Fail across different <b>Shifts</b>",
				xaxis = list(title = ""), showlegend = FALSE
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_day_of_week <- renderPlotly({
			if (nrow(dayOfWeekPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = dayOfWeekPlotData,
				x = ~value, color = ~key, text = ~value, colors = c("#edb009", "#5ac938"),
				y = reorder(dayOfWeekPlotData$DayOfWeek, -dayOfWeekPlotData$order_value),
				height = 400, textposition = "auto",
				textfont = list(color = "#000000"),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s count</i>: <b>%{x}</b><extra></extra>"
				)
			) %>% layout(
				title = "Pass-Fail across different <b>days of the week</b>",
				xaxis = list(title = ""), showlegend = FALSE
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_operator <- renderPlotly({
			if (nrow(operatorPlotData) == 0) return(ggplotly(textPlot()))
			plot_ly(
				data = operatorPlotData,
				x = ~value, color = ~key, text = ~value, colors = c("#edb009", "#5ac938"),
				y = reorder(operatorPlotData$Opr, operatorPlotData$value),
				height = 200, textposition = "auto",
				textfont = list(color = "#000000"),
				type = "bar", orientation = 'h',
				hovertemplate = paste(
					"<i>%{y}'s count</i>: <b>%{x}</b><extra></extra>"
				)
			) %>% layout(
				title = "Pass-Fail across different <b>Operators</b>",
				xaxis = list(title = ""), showlegend = FALSE
			) %>% config(displayModeBar = FALSE)
		})
		output$stratification_legends <- renderUI({
			tags$img(src = "stratification_legends.png", height = "40px", width = "200px", align = "center")
		})
		shiftOneData <- constFields %>% left_join(unformattedData %>% filter(shift == shiftNames[1]), by = c("Family", "Cust")) %>%
			select(Family, Customer = Cust, Pass = passCount, Fail = failCount) %>% arrange(Family, Customer)
		shiftTwoData <- constFields %>% left_join(unformattedData %>% filter(shift == shiftNames[2]), by = c("Family", "Cust")) %>%
			arrange(Family, Cust) %>% select(Pass = passCount, Fail = failCount)
		shiftThreeData <- constFields %>% left_join(unformattedData %>% filter(shift == shiftNames[3]), by = c("Family", "Cust")) %>%
			arrange(Family, Cust) %>% select(Pass = passCount, Fail = failCount)
		observeButton <- data.frame(
			"Edit Checksheet" = shinyInput(actionButton, nrow(shiftOneData), 'button_', label = "check sheet", onclick = 'Shiny.setInputValue(\"navigate_check_sheet\", this.id, {priority: \"event\"})')
		)
		stratificationTable <<- cbind(shiftOneData, shiftTwoData, shiftThreeData, observeButton)
		kable(stratificationTable, "html", escape = FALSE) %>%
			kable_styling("striped", full_width = F) %>%
			add_header_above(c(" " = 2, "Shift 1" = 2, "Shift 2" = 2, "Shift 3" = 2, " " = 1))
	}
	observeEvent(input$navigate_check_sheet, {
		selectedRow <- as.numeric(strsplit(input$navigate_check_sheet, "_")[[1]][2])
		selectedData <- stratificationTable[selectedRow,]
		familyFilter <- selectedData$Family
		CustomerFilter <- selectedData$Cust
		fromDateFilter <- input$stratification_filters_date_from
		toDateFilter <- input$stratification_filters_date_to
		modelFilter <- input$stratification_filters_model
		operatorFilter <- input$stratification_filters_operator
		checkSheetFilterData <<- mainData %>% filter(
			Family %in% familyFilter & Cust %in% CustomerFilter & Date >= fromDateFilter &
			Date <= toDateFilter & Model %in% modelFilter & operatorFilter %in% operatorFilter &
			Result == failName
		)
		checkSheetFilterDataDisplay <<- checkSheetFilterData %>%
			select(Date, shift, Model, "Defects Category" = DEFECTS_CATEGORY) %>% mutate("Defects Quantity" = 1)
		output$check_sheet_table_ui <- renderDT({
			datatable(
				checkSheetFilterDataDisplay,
				rownames = FALSE,
				editable = TRUE,
				class = "cell-border stripe",
				options = list(dom = 'tip')
			)
		})
		tableOutputProxy <- dataTableProxy("check_sheet_table_ui")
		observeEvent(input$check_sheet_table_ui_cell_edit, {
			info = input$check_sheet_table_ui_cell_edit
			if (!info$value %in% defectsCategories) {
				replaceData(tableOutputProxy, checkSheetFilterDataDisplay, resetPaging = FALSE, rownames = FALSE)
				return()
			}
			if (info$col == 3) {
				row_id <- checkSheetFilterData[info$row, "id"]
				causeEffectData <- data.frame(
					effect = info$value,
					Family = checkSheetFilterData[info$row, "Family"],
					Cust = checkSheetFilterData[info$row, "Cust"],
					Model = checkSheetFilterData[info$row, "Model"]
				)
				if (hasDbConnection) updateDefectInDB(table_name = currentTableName, id = row_id, defect_cat = info$value, causeEffectData)
				checkSheetFilterData$DEFECTS_CATEGORY[checkSheetFilterData$id == row_id] <<- info$value
				checkSheetFilterDataDisplay[info$row, "Defects Category"] <<- info$value
				mainData[mainData$id == row_id, "DEFECTS_CATEGORY"] <<- info$value
				mainData[mainData$id == row_id, "CAUSE"] <<- fishBoneSkeleton
				plotData <- mainData %>% filter(DEFECTS_CATEGORY %in% defectsCategories)
				pareto__trigger$trigger()
				replaceData(tableOutputProxy, checkSheetFilterDataDisplay, resetPaging = FALSE, rownames = FALSE)
			} else {
				replaceData(tableOutputProxy, checkSheetFilterDataDisplay, resetPaging = FALSE, rownames = FALSE)
			}
		})
		showModal(
			modalDialog(
				title = "Check Sheet",
				size = "l",
				fluidPage(
					align = "center",
					fluidRow(
						HTML(
							paste0(
								"<b>Please enter the Defects category from one of these values:</b><br>",
								paste(defectsCategories, collapse = ", ")
							)
						)
					),
					fluidRow(DTOutput("check_sheet_table_ui"))
				),
				footer = NULL,
				easyClose = TRUE
			)
		)
	})

	######################################## PARETO ########################################
	output$pareto_filters <- renderUI({
		plots__trigger$depend()
		pareto__trigger$depend()
		tableData <- mainData %>% filter(DEFECTS_CATEGORY %in% defectsCategories)
		if (nrow(tableData) == 0) return()
		defectsList <- unique(tableData$DEFECTS_CATEGORY)
		familyList <- unique(tableData$Family)
		customerList <- unique(tableData$Cust)
		modelList <- unique(tableData$Model)
		shiftList <- unique(tableData$shift)
		machineOptions <- unique(tableData$Machine)
		minDate <- min(tableData$Date)
		maxDate <- max(tableData$Date)
		fluidRow(
			column(
				3,
				dateInput(
					"pareto_filters_date_from", "From date",
					minDate, minDate, maxDate
				)
			),
			column(
				3,
				dateInput(
					"pareto_filters_date_to", "To date",
					maxDate, minDate, maxDate
				)
			),
			column(
				3,
				pickerInput(
					"pareto_filters_machine", "Machine",
					machineOptions, machineOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				3,
				pickerInput(
					"pareto_filters_family", "Family",
					familyList, familyList, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				3,
				pickerInput(
					"pareto_filters_customer", "Customer",
					customerList, customerList, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				3,
				pickerInput(
					"pareto_filters_model", "Model",
					modelList, modelList, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				3,
				pickerInput(
					"pareto_filters_shift", "Shift",
					shiftList, shiftList, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				3,
				pickerInput(
					"pareto_filters_defects", "Defects",
					defectsList, defectsList, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			)
		)
	})
	output$pareto_plot <- renderPlot({
		plots__trigger$depend()
		pareto__trigger$depend()
		plotData <- mainData %>% filter(DEFECTS_CATEGORY %in% defectsCategories)
		if (nrow(plotData) == 0) return(textPlot("Please add Defects from the Stratification tab to get the Pareto chart"))
		plotData <- tryCatch({
			plotData %>%
			filter(
				DEFECTS_CATEGORY %in% input$pareto_filters_defects & Family %in% input$pareto_filters_family &
				Cust %in% input$pareto_filters_customer & Model %in% input$pareto_filters_model &
				shift %in% input$pareto_filters_shift & Machine %in% input$pareto_filters_machine &
				Date >= input$pareto_filters_date_from & Date <= input$pareto_filters_date_to
			)
		}, error = function(err) { return(data.frame()) })
		if (nrow(plotData) == 0) return(textPlot())
		plotData$Defects_Qty <- as.numeric(plotData$Defects_Qty)
		plotData <- plotData %>% group_by(DEFECTS_CATEGORY) %>% summarise(count_defects = sum(Defects_Qty)) %>% ungroup()
		defect <- plotData$count_defects
		names(defect) <- plotData$DEFECTS_CATEGORY
		pareto.chart(defect, ylab = "Frequency of defects")
	})
	output$pareto_tables <- renderDT({
		plots__trigger$depend()
		pareto__trigger$depend()
		causeEffectData <- mainData %>%
			filter(
				DEFECTS_CATEGORY %in% input$pareto_filters_defects & Family %in% input$pareto_filters_family &
				Cust %in% input$pareto_filters_customer & Model %in% input$pareto_filters_model &
				shift %in% input$pareto_filters_shift
			) %>%
			select(
				id, Date, Shift = shift, Machine, Customer = Cust, Family, Model,
				"Defect Category" = DEFECTS_CATEGORY, CAUSE
			)
		if (nrow(causeEffectData) == 0) return(data.frame())
		leftData <- causeEffectData %>% select(-c(CAUSE))
		outData <- data.frame()
		for (i in 1:nrow(causeEffectData)) {
			causeFromJson <- fromJSON(causeEffectData$CAUSE[i]) %>%
				arrange(Man_Cause, Method_Cause, Machine_Cause, Material_Cause, Measurement_Cause, Environment_Cause)
			causeFromJson <- causeFromJson[
				order(causeFromJson$Man_Cause, causeFromJson$Method_Cause,
					causeFromJson$Machine_Cause, causeFromJson$Material_Cause,
					causeFromJson$Measurement_Cause, causeFromJson$Environment_Cause, decreasing = TRUE),
			]
			causeFromJson$id = causeEffectData$id[i]
			outData <- outData %>% rbind(left_join(leftData, causeFromJson, by = "id"))
		}
		checkSheetCreateData <<- outData[complete.cases(outData),]
		return(
			datatable(
				checkSheetCreateData,
				rownames = FALSE,
				editable = TRUE,
				class = "cell-border stripe",
				options = list(dom = 'tip')
			)
		)
	})
	checkSheetTableOutputProxy <- dataTableProxy("pareto_tables")
	observeEvent(input$pareto_tables_cell_edit, {
		info = input$pareto_tables_cell_edit
		if (info$col < 8) {
			replaceData(checkSheetTableOutputProxy, checkSheetCreateData, resetPaging = FALSE, rownames = FALSE)
			return()
		} else {
			checkSheetCreateData[info$row, info$col + 1] <<- info$value
			updateId <- checkSheetCreateData[info$row, "id"]
			updateData <- checkSheetCreateData %>% filter(id == updateId) %>%
				select(Man_Cause, Method_Cause, Machine_Cause, Material_Cause, Measurement_Cause, Environment_Cause)
			updateCause <- toJSON(updateData)
			if (hasDbConnection) {
				updateNewCause(table_name = currentTableName, updateId, updateCause)
			}
			mainData[mainData$id == updateId, "CAUSE"] <<- updateCause
			fish_bone__trigger$trigger()
		}
	})

	######################################## CAUSE & EFFECT ########################################
	output$cause_effect_filters <- renderUI({
		plots__trigger$depend()
		pareto__trigger$depend()
		tableData <- mainData %>% filter(DEFECTS_CATEGORY %in% defectsCategories)
		if (nrow(tableData) == 0) return()
		defectsOptions <- unique(tableData$DEFECTS_CATEGORY)
		familyOptions <- unique(tableData$Family)
		custOptions <- unique(tableData$Cust)
		modelOptions <- unique(tableData$Model)
		shiftOptions <- unique(tableData$shift)
		minDate <- min(tableData$Date)
		maxDate <- max(tableData$Date)
		machineOptions <- unique(tableData$Machine)
		fluidRow(
			column(
				3,
				dateInput(
					"cause_effect_filters_date_from", "From date",
					minDate, minDate, maxDate
				)
			),
			column(
				3,
				dateInput(
					"cause_effect_filters_date_to", "To date",
					maxDate, minDate, maxDate
				)
			),
			column(
				3,
				pickerInput(
					"cause_effect_filters_machine", "Machine",
					machineOptions, machineOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				3,
				pickerInput(
					"cause_effect_filters_family", "Family",
					familyOptions, familyOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				3,
				pickerInput(
					"cause_effect_filters_cust", "Customer",
					custOptions, custOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				3,
				pickerInput(
					"cause_effect_filters_model", "Model",
					modelOptions, modelOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				3,
				pickerInput(
					"cause_effect_filters_shift", "Shift",
					shiftOptions, shiftOptions, multiple = TRUE,
					options = pickerOptions(actionsBox = TRUE, selectAllText = "All", deselectAllText = "None")
				)
			),
			column(
				3,
				pickerInput(
					"cause_effect_filters_defect_cat", "Defect Category",
					defectsOptions, defectsOptions[1], multiple = FALSE
				)
			)
		)
	})
	observeEvent(c(input$cause_effect_filters_family, input$cause_effect_filters_cust, input$cause_effect_filters_model), {
		tableData <- mainData %>%
			filter(
				DEFECTS_CATEGORY %in% defectsCategories & Family %in% input$cause_effect_filters_family &
				Cust %in% input$cause_effect_filters_cust & Model %in% input$cause_effect_filters_model &
				Date >= input$cause_effect_filters_date_from & Date <= input$cause_effect_filters_date_to &
				Machine %in% input$cause_effect_filters_machine & shift %in% input$cause_effect_filters_shift
			)
			defectsOptions <- unique(tableData$DEFECTS_CATEGORY)
			updatePickerInput(session, "cause_effect_filters_defect_cat", selected = defectsOptions[1], choices = defectsOptions)
	})
	output$cause_effect_fish_bone_plot <- renderPlot({
		plots__trigger$depend()
		pareto__trigger$depend()
		fish_bone__trigger$depend()
		plotData <- tryCatch({
			mainData %>%
			filter(
				DEFECTS_CATEGORY %in% defectsCategories & Family %in% input$cause_effect_filters_family &
				Cust %in% input$cause_effect_filters_cust & Model %in% input$cause_effect_filters_model &
				Date >= input$cause_effect_filters_date_from & Date <= input$cause_effect_filters_date_to &
				Machine %in% input$cause_effect_filters_machine & shift %in% input$cause_effect_filters_shift &
				DEFECTS_CATEGORY == input$cause_effect_filters_defect_cat
			) %>% select(Family, Cust, Model, DEFECTS_CATEGORY, CAUSE)
		}, error = function(err) { return(data.frame()) })
		if (nrow(plotData) == 0) return(textPlot())
		plotCause <- removeEmptyFishbones(as.list(fromJSON(plotData$CAUSE[1])))
		cause.and.effect(
			cause = plotCause,
			effect = plotData$DEFECTS_CATEGORY[1]
		)
	})

	######################################## CLOSE R PROCESS WHEN SESSION ENDS ########################################
	# session$onSessionEnded(function() {
	# 	stopApp()
	# })
}

shinyApp(ui, server)