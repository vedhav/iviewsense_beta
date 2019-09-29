source("global.R")
data_source_body <- bs4TabItem(
	tabName = "data_source",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Data Source")
			),
			fluidRow(
				align = "center", style = "margin-top: 15vh",
				column(
					12,
					tags$div(
						title = "Click to change data source",
						prettyToggle(
							inputId = "remote_or_local",
							label_on = "Use remote database",
							label_off = "Use local csv or excel file",
							icon_on = icon("file-excel"),
							icon_off = icon("database"),
							status_on = "default",
							status_off = "default",
							bigger = TRUE
						)
					)
				),
				column(
					12, style = "margin-top: 10vh",
					uiOutput("data_source_body_ui")
				)
			)
		)
	)
)


stratification_body <- bs4TabItem(
	tabName = "stratification",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Stratification")
			)
		)
	)
)


control_chart_body <- bs4TabItem(
	tabName = "control_chart",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Control chart")
			)
		)
	)
)


histogram_body <- bs4TabItem(
	tabName = "histogram",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Histogram")
			)
		)
	)
)


scatter_plot_body <- bs4TabItem(
	tabName = "scatter_plot",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Scatter plot")
			)
		)
	)
)


pareto_body <- bs4TabItem(
	tabName = "pareto",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Pareto")
			)
		)
	)
)


cause_effect_body <- bs4TabItem(
	tabName = "cause_effect",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Cause & effect")
			)
		)
	)
)


check_sheet_body <- bs4TabItem(
	tabName = "check_sheet",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Check sheet")
			)
		)
	)
)

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
				padding: 8px 32px; display: inline-block; border-radius: 40px;
				transition: all .2s; position: relative; font-size: 12px;
				border: none; cursor: pointer; background-color: #4079fb; color: #ffffff;}
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
	mainData <- data.frame()
	observeEvent(input$remote_or_local, {
		output$data_source_body_ui <- renderUI({
			if (input$remote_or_local %% 2 == 0) {
				ui <- fileInput("data_source_input_file", "Upload your file")
			} else {
				ui <- HTML("The data from PostgreSQL will be used for analysis!")
			}
			return(ui)
		})
	})
}

shinyApp(ui, server)