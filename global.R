source("requirements.R")
source("constants.R")
source("helper.R")


summary_body <- bs4TabItem(
	tabName = "summary",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Summary")
			),
			fluidRow(
				column(
					12,
					uiOutput("summary_body_ui")
				)
			)
		)
	)
)

data_source_body <- bs4TabItem(
	tabName = "data_source",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Data Source")
			),
			fluidRow(
				align = "center", style = "margin-top: 5vh",
				column(
					12,
					tags$div(
						title = "Click to change data source",
						prettyToggle(
							inputId = "remote_or_local",
							label_on = "Use local csv file (click to change)",
							label_off = "Use remote database (click to change)",
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


control_chart_body <- bs4TabItem(
	tabName = "control_chart",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Control chart"),
				column(12, align = "center", uiOutput("control_chart_filters")),
				column(12, plotOutput("control_chart_plot_xbar_one")),
				column(
					12, style = "margin-top: 5vh",
					fluidRow(
						column(2, br()),
						column(3, numericInput("control_chart_r_lcl", "Enter the LCL", 0, width = "100%")),
						column(3, numericInput("control_chart_r_ucl", "Enter the UCL", 0, width = "100%")),
						column(
							1, style = "margin-top: 35px",
							prettySwitch(inputId = "control_chart_r_manual_automatic", label = "Actual",  status = "primary", slim = TRUE)
						)
					)
				),
				column(12, plotOutput("control_chart_plot_xbar_r"))
			)
		)
	)
)


histogram_body <- bs4TabItem(
	tabName = "histogram",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Histogram"),
				column(12, align = "center", uiOutput("histogram_filters")),
				column(12, plotOutput("histogram_plot"))
			)
		)
	)
)


scatter_plot_body <- bs4TabItem(
	tabName = "scatter_plot",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Scatter plot"),
				column(12, align = "center", uiOutput("scatter_plot_filters")),
				column(12, plotlyOutput("scatter_plot"))
			)
		)
	)
)


stratification_body <- bs4TabItem(
	tabName = "stratification",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Stratification"),
				column(12, align = "center", uiOutput("stratification_filters")),
				column(12, align = "center", tableOutput("stratification_table")),
				column(12, br()),
				column(12, align = "center", uiOutput("stratification_legends")),
				column(12, br()),
				column(4, plotlyOutput("stratification_family", height = 200)),
				column(4, plotlyOutput("stratification_customer", height = 200)),
				column(4, plotlyOutput("stratification_model", height = 200)),
				column(12, br()),
				column(4, plotlyOutput("stratification_shift")),
				column(4, plotlyOutput("stratification_day_of_week")),
				column(4, plotlyOutput("stratification_operator"))
			)
		)
	)
)


pareto_body <- bs4TabItem(
	tabName = "pareto",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Pareto"),
				column(12, align = "center", uiOutput("pareto_filters")),
				column(12, plotOutput("pareto_plot")),
				column(
					12, style = "overflow-y: scroll;",
					DTOutput("pareto_tables")
				)
			)
		)
	)
)


cause_effect_body <- bs4TabItem(
	tabName = "cause_effect",
	tags$div(
		fluidPage(
			fluidRow(
				column(12, align = "center", style = "font-size: 20px;", "Cause & effect"),
				column(12, uiOutput("cause_effect_filters")),
				column(12, plotOutput("cause_effect_fish_bone_plot"))
			)
		)
	)
)
