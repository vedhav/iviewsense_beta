popUpWindow <- function (popUpText, title = NULL, footer = NULL, easyClose = TRUE,
	color = "#333", bg_color = "#f7f7f7") {
	tags$div(
		class = "showmodal",
		showModal(
			modalDialog(
				style = paste0('color: ', color, '; background-color: ', bg_color),
				title = title, tags$div(HTML(popUpText), align = "center"), footer = footer, easyClose = easyClose
			)
		)
	)
}