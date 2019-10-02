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

textPlot <- function(text = "No data avaliable", color = "#000000") {
	plot <- ggplot()+
		geom_text(aes(x = 0, y = 0, label = text), size = 6, color = color) +
		labs(x = '', y = '') +
		theme(panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
	return(plot)
}

getShifts <- function(hourList) {
	hourList <- hour(hourList)
	shiftString <- rep("Date Error", length(hourList))
	shiftOnecondition <- hourList < 7
	shiftTwocondition <- hourList >= 7 & hourList < 15
	shiftThreecondition <- hourList >= 15
	shiftString[shiftOnecondition] <- shiftNames[1]
	shiftString[shiftTwocondition] <- shiftNames[2]
	shiftString[shiftThreecondition] <- shiftNames[3]
	return(shiftString)
}

evaluateFailPass <- function(passFail) {
	returnBool <- rep(NA, length(passFail))
	returnBool[passFail == passName] <- TRUE
	returnBool[passFail == failName] <- FALSE
	return(returnBool)
}