# CSS related constants
bodyFontSize <<- "12px"
buttonStyle <<- "background-color: #4079fb; color: #fff;"

makeReactiveTrigger <<- function() {
	rv <- reactiveValues(a = 0)
	list(
		depend = function() {
			rv$a
			invisible()
		},
		trigger = function() {
			rv$a <- isolate(rv$a + 1)
		}
	)
}

histogram__trigger <<- makeReactiveTrigger()

machinesList <<- c("Geartek", "Testek", "Elnix", "Dynaspede", "RT Tek")
shiftNames <<- c("Shift One", "Shift Two", "Shift Three")

minDate <<- NULL
maxDate <<- NULL