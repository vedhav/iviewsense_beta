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

plots__trigger <<- makeReactiveTrigger()

machinesList <<- c("Geartek", "Testek", "Elnix", "Dynaspede", "RT Tek")
shiftNames <<- c("Shift One", "Shift Two", "Shift Three")
timeFilterTypes <<- c("Date", "Month", "Year")
passName <<- "Pass!"
failName <<- "Fail!"

minDate <<- NULL
maxDate <<- NULL
familyOptions <- NULL
custOptions <- NULL
modelOptions <- NULL
resultOptions <- NULL
operatorOptions <- NULL


# Database related constants

#local
# hostUserName <<- 'postgres'
# hostPassword <<- 'admin@123'
# hostIP <<- '127.0.0.1'
# dbPort <<- 5432
# dbName <<- 'rane'

#live
hostUserName <<- 'postgres'
hostPassword <<- 'postgres'
hostIP <<- '127.0.0.1'
dbPort <<- 5432
dbName <<- '7QcT'