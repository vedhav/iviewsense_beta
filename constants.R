# CSS related constants
bodyFontSize <<- "12px"
buttonStyle <<- "background-color: #4079fb; color: #ffffff; margin-top: 20px"

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
pareto__trigger <<- makeReactiveTrigger()
fish_bone__trigger <<- makeReactiveTrigger()

machinesList <<- c("Geartek", "Testek", "Elnix", "Dynaspede", "RT Tek")
shiftNames <<- c("Shift One", "Shift Two", "Shift Three")
timeFilterTypes <<- c("Date", "Month", "Year")
passName <<- "Pass!"
failName <<- "Fail!"
dataColumnNames <<- c(
	'Date_Time', 'Machine', 'Family', 'Cust', 'Model', 'Gear_No', 'Stn', 'Opr', 'Result', 'Defects_Category',
	'NPD', 'Flow', 'Tempr', 'Leak_CW', 'Leak_CCW', 'Leak_Pr_CW', 'Leak_Pr_CCW',
	'Eff_CW', 'Eff_CCW', 'TT_CW', 'TT_CCW', 'TT_Diff', 'Wr_Slv_Pr', 'Blk_Psge_Pr',
	'TB_1', 'TB_2', 'Hys_CW', 'Hys_CCW', 'Direction', 'PCW', 'PCCW', 'CW_1', 'CCW_1',
	'CW_2', 'CCW_2', 'CW_3', 'CCW_3', 'CW_4', 'CCW_4', 'CW_5', 'CCW_5', 'CW_6',
	'CCW_6', 'CW_7', 'CCW_7', 'CW_8', 'CCW_8', 'CW_9', 'CCW_9', 'CW_10', 'CCW_10'
)
dataColumnNamesString <- "
	Date_Time, Machine, Family, Cust, Model, Gear_No, Stn, Opr, Result, Defects_Category,<br>
	NPD, Flow, Tempr, Leak_CW, Leak_CCW, Leak_Pr_CW, Leak_Pr_CCW, Eff_CW, Eff_CCW, TT_CW,<br>
	TT_CCW, TT_Diff, Wr_Slv_Pr, Blk_Psge_Pr, TB_1, TB_2, Hys_CW, Hys_CCW, Direction, PCW,<br>
	PCCW, CW_1, CCW_1, CW_2, CCW_2, CW_3, CCW_3, CW_4, CCW_4, CW_5, CCW_5, CW_6, CCW_6, CW_7,<br>
	CCW_7, CW_8, CCW_8, CW_9, CCW_9, CW_10, CCW_10
"
numericColumns <<- c(
	'NPD', 'Flow', 'Tempr', 'Leak_CW', 'Leak_CCW', 'Leak_Pr_CW', 'Leak_Pr_CCW',
	'Eff_CW', 'Eff_CCW', 'TT_CW', 'TT_CCW', 'TT_Diff', 'Wr_Slv_Pr', 'Blk_Psge_Pr',
	'TB_1', 'TB_2', 'Hys_CW', 'Hys_CCW', 'Direction', 'PCW', 'PCCW', 'CW_1', 'CCW_1',
	'CW_2', 'CCW_2', 'CW_3', 'CCW_3', 'CW_4', 'CCW_4', 'CW_5', 'CCW_5', 'CW_6',
	'CCW_6', 'CW_7', 'CCW_7', 'CW_8', 'CCW_8', 'CW_9', 'CCW_9', 'CW_10', 'CCW_10'
)
defectsCategories <<- c(
	"Low-cycle Fatigue", "High-Cycle Fatigue", "Abrasive wear",
	"Corrosive Wear", "Spalling", "Breakage", "Pitting",
	"Macropitting", "Micropitting", "Subcase Failure", "Adhesion",
	"Abrasion", "Corrosion", "Fretting Corrosion", "Polishing",
	"Electric Discharge", "Cavitation", "Errosion", "Scuffing",
	"Brittle Fracture", "Ductile Fracture", "Mixed-Mode Fracture",
	"Plastic Deformation", "Hardening Cracks", "Grinding Cracks",
	"Rimand Web Cracks", "Case-Core Seperation"
)

emptyVector <- c(rep("", 10))
fishBoneSkeleton <<- toJSON(
	data.frame(
		Man_Cause = emptyVector,
		Method_Cause = emptyVector,
		Machine_Cause = emptyVector,
		Material_Cause = emptyVector,
		Measurement_Cause = emptyVector,
		Environment_Cause = emptyVector
	)
)

minDate <<- NULL
maxDate <<- NULL
familyOptions <- NULL
custOptions <- NULL
modelOptions <- NULL
resultOptions <- NULL
operatorOptions <- NULL
machineOptions <- NULL


# Database related constants
# 
# # PostgreSQL
# # databaseDriver <<- dbDriver("PostgreSQL")
# connectionAdapter <<- "postgres"
# hostUserName <<- 'postgres'
# hostPassword <<- 'admin@123'
# hostIP <<- '127.0.0.1'
# dbPort <<- 5432
# dbName <<- 'rane'

# MySQL
# # local
# databaseDriver <<- MySQL()
# connectionAdapter <<- "mysql"
# hostUserName <<- 'root'
# hostPassword <<- 'admin@123'
# hostIP <<- '127.0.0.1'
# dbPort <<- 3306
# dbName <<- 'rane'
# live
# databaseDriver <<- MySQL()
# connectionAdapter <<- "mysql"
# hostUserName <<- 'iviewsen_ivsadmn'
# hostPassword <<- 'Good2019$'
# hostIP <<- '199.79.62.17'
# dbPort <<- 3306
# dbName <<- 'iviewsen_testresults'

# new live
databaseDriver <<- MySQL()
connectionAdapter <<- "mysql"
hostUserName <<- 'vedha'
hostPassword <<- 'mySQLPassword12!@'
hostIP <<- '159.65.228.248'
dbPort <<- 3306
dbName <<- 'iviewsen_testresults'