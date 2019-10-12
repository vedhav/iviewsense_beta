source("global.R")

# Dumping rane data
main_data_1 <- read_xlsx("sampleData.xlsx", sheet = 1, col_names = TRUE)
main_data_2 <- read_xlsx("sampleData.xlsx", sheet = 2, col_names = TRUE)
main_data_3 <- read_xlsx("sampleData.xlsx", sheet = 3, col_names = TRUE)
main_data_4 <- read_xlsx("sampleData.xlsx", sheet = 4, col_names = TRUE)
main_data_5 <- read_xlsx("sampleData.xlsx", sheet = 5, col_names = TRUE)

dataColumnNames <- c(
	'Date_Time', 'Family', 'Cust', 'Model', 'Gear_No', 'Stn', 'Opr', 'Result',
	'NPD', 'Flow', 'Tempr', 'Leak_CW', 'Leak_CCW', 'Leak_Pr_CW', 'Leak_Pr_CCW',
	'Eff_CW', 'Eff_CCW', 'TT_CW', 'TT_CCW', 'TT_Diff', 'Wr_Slv_Pr', 'Blk_Psge_Pr',
	'TB_1', 'TB_2', 'Hys_CW', 'Hys_CCW', 'Direction', 'PCW', 'PCCW', 'CW_1', 'CCW_1',
	'CW_2', 'CCW_2', 'CW_3', 'CCW_3', 'CW_4', 'CCW_4', 'CW_5', 'CCW_5', 'CW_6',
	'CCW_6', 'CW_7', 'CCW_7', 'CW_8', 'CCW_8', 'CW_9', 'CCW_9', 'CW_10', 'CCW_10'
)

names(main_data_1) <- dataColumnNames
main_data_1$Machine <- "Geartek"
names(main_data_2) <- dataColumnNames
main_data_2$Machine <- "Testek"
names(main_data_3) <- dataColumnNames
main_data_3$Machine <- "Elnix"
names(main_data_4) <- dataColumnNames
main_data_4$Machine <- "Dynaspede"
names(main_data_5) <- dataColumnNames
main_data_5$Machine <- "RT Tek"

mainData <- rbind(main_data_1, main_data_2, main_data_3, main_data_4, main_data_5)
mainData$Defects_Category <- ""

insert("testresults", mainData)



# Dumping the general data
mainData <- read_xlsx("sample_data_general.xlsx", sheet = 1, col_names = TRUE)
insert("testresults_general", mainData)
