source("global.R")

main_data_1 <- read_xlsx("sampleData.xlsx", sheet = 1, col_names = TRUE)
main_data_2 <- read_xlsx("sampleData.xlsx", sheet = 2, col_names = TRUE)
main_data_3 <- read_xlsx("sampleData.xlsx", sheet = 3, col_names = TRUE)
main_data_4 <- read_xlsx("sampleData.xlsx", sheet = 4, col_names = TRUE)
main_data_5 <- read_xlsx("sampleData.xlsx", sheet = 5, col_names = TRUE)

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