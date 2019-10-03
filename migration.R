source("global.R")


mainData <- read_xlsx("Geartek.xlsx", sheet = 1, col_names = TRUE)
mainData$Machine <- "Geartek"

insert("testresults", mainData)
execute("TRUNCATE TABLE testresults RESTART IDENTITY;")
insert("testresults", mainData)

dataFromDb <- selectDbQuery("SELECT * FROM testresults")

dataFromDb[, 11:51] <- sapply(dataFromDb[, 11:51], as.numeric)

paste(paste0("'", names(mainData), "'"), collapse = ", ")