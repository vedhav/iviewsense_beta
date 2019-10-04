source("global.R")


mainData <- read_xlsx("Geartek.xlsx", sheet = 1, col_names = TRUE)
mainData$Machine <- "Geartek"

insert("testresults", mainData)
execute("TRUNCATE TABLE testresults RESTART IDENTITY;")
insert("testresults", mainData)

dataFromDb <- selectDbQuery("SELECT * FROM testresults")

dataFromDb[, 11:51] <- sapply(dataFromDb[, 11:51], as.numeric)

paste(paste0("'", names(mainData), "'"), collapse = ", ")




# Run this to populate the defects table
# ALTER TABLE `defects` ADD `test_id` INT NOT NULL AFTER `id`;
source("global.R")
mainData <- read_xlsx("Geartek.xlsx", sheet = 1, col_names = TRUE)
mainData$Machine <- "Geartek"
data <- mainData %>% filter(Result == failName)
defectsTable <- data.frame(
    test_id = c(1:nrow(data)),
    Machine = data$Machine,
    Date_Time = data$Date_Time,
    Family = data$Family,
    Model = data$Model,
    Stn = data$Stn,
    Opr = data$Opr,
    defects_category = "",
    defects_qty = 1
)
# execute("TRUNCATE TABLE defects")
insert("defects", defectsTable)

# Use this to add defects to testresults
# ALTER TABLE `testresults` ADD `Defects_Category` VARCHAR(45) NOT NULL AFTER `Result`, ADD `Defects_Qty` VARCHAR(45) NOT NULL DEFAULT '1' AFTER `Defects_Category`;