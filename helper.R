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

shinyInput <- function(FUN, len, id, ...) {
    if (len == 0) {
        return("")
    }
    inputs <- character(len)
    for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
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

formatData <- function(data) {
    if ("id" %in% names(data)) {
        idData <- data$id
        data$id <- NULL
    } else {
        idData <- c(1:nrow(data))
    }
    if ("Machine" %in% names(data)) {
        machineData <- data$Machine
        data$Machine <- NULL
    } else {
        machineData <- "No Machine Name was specified!"
    }
    idAndMachineData <- data.frame(
        id = idData,
        Machine = machineData
    )
    data <- cbind(idAndMachineData, data)
    data[, numericColumns] <- sapply(data[, numericColumns], as.numeric)
    data$Opr[is.na(data$Opr)] <- "NA"
    data$Date_Time <- as.POSIXct(data$Date_Time)
    data$shift <- getShifts(data$Date_Time)
    data$Date <- as.Date(data$Date_Time, tz = "")
    data$Machine <- as.character(data$Machine)
    data$Defects_Qty <- 1
    return(data)
}

updateDefectInDB <- function(id, defect_cat, causeEffectData) {
    # print(paste0("UPDATE testresults SET Defects_Category = ", defect_cat, " WHERE id = ", id))
    execute(
        "UPDATE testresults SET Defects_Category = ? WHERE id = ?",
        list(defect_cat, id)
    )
    execute(
        "UPDATE testresults SET cause = ? WHERE id = ?",
        list(fishBoneSkeleton, id)
    )
}

updateNewCause <- function(id, causeJSON) {
    execute(
        'UPDATE testresults SET cause = ? WHERE id = ?',
        list(causeJSON, id)
    )
}

removeEmptyFishbones <- function(causeList) {
    for (i in 1:length(causeList)) {
        causeList[[i]] <- causeList[[i]][causeList[[i]] != ""]
    }
    return(causeList)
}

killDbxConnections <- function () {
    all_cons <- dbListConnections(databaseDriver)
    for(con in all_cons)
        dbxDisconnect(con)
    print(paste(length(all_cons), " connections killed."))
}

#pass in the tablename along with a dataframe of values(can contain multiple rows)
insert <- function(tableName, values) {
    #adding support for '\' - extending dbx
    for(field in 1:length(values))
    {
        if(typeof(values[[field]]) == 'character')
        {
            values[[field]] <- gsub("[\\]", "\\\\\\\\", values[[field]])
        }
    }
    conn <- tryCatch({
        dbxConnect(
            adapter = connectionAdapter,
            user = hostUserName,
            password = hostPassword,
            host = hostIP,
            port = dbPort,
            dbname = dbName
        )
    }, error = function(err) {
        killDbxConnections()
        dbxConnect(
            adapter = connectionAdapter,
            user = hostUserName,
            password = hostPassword,
            host = hostIP,
            port = dbPort,
            dbname = dbName
        )
    })
    result <- suppressWarnings(dbxInsert(conn, tableName, values, batch_size = 1000))
    on.exit(dbxDisconnect(conn))
    return(result)
}

execute <- function(query, params = NULL) {
    conn <- tryCatch({
        dbxConnect(
            adapter = connectionAdapter,
            user = hostUserName,
            password = hostPassword,
            host = hostIP,
            port = dbPort,
            dbname = dbName
        )
    }, error = function(err) {
        killDbxConnections()
        dbxConnect(
            adapter = connectionAdapter,
            user = hostUserName,
            password = hostPassword,
            host = hostIP,
            port = dbPort,
            dbname = dbName
        )
    })
    dbxExecute(conn, query, params = params)
    on.exit(dbxDisconnect(conn))
}

#pass in the query with ? as placeholders(if needed) and params as a list
selectDbQuery <- function(query, params = NULL) {
    conn <- tryCatch({
        dbxConnect(
            adapter = connectionAdapter,
            user = hostUserName,
            password = hostPassword,
            host = hostIP,
            port = dbPort,
            dbname = dbName
        )
    }, error = function(err) {
        killDbxConnections()
        dbxConnect(
            adapter = connectionAdapter,
            user = hostUserName,
            password = hostPassword,
            host = hostIP,
            port = dbPort,
            dbname = dbName
        )
    })
    result <- NULL
    if(!is.null(params))
    {
        #adding support for '\' - extending dbx
        for(field in 1:length(params))
        {
            if(typeof(params[[field]]) == 'character')
            {
                params[[field]] <- gsub("[\\]", "\\\\\\\\", params[[field]])
            }
        }
        result <- suppressWarnings(dbxSelect(conn, query, params))
    }
    else
    {
        result <- suppressWarnings(dbxSelect(conn, query))
    }
    on.exit(dbxDisconnect(conn))
    return(result)
}

#pass in the tablename along with a dataframe of values(can contain multiple rows)
delete <- function(tableName, values) {
    conn <- tryCatch({
        dbxConnect(
            adapter = connectionAdapter,
            user = hostUserName,
            password = hostPassword,
            host = hostIP,
            port = dbPort,
            dbname = dbName
        )
    }, error = function(err) {
        killDbxConnections()
        dbxConnect(
            adapter = connectionAdapter,
            user = hostUserName,
            password = hostPassword,
            host = hostIP,
            port = dbPort,
            dbname = dbName
        )
    })
    result <- suppressWarnings(dbxDelete(conn, tableName, where = values))
    on.exit(dbxDisconnect(conn))
    return(result)
}

#pass in the tablename along with a dataframe of values(can contain multiple rows)
upsert <- function(tableName, values, where_cols = 'id') {
    query <- "select column_name from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = ? and TABLE_SCHEMA = ?"
    params <- list(tableName, dbName)
    schema <- selectDbQuery(query, params)
    if("updated_at" %in% schema$column_name)
    {
        Sys.setenv(TZ='GMT')
        updated_at <- suppressWarnings(Sys.time())
        if("updated_at" %in% colnames(values)){
            values["updated_at"] <- updated_at
        }
        else{
            values <- cbind(values, data.frame("updated_at" = updated_at))
        }
    }
    #adding support for '\' - extending dbx
    for(field in 1:length(values))
    {
        if(typeof(values[[field]]) == 'character')
        {
            values[[field]] <- gsub("[\\]", "\\\\\\\\", values[[field]])
        }
    }
    conn <- tryCatch({
        dbxConnect(
            adapter = connectionAdapter,
            user = hostUserName,
            password = hostPassword,
            host = hostIP,
            port = dbPort,
            dbname = dbName
        )
    }, error = function(err) {
        killDbxConnections()
        dbxConnect(
            adapter = connectionAdapter,
            user = hostUserName,
            password = hostPassword,
            host = hostIP,
            port = dbPort,
            dbname = dbName
        )
    })
    result <- suppressWarnings(dbxUpsert(conn, tableName, values, where_cols))
    on.exit(dbxDisconnect(conn))
    return(result)
}