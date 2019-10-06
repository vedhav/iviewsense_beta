# Beta version of iviewsense

## File structure

- requirements.R contains all the packages required to run this application
- constants.R contains the global constants required for this application
- helper.R contains the global functions that are used everywhere thrughout the application
- global.R contains the UI elements essential for the application
- app.R is the main file which runs the shiny application

## Data

- Geartek.xlsx is a sample data to run this application


## Prerequisite to installation

- You have to install R in your computer https://cran.r-project.org/bin/windows/base/
- Once R is installed it will be present in a folder like C:\Program Files\R\R-3.6.1\bin.<br>
Please add this to the Envirnment path variable.<br>
Refer https://www.architectryan.com/2018/03/17/add-to-the-path-on-windows-10/
- Make sure you have installed these packages by running this command in R.
```
install.packages(
	c(
		"shiny", "shinyWidgets", "shinyjs", "shinyBS", "shinyalert", "bs4Dash",
		"tidyverse", "plotly", "SixSigma", "assertthat", "qcc", "readxl", "lubridate", "knitr",
		"kableExtra", "dbx", "RPostgreSQL", "RMySQL", "DT", "jsonlite", "shinyShortcut"
	)
)
```
- Make sure that you have a local MySQL database. For installation refer http://www.wampserver.com/en/


## Installation

- Please make sure that the prerequisite to the installation is done
- Copy all the contents of a folder of your choice
- Run "installer.bat" This should install your program.
- To open the program double click shinyShortcut.vbs
- You can create a shortcut to shinyShortcut.vbs anywhere to open this application