Production_CHR
==============

Production CHR Code for GAM Modeling

INSTALLATION:

To initialize, install the following software packages:
1. The latest version of R from http://cran.us.r-project.org/
2. The latest version of Rstudio for desktop from https://www.rstudio.com/ide/download/
Then start Rstudio and enter the following commands into the R-console window to install the required packages:
install.packages("devtools", dependencies = TRUE)
install.packages("shiny", dependencies = TRUE)

TO RUN AFTER INSTALLATION:

To run enter the following commands into the R-console window:
library(devtools)
library(shiny)
install_github("albre116/Production_CHR",auth_token="485fbfa16cde833aaa1b567fb1b99ea3d7c5f2ec")
shiny::runApp(system.file(package='CHRproduction'))
