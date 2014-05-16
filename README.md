Production_CHR
==============

Production CHR Code for GAM Modeling

To run enter the following commands into the R-console window:
library(devtools)
library(shiny)
install_github("albre116/Production_CHR",auth_token="90bf45bf2cb65eca874c264193c3e3536e823d8a")
shiny::runApp(system.file(package='CHRproduction'))
