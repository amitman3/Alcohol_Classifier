# reqd_packages: install and load multiple R packages.
# check to see if listed packages are installed. 
# Install them if they are not, and load them into the session
reqd_packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
##################################################################
# usage
# pkgs <- c("ggplot2", "plyr", "reshape2", "caret")
# reqd_packages(pkgs)