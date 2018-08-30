library(devtools)
if (!"testthat" %in% installed.packages()) install.packages("testthat")
if (!"REddyProc" %in% installed.packages()) install.packages("REddyProc")
if (!"PEcAn.logger" %in% installed.packages()) install_github("pecanproject/pecan/base/logger")
if (!"PEcAn.remote" %in% installed.packages()) install_github("pecanproject/pecan/base/remote")
if (!"PEcAn.utils" %in% installed.packages()) install_github("pecanproject/pecan/base/utils")
if (!"PEcAn.DB" %in% installed.packages()) install_github("pecanproject/pecan/base/db")
if (!"PEcAn.data.atmosphere" %in% installed.packages()) install_github("pecanproject/pecan/base/data.atmosphere")
if (!"rnoaa" %in% installed.packages()) install.packages("rnoaa")






