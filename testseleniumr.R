library(RSelenium)
system(paste("open","/Users/meganraisle/Documents/OEF/batchselenium.command"))
Sys.sleep(5)

remDr <- remoteDriver(browserName = "chrome")
remDr$open(silent = TRUE)
remDr$navigate("http://www.google.com")