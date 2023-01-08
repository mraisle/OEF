#ok, what i think we want rselenium to do is hit "show more" on the table of articles and download that 
#comprehensive table 


library(RSelenium)
library(rvest)
library(V8)

driver <- rsDriver(browser=c("firefox"))
remote_driver <- driver[["client"]]
remote_driver$open()


remDr <- rsDriver(browser='chrome', port=4444L)
browser <- remDr$client
browser$open()
browser$navigate("https://muckrack.com/john-deem/articles")


#options to move forwrad
- try these options to get rselenium to work - https://stackoverflow.com/questions/31124702/rselenium-unknownerror-java-lang-illegalstateexception-with-google-chrome/31188481#31188481


library(httr)
r <-GET("https://muckrack.com/john-deem/articles")
json <- content(r,as="parsed")