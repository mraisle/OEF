#now let's try the scraping way

library(rvest)
simple <- read_html("https://dataquestio.github.io/web-scraping-pages/simple.html")
simple

marisa <- read_html("https://muckrack.com/marisa-mecke") %>%
  html_nodes(".news-story-title") %>%
  html_text()
marisa

marisa <- read_html("https://muckrack.com/link/oG8xW9/how-the-inflation-reduction-acts-climate-change-provisions-may-impact-georgia-emissions") %>%
  html_nodes(".news-story-body") %>%
  html_text()
marisa

forecasts <- read_html("https://forecast.weather.gov/MapClick.php?lat=37.7771&lon=-122.4196#.Xl0j6BNKhTY") %>%
  html_nodes(".temp") %>%
  html_text()


class = news-story-title



#questions i have - how to get at text within an <a> ref
# can scrap from the subwebpage but not the main page


url = "https://muckrack.com/thatreporter/articles"

soup <- read_html(url)

urls_on_page <- soup %>%
  html_elements("h4") %>%
  html_attr("class") %>%
  html_text("news-story-title")

#ok still need to figure out how to get the webpage to expand but i now know exactly what I want, i want the link and the title from the "News story title" section 
# i also need the text from the "news story byline class 


report_urls <- urls_on_page[grepl("wral", urls_on_page)]




#some more testing 
starwars <- read_html("https://rvest.tidyverse.org/articles/starwars.html")
films <- starwars %>% html_elements("section")
films


liz <- read_html("john.html")
liznodes <- liz %>% html_nodes(".news-story-title") %>% html_text
liznodes



my_session <- session("https://scrapethissite.com/pages/simple/")
my_nodes <- my_session %>% html_elements(".country")

my_attributes <- liz %>% html_elements(".news-") %>% html_attr("class")


url <- ("https://muckrack.com/byadamwagner/articles")

cac <- ("https://www.citact.org/")

soup <- read_html(cac)

urls_on_page <- soup %>%
  html_elements(".romana_header_bottom") %>%
  html_attr("href")

report_urls <- urls_on_page[grepl("yahoo", urls_on_page)]

#ok think the problkem MAY be that the data is loaded dynamically using javascript 

page <- read_html('https://muckrack.com/john-deem/articles')

data <- page %>% 
  toString() %>% 
  jsonlite::parse_json()

print(data[[1]]$listProducts)

library(RSelenium)


remDr <- rsDriver(browser='chrome', port=4444L)
browser <- remDr$client
browser$open()
browser$navigate("https://muckrack.com/john-deem/articles")


#options to move forwrad
- try these options to get rselenium to work - https://stackoverflow.com/questions/31124702/rselenium-unknownerror-java-lang-illegalstateexception-with-google-chrome/31188481#31188481

