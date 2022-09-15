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