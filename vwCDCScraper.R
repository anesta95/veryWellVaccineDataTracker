library(xml2)
library(rvest)
library(httr)
library(htmltools)
library(htmlwidgets)
library(curl)
library(splashr)
library(rlang)
library(lubridate)
library(janitor)
library(gmailr)
library(tidyverse)

setwd("C:\\Users\\anesta\\Documents\\Verywell_Vaccine_Data_Tracker")

# system("powershell -command \".\\startDockerScraper.ps1\"")
shell(cmd = ".\\startDockerScraper.ps1", shell = "powershell", wait = F)

# system("powershell -command \"docker pull scrapinghub/splash:latest --disable-browser-caches\"")
# dockerContainerID <- system("powershell -command \"docker run -p 5023:5023 -p 8050:8050 -p 8051:8051 scrapinghub/splash:latest --disable-browser-caches\"")
Sys.sleep(360)
shell(cmd = "^C", shell = "powershell", wait = F)




