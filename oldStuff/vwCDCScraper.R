library(xml2)
library(rvest)
library(httr)
library(htmltools)
library(htmlwidgets)
library(curl)
library(splashr)
library(rlang)
library(lubridate)
library(gmailr)
library(googledrive)
library(googlesheets4)
library(tidyverse)

setwd("C:\\Users\\anesta\\Documents\\Verywell_Vaccine_Data_Tracker")


shell(cmd = ".\\startDockerScraper.ps1", shell = "powershell", wait = F)

Sys.sleep(200)
shell(cmd = "^C", shell = "powershell", wait = F)




