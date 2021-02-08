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
library(googledrive)
library(googlesheets4)
library(tidyverse)

setwd("C:\\Users\\anesta\\Documents\\Verywell_Vaccine_Data_Tracker")

statePops <- read_csv("populationEstimates.csv", col_types = "ciici")

vaccineEligibility <- read_csv("vaccineEligibilityData.csv", 
                               col_types = "ciiiiiiiici")

stateFIPS <- read_csv("stateFIPSCodes.csv", col_types = "cci")

Sys.sleep(35)

errorEmailorGitPush <- function(cnd) {
  if (class(cnd)[2] == "rlang_error") {
    # https://blog.mailtrap.io/send-emails-with-gmail-api/
    # https://github.com/r-lib/gmailr#setup
    # https://github.com/jennybc/send-email-with-r#create-a-project-in-google-developers-console
    # https://support.google.com/cloud/answer/9110914#skip
    # https://support.google.com/cloud/answer/9110914#mark-internal
    # https://github.com/r-lib/gmailr
    # https://console.developers.google.com/apis/credentials?authuser=1&project=vwvaccinedatatrackerproj&supportedpurview=project
    
    gm_auth_configure(path = "./GmailCredentials.json")
    
     
    gm_auth(email = "adriannesta@gmail.com", scopes = "send")
    
    email <- gm_mime() %>% 
      gm_to(c("anesta@dotdash.com", "amorelli@dotdash.com")) %>% 
      gm_from("adriannesta@gmail.com") %>% 
      gm_subject("Error in updating Vaccine Data Tracker") %>% 
      gm_text_body(paste("The error that occured in the update was:", class(cnd)[1]))
    
    Sys.sleep(5)
    
    # Error logging
    # http://www.seancarney.ca/2020/10/09/error-catching-logging-and-reporting-in-r-with-trycatchlog/
    
    gm_send_message(email)
  } else {
    # pushToGithub
    # https://stackoverflow.com/questions/5343068/is-there-a-way-to-cache-github-credentials-for-pushing-commits
    # https://stackoverflow.com/questions/5343068/is-there-a-way-to-cache-github-credentials-for-pushing-commits#:~:text=To%20cache%20your%20GitHub%20password,time%20it%20talks%20to%20GitHub.
    # https://docs.github.com/en/github/using-git/caching-your-github-credentials-in-git
    # https://docs.github.com/en/free-pro-team@latest/github/using-git/caching-your-github-credentials-in-git
    shell(cmd = ".\\pushToGit.ps1", shell = "powershell")
    
    
    
  } 
}

finalResult <- tryCatch(
  {
    splash("localhost") %>% 
      splashr::render_html("https://covid.cdc.gov/covid-data-tracker/#vaccinations", wait = 5) -> cdcPage
    Sys.sleep(10)
    cdcTable <- cdcPage %>% 
      html_node(css = "#vaccinations-table") %>% 
      html_table(header = NA, fill = T) %>% 
      clean_names() %>% 
      mutate(across(.cols = contains("Doses"), as.numeric)) %>% 
      mutate(state_territory_federal_entity = if_else(
        state_territory_federal_entity == "New York State", "New York", 
        state_territory_federal_entity)) %>% 
      mutate(date = Sys.Date()) %>% 
      left_join(stateFIPS, by = c("state_territory_federal_entity" = "name"))
    Sys.sleep(10)
    

    cdcFullTable <- read_csv("cdcFullTable.csv", col_types = "ciiiiiiiiDci")
    
    #cdcFullTableUpdated <- cdcFullTable
    cdcFullTableUpdated <- bind_rows(cdcTable, cdcFullTable)
    
    write_csv(cdcFullTableUpdated, "cdcFullTable.csv")
    
    Sys.sleep(5)
    
    cdcWWWNontotal <- cdcTable %>% 
      filter(state_territory_federal_entity %in% c(state.name, "District of Columbia")) %>% 
      select(state_territory_federal_entity, 
             total_doses_delivered, 
             total_doses_administered,
             people_with_2_doses_per_100k
      ) %>% 
      arrange(desc(people_with_2_doses_per_100k)) %>% 
      mutate(total_doses_delivered = as.character(total_doses_delivered),
             total_doses_administered = as.character(total_doses_administered),
             people_with_2_doses_per_100k = as.character(people_with_2_doses_per_100k))
    
    cdcWWWTotal <- tibble_row(
      state_territory_federal_entity = "U.S. Total",
      total_doses_delivered = format(sum(cdcTable$total_doses_delivered, na.rm = T), big.mark = ",", scientific = F),
      total_doses_administered = format(sum(cdcTable$total_doses_administered, na.rm = T), big.mark = ",", scientific = F),
      people_with_2_doses_per_100k = format(round((sum(cdcTable$people_with_2_doses, na.rm = T) / 328580394L) * 100000), big.mark = ",", scientific = F)
    )
    
    
    
    cdcWWWFormatted <- bind_rows(cdcWWWTotal, cdcWWWNontotal)
    
    
    cdcWWWFormatted %>% write_csv("cdcWWWFormatted.csv")
    
    
    Sys.sleep(5)
    
    cdcMap <- cdcTable %>% 
      inner_join(vaccineEligibility, by = "fips_code") %>% 
      mutate(`% of Currently Eligible Vaccinated` = round((people_with_2_doses / Total_people_to_vaccinate) * 100, digits = 1))%>% 
      select(state_territory_federal_entity, `% of Currently Eligible Vaccinated`, 
             Total_people_to_vaccinate) %>% 
      rename(
        ID = state_territory_federal_entity,
        `Total Currently Eligible` = Total_people_to_vaccinate
      ) 
    
    cdcMap %>% write_csv("cdcMap.csv")
    
    Sys.sleep(5)
    
    
    areWeThereYetNontotal <- cdcFullTableUpdated %>% 
      filter(date %in% c(max(date), max(date) - 7)) %>% 
      mutate(`% Population with 2 Vaccines` = (people_with_2_doses_per_100k / 1000),
             `Doses administered in the last week` = total_doses_administered - lead(
               total_doses_administered, n = 63),
             `1+ Doses adminstered in the last week` = people_with_1_doses - lead(
               people_with_1_doses, n = 63)) %>% 
      filter(`Doses administered in the last week` != 0) %>% 
      inner_join(statePops, by = "fips_code") %>% 
      mutate(`70% of population` = .7 * Total_Pop_Estimate,
             `Estimated to 70% Pop 2 Doses` = strftime(base::as.Date(
               round(
                 (
                   (
                     (
                       (`70% of population` - people_with_1_doses) / 
               `1+ Doses adminstered in the last week`)) * 7) + 28), 
               origin = "1970-01-01") + as.integer(Sys.Date()), format = "%B %Y"),
             testSort = round(((((`70% of population` - people_with_1_doses) / 
                                   `1+ Doses adminstered in the last week`)) * 7) + 28)) %>%
      arrange(testSort) %>% 
      select(state_territory_federal_entity, `% Population with 2 Vaccines`,
             `Doses administered in the last week`, `Estimated to 70% Pop 2 Doses`) %>% 
      mutate(`% Population with 2 Vaccines` = round(`% Population with 2 Vaccines`, 2),
             `Doses administered in the last week` = format(`Doses administered in the last week`, big.mark = ",", scientific = F))
      

      

      onePlusVaxLastWeek <- cdcFullTableUpdated %>% 
        filter(date %in% c(max(date), max(date) - 7)) %>%
        mutate(`1+ Doses adminstered in the last week` = people_with_1_doses - lead(
          people_with_1_doses, n = 63)) %>% 
        filter(!is.na(`1+ Doses adminstered in the last week`)) %>% 
        pull(`1+ Doses adminstered in the last week`) %>% 
        sum()
      

      areWeThereYetTotal <- tibble_row(
        state_territory_federal_entity = "U.S. Total", 
        `% Population with 2 Vaccines` = round((sum(cdcTable$people_with_2_doses, na.rm = T) / 328580394L) * 100, 2),
        `Doses administered in the last week` = format(
          sum(
            as.integer(
            str_remove_all(
              str_trim(areWeThereYetNontotal$`Doses administered in the last week`), ",")), na.rm = T
            ), big.mark = ",", scientific = F),
        `Estimated to 70% Pop 2 Doses` = strftime(
          base::as.Date(
            (round(
              (
                (
                  (
                    (328580394L * .7) - sum(cdcTable$people_with_1_doses, na.rm = T)) / 
                       onePlusVaxLastWeek) * 7) + 28) + as.integer(Sys.Date())), origin = "1970-01-01"
            ), format = "%B %Y")
      )
    
    areWeThereYet <- bind_rows(areWeThereYetTotal,
                               areWeThereYetNontotal)
    
    
    write_csv(areWeThereYet, "areWeThereYet.csv")
    
    
  }, error = function(cond) {
    condFull <- error_cnd(class = "vwDataTrackerError", message = paste("An error occured with the update:", 
                                cond, "on", Sys.Date(), "\n"
    ))
    
    write(condFull[["message"]], "./errorLog.txt", append = T)
    
    return(condFull)
  }
)

shell(cmd = ".\\stopDockerScraper.ps1", shell = "powershell", wait = F)
Sys.sleep(5)
if (wday(Sys.Date(), label = F) == 2) {
  
  drive_auth(email = "anesta@dotdash.com")
  gs4_auth(token = drive_token())
  
  
  rate <- rate_delay(61, max_times = 3)
  
  writeFirst <- insistently(~write_sheet(ss = "1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo",
                           data = cdcWWWFormatted,
                           sheet = as.character(Sys.Date())),
                           rate = rate,
                           quiet = F)
  
  writeFirst()
  
  Sys.sleep(5)
  
  writeSecond <- insistently(~range_write(ss = "1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo",
                                          data = cdcMap,
                                          sheet = as.character(Sys.Date()),
                                          range = "F1",
                                          col_names = T),
                             rate = rate,
                             quiet = F)
  
  writeSecond()
  
  Sys.sleep(5)
  
  writeThird <- insistently(~range_write(ss = "1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo",
                                         data = areWeThereYet,
                                         sheet = as.character(Sys.Date()),
                                         range = "J1",
                                         col_names = T),
                            rate = rate,
                            quiet = F)
  
  writeThird()
  
}  

Sys.sleep(10)
errorEmailorGitPush(finalResult)

# http://www.seancarney.ca/2020/10/11/scheduling-r-scripts-to-run-automatically-in-windows/
## https://www.windowscentral.com/how-create-and-run-your-first-powershell-script-file-windows-10
# https://stackoverflow.com/questions/5343068/is-there-a-way-to-cache-github-credentials-for-pushing-commits#:~:text=To%20cache%20your%20GitHub%20password,time%20it%20talks%20to%20GitHub.
# https://docs.github.com/en/github/using-git/caching-your-github-credentials-in-git










