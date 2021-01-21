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

statePops <- read_csv("populationEstimates.csv", col_types = "ciici")

vaccineEligibility <- read_csv("vaccineEligibilityData.csv", 
                               col_types = "ciiiiiiici")

stateFIPS <- read_csv("stateFIPSCodes.csv", col_types = "cci")

Sys.sleep(45)

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
    
    # google_app <- httr::oauth_app(
    #   "rProjCreds", 
    #   key = rjson::fromJSON(file = "./GmailCredentials.json")$web$client_id,
    #   secret = rjson::fromJSON(file = "./GmailCredentials.json")$web$client_secret
    # )
    
    # use_secret_file("./GmailCredentials.json")
    # 
    gm_auth(email = "adriannesta@gmail.com", scopes = "send")
    
    email <- gm_mime() %>% 
      gm_to(c("anesta@dotdash.com", "amorelli@dotdash.com")) %>% 
      gm_from("adriannesta@gmail.com") %>% 
      gm_subject("Error in updating Vaccine Data Tracker") %>% 
      gm_text_body(paste("The error that occured in the update was:", class(cnd)[1], "on", Sys.Date()))
    
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
    
    # system(paste0("powershell -command \"docker stop ", dockerContainerID, "\""))
    # Sys.sleep(10)
    # system(paste0("powershell -command \"docker rm ", dockerContainerID, "\""))
    # Sys.sleep(10)
    
    cdcFullTable <- read_csv("cdcFullTable.csv", col_types = "ciiiiiiiiDci")
    
    cdcFullTableUpdated <- bind_rows(cdcTable, cdcFullTable)
    
    write_csv(cdcFullTableUpdated, "cdcFullTable.csv")
    
    Sys.sleep(5)
    
    cdcWWWNontotal <- cdcTable %>% 
      filter(state_territory_federal_entity %in% c(state.name, "District of Columbia")) %>% 
      select(state_territory_federal_entity, 
             total_doses_distributed, 
             total_doses_administered,
             doses_distributed_per_100k,
             doses_administered_per_100k
      )
    
    # %>% 
    # adorn_totals(where = "row", name = "U.S. Total")
    
    cdcWWWTotal <- tibble_row(
      state_territory_federal_entity = "U.S. Total",
      total_doses_distributed = sum(cdcWWWNontotal$total_doses_distributed, na.rm = T),
      total_doses_administered = sum(cdcWWWNontotal$total_doses_administered, na.rm = T),
      doses_distributed_per_100k = mean(cdcWWWNontotal$doses_distributed_per_100k, na.rm = T),
      doses_administered_per_100k = mean(cdcWWWNontotal$doses_administered_per_100k, na.rm = T)
    )
    
    # cdcWWW[which(cdcWWW$state_territory_federal_entity == "U.S. Total"), 
    #        c("doses_distributed_per_100k", "doses_administered_per_100k")] <- c(NA_integer_, NA_integer_)
    # 
    # cdcWWW[which(cdcWWW$state_territory_federal_entity == "U.S. Total"), 
    #        c("doses_distributed_per_100k", "doses_administered_per_100k")] <- c(round(mean(cdcWWW$doses_distributed_per_100k, na.rm = T)),
    #                                                                             round(mean(cdcWWW$doses_administered_per_100k, na.rm = T)))
    # cdcWWWTotal <- cdcWWW %>% 
    #   filter(state_territory_federal_entity == "U.S. Total")
    # 
    # cdcWWWNonTotals <- cdcWWW %>% 
    #   filter(state_territory_federal_entity != "U.S. Total") %>% 
    #   arrange(desc(doses_administered_per_100k))
    
    cdcWWWFormatted <- bind_rows(cdcWWWTotal, cdcWWWNontotal)
    
    # cdcWWWFormatted[which(cdcWWWFormatted$state_territory_federal_entity == "Total"), "state_territory_federal_entity"] <- "U.S. Total"
    
    cdcWWWFormatted %>% write_csv("cdcWWWFormatted.csv")
    
    Sys.sleep(5)
    
    statePercVacc <- cdcTable %>% 
      inner_join(statePops, by = c("state_territory_federal_entity" = "Geographic_Area_Name")) %>% 
      mutate(`% of Currently Eligable Vaccinated` = total_doses_administered / Total_18plus_Pop_Estimate) %>% 
      select(state_territory_federal_entity, 
             `% of Currently Eligable Vaccinated`, 
             Total_18plus_Pop_Estimate,
             doses_administered_per_100k
      ) %>% 
      rename(
        `Total Currently Eligible` = Total_18plus_Pop_Estimate,
        `Administered per 100K` = doses_administered_per_100k
      ) %>% 
      rename_with(~paste("As of", strftime(as.character(Sys.Date()), format = "%m/%d")), contains("state")) %>% 
      arrange(desc(`% of Currently Eligable Vaccinated`))
    
    statePercVacc %>% write_csv("statePercVacc.csv")
    
    Sys.sleep(5)
    
    cdcMap <- cdcTable %>% 
      inner_join(statePops, by = c("state_territory_federal_entity" = "Geographic_Area_Name")) %>% 
      mutate(`% of Currently Eligable Vaccinated` = total_doses_administered / Total_18plus_Pop_Estimate,
             `Percent Administered of Distributed` = total_doses_administered / total_doses_distributed,
             `Remaining Supply` = 1 - `Percent Administered of Distributed`
      ) %>% 
      select(
        state_territory_federal_entity, 
        `% of Currently Eligable Vaccinated`,
        Total_18plus_Pop_Estimate,
        total_doses_distributed, 
        total_doses_administered,
        doses_distributed_per_100k,
        doses_administered_per_100k,
        `Percent Administered of Distributed`,
        `Remaining Supply`
      ) %>% 
      rename(
        ID = state_territory_federal_entity,
        `Total Currently Eligible` = Total_18plus_Pop_Estimate,
        `Total Distributed` = total_doses_distributed,
        `Total Administered` = total_doses_administered,
        `Distributed per 100K` = doses_distributed_per_100k,
        `Administered per 100K` = doses_administered_per_100k
      ) 
    
    cdcMap %>% write_csv("cdcMap.csv")
    
    Sys.sleep(5)
    
    areWeThereYetNontotal <- cdcFullTableUpdated %>% 
      filter(date %in% c(max(date), max(date) - 7)) %>% 
      mutate(`Adults Vaccinated in Last Week` = total_doses_administered - lead(
        total_doses_administered, n = 63)) %>% 
      filter(!is.na(`Adults Vaccinated in Last Week`)) %>% 
      inner_join(statePops, by = "fips_code") %>% 
      mutate(`Percent Adults Vaccinated` = (
        total_doses_administered / Total_18plus_Pop_Estimate) * 100,
        `70% of pop` = .7 * Total_Pop_Estimate,
        `Weeks left to achieve 70% of state at current rate` = (
          `70% of pop` - total_doses_administered) / `Adults Vaccinated in Last Week`,
        `months left to 70% of adults with first doses` = `Weeks left to achieve 70% of state at current rate` / 4,
        `Estimated 70% of the population 1st Dose` = strftime(base::as.Date(
          as.integer(Sys.Date()) + (round(`Weeks left to achieve 70% of state at current rate` * 7)), origin = "1970-01-01"),
          format = "%B %Y"
        )) %>% 
      rename(State = state_territory_federal_entity, 
             `Population size` = Total_Pop_Estimate,
             `ADULT Population Size` = Total_18plus_Pop_Estimate) %>% 
      select(State, `Percent Adults Vaccinated`, `Adults Vaccinated in Last Week`,
             `Estimated 70% of the population 1st Dose`, total_doses_administered,
             `Population size`, `ADULT Population Size`, `70% of pop`, 
             `months left to 70% of adults with first doses`, 
             `Weeks left to achieve 70% of state at current rate`)
    
    
    areWeThereYetTotal <- tibble_row(
      State = "U.S. Total",
      `Percent Adults Vaccinated` = mean(areWeThereYetNontotal$`Percent Adults Vaccinated`, na.rm = T),
      `Adults Vaccinated in Last Week` = sum(areWeThereYetNontotal$`Adults Vaccinated in Last Week`, na.rm = T),
      `Estimated 70% of the population 1st Dose` = NA,
      total_doses_administered = sum(areWeThereYetNontotal$total_doses_administered, na.rm = T),
      `Population size` = sum(areWeThereYetNontotal$`Population size`, na.rm = T),
      `ADULT Population Size` = sum(areWeThereYetNontotal$`ADULT Population Size`, na.rm = T),
      `70% of pop` = (.7 * sum(areWeThereYetNontotal$`Population size`, na.rm = T)),
      `months left to 70% of adults with first doses` = ((
        sum(areWeThereYetNontotal$`70% of pop`, 
            na.rm = T) - sum(
              areWeThereYetNontotal$total_doses_administered, 
              na.rm = T)) / sum(
                areWeThereYetNontotal$`Adults Vaccinated in Last Week`, 
                na.rm = T) / 4),
      `Weeks left to achieve 70% of state at current rate` = (
        sum(areWeThereYetNontotal$`70% of pop`, 
            na.rm = T) - sum(
              areWeThereYetNontotal$total_doses_administered, 
              na.rm = T)) / sum(
                areWeThereYetNontotal$`Adults Vaccinated in Last Week`, 
                na.rm = T)
    )
    
    # %>% 
    #   adorn_totals(where = "row", name = "U.S. Total")
    
    # areWeThereYetRough[which(areWeThereYetRough$State == "U.S. Total"),
    #                    c("Percent Adults Vaccinated",
    #                      "Estimated 70% of the population 1st Dose",
    #                      "months left to 70% of adults with first doses",
    #                      "Weeks left to achieve 70% of state at current rate")] <- NA
    # 
    # 
    # areWeThereYetRough[which(areWeThereYetRough$State == "U.S. Total"),
    #                    "Percent Adults Vaccinated"] <- round(mean(areWeThereYetRough$`Percent Adults Vaccinated`, na.rm = T), 2)
    # 
    # areWeThereYetRough[which(areWeThereYetRough$State == "U.S. Total"),
    #                    "months left to 70% of adults with first doses"] <- round(mean(areWeThereYetRough$`months left to 70% of adults with first doses`, na.rm = T), 1)
    # 
    # areWeThereYetRough[which(areWeThereYetRough$State == "U.S. Total"),
    #                    "Weeks left to achieve 70% of state at current rate"] <- round(mean(areWeThereYetRough$`Weeks left to achieve 70% of state at current rate`, na.rm = T), 1)
    
    areWeThereYet2 <- bind_rows(areWeThereYetTotal,
                               areWeThereYetNontotal)
    
    areWeThereYet <- areWeThereYet2 %>% 
      mutate(`Estimated 70% of the population 1st Dose` = strftime(base::as.Date(
        as.integer(Sys.Date()) + (round(`Weeks left to achieve 70% of state at current rate` * 7)), origin = "1970-01-01"),
        format = "%B %Y"
      ))
    
    # areWeThereYetRoughTotals <- areWeThereYetRough2 %>% filter(State == "U.S. Total")
    # 
    # areWeThereYetRoughRest <- areWeThereYetRough2 %>% 
    #   filter(State != "U.S. Total") %>% 
    #   arrange(`Weeks left to achieve 70% of state at current rate`)
    
    # areWeThereYet[which(areWeThereYet$State == "Total"), "State"] <- "U.S. Total"
    
    write_csv(areWeThereYet, "areWeThereYet.csv")
    
  }, error = function(cond) {
    condFull <- error_cnd(paste("An error occured with the update:", 
                                cond
    ))
    
    return(condFull)
  }
)
shell(cmd = ".\\stopDockerScraper.ps1", shell = "powershell", wait = F)
Sys.sleep(10)
errorEmailorGitPush(finalResult)

# http://www.seancarney.ca/2020/10/11/scheduling-r-scripts-to-run-automatically-in-windows/
## https://www.windowscentral.com/how-create-and-run-your-first-powershell-script-file-windows-10
# https://stackoverflow.com/questions/5343068/is-there-a-way-to-cache-github-credentials-for-pushing-commits#:~:text=To%20cache%20your%20GitHub%20password,time%20it%20talks%20to%20GitHub.
# https://docs.github.com/en/github/using-git/caching-your-github-credentials-in-git










