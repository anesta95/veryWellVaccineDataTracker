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


statePops <- read_csv("populationEstimates.csv", col_types = "cii")

# system("powershell -command \".\\startDockerScraper.ps1\"")
shell(cmd = ".\\startDockerScraper.ps1", shell = "powershell", wait = F)

# system("powershell -command \"docker pull scrapinghub/splash:latest --disable-browser-caches\"")
# dockerContainerID <- system("powershell -command \"docker run -p 5023:5023 -p 8050:8050 -p 8051:8051 scrapinghub/splash:latest --disable-browser-caches\"")
Sys.sleep(120)

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
    Sys.sleep(35)
    cdcTable <- cdcPage %>% 
      html_node(css = "#vaccinations-table") %>% 
      html_table(header = NA, fill = T) %>% 
      mutate(across(.cols = contains("Doses"), as.numeric)) %>% 
      mutate(date = Sys.Date())
    Sys.sleep(15)
    shell(cmd = ".\\stopDockerScraper.ps1", shell = "powershell", wait = F)
    Sys.sleep(15)
    # system(paste0("powershell -command \"docker stop ", dockerContainerID, "\""))
    # Sys.sleep(10)
    # system(paste0("powershell -command \"docker rm ", dockerContainerID, "\""))
    # Sys.sleep(10)
    
    cdcFullTable <- read_csv("cdcFullTable.csv", col_types = "ciiiiiiiiD")
    
    cdcFullTableUpdated <- bind_rows(cdcTable, cdcFullTable)
    
    write_csv(cdcFullTableUpdated, "cdcFullTable.csv")
    
    Sys.sleep(5)
    
    cdcWWW <- cdcTable %>% 
      clean_names() %>% 
      filter(state_territory_federal_entity %in% c(state.name, "District of Columbia")) %>% 
      select(state_territory_federal_entity, 
             total_doses_distributed, 
             total_doses_administered,
             doses_distributed_per_100k,
             doses_administered_per_100k
      ) %>% 
    adorn_totals(where = "row")
    
    cdcWWW[which(cdcWWW$state_territory_federal_entity == "Total"), 
           c("doses_distributed_per_100k", "doses_administered_per_100k")] <- c(NA_integer_, NA_integer_)
    
    cdcWWW[which(cdcWWW$state_territory_federal_entity == "Total"), 
           c("doses_distributed_per_100k", "doses_administered_per_100k")] <- c(round(mean(cdcWWW$doses_distributed_per_100k, na.rm = T)),
                                                                                round(mean(cdcWWW$doses_administered_per_100k, na.rm = T)))
    cdcWWWTotal <- cdcWWW %>% 
      filter(state_territory_federal_entity == "Total")
    
    cdcWWWNonTotals <- cdcWWW %>% 
      arrange(desc(doses_administered_per_100k))
    
    cdcWWWFormatted <- bind_rows(cdcWWWTotal, cdcWWWNonTotals)
    
    cdcWWWFormatted[which(cdcWWWFormatted$state_territory_federal_entity == "Total"), "state_territory_federal_entity"] <- "U.S. Total"
    
    cdcWWWFormatted %>% write_csv("cdcWWWFormatted.csv")
    
    Sys.sleep(5)
    
    statePercVacc <- cdcTable %>% 
      clean_names() %>%
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
      clean_names() %>%
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
        `Administered per 100K` = doses_administered_per_100k,
        
      )
    
    cdcMap %>% write_csv("cdcMap.csv")
    
  }, error = function(cond) {
    condFull <- error_cnd(paste("An error occured with the update:", 
                                cond
    ))
    
    return(condFull)
  }
)

shell(cmd = ".\\startDockerScraper.ps1", shell = "powershell", wait = F)
Sys.sleep(20)
errorEmailorGitPush(finalResult)

# http://www.seancarney.ca/2020/10/11/scheduling-r-scripts-to-run-automatically-in-windows/
## https://www.windowscentral.com/how-create-and-run-your-first-powershell-script-file-windows-10
# https://stackoverflow.com/questions/5343068/is-there-a-way-to-cache-github-credentials-for-pushing-commits#:~:text=To%20cache%20your%20GitHub%20password,time%20it%20talks%20to%20GitHub.
# https://docs.github.com/en/github/using-git/caching-your-github-credentials-in-git










