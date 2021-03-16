library(rlang)
library(lubridate)
library(janitor)
library(googledrive)
library(googlesheets4)
library(rjson)
library(magrittr)
library(tidyverse)

setwd("C:\\Users\\anesta\\Documents\\Verywell_Vaccine_Data_Tracker")

statePops <- read_csv("./referenceData/populationEstimates.csv", col_types = "ciici")

vaccineEligP1All <- read_csv("./referenceData/2021_02_23_vaccineEligibilityDataW1.csv", col_types = "ciiiiiiiiiiiiiici")

stateFIPS <- read_csv("./referenceData/stateFIPSCodes.csv", col_types = "cci")

Sys.sleep(3)

shell(cmd = "./gitPullLatestCDC.ps1", shell = "powershell")

Sys.sleep(10)

# https://www.benlcollins.com/formula-examples/dynamic-named-ranges/
# googleSheetWriter <- function(sheetID, dataSet, firstInstance = F, range = NA) {
#   rate <- rate_delay(61, max_times = 3)
#   
#   if (firstInstance) {
#     insistently(~write_sheet(ss = sheetID,
#                              data = dataSet,
#                              sheet = as.character(Sys.Date())),
#                 rate = rate,
#                 quiet = F)
#   } else {
#     insistently(~range_write(ss = sheetID,
#                              data = dataSet,
#                              sheet = as.character(Sys.Date()),
#                              range = range,
#                              col_names = T),
#                 rate = rate,
#                 quiet = F)
#   }
# }

# errorEmailorGitPush <- function(cnd) {
#   if (class(cnd)[2] == "rlang_error") {
#     # https://blog.mailtrap.io/send-emails-with-gmail-api/
#     # https://github.com/r-lib/gmailr#setup
#     # https://github.com/jennybc/send-email-with-r#create-a-project-in-google-developers-console
#     # https://support.google.com/cloud/answer/9110914#skip
#     # https://support.google.com/cloud/answer/9110914#mark-internal
#     # https://github.com/r-lib/gmailr
#     # https://console.developers.google.com/apis/credentials?authuser=1&project=vwvaccinedatatrackerproj&supportedpurview=project
#     
#     gm_auth_configure(path = "./GmailCredentials.json")
#     
#      
#     gm_auth(email = "adriannesta@gmail.com", scopes = "send")
#     
#     email <- gm_mime() %>% 
#       gm_to(c("anesta@dotdash.com", "amorelli@dotdash.com")) %>% 
#       gm_from("adriannesta@gmail.com") %>% 
#       gm_subject("Error in updating Vaccine Data Tracker") %>% 
#       gm_text_body(paste("The error that occured in the update was:", class(cnd)[1]))
#     
#     Sys.sleep(5)
#     
#     # Error logging
#     # http://www.seancarney.ca/2020/10/09/error-catching-logging-and-reporting-in-r-with-trycatchlog/
#     
#     gm_send_message(email)
#   } else {
#     # pushToGithub
#     # https://stackoverflow.com/questions/5343068/is-there-a-way-to-cache-github-credentials-for-pushing-commits
#     # https://stackoverflow.com/questions/5343068/is-there-a-way-to-cache-github-credentials-for-pushing-commits#:~:text=To%20cache%20your%20GitHub%20password,time%20it%20talks%20to%20GitHub.
#     # https://docs.github.com/en/github/using-git/caching-your-github-credentials-in-git
#     # https://docs.github.com/en/free-pro-team@latest/github/using-git/caching-your-github-credentials-in-git
#     shell(cmd = "./pushToGit.ps1", shell = "powershell")
#     
#     
#     
#   } 
# }

finalResult <- tryCatch(
  {
    
    cdcTable <- fromJSON(file = "https://raw.githubusercontent.com/anesta95/veryWellVaccineDataTracker/main/cdcVaccines.json") %>% 
      extract2(2) %>%
      map_df(`[`) %>% 
      mutate(Date = base::as.Date(Date)) %>% 
      mutate(LongName = if_else(
        LongName == "New York State", "New York", 
        LongName))
    
    
    
    # splash("localhost") %>% 
    #   splashr::render_html("https://covid.cdc.gov/covid-data-tracker/#vaccinations", wait = 5) -> cdcPage
    
    
    # cdcTable <- cdcPage %>% 
    #   html_node(css = "#vaccinations-table") %>% 
    #   html_table(header = NA, fill = T) %>% 
    #   clean_names() %>% 
    #   mutate(across(.cols = contains("Doses"), as.numeric)) %>% 
    #   mutate(state_territory_federal_entity = if_else(
    #     state_territory_federal_entity == "New York State", "New York", 
    #     state_territory_federal_entity)) %>% 
    #   mutate(date = Sys.Date()) %>% 
    #   left_join(stateFIPS, by = c("state_territory_federal_entity" = "name"))
    
    Sys.sleep(10)
    
    # names(cdcTable) <- c("state_territory_federal_entity", "total_doses_delivered",
    #                      "total_doses_administered", "doses_delivered_per_100k", 
    #                      "doses_administered_per_100k",	"people_with_1_doses", 
    #                      "people_with_1_doses_per_100k", "people_with_2_doses", 
    #                      "people_with_2_doses_per_100k", "date", "postal_code",	
    #                      "fips_code")
    
    
    cdcFullTable <- read_csv("./chartData/cdcFullTable.csv", col_types = "Dccciciiiiiiiiiddidiiiidiiiiidiiiiiiiiiiiiiiiiidididiiiiiiiiii")
    
    if (unique(cdcTable$Date) == max(cdcFullTable$Date)) {
      cdcFullTableUpdated <- cdcFullTable
    } else {
      cdcFullTableUpdated <- bind_rows(cdcTable, cdcFullTable)
    }
    
    write_csv(cdcFullTableUpdated, "./chartData/cdcFullTable.csv")
    
    Sys.sleep(5)
    
    cdcWWWNontotal <- cdcTable %>% 
      filter(!(Location %in% c("BP2", "DD2", "IH2", "VA2", "LTC", "US"))) %>% 
      mutate(Complete_Vaccinations_Per_100K = round(((Administered_Dose2_Recip + Administered_Janssen) / Census2019) * 100000),
             Administered_Dose1_Per_100K = Administered_Dose1_Pop_Pct * 1000) %>% 
      select(LongName, 
             Doses_Distributed, 
             Doses_Administered,
             Administered_Dose1_Per_100K,
             Complete_Vaccinations_Per_100K
      ) %>% 
      arrange(desc(Complete_Vaccinations_Per_100K)) %>% 
      mutate(
        Doses_Distributed = as.character(Doses_Distributed),
        Doses_Administered = as.character(Doses_Administered),
        Administered_Dose1_Per_100K = as.character(Administered_Dose1_Per_100K),
        Complete_Vaccinations_Per_100K = as.character(Complete_Vaccinations_Per_100K))
    
    cdcWWWTotal <- tibble_row(
      LongName = "U.S. Total",
      Doses_Distributed = format(sum(as.integer(cdcWWWNontotal$Doses_Distributed), na.rm = T), big.mark = ",", scientific = F),
      Doses_Administered = format(sum(as.integer(cdcWWWNontotal$Doses_Administered), na.rm = T), big.mark = ",", scientific = F),
      Administered_Dose1_Per_100K = format(round((pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Dose1_Pop_Pct"])) * 1000), big.mark = ",", scientific = F),
      Complete_Vaccinations_Per_100K = format(
        round(
          ((pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Dose2_Recip"]) + pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Janssen"])) / 331996199L) * 100000
        ), big.mark = ",", scientific = F
      )
    )
    
    
    
    cdcWWWFormatted <- bind_rows(cdcWWWTotal, cdcWWWNontotal) %>% 
      rename(State = LongName,
             `Total Doses Delivered` = Doses_Distributed,
             `Total Doses Administered` = Doses_Administered,
             `People with 1 Dose per 100k` = Administered_Dose1_Per_100K,
             `People Completely Vaccinated per 100k` = Complete_Vaccinations_Per_100K
      )
    
    
    cdcWWWFormatted %>% write_csv("./chartData/cdcWWWFormatted.csv")
    
    
    Sys.sleep(5)
    
    cdcMap <- cdcTable %>% 
      mutate(Complete_Vaccinations = round(Administered_Dose2_Recip + Administered_Janssen)) %>% 
      inner_join(vaccineEligP1All, by = c("Location" = "postal_code")) %>% 
      mutate(`% of Currently Eligible Vaccinated` = round((Complete_Vaccinations / Total_people_to_vaccinate) * 100, digits = 1))%>% 
      select(LongName, `% of Currently Eligible Vaccinated`, 
             Total_people_to_vaccinate) %>% 
      rename(
        ID = LongName,
        `Total Currently Eligible` = Total_people_to_vaccinate
      ) 
    
    cdcMap %>% write_csv("./chartData/cdcMap.csv")
    
    Sys.sleep(5)
    
    
    areWeThereYetNontotal <- cdcFullTableUpdated %>% 
      filter(Date %in% c(max(Date), max(Date) - 7)) %>%
      filter(!(LongName %in% c("Bureau of Prisons", "Long Term Care", 
                               "Dept of Defense",
                               "Indian Health Svc",
                               "Veterans Health", "United States"))) %>%
      mutate(#`% Population with 2 Vaccines` = (people_with_2_doses_per_100k / 1000),
        `Doses administered in the last week` = Doses_Administered - lead(
          Doses_Administered, n = 59),
        `1+ Doses adminstered in the last week` = Administered_Dose1_Recip - lead(
          Administered_Dose1_Recip, n = 59)) %>% 
      #filter(!is.na(`Doses administered in the last week`)) %>%
      filter(`Doses administered in the last week` != 0) %>% 
      inner_join(statePops, by = c("Location" = "postal_code")) %>% 
      mutate(`70% of population` = .7 * Census2019,
             `Estimated to 70% Pop 2 Doses` = strftime(base::as.Date(
               round(
                 (
                   (
                     (
                       (`70% of population` - Administered_Dose1_Recip) / 
                         `1+ Doses adminstered in the last week`)) * 7) + 28), 
               origin = "1970-01-01") + as.integer(Sys.Date()), format = "%B %Y"),
             testSort = round(((((`70% of population` - Administered_Dose1_Recip) / 
                                   `1+ Doses adminstered in the last week`)) * 7) + 28),
             Completely_Vaccinated_Pop_Pct = round(((Administered_Dose2_Recip + Administered_Janssen) / Census2019) * 100)) %>%
      arrange(testSort) %>% 
      select(LongName, Completely_Vaccinated_Pop_Pct,
             `Doses administered in the last week`, `Estimated to 70% Pop 2 Doses`) %>% 
      mutate(`Doses administered in the last week` = format(`Doses administered in the last week`, big.mark = ",", scientific = F)) %>% 
      rename(`% Population with 2 Vaccines` = Completely_Vaccinated_Pop_Pct, 
             State = LongName)
    
    
    
    
    onePlusVaxLastWeek <- cdcFullTableUpdated %>% 
      filter(Date %in% c(max(Date), max(Date) - 7)) %>%
      # filter(LongName == "United States") %>% 
      filter(!(LongName %in% c("Bureau of Prisons", "Long Term Care",
                               "Dept of Defense",
                               "Indian Health Svc",
                               "Veterans Health", "United States"))) %>%
      mutate(`1+ Doses adminstered in the last week` = Administered_Dose1_Recip - lead(
        Administered_Dose1_Recip, n = 59)) %>% 
      filter(!is.na(`1+ Doses adminstered in the last week`)) %>% 
      pull(`1+ Doses adminstered in the last week`) %>% 
      sum()
    
    
    areWeThereYetTotal <- tibble_row(
      State = "U.S. Total", 
      `% Population with 2 Vaccines` =
        round(
          ((pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Dose2_Recip"]) + pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Janssen"])) / 331996199L) * 100, 1
        ),
      #pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Dose2_Pop_Pct"]), #round((sum(cdcTable$people_with_2_doses, na.rm = T) / 328580394L) * 100, 2),
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
                  (331996199L * .7) - pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Dose1_Recip"])) / 
                  onePlusVaxLastWeek) * 7) + 28) + as.integer(Sys.Date())), origin = "1970-01-01"
        ), format = "%B %Y")
    )
    
    # Prev US Pop Estimate 328580394L
    areWeThereYet <- bind_rows(areWeThereYetTotal, areWeThereYetNontotal)
    
    
    write_csv(areWeThereYet, "./chartData/areWeThereYet.csv")
    
    
    # supplyProjection <- cdcFullTableUpdated %>%
    #   filter(Date %in% c(max(Date), max(Date) - 7)) %>%
    #   filter(!(LongName %in% c("Bureau of Prisons", "Long Term Care",
    #                            "Dept of Defense",
    #                            "Indian Health Svc",
    #                            "Veterans Health"))) %>%
    #   arrange(desc(Date), LongName) %>%
    #   mutate(totalDosesDistrb = ((Distributed_Moderna / 2) + (Distributed_Pfizer / 2) + Distributed_Janssen),
    #          `Doses delivered in the last week` = totalDosesDistrb - lead(
    #            totalDosesDistrb, n = 60
    #          )) %>%
    #   filter(`Doses delivered in the last week` != 0) %>%
    #   inner_join(statePops, by = c("Location" = "postal_code")) %>%
    #   mutate(`Estimated to 100% Vaccine Available 18+` = strftime(base::as.Date(
    #     round(
    #       (
    #         (
    #           (
    #             (Census2019_18PlusPop_2 - totalDosesDistrb) /
    #               `Doses delivered in the last week`)) * 7)),
    #     origin = "1970-01-01") + as.integer(Sys.Date()))) %>%
    #   select(Date, LongName, Census2019_18PlusPop_2, totalDosesDistrb, `Doses delivered in the last week`,
    #          `Estimated to 100% Vaccine Available 18+`) %>%
    #   rename(State = LongName, `Total Doses Distributed` = totalDosesDistrb, `Total Population` = Census2019_18PlusPop_2) %>%
    #   arrange(`Estimated to 100% Vaccine Available 18+`)
    # 
    # write_csv(supplyProjection, "../supplyProjection20210316.csv")
    
  }, error = function(cond) {
    condFull <- error_cnd(class = "vwDataTrackerError", message = paste("An error occured with the update:", 
                                                                        cond, "on", Sys.Date(), "\n"
    ))
    
    write(condFull[["message"]], "./errorLog.txt", append = T)
    
    return(condFull)
  }
)

# shell(cmd = ".\\stopDockerScraper.ps1", shell = "powershell", wait = F)
Sys.sleep(5)
if (wday(Sys.Date(), label = F) == 2) {
  
  drive_auth(email = "anesta@dotdash.com")
  gs4_auth(token = drive_token())
  
  
  rate <- rate_delay(61, max_times = 3)
  # googleSheetWriter("1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo", cdcWWWFormatted, firstInstance = T)
  # googleSheetWriter(sheetID = "1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo", dataSet = cdcWWWFormatted,
  #                   firstInstance = T)
  
  writeFirst <- insistently(~write_sheet(ss = "1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo",
                            data = cdcWWWFormatted,
                            sheet = as.character(Sys.Date())),
                            rate = rate,
                            quiet = F)

  writeFirst()
  
  Sys.sleep(5)
  
  # googleSheetWriter("1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo", cdcMap, range = "F1")
  
   writeSecond <- insistently(~range_write(ss = "1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo",
                                           data = cdcMap,
                                           sheet = as.character(Sys.Date()),
                                           range = "F1",
                                           col_names = T),
                              rate = rate,
                              quiet = F)
   
  writeSecond()
  Sys.sleep(5)
  
  # googleSheetWriter("1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo", areWeThereYet, range = "J1")
  
   writeThird <- insistently(~range_write(ss = "1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo",
                                          data = areWeThereYet,
                                          sheet = as.character(Sys.Date()),
                                          range = "J1",
                                          col_names = T),
                             rate = rate,
                             quiet = F)
   
  writeThird()
  
  Sys.sleep(5)
  
  aWTYNontotalAnalysis <- cdcFullTableUpdated %>% 
    filter(Date %in% c(max(Date), max(Date) - 7)) %>%
    filter(!(LongName %in% c("Bureau of Prisons", "Long Term Care", 
                             "Dept of Defense",
                             "Indian Health Svc",
                             "Veterans Health", "United States"))) %>%
    mutate(`1+ Doses adminstered in the last week` = Administered_Dose1_Recip - lead(
      Administered_Dose1_Recip, n = 59)) %>% 
    filter(`1+ Doses adminstered in the last week` != 0) %>% 
    mutate(`70% of population` = .7 * Census2019,
           `Estimated to 70% Pop 2 Doses` = base::as.Date(
             round(
               (
                 (
                   (
                     (`70% of population` - Administered_Dose1_Recip) / 
                       `1+ Doses adminstered in the last week`)) * 7) + 28), 
             origin = "1970-01-01") + as.integer(Sys.Date())) %>%
    arrange(LongName) %>% 
    select(LongName, `Estimated to 70% Pop 2 Doses`) %>% 
    rename(State = LongName)
  
  aWTYTotalAnalysis <- tibble_row(
    State = "U.S. Total", 
    `Estimated to 70% Pop 2 Doses` = 
      base::as.Date(
        (round(
          (
            (
              (
                (331996199L * .7) - pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Dose1_Recip"])) / 
                onePlusVaxLastWeek) * 7) + 28) + as.integer(Sys.Date())), origin = "1970-01-01"
      )
  )
  
  aWTYFull <- bind_rows(aWTYTotalAnalysis, aWTYNontotalAnalysis) %>% 
    pivot_wider(names_from = State, values_from = `Estimated to 70% Pop 2 Doses`)
  
  aWTYRow <- (((as.integer(max(cdcFullTableUpdated$Date)) + 7) - as.integer(base::as.Date("2021-01-25"))) / 7) + 3
  Sys.sleep(5)
  # googleSheetWriter("1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo", aWTYFull, range = paste0("K", aWTYRow))
  
  writeSummary1 <- insistently(~range_write(ss = "1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo",
                                            data = aWTYFull,
                                            sheet = "Copy of Comparisons",
                                            range = paste0("D", aWTYRow),
                                            col_names = F),
                               rate = rate,
                               quiet = F)
  
  writeSummary1()
  Sys.sleep(5)
  pplW2Doses <- cdcTable %>% 
    filter(!(Location %in% c("BP2", "DD2", "IH2", "VA2", "LTC"))) %>% 
    mutate(Complete_Vaccinations_Per_100K = round(((Administered_Dose2_Recip + Administered_Janssen) / Census2019) * 100000)) %>% 
    arrange(LongName) %>% 
    select(Date, LongName, Complete_Vaccinations_Per_100K) %>% 
    pivot_wider(names_from = LongName, values_from = Complete_Vaccinations_Per_100K) %>% 
    select(`United States`, everything()) %>% 
    select(-Date)
  
  pplW2DosesRow <- (((as.integer(max(cdcFullTableUpdated$Date)) + 7) - as.integer(base::as.Date("2021-01-25"))) / 7) + 66
  
  writeSummary2 <- insistently(~range_write(ss = "1m3tOPe_Z85sVsBqNV_ob3OCZtiw3gnyftIk0iKpTwlo",
                                            data = pplW2Doses,
                                            sheet = "Copy of Comparisons",
                                            range = paste0("D", pplW2DosesRow),
                                            col_names = F),
                               rate = rate,
                               quiet = F)
  
  writeSummary2()
  
  
  
}  


Sys.sleep(10)
if (class(finalResult)[2] != "rlang_error") {
  shell(cmd = "./pushToGit.ps1", shell = "powershell")
}


# http://www.seancarney.ca/2020/10/11/scheduling-r-scripts-to-run-automatically-in-windows/
# https://www.windowscentral.com/how-create-and-run-your-first-powershell-script-file-windows-10
# https://stackoverflow.com/questions/5343068/is-there-a-way-to-cache-github-credentials-for-pushing-commits#:~:text=To%20cache%20your%20GitHub%20password,time%20it%20talks%20to%20GitHub.
# https://docs.github.com/en/github/using-git/caching-your-github-credentials-in-git


# cdcFullTableUpdated %>%
#   filter(!(LongName %in% c("Bureau of Prisons", "Long Term Care",
#                            "Dept of Defense",
#                            "Indian Health Svc",
#                            "Veterans Health", "United States"))) %>%
#   filter(Date %in% c(max(Date), max(Date) - 7)) %>%
#   mutate(`% Population with 2 Vaccines` = (Administered_Dose2_Per_100K / 1000),
#          `Pct Chg Doses administered` = (Doses_Administered - lead(
#            Doses_Administered, n = 59)) / lead(Doses_Administered, n = 59),
#          `Pct Chg Ppl w 1 Doses` = (Administered_Dose1 - lead(
#            Administered_Dose1, n = 59)) / lead(Administered_Dose1, n = 59),
#          `Pct Chg Ppl w 2 Doses` = (Administered_Dose2 - lead(
#            Administered_Dose2, n = 59)) / lead(Administered_Dose2, n = 59)) %>%
#   filter(`Pct Chg Doses administered` != 0) %>%
#   select(LongName, `Pct Chg Doses administered`,
#          `Pct Chg Ppl w 1 Doses`, `Pct Chg Ppl w 2 Doses`) %>%
#   arrange(desc(`Pct Chg Ppl w 1 Doses`)) %>% 
#   write_csv(paste0("dosesAdminVsPplW1Dose", as.character(Sys.Date()), ".csv"))







