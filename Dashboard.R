library(shiny)
library(shinydashboard)
library(shinythemes)
library(stringr) 
library(readxl) 
library(openxlsx)
library(XLConnect)
library(stringi)
library(tidyverse)
library(ggrepel)
library(DT)
library(forecast)
library(rvest)
library(lifecontingencies)
library(data.table)
library(rgeos)
library(rgdal)
library(maptools)
library(sf)


#start of downloading and processing mortality data

URL_mortality <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2flifeexpectancies%2fdatasets%2fnationallifetablesunitedkingdomreferencetables%2fcurrent/nationallifetables3yearuk.xlsx"

path_tmp <- tempfile()
download.file(URL_mortality, destfile=path_tmp, mode = "wb")
dflist <- readWorksheetFromFile(path_tmp, sheet = c(5:42), startRow = 7)

currentYearTableRaw <- readWorksheetFromFile(path_tmp, sheet = 5, startRow = 7)

currentYear <- strtoi(stri_sub((readWorksheetFromFile(path_tmp, sheet = 5)[3,1]),-4,-1))

#end of downloading and processing mortality data



#start of mortality source

url_mortality_website_page <- "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables"

url_mortality_website_raw <- read_html(url_mortality_website_page)

latest_and_next_mortality <- url_mortality_website_raw %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//p[@class = 'col col--md-12 col--lg-15 meta__item']") %>%
  rvest::html_text()

release_date_mortality <- sub('.*Release date: ', '', latest_and_next_mortality[2])
next_release_date_mortality <- stri_sub((sub('.*Next release: ', '', latest_and_next_mortality[3])), 1, -2)


#end of mortality source



#Start of Mortality Graph code

male_list <- list()
female_list <- list()

for (i in 1:length(dflist)) {
  maleValue = (as.data.frame(dflist[i]))[1,6]
  femaleValue = (as.data.frame(dflist[i]))[1,12]
  
  male_list[[length(male_list) + 1]] <- maleValue
  female_list[[length(female_list) + 1]] <- femaleValue
}

Expectancy_Table <- data.frame(year = seq(currentYear, length.out = length(dflist), by = -1), 
                               male = matrix(unlist(male_list), nrow=length(male_list), byrow=TRUE),
                               female = matrix(unlist(female_list), nrow=length(female_list), byrow=TRUE)) %>%
  map_df(rev)

Expectancy_Table_longer <- Expectancy_Table %>% 
  pivot_longer(!year, names_to = "gender", values_to = "expectancy") %>%
  mutate_at("gender", factor)

##End of Mortality Graph code



##Start of Current Year Table code

currentYearTable <- currentYearTableRaw[c(1,3,6,9,12)]

colnames(currentYearTable) <- c("age", "qx_male", "ex_male", "qx_female", "ex_female")

currentYearTable <- currentYearTable %>%
  pivot_longer(!age, names_to = c(".value", "gender"), names_sep = "_")


##End of Current Year Table code



##Start of ARIMA Forecasting code

predict_timeframe <- 20

forecastted_model_male <- auto.arima(Expectancy_Table$male)
forecastted_model_female <- auto.arima(Expectancy_Table$female)

futurevalues_male <- forecast(forecastted_model_male, h=predict_timeframe, level = c(95))
futurevalues_female <- forecast(forecastted_model_female, h=predict_timeframe, level = c(95))

futureExpectancyTable <- data.frame(year = seq(currentYear + 1, length.out = predict_timeframe, by = 1),
                                    predicted_mean.male = futurevalues_male$mean,
                                    predicted_lower.male = futurevalues_male$lower,
                                    predicted_upper.male = futurevalues_male$upper,
                                    predicted_mean.female = futurevalues_female$mean,
                                    predicted_lower.female = futurevalues_female$lower,
                                    predicted_upper.female = futurevalues_female$upper
)

colnames(futureExpectancyTable) <- c("year", "expectancy_forecast.mean.male", "expectancy_forecast.lower.male", "expectancy_forecast.upper.male", "expectancy_forecast.mean.female", "expectancy_forecast.lower.female", "expectancy_forecast.upper.female")

futureExpectancyTableLonger <- futureExpectancyTable %>%
  pivot_longer(!year, names_to = c(".value", "gender"), names_sep = "_")

future_last_values <- futureExpectancyTableLonger %>%
  group_by(gender) %>%
  filter(gender == "forecast.mean.male" | gender == "forecast.mean.female") %>%
  mutate_at(vars(expectancy), funs(round(., 2))) %>%
  top_n(1, expectancy)

future_first_values <- futureExpectancyTableLonger %>%
  group_by(gender) %>%
  filter(gender == "forecast.mean.male" | gender == "forecast.mean.female") %>%
  mutate_at(vars(expectancy), funs(round(., 2))) %>%
  top_n(-1, expectancy)

future_values_need <- rbind(future_first_values, future_last_values)

##End of ARIMA Forecasting code



##Start of Combining Present and future code

intermediate_table <- Expectancy_Table_longer %>%
  filter(year == currentYear) %>%
  mutate(gender = case_when(
    gender == "female" ~ "forecast.mean.female",
    gender == "male" ~ "forecast.mean.male"
  ))

present_and_future <- rbind(Expectancy_Table_longer, intermediate_table, futureExpectancyTableLonger)

present_last_values <- Expectancy_Table_longer %>%
  group_by(gender) %>%
  top_n(1, expectancy)

present_and_future_values <- rbind(present_last_values, future_last_values)

##End of Combining Present and future code


##Start of 30 years old mortality graph

male_list_30 <- list()
female_list_30 <- list()

for (i in 1:length(dflist)) {
  maleValue_30 = (as.data.frame(dflist[i]))[31,6]
  femaleValue_30 = (as.data.frame(dflist[i]))[31,12]
  
  male_list_30[[length(male_list_30) + 1]] <- maleValue_30
  female_list_30[[length(female_list_30) + 1]] <- femaleValue_30
}

Expectancy_Table_30 <- data.frame(year = seq(currentYear, length.out = length(dflist), by = -1), 
                                  male = matrix(unlist(male_list_30), nrow=length(male_list_30), byrow=TRUE),
                                  female = matrix(unlist(female_list_30), nrow=length(female_list_30), byrow=TRUE)) %>%
  map_df(rev)

Expectancy_Table_30 <- Expectancy_Table_30 %>%
  mutate(male = male + 30) %>%
  mutate(female = female + 30)

Expectancy_Table_longer_30 <- Expectancy_Table_30 %>% 
  pivot_longer(!year, names_to = "gender", values_to = "expectancy") %>%
  mutate_at("gender", factor)

forecastted_model_male_30 <- auto.arima(Expectancy_Table_30$male)
forecastted_model_female_30 <- auto.arima(Expectancy_Table_30$female)

futurevalues_male_30 <- forecast(forecastted_model_male_30, h=predict_timeframe, level = c(95))
futurevalues_female_30 <- forecast(forecastted_model_female_30, h=predict_timeframe, level = c(95))

futureExpectancyTable_30 <- data.frame(year = seq(currentYear + 1, length.out = predict_timeframe, by = 1),
                                       predicted_mean.male = futurevalues_male_30$mean,
                                       predicted_lower.male = futurevalues_male_30$lower,
                                       predicted_upper.male = futurevalues_male_30$upper,
                                       predicted_mean.female = futurevalues_female_30$mean,
                                       predicted_lower.female = futurevalues_female_30$lower,
                                       predicted_upper.female = futurevalues_female_30$upper
)

colnames(futureExpectancyTable_30) <- c("year", "expectancy_forecast.mean.male", "expectancy_forecast.lower.male", "expectancy_forecast.upper.male", "expectancy_forecast.mean.female", "expectancy_forecast.lower.female", "expectancy_forecast.upper.female")

futureExpectancyTableLonger_30 <- futureExpectancyTable_30 %>%
  pivot_longer(!year, names_to = c(".value", "gender"), names_sep = "_")

future_last_values_30 <- futureExpectancyTableLonger_30 %>%
  group_by(gender) %>%
  filter(gender == "forecast.mean.male" | gender == "forecast.mean.female") %>%
  mutate_at(vars(expectancy), funs(round(., 2))) %>%
  top_n(1, expectancy)

future_first_values_30 <- futureExpectancyTableLonger_30 %>%
  group_by(gender) %>%
  filter(gender == "forecast.mean.male" | gender == "forecast.mean.female") %>%
  mutate_at(vars(expectancy), funs(round(., 2))) %>%
  top_n(-1, expectancy)

future_values_need_30 <- rbind(future_first_values_30, future_last_values_30)

intermediate_table_30 <- Expectancy_Table_longer_30 %>%
  filter(year == currentYear) %>%
  mutate(gender = case_when(
    gender == "female" ~ "forecast.mean.female",
    gender == "male" ~ "forecast.mean.male"
  ))

present_and_future_30 <- rbind(Expectancy_Table_longer_30, intermediate_table_30, futureExpectancyTableLonger_30)

present_last_values_30 <- Expectancy_Table_longer_30 %>%
  group_by(gender) %>%
  top_n(1, expectancy)

present_and_future_values_30 <- rbind(present_last_values_30, future_last_values_30)

##End of 30 years old mortality graph



##Start of 60 years old mortality graph

male_list_60 <- list()
female_list_60 <- list()

for (i in 1:length(dflist)) {
  maleValue_60 = (as.data.frame(dflist[i]))[61,6]
  femaleValue_60 = (as.data.frame(dflist[i]))[61,12]
  
  male_list_60[[length(male_list_60) + 1]] <- maleValue_60
  female_list_60[[length(female_list_60) + 1]] <- femaleValue_60
}

Expectancy_Table_60 <- data.frame(year = seq(currentYear, length.out = length(dflist), by = -1), 
                                  male = matrix(unlist(male_list_60), nrow=length(male_list_60), byrow=TRUE),
                                  female = matrix(unlist(female_list_60), nrow=length(female_list_60), byrow=TRUE)) %>%
  map_df(rev)

Expectancy_Table_60 <- Expectancy_Table_60 %>%
  mutate(male = male + 60) %>%
  mutate(female = female + 60)

Expectancy_Table_longer_60 <- Expectancy_Table_60 %>% 
  pivot_longer(!year, names_to = "gender", values_to = "expectancy") %>%
  mutate_at("gender", factor)

forecastted_model_male_60 <- auto.arima(Expectancy_Table_60$male)
forecastted_model_female_60 <- auto.arima(Expectancy_Table_60$female)

futurevalues_male_60 <- forecast(forecastted_model_male_60, h=predict_timeframe, level = c(95))
futurevalues_female_60 <- forecast(forecastted_model_female_60, h=predict_timeframe, level = c(95))

futureExpectancyTable_60 <- data.frame(year = seq(currentYear + 1, length.out = predict_timeframe, by = 1),
                                       predicted_mean.male = futurevalues_male_60$mean,
                                       predicted_lower.male = futurevalues_male_60$lower,
                                       predicted_upper.male = futurevalues_male_60$upper,
                                       predicted_mean.female = futurevalues_female_60$mean,
                                       predicted_lower.female = futurevalues_female_60$lower,
                                       predicted_upper.female = futurevalues_female_60$upper
)

colnames(futureExpectancyTable_60) <- c("year", "expectancy_forecast.mean.male", "expectancy_forecast.lower.male", "expectancy_forecast.upper.male", "expectancy_forecast.mean.female", "expectancy_forecast.lower.female", "expectancy_forecast.upper.female")

futureExpectancyTableLonger_60 <- futureExpectancyTable_60 %>%
  pivot_longer(!year, names_to = c(".value", "gender"), names_sep = "_")

future_last_values_60 <- futureExpectancyTableLonger_60 %>%
  group_by(gender) %>%
  filter(gender == "forecast.mean.male" | gender == "forecast.mean.female") %>%
  mutate_at(vars(expectancy), funs(round(., 2))) %>%
  top_n(1, expectancy)

future_first_values_60 <- futureExpectancyTableLonger_60 %>%
  group_by(gender) %>%
  filter(gender == "forecast.mean.male" | gender == "forecast.mean.female") %>%
  mutate_at(vars(expectancy), funs(round(., 2))) %>%
  top_n(-1, expectancy)

future_values_need_60 <- rbind(future_first_values_60, future_last_values_60)

intermediate_table_60 <- Expectancy_Table_longer_60 %>%
  filter(year == currentYear) %>%
  mutate(gender = case_when(
    gender == "female" ~ "forecast.mean.female",
    gender == "male" ~ "forecast.mean.male"
  ))

present_and_future_60 <- rbind(Expectancy_Table_longer_60, intermediate_table_60, futureExpectancyTableLonger_60)

present_last_values_60 <- Expectancy_Table_longer_60 %>%
  group_by(gender) %>%
  top_n(1, expectancy)

present_and_future_values_60 <- rbind(present_last_values_60, future_last_values_60)

##End of 60 years old mortality graph



##Start of UI Forecasting Table

futureExpectancyTableDisplay <- futureExpectancyTable_30

colnames(futureExpectancyTableDisplay) <- c("year", "expectancy_forecast.mean.ma", "expectancy_forecast.lower.ma", "expectancy_forecast.upper.ma", "expectancy_forecast.mean.fe", "expectancy_forecast.lower.fe", "expectancy_forecast.upper.fe")

futureExpectancyTableDisplay <- futureExpectancyTableDisplay %>%
  pivot_longer(!year, names_pattern = "(.*)(..)$", names_to = c("limit", "gender")) %>% 
  mutate(limit=ifelse(limit=="", "value", limit))

futureExpectancyTableDisplay <- pivot_wider(futureExpectancyTableDisplay, id_cols = c(year, gender), names_from = limit, values_from = value, names_repair = "check_unique")

colnames(futureExpectancyTableDisplay) <- c("year", "gender", "Forecasted Expected Lifetime of Current 30 Year Olds", "Lower 95% CI", "Upper 95% CI")

futureExpectancyTableDisplay <- futureExpectancyTableDisplay %>%
  mutate(gender = case_when(
    gender == "fe" ~ "female",
    gender == "ma" ~ "male"
  ) 
  ) %>%
  mutate_at(vars(-c(year, gender)), funs(round(., 2)))


##End of UI Forecasting Table



##Start of web scraping newspaper Independent

independent_raw_page <- "https://www.independent.co.uk/topic/life-insurance"
independent_raw <- read_html(independent_raw_page)

title <- independent_raw %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//a[contains(@class, 'title')]") %>%
  rvest::html_text()

urls <- independent_raw %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//a[contains(@class, 'title')]") %>%
  rvest::html_attr("href")

independent_news <- data.frame(title, urls)

independent_news[] <- lapply(independent_news, as.character)

independent_news <- independent_news %>%
  mutate(urls = paste("https://www.independent.co.uk", urls, sep = ""))

## End of web scraping newspaper Independent



## Start of web scraping Insurnace Business Mag

insurance_business_mag_raw_page <- "https://www.insurancebusinessmag.com/uk/news/breaking-news/"
insurance_business_mag_raw <- read_html(insurance_business_mag_raw_page)

title_ib_main <- insurance_business_mag_raw %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//h2[@class = 'article_heading']//a") %>%
  rvest::html_text()

title_ib_sub <- insurance_business_mag_raw %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//h3[@class = 'article_heading']//a") %>%
  rvest::html_text()

urls_ib_main <- insurance_business_mag_raw %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//h2[@class = 'article_heading']//a") %>%
  rvest::html_attr("href")

urls_ib_sub <- insurance_business_mag_raw %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//h3[@class = 'article_heading']//a") %>%
  rvest::html_attr("href")

ibm_main <- data.frame(title_ib_main, urls_ib_main)
colnames(ibm_main) <- c("title", "urls")

ibm_sub <- data.frame(title_ib_sub, urls_ib_sub)
colnames(ibm_sub) <- c("title", "urls")

insurance_business_mag <- rbind(ibm_main, ibm_sub)

insurance_business_mag[] <- lapply(insurance_business_mag, as.character)

insurance_business_mag <-insurance_business_mag %>%
  mutate(urls = paste("https://www.insurancebusinessmag.com", urls, sep = ""))

## End of web scraping Insurance Business Mag



##start of Cancer Mortality for Age

cancer_mortality_age <- "https://www.cancerresearchuk.org/sites/default/files/cancer-stats/deaths_crude_mf_allcancer_m18/deaths_crude_mf_allcancer_m18.xlsx"

path_tmp_cancer <- tempfile()
download.file(cancer_mortality_age, destfile=path_tmp_cancer, mode = "wb")

cancer_mortality_age_graph_year <- read_xlsx(path_tmp_cancer,sheet = 1, n_max = 2)

cancer_mortality_age_graph_year <- strtoi(stri_sub(colnames(cancer_mortality_age_graph_year), -4, -1))

cancer_mortality_age_df <- read_xlsx(path_tmp_cancer,sheet = 1, skip = 4, n_max = 19)

colnames(cancer_mortality_age_df) <- c("age_range", "deaths_female", "deaths_male", "rates_female", "rates_male")

cancer_mortality_age_df <- cancer_mortality_age_df %>%
  mutate(rates_female = cancer_mortality_age_df$rates_female / 100000) %>%
  mutate(rates_male = cancer_mortality_age_df$rates_male / 100000)

cancer_mortality_age_df_longer <- cancer_mortality_age_df %>%
  pivot_longer(!age_range, names_to = c(".value", "gender"), names_sep = "_") %>%
  mutate_at("gender", factor)

## end of cancer mrotality for age



## start of coronavirus map data

region_coronavirus_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesByPublishDate&metric=newDeathsByDeathDate&format=csv"
region_coronavirus_raw <- fread(region_coronavirus_url)

new_cases_latest_date = region_coronavirus_raw[1]$date

new_cases_region <- region_coronavirus_raw %>%
  filter(date == new_cases_latest_date)

new_cases_region <- new_cases_region[, -6]

colnames(new_cases_region) <- c("areaCode", "Region", "areaType", "date", "count")

new_cases_region %>%
  mutate_at("Region", factor)

total_new_cases <- sum(new_cases_region$count)

new_deaths_latest_date = region_coronavirus_raw[2]$date

new_deaths_region <- region_coronavirus_raw %>%
  filter(date == new_deaths_latest_date)

new_deaths_region <- new_deaths_region[, -5]

colnames(new_deaths_region) <- c("areaCode", "Region", "areaType", "date", "count")

new_deaths_region %>%
  mutate_at("Region", factor)

total_new_deaths <- sum(new_deaths_region$count)

england_map <- rgdal::readOGR('/Users/ddwongramos/Desktop/Uni/Dissertation/Own Work/Boundaries.geojson')
##make sure this is your correct working directory

##map data source: "https://opendata.arcgis.com/datasets/8d3a9e6e7bd445e2bdcc26cdf007eac7_4.geojson"

england_map <- st_as_sf(england_map)
names(england_map)[3] <- "Region"
england_map <- england_map[-2]

england_cases_map <- england_map %>%
  left_join(new_cases_region, by = "Region")

england_deaths_map <- england_map %>%
  left_join(new_deaths_region, by = "Region")

coronavirus_cases_map <- ggplot(england_cases_map, aes(fill = count)) + 
  geom_sf(lwd = .2 ,color = 'black') + 
  scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  geom_text(aes(label = "North East", x = -1.95, y = 54.95), size = 4) +
  geom_text(aes(label = "North West", x = -2.99, y = 53.62), size = 4) +
  geom_text(aes(label = "Yorkshire and The Humber", x = -0.33, y = 54), size = 4) +
  geom_text(aes(label = "East Midlands", x = -0.79, y = 53.1), size = 4) +
  geom_text(aes(label = "West Midlands", x = -2.5, y = 52.5), size = 4) +
  geom_text(aes(label = "East of England", x = 0.69, y = 52.5), size = 4) +
  geom_text(aes(label = "London", x = -0.12, y = 51.5), size = 4) +
  geom_text(aes(label = "South East", x = -0.35, y = 51.1), size = 4) +
  geom_text(aes(label = "South West", x = -3.39, y = 51), size = 4) +
  theme_bw() +
  xlab("Latitude") + 
  ylab("Longitude")

coronavirus_deaths_map <- ggplot(england_deaths_map, aes(fill = count)) + 
  geom_sf(lwd = .2 ,color = 'black') + 
  scale_fill_gradient(low = "white", high = "blue", na.value = NA) +
  geom_text(aes(label = "North East", x = -1.95, y = 54.95), size = 4) +
  geom_text(aes(label = "North West", x = -2.99, y = 53.62), size = 4) +
  geom_text(aes(label = "Yorkshire and The Humber", x = -0.33, y = 54), size = 4) +
  geom_text(aes(label = "East Midlands", x = -0.79, y = 53.1), size = 4) +
  geom_text(aes(label = "West Midlands", x = -2.5, y = 52.5), size = 4) +
  geom_text(aes(label = "East of England", x = 0.69, y = 52.5), size = 4) +
  geom_text(aes(label = "London", x = -0.12, y = 51.5), size = 4) +
  geom_text(aes(label = "South East", x = -0.35, y = 51.1), size = 4) +
  geom_text(aes(label = "South West", x = -3.39, y = 51), size = 4) +
  theme_bw() +
  xlab("Latitude") + 
  ylab("Longitude")

## end of coronavirus map data



#start of total coronavirus data

coronavirus_url<- "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=cumCasesByPublishDate&metric=cumVaccinesGivenByPublishDate&metric=cumDeaths28DaysByPublishDate&format=csv"

coronavirus_raw <- fread(coronavirus_url)

coronavirus_raw <- head(coronavirus_raw, 28)

coronavirus_overall_cases <- coronavirus_raw$cumCasesByPublishDate[1]
coronavirus_overall_deaths <- coronavirus_raw$cumDeaths28DaysByPublishDate[1]
coronavirus_overall_vaccines <- coronavirus_raw$cumVaccinesGivenByPublishDate[2]

coronavirus_raw <- coronavirus_raw %>%
  map_df(rev)

UK_population_estimate <- 68172098

## end of total coronavirus data


## start of CVD data

URL_critical_illness_mortality <- "https://www.bhf.org.uk/-/media/files/research/heart-statistics/cvd-statistics-2021-chapter-1-mortality-final.xlsx?la=en&rev=eccc5ffabf764eb382286bf4655d08fc&hash=F904901FFE44E0AD9BA8F984601D7185AA8DCDDF"

URL_critical_illness_incidence <- "https://www.bhf.org.uk/-/media/files/research/heart-statistics/cvd-statistics-2021-chapter-2-morbidity-final.xlsx?la=en&rev=60e5e6189dcf4c5bb8d34021233200c7&hash=8DC1EE6B0E721C6F1D3EAE55EDAED8271A943DFA"

path_tmp_ci_mortality <- tempfile()
path_tmp_ci_incidence <- tempfile()

download.file(URL_critical_illness_mortality, destfile=path_tmp_ci_mortality, mode = "wb")
download.file(URL_critical_illness_incidence, destfile=path_tmp_ci_incidence, mode = "wb")

mortality_heart_raw <- readWorksheetFromFile(path_tmp_ci_mortality, sheet = 4, startRow = 4, endRow = 43)
mortality_heart_raw <- mortality_heart_raw[c(1,3,4)]

incidence_ci_male_raw <- readWorksheetFromFile(path_tmp_ci_incidence, sheet = 5, startRow = 37, endRow = 43, startCol = 2, header = FALSE)
incidence_ci_female_raw <- readWorksheetFromFile(path_tmp_ci_incidence, sheet = 6, startRow = 37, endRow = 43, startCol = 2, header = FALSE)

mortality_heart <- tail(mortality_heart_raw, 9)

cvd_year <- mortality_heart[9,]$Year

colnames(mortality_heart) <- c("year", "male", "female")

mortality_cvd_longer <- mortality_heart %>%
  pivot_longer(!year, names_to = "gender", values_to = "mortality") %>%
  mutate_at("gender", factor) %>% 
  mutate_at(vars(mortality), funs(round(., 2)))

incidence_ci_male <- tail(as.data.frame(t(incidence_ci_male_raw[1,])), 9)

incidence_ci_female <- tail(as.data.frame(t(incidence_ci_female_raw[1,])), 9)

incidence_ci_male <- incidence_ci_male %>%
  mutate(year = mortality_heart$year) %>%
  mutate(female = incidence_ci_female$`1`)

heart_and_circulatory_disease <- incidence_ci_male[c(2,1,3)]

colnames(heart_and_circulatory_disease) <- c("year", "male", "female")

heart_and_circulatory_disease$male <- as.numeric(as.character(heart_and_circulatory_disease$male))
heart_and_circulatory_disease$female <- as.numeric(as.character(heart_and_circulatory_disease$female))

incidence_cvd_longer <- heart_and_circulatory_disease %>%
  pivot_longer(!year, names_to = "gender", values_to = "incidence") %>%
  mutate_at("gender", factor)

heart_attack_male <- incidence_ci_male_raw[3,17]
stroke_male <- incidence_ci_male_raw[6,17]

heart_attack_female <- incidence_ci_female_raw[3,17]
stroke_female <- incidence_ci_female_raw[6,17]

##end of CVD data



##current time

current_time <- Sys.time()

## end current time



ui <- navbarPage("Actuarial Dashboard", id = "actuarial_dashboard", theme = shinytheme("spacelab"),
                 
                 tabPanel("Home",
                          
                          sidebarLayout(
                            
                            sidebarPanel(width = 3,
                                         h4("Recent Newspaper Articles"),
                                         
                                         tabsetPanel(selected = "Insurance Business Mag",
                                                     tabPanel("Independent",
                                                              
                                                              br(),
                                                              tags$a(href=independent_news[1,]$urls, independent_news[1,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=independent_news[2,]$urls, independent_news[2,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=independent_news[3,]$urls, independent_news[3,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=independent_news[4,]$urls, independent_news[4,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=independent_news[5,]$urls, independent_news[5,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=independent_news[6,]$urls, independent_news[6,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=independent_news[7,]$urls, independent_news[7,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=independent_news[8,]$urls, independent_news[8,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=independent_news[9,]$urls, independent_news[9,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=independent_news[10,]$urls, independent_news[10,]$title),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br()
                                                              
                                                     ),
                                                     tabPanel("Insurance Business Mag",
                                                              
                                                              br(),
                                                              tags$a(href=insurance_business_mag[1,]$urls, insurance_business_mag[1,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=insurance_business_mag[2,]$urls, insurance_business_mag[2,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=insurance_business_mag[3,]$urls, insurance_business_mag[3,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=insurance_business_mag[4,]$urls, insurance_business_mag[4,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=insurance_business_mag[5,]$urls, insurance_business_mag[5,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=insurance_business_mag[6,]$urls, insurance_business_mag[6,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=insurance_business_mag[7,]$urls, insurance_business_mag[7,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=insurance_business_mag[8,]$urls, insurance_business_mag[8,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=insurance_business_mag[9,]$urls, insurance_business_mag[9,]$title),
                                                              br(),
                                                              br(),
                                                              tags$a(href=insurance_business_mag[10,]$urls, insurance_business_mag[10,]$title),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br()
                                                     ),
                                                     p("Dashboard Last Updated: "),
                                                     p(current_time),
                                                     br(),
                                                     br(),
                                                     p("Code Last Updated: "),
                                                     p("2021-05-04 06:23:23")
                                         )
                            ),
                            
                            mainPanel(width = 9,
                                      
                                      fluidRow(
                                        column(6, align = "center",
                                               actionLink("mortality_link", label = "Mortality", style='font-size:150%'),
                                               plotOutput("Present_and_future_graph_main")),
                                        column(6, align = "center",
                                               actionLink("cancer_link", label = "Cancer", style='font-size:150%'),
                                               plotOutput("cancer_mortality_age_rate_graph_main")
                                        )
                                      ),
                                      
                                      fluidRow(
                                        br(),
                                        br(),
                                      ),
                                      
                                      fluidRow(
                                        
                                        column(7, align = "center",
                                               actionLink("coronavirus_link", label = "COVID-19", style='font-size:150%'),
                                               h3(paste0("As of ", new_cases_latest_date, sep = "")),
                                               br(),
                                               br(),
                                               strong("Total Cases: "),
                                               (format(coronavirus_overall_cases,big.mark=",", scientific=FALSE)),
                                               br(),
                                               br(),
                                               strong("Total Deaths: "),
                                               (format(coronavirus_overall_deaths, big.mark="," ,scientific=FALSE)),
                                               br(),
                                               br(),
                                               strong("Total Vaccinations: "),
                                               (format(coronavirus_overall_vaccines, big.mark="," ,scientific=FALSE)),
                                               
                                        ),
                                        
                                        column(5,
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(width = 12,
                                                              
                                                              h4("Whole Life Insurance Male Calculator (Expected Present Value)*"),
                                                              em("*Rough Estimate Only"),
                                                              br(),
                                                              actionLink("calculator_link", label = "For Other Calculators", style='font-size:80%'),
                                                              br(),
                                                              br(),
                                                              sliderInput("wl_insurance_age", "Age: ", min = 0, 100, value = 60),
                                                              numericInput("wl_insurance_amount", "Amount: ", value = 0),
                                                              numericInput("wl_insurance_ir", "Interest Rate: ", value = 0.04),
                                                              br(),
                                                              fluidRow(
                                                                column(5,
                                                                       p("Rough Estimate Value: ")
                                                                ),
                                                                column(5,
                                                                       textOutput("wl_insurance_main")
                                                                )
                                                              )
                                                 ),
                                                 mainPanel(width = 0)
                                               )
                                        )
                                      )  
                            )
                          )
                 ),
                 tabPanel("Mortality",
                          
                          tabsetPanel(selected = "30 Years Old",
                                      
                                      tabPanel("0 Years Old",
                                               
                                               tabsetPanel(selected = "Present + ARIMA Forecast",
                                                           
                                                           tabPanel("Present", 
                                                                    
                                                                    fluidRow(
                                                                      column(9,
                                                                             plotOutput("LE_graph")   
                                                                      ),
                                                                      column(2, offset = 1, style = "margin-top: 35px;", 
                                                                             checkboxGroupInput("gender", "Gender", choices = c("male", "female"), selected = c("male", "female"))   
                                                                      )
                                                                    )
                                                           ),
                                                           tabPanel("ARIMA Forecast",
                                                                    
                                                                    fluidRow(
                                                                      column(9,
                                                                             plotOutput("FutureLE_graph")   
                                                                      ),
                                                                      column(3, style = "margin-top: 15px;", 
                                                                             
                                                                             h3("Models Used:"),
                                                                             br(),
                                                                             strong("Forecast Interval: "),
                                                                             (currentYear + 1),
                                                                             (" - "),
                                                                             (currentYear + 20),
                                                                             br(),
                                                                             br(),
                                                                             strong("Male Forecast Model: "),
                                                                             span(futurevalues_male$method, style = "color:blue"),
                                                                             br(),
                                                                             br(),
                                                                             strong("Female Forecast Model: "),
                                                                             span(futurevalues_female$method, style = "color:red"),
                                                                             br(),
                                                                             br(),
                                                                             br(),
                                                                             p("Black lines represent 95% Confidence Interval"))
                                                                      
                                                                    )
                                                           ),
                                                           tabPanel("Present + ARIMA Forecast",
                                                                    
                                                                    fluidRow(
                                                                      column(9,
                                                                             plotOutput("Present_and_future_graph")   
                                                                      ),
                                                                      column(3, style = "margin-top: 15px;", 
                                                                             
                                                                             h3("Models Used:"),
                                                                             br(),
                                                                             strong("Forecast Interval: "),
                                                                             (currentYear + 1),
                                                                             (" - "),
                                                                             (currentYear + 20),
                                                                             br(),
                                                                             br(),
                                                                             strong("Male Forecast Model: "),
                                                                             span(futurevalues_male$method, style = "color:blue"),
                                                                             br(),
                                                                             br(),
                                                                             strong("Female Forecast Model: "),
                                                                             span(futurevalues_female$method, style = "color:red"),
                                                                             br(),
                                                                             br(),
                                                                             br(),
                                                                             p("Black lines represent 95% Confidence Interval"))
                                                                      
                                                                    )
                                                                    
                                                           )
                                               )
                                      ),
                                      
                                      tabPanel("30 Years Old",
                                               
                                               tabsetPanel(selected = "Present + ARIMA Forecast",
                                                           
                                                           tabPanel("Present", 
                                                                    
                                                                    fluidRow(
                                                                      column(9,
                                                                             plotOutput("LE_graph_30")   
                                                                      ),
                                                                      column(2, offset = 1, style = "margin-top: 35px;", 
                                                                             checkboxGroupInput("gender_30", "Gender", choices = c("male", "female"), selected = c("male", "female"))   
                                                                      )
                                                                    )
                                                           ),
                                                           tabPanel("ARIMA Forecast",
                                                                    
                                                                    fluidRow(
                                                                      column(9,
                                                                             plotOutput("FutureLE_graph_30")   
                                                                      ),
                                                                      column(3, style = "margin-top: 15px;", 
                                                                             
                                                                             h3("Models Used:"),
                                                                             br(),
                                                                             strong("Forecast Interval: "),
                                                                             (currentYear + 1),
                                                                             (" - "),
                                                                             (currentYear + 20),
                                                                             br(),
                                                                             br(),
                                                                             strong("Male Forecast Model: "),
                                                                             span(futurevalues_male_30$method, style = "color:blue"),
                                                                             br(),
                                                                             br(),
                                                                             strong("Female Forecast Model: "),
                                                                             span(futurevalues_female_30$method, style = "color:red"),
                                                                             br(),
                                                                             br(),
                                                                             br(),
                                                                             p("Black lines represent 95% Confidence Interval"))
                                                                      
                                                                    )
                                                                    
                                                                    
                                                           ),
                                                           tabPanel("Present + ARIMA Forecast",
                                                                    
                                                                    fluidRow(
                                                                      column(9,
                                                                             plotOutput("Present_and_future_graph_30")   
                                                                      ),
                                                                      column(3, style = "margin-top: 15px;", 
                                                                             
                                                                             h3("Models Used:"),
                                                                             br(),
                                                                             strong("Forecast Interval: "),
                                                                             (currentYear + 1),
                                                                             (" - "),
                                                                             (currentYear + 20),
                                                                             br(),
                                                                             br(),
                                                                             strong("Male Forecast Model: "),
                                                                             span(futurevalues_male_30$method, style = "color:blue"),
                                                                             br(),
                                                                             br(),
                                                                             strong("Female Forecast Model: "),
                                                                             span(futurevalues_female_30$method, style = "color:red"),
                                                                             br(),
                                                                             br(),
                                                                             br(),
                                                                             p("Black lines represent 95% Confidence Interval"))
                                                                      
                                                                    )
                                                                    
                                                           )
                                               )
                                               
                                      ),
                                      
                                      tabPanel("60 Years Old",
                                               
                                               tabsetPanel(selected = "Present + ARIMA Forecast",
                                                           
                                                           tabPanel("Present", 
                                                                    
                                                                    fluidRow(
                                                                      column(9,
                                                                             plotOutput("LE_graph_60")   
                                                                      ),
                                                                      column(2, offset = 1, style = "margin-top: 35px;", 
                                                                             checkboxGroupInput("gender_60", "Gender", choices = c("male", "female"), selected = c("male", "female"))   
                                                                      )
                                                                    )
                                                           ),
                                                           tabPanel("ARIMA Forecast",
                                                                    
                                                                    fluidRow(
                                                                      column(9,
                                                                             plotOutput("FutureLE_graph_60")   
                                                                      ),
                                                                      column(3, style = "margin-top: 15px;", 
                                                                             
                                                                             h3("Models Used:"),
                                                                             br(),
                                                                             strong("Forecast Interval: "),
                                                                             (currentYear + 1),
                                                                             (" - "),
                                                                             (currentYear + 20),
                                                                             br(),
                                                                             br(),
                                                                             strong("Male Forecast Model: "),
                                                                             span(futurevalues_male_60$method, style = "color:blue"),
                                                                             br(),
                                                                             br(),
                                                                             strong("Female Forecast Model: "),
                                                                             span(futurevalues_female_60$method, style = "color:red"),
                                                                             br(),
                                                                             br(),
                                                                             br(),
                                                                             p("Black lines represent 95% Confidence Interval"))
                                                                      
                                                                    )
                                                                    
                                                           ),
                                                           tabPanel("Present + ARIMA Forecast",
                                                                    
                                                                    fluidRow(
                                                                      column(9,
                                                                             plotOutput("Present_and_future_graph_60")   
                                                                      ),
                                                                      column(3, style = "margin-top: 15px;", 
                                                                             
                                                                             h3("Models Used:"),
                                                                             br(),
                                                                             strong("Forecast Interval: "),
                                                                             (currentYear + 1),
                                                                             (" - "),
                                                                             (currentYear + 20),
                                                                             br(),
                                                                             br(),
                                                                             strong("Male Forecast Model: "),
                                                                             span(futurevalues_male_60$method, style = "color:blue"),
                                                                             br(),
                                                                             br(),
                                                                             strong("Female Forecast Model: "),
                                                                             span(futurevalues_female_60$method, style = "color:red"),
                                                                             br(),
                                                                             br(),
                                                                             br(),
                                                                             p("Black lines represent 95% Confidence Interval"))
                                                                      
                                                                      
                                                                    )
                                                                    
                                                           )
                                               )
                                               
                                      )
                          ),
                          
                          br(),
                          br(),
                          
                          tabsetPanel(
                            tabPanel("Current Year Mortality Probabilities",
                                     
                                     fluidRow(
                                       column(4, offset = 1, style = "margin-top: 25px;", 
                                              sliderInput("age", "Age: ", min = 0, max = 100, value = 0),
                                              br(),
                                              strong("Current Year: "),
                                              (currentYear),
                                              (" (Latest Data Available)"),
                                              br(),
                                              br(),
                                              strong("qx: "),
                                              ("the mortality rate between age x and x+1"),
                                              br(),
                                              br(),
                                              strong("ex: "),
                                              ("the average period expectation of life at exact age x"),
                                              br(),
                                              br(),
                                              br()
                                              
                                              
                                       ),
                                       column(6, style = "margin-top: 15px;", 
                                              DT::dataTableOutput("currentYearTable_graph")  
                                       )
                                     )
                            ),
                            tabPanel("ARIMA Forecasted Expected Life Span",
                                     
                                     fluidRow(
                                       column(4, offset = 1, style = "margin-top: 25px;", 
                                              sliderInput("forecast_year", "Year: ", min = currentYear + 1, currentYear + 21, value = currentYear + 1)
                                       ),
                                       column(6, style = "margin-top: 15px;", 
                                              DT::dataTableOutput("forecast_expected_values_graph")  
                                       )
                                     )
                                     
                            )
                            
                          )  
                          
                 ),
                 tabPanel("Critical Illness",
                          
                          tabsetPanel(
                            tabPanel("Cancer (Mortality by Age)", style = "margin-top: 35px;", 
                                     
                                     plotOutput("cancer_mortality_age_graph"),
                                     br(),
                                     br(),
                                     plotOutput("cancer_mortality_age_rate_graph")
                            ),
                            tabPanel("Heart and Circulatory Diseases (Mortality and Incidence)", 
                                     
                                     fluidRow(
                                       
                                       column(3,
                                              
                                              fluidRow(
                                                
                                                column(2,
                                                       
                                                ),
                                                column(9,
                                                       
                                                       h3("Current Year Statistics"),
                                                       strong("Current Year: "),
                                                       (cvd_year),
                                                       br(),
                                                       br(),
                                                       strong("Myocardial Infarction (Heart Attack) Incidence"),
                                                       br(),
                                                       span("Male: ", style = "color:blue"),
                                                       (format(round(heart_attack_male, digits = 0), big.mark=",", scientific=FALSE)),
                                                       br(),
                                                       span("Female: ", style = "color:red"),
                                                       (format(round(heart_attack_female, digits = 0), big.mark=",", scientific=FALSE)),
                                                       br(),
                                                       br(),
                                                       strong("Stroke Incidence"),
                                                       br(),
                                                       span("Male: ", style = "color:blue"),
                                                       (format(round(stroke_male, digits = 0), big.mark=",", scientific=FALSE)),
                                                       br(),
                                                       span("Female: ", style = "color:red"),
                                                       (format(round(stroke_female, digits = 0), big.mark=",", scientific=FALSE)),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       strong("Notes: "),
                                                       em("Mycardical infarction and stroke are types of heart and circulatory diseases")
                                                       
                                                )
                                              ) 
                                       ),
                                       
                                       column(9,
                                              
                                              fluidRow(
                                                column(12,
                                                       plotOutput("cvd_mortality_graph")
                                                       
                                                ),
                                                column(12,
                                                       plotOutput("cvd_incidence_graph")
                                                       
                                                )       
                                                
                                              ))
                                       
                                     )
                                     
                            ),
                            tabPanel("Other Cancer Datasets + Statistics",
                                     
                                     fluidRow(column(10, offset = 1,
                                                     
                                                     br(),      
                                                     h2("Links: "),
                                                     br(),
                                                     br(),
                                                     tags$a(href = "https://www.cancerresearchuk.org/sites/default/files/cancer-stats/mort_5_common_male_m18/mort_5_common_male_m18.xlsx", "Most Common Types of Cancer that Result in Death (Male)"),
                                                     br(),
                                                     tags$a(href = "https://www.cancerresearchuk.org/sites/default/files/cancer-stats/mort_5_common_female_m18/mort_5_common_female_m18.xlsx", "Most Common Types of Cancer that Result in Death (Female)"),
                                                     br(),
                                                     tags$a(href = "https://www.cancerresearchuk.org/sites/default/files/cancer-stats/inc_20_common_i17/inc_20_common_i17.xls", "Most Common Types of Cancer"),
                                                     br(),      
                                                     br(),
                                                     br()
                                                     
                                     ))      
                            )
                          )
                          
                 ),
                 tabPanel("COVID-19",
                          
                          fluidRow(
                            column(3,
                                   
                                   box(
                                     title = "Total New Positive Cases Today*", width = 12, background = "red",
                                     ("Date of Information: "),
                                     (paste0(new_cases_latest_date)),
                                     br(),
                                     br(),
                                     br(),
                                     h4(total_new_cases)
                                   ),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   box(
                                     title = "Total Deaths Today*", width = 12, background = "light-blue",
                                     ("Date of Information: "),
                                     (paste0(new_deaths_latest_date)),
                                     br(),
                                     br(),
                                     br(),
                                     h4(total_new_deaths)
                                   ),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   em("*The values only reflect England and not the whole of UK")
                                   
                            ),
                            column(9,
                                   
                                   tabsetPanel(
                                     tabPanel("By Region",
                                              fluidRow(
                                                column(9, style = "margin-top: 25px;",
                                                       plotOutput("coronavirus_map")
                                                       
                                                       
                                                       
                                                ),
                                                column(3, style = "margin-top: 35px;",
                                                       radioButtons("region_choice", "Option: ", choices = c("New Positive Cases", "Deaths"))
                                                )
                                                
                                              )
                                              
                                     ),
                                     tabPanel("Total",
                                              fluidRow(
                                                column(9, style = "margin-top: 25px;",
                                                       plotOutput("coronavirus_cases_graph"),
                                                       plotOutput("coronavirus_vaccines_graph")
                                                ),
                                                column(3, style = "margin-top: 35px;",
                                                       strong("UK Population Estimate: "),
                                                       (format(UK_population_estimate,big.mark=",", scientific=FALSE)),
                                                       br(),
                                                       em("*Population value does not auto-update"),
                                                       br(),
                                                       br(),
                                                       strong(span("% of UK Population Tested Positive", style = "color:blue")),
                                                       br(),
                                                       br(),
                                                       strong(span("% of UK Population Vaccined", style = "color:green"))
                                                )
                                                
                                              )
                                     )
                                     
                                     
                                   )
                                   
                            )
                            
                          )
                          
                 ),
                 tabPanel("Calculator",
                          
                          
                          
                          sidebarLayout(
                            
                            sidebarPanel(width = 12,
                                         
                                         h2("Life Insurance Calculator (Expected Present Value)*"),
                                         em("*Rough Estimate Only"),
                                         br(),
                                         br(),
                                         radioButtons("insurance_type", "Type of Insurance: ", choices = c("Whole Life Insurance", "N-Year Endowment Insurance", "Pure Endowment Insurance") ),
                                         radioButtons("insurance_gender", "Gender: ", choices = c("Male", "Female")),
                                         sliderInput("insurance_age", "Age: ", min = 0, 100, value = 60),
                                         numericInput("insured_years", "Length of Contract: (Leave Blank if Whole Life Insurance)", value = 0),
                                         numericInput("insurance_amount", "Amount: ", value = 0),
                                         numericInput("insurance_ir", "Interest Rate: ", value = 0.04),
                                         br(),
                                         fluidRow(
                                           column(5,
                                                  p("Rough Estimate Value: ")
                                           ),
                                           column(5,
                                                  textOutput("insurance_value")
                                           )
                                         )                                
                                         
                            ),
                            mainPanel(width = 0)
                          )
                          
                          
                 ),
                 tabPanel("Data Information",
                          
                          fluidRow(column(10, offset = 1,
                                          
                                          h3("Newspaper: "),
                                          tags$a(href = independent_raw_page, "The Independent - UK"), 
                                          br(),
                                          tags$a(href = insurance_business_mag_raw_page, "Insurance Business - UK"), 
                                          br(),
                                          br(),
                                          
                                          h3("Mortality: "),
                                          strong("Data Source: "),
                                          tags$a(href = url_mortality_website_page, "Office for National Statistics - UK"), 
                                          br(),
                                          strong("Last Updated: "),
                                          (release_date_mortality),
                                          br(),
                                          strong("Latest Data Available: "),
                                          (currentYear),
                                          br(),
                                          strong("Next Release: "),
                                          (next_release_date_mortality),
                                          br(),
                                          br(),
                                          
                                          h3("Critical Illness: "),
                                          strong("Data Source: "),
                                          tags$a(href = "https://www.cancerresearchuk.org/", "Cancer Research UK"),
                                          (", "),
                                          tags$a(href = "https://www.bhf.org.uk/what-we-do/our-research/heart-statistics/heart-statistics-publications/cardiovascular-disease-statistics-2021", "British Heart Foundation"),
                                          br(),
                                          strong("Last Updated: "),
                                          ("Multiple Dates"),
                                          br(),
                                          br(),
                                          
                                          h3("COVID-19: "),
                                          strong("Data Source: "),
                                          tags$a(href = "https://coronavirus.data.gov.uk/", "Coronavirus (COVID-19) UK"),
                                          br(),
                                          strong("Last Updated: "),
                                          (new_cases_latest_date),
                                          br(),
                                          strong("Next Release: "),
                                          ("Daily Updates"),
                                          br(),
                                          strong("Population Source: "),
                                          tags$a(href = "https://www.worldometers.info/world-population/uk-population/", "World O Meter - UK Population"),
                                          br(),
                                          br(),
                                          
                                          h3("Calculator: "),
                                          strong("Data Source: "),
                                          tags$a(href = "https://www.actuaries.org.uk/learn-and-develop/continuous-mortality-investigation/cmi-mortality-and-morbidity-tables/92-series-tables", "Institute and Faculty of Actuaries AM92 AF92 Mortality Tables"),
                                          br(),
                                          strong("R-Package Used: "),
                                          ("lifecontingencies"),
                                          br(),
                                          em("*Probabilities for < 16 and > 92 have been derived from a Brass - Logit model fit on SoA life table"),
                                          br(),
                                          br(),
                                          br(),
                                          h3("Dashboard created by Shun Nok Daniel Wong"),
                                          strong("Email: "),
                                          ("sndw1@student.le.ac.uk")
                                          
                                          
                          ))
                          
                          
                          
                 )
                 
                 
)


server <- function(input, output, session) {
  
  
  reactive_LE <- reactive ({
    
    if(is.null(input$gender)){
      
      return(Expectancy_Table_longer)
    } 
    
    if(length(input$gender) == 1){
      
      return(Expectancy_Table_longer[Expectancy_Table_longer$gender %in% input$gender,])
      
    }else {
      
      return(Expectancy_Table_longer)
    } 
    
  })
  
  
  reactive_LE_30 <- reactive ({
    
    if(is.null(input$gender_30)){
      
      return(Expectancy_Table_longer_30)
    } 
    
    if(length(input$gender_30) == 1){
      
      return(Expectancy_Table_longer_30[Expectancy_Table_longer_30$gender %in% input$gender_30,])
      
    }else {
      
      return(Expectancy_Table_longer_30)
    } 
    
  })
  
  reactive_LE_60 <- reactive ({
    
    if(is.null(input$gender_60)){
      
      return(Expectancy_Table_longer_60)
    } 
    
    if(length(input$gender_60) == 1){
      
      return(Expectancy_Table_longer_60[Expectancy_Table_longer_60$gender %in% input$gender_60,])
      
    }else {
      
      return(Expectancy_Table_longer_60)
    } 
    
  })
  
  reactive_LE_end <- reactive ({
    
    if(is.null(input$gender)){
      
      last_values <- Expectancy_Table_longer %>%
        group_by(gender) %>%
        top_n(1, expectancy)
      
      return(last_values)
    } 
    
    male <- "male" %in% input$gender
    female <- "female" %in% input$gender
    
    if (male & female){
      
      last_values <- Expectancy_Table_longer %>%
        group_by(gender) %>%
        top_n(1, expectancy)
      
      return(last_values)
    } else if (male) {
      
      last_values <- Expectancy_Table_longer %>%
        filter(gender == "male") %>%
        top_n(1, expectancy)
      
      return(last_values)
      
    } else if (female) {
      
      last_values <- Expectancy_Table_longer %>%
        filter(gender == "female") %>%
        top_n(1, expectancy)
      
      return(last_values)
    }
    
  })
  
  
  reactive_LE_end_30 <- reactive ({
    
    if(is.null(input$gender_30)){
      
      last_values_30 <- Expectancy_Table_longer_30 %>%
        group_by(gender) %>%
        top_n(1, expectancy)
      
      return(last_values_30)
    } 
    
    male <- "male" %in% input$gender_30
    female <- "female" %in% input$gender_30
    
    if (male & female){
      
      last_values_30 <- Expectancy_Table_longer_30 %>%
        group_by(gender) %>%
        top_n(1, expectancy)
      
      return(last_values_30)
    } else if (male) {
      
      last_values_30 <- Expectancy_Table_longer_30 %>%
        filter(gender == "male") %>%
        top_n(1, expectancy)
      
      return(last_values_30)
      
    } else if (female) {
      
      last_values_30 <- Expectancy_Table_longer_30 %>%
        filter(gender == "female") %>%
        top_n(1, expectancy)
      
      return(last_values_30)
    }
    
  })
  
  
  reactive_LE_end_60 <- reactive ({
    
    if(is.null(input$gender_60)){
      
      last_values_60 <- Expectancy_Table_longer_60 %>%
        group_by(gender) %>%
        top_n(1, expectancy)
      
      return(last_values_60)
    } 
    
    male <- "male" %in% input$gender_60
    female <- "female" %in% input$gender_60
    
    if (male & female){
      
      last_values_60 <- Expectancy_Table_longer_60 %>%
        group_by(gender) %>%
        top_n(1, expectancy)
      
      return(last_values_60)
    } else if (male) {
      
      last_values_60 <- Expectancy_Table_longer_60 %>%
        filter(gender == "male") %>%
        top_n(1, expectancy)
      
      return(last_values_60)
      
    } else if (female) {
      
      last_values_60 <- Expectancy_Table_longer_60 %>%
        filter(gender == "female") %>%
        top_n(1, expectancy)
      
      return(last_values_60)
    }
    
  })
  
  
  reactive_age <- reactive ({
    
    reactiveTable <- currentYearTable %>%
      filter(age == input$age)
    
    return(reactiveTable)
  })
  
  
  reactive_year <- reactive ({
    
    reactiveTable_for_forecast <- futureExpectancyTableDisplay %>%
      filter(year == input$forecast_year)
    
    return(reactiveTable_for_forecast)
  })
  
  
  reactive_insurance_type <- reactive ({
    
    if (input$insurance_type == "Whole Life Insurance" && input$insurance_gender == "Male"){
      
      insurance_option = 1
      return(insurance_option)
      
    } else if (input$insurance_type == "Whole Life Insurance" && input$insurance_gender == "Female"){
      
      insurance_option = 2
      return(insurance_option)
      
    } else if (input$insurance_type == "N-Year Endowment Insurance" && input$insurance_gender == "Male"){
      
      insurance_option = 3
      return(insurance_option)
      
    } else if (input$insurance_type == "N-Year Endowment Insurance" && input$insurance_gender == "Female"){
      
      insurance_option = 4
      return(insurance_option)
      
    } else if (input$insurance_type == "Pure Endowment Insurance" && input$insurance_gender == "Male"){
      
      insurance_option = 5
      return(insurance_option)
      
    } else if (input$insurance_type == "Pure Endowment Insurance" && input$insurance_gender == "Female"){
      
      insurance_option = 6
      return(insurance_option)
      
    }
    
  })
  
  reactive_map <- reactive ({
    
    if (input$region_choice == "New Positive Cases"){
      
      return(coronavirus_cases_map)
    }else{
      
      return(coronavirus_deaths_map)
    }
    
  })
  
  
  forecast_linetype <- c(female = 'solid', male = 'solid', forecast.mean.male = "longdash", forecast.lower.male = "dotted", forecast.upper.male = "dotted", forecast.mean.female = "longdash", forecast.lower.female = "dotted", forecast.upper.female = "dotted")
  
  gender_colour <- c(female = 'red', male = 'blue', forecast.mean.male = "blue", forecast.lower.male = "black", forecast.upper.male = "black", forecast.mean.female = "red", forecast.lower.female = "black", forecast.upper.female = "black")
  
  output$LE_graph <- renderPlot({
    LE_graph <- ggplot(data=reactive_LE(), aes(x = year, y = expectancy, group = gender, colour = gender)) + 
      geom_line() +
      ggtitle("Average Expected Lifetime of 0 Year Olds Born in that Year") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Year") + 
      ylab("Expected Life Span") + 
      xlim(currentYear - 37, currentYear + 20) +
      ylim(65, 95) + 
      theme(legend.position = "none") + 
      scale_color_manual(values = gender_colour) +
      geom_text_repel(aes(label = expectancy), data = reactive_LE_end(), fontface ="plain", color = "black", size = 3)
    
    print(LE_graph)
  })
  
  
  output$LE_graph_30 <- renderPlot({
    LE_graph_30 <- ggplot(data=reactive_LE_30(), aes(x = year, y = expectancy, group = gender, colour = gender)) + 
      geom_line() +
      ggtitle("Average Expected Lifetime of 30 Year Olds in that Year") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Year") + 
      ylab("Expected Life Span") + 
      xlim(currentYear - 37, currentYear + 20) +
      ylim(65, 95) + 
      theme(legend.position = "none") + 
      scale_color_manual(values = gender_colour) +
      geom_text_repel(aes(label = expectancy), data = reactive_LE_end_30(), fontface ="plain", color = "black", size = 3)
    
    print(LE_graph_30)
  })
  
  
  output$LE_graph_60 <- renderPlot({
    LE_graph_60 <- ggplot(data=reactive_LE_60(), aes(x = year, y = expectancy, group = gender, colour = gender)) + 
      geom_line() +
      ggtitle("Average Expected Lifetime of 60 Year Olds in that Year") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Year") + 
      ylab("Expected Life Span") + 
      xlim(currentYear - 37, currentYear + 20) +
      ylim(65, 95) + 
      theme(legend.position = "none") + 
      scale_color_manual(values = gender_colour) +
      geom_text_repel(aes(label = expectancy), data = reactive_LE_end_60(), fontface ="plain", color = "black", size = 3)
    
    print(LE_graph_60)
  })
  
  
  output$currentYearTable_graph <- DT::renderDataTable(
    DT::datatable(reactive_age(), options = list(searching = FALSE, paging = FALSE))
  )
  
  output$FutureLE_graph <- renderPlot({
    FutureLE_graph <- ggplot(data=futureExpectancyTableLonger, aes(x = year, y = expectancy, group = gender, colour = gender)) + 
      geom_line() +
      ggtitle("Average Expected Lifetime of 0 Year Olds Born in that Year (ARIMA Forecast)") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Year") + 
      ylab("Expected Life Span") + 
      xlim(currentYear - 37, currentYear + 20) +
      ylim(65, 95) + 
      theme(legend.position = "none") + 
      scale_color_manual(values = gender_colour) +
      geom_text_repel(aes(label = expectancy), data = future_values_need, fontface ="plain", color = "black", size = 3)
    
    print(FutureLE_graph)
  })
  
  
  output$FutureLE_graph_30 <- renderPlot({
    FutureLE_graph_30 <- ggplot(data=futureExpectancyTableLonger_30, aes(x = year, y = expectancy, group = gender, colour = gender)) + 
      geom_line() +
      ggtitle("Average Expected Lifetime of 30 Year Olds in that Year (ARIMA Forecast)") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Year") + 
      ylab("Expected Life Span") + 
      xlim(currentYear - 37, currentYear + 20) +
      ylim(65, 95) + 
      theme(legend.position = "none") + 
      scale_color_manual(values = gender_colour) +
      geom_text_repel(aes(label = expectancy), data = future_values_need_30, fontface ="plain", color = "black", size = 3)
    
    print(FutureLE_graph_30)
  })
  
  
  output$FutureLE_graph_60 <- renderPlot({
    FutureLE_graph_60 <- ggplot(data=futureExpectancyTableLonger_60, aes(x = year, y = expectancy, group = gender, colour = gender)) + 
      geom_line() +
      ggtitle("Average Expected Lifetime of 60 Year Olds in that Year (ARIMA Forecast)") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Year") + 
      ylab("Expected Life Span") + 
      xlim(currentYear - 37, currentYear + 20) +
      ylim(65, 95) + 
      theme(legend.position = "none") + 
      scale_color_manual(values = gender_colour) +
      geom_text_repel(aes(label = expectancy), data = future_values_need_60, fontface ="plain", color = "black", size = 3)
    
    print(FutureLE_graph_60)
  })
  
  
  output$forecast_expected_values_graph <- DT::renderDataTable(
    DT::datatable(reactive_year(), options = list(searching = FALSE, paging = FALSE))
  )
  
  output$Present_and_future_graph <- renderPlot({
    Present_and_future_graph <- ggplot(data=present_and_future, aes(x = year, y = expectancy, group = gender, colour = gender)) + 
      geom_line(aes(linetype=gender)) +
      ggtitle("Average Expected Lifetime of 0 Year Olds Born in that Year (Present + ARIMA Forecast)") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Year") + 
      ylab("Expected Life Span") + 
      xlim(currentYear - 37, currentYear + 20) +
      ylim(65, 95) + 
      theme(legend.position = "none") + 
      scale_color_manual(values = gender_colour) +
      scale_linetype_manual(values = forecast_linetype) + 
      geom_text_repel(aes(label = expectancy), data = present_and_future_values, fontface ="plain", color = "black", size = 3)
    
    print(Present_and_future_graph)
  })
  
  
  output$Present_and_future_graph_30 <- renderPlot({
    Present_and_future_graph_30 <- ggplot(data=present_and_future_30, aes(x = year, y = expectancy, group = gender, colour = gender)) + 
      geom_line(aes(linetype=gender)) +
      ggtitle("Average Expected Lifetime of 30 Year Olds in that Year (Present + ARIMA Forecast)") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Year") + 
      ylab("Expected Life Span") + 
      xlim(currentYear - 37, currentYear + 20) +
      ylim(65, 95) + 
      theme(legend.position = "none") + 
      scale_color_manual(values = gender_colour) +
      scale_linetype_manual(values = forecast_linetype) + 
      geom_text_repel(aes(label = expectancy), data = present_and_future_values_30, fontface ="plain", color = "black", size = 3)
    
    print(Present_and_future_graph_30)
  })
  
  
  output$Present_and_future_graph_60 <- renderPlot({
    Present_and_future_graph_60 <- ggplot(data=present_and_future_60, aes(x = year, y = expectancy, group = gender, colour = gender)) + 
      geom_line(aes(linetype=gender)) +
      ggtitle("Average Expected Lifetime of 60 Year Olds in that Year (Present + ARIMA Forecast)") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Year") + 
      ylab("Expected Life Span") + 
      xlim(currentYear - 37, currentYear + 20) +
      ylim(65, 95) + 
      theme(legend.position = "none") + 
      scale_color_manual(values = gender_colour) +
      scale_linetype_manual(values = forecast_linetype) + 
      geom_text_repel(aes(label = expectancy), data = present_and_future_values_60, fontface ="plain", color = "black", size = 3)
    
    print(Present_and_future_graph_60)
  })
  
  
  output$Present_and_future_graph_main <- renderPlot({
    Present_and_future_graph_main <- ggplot(data=present_and_future_30, aes(x = year, y = expectancy, group = gender, colour = gender)) + 
      geom_line(aes(linetype=gender)) +
      ggtitle("Average Expected Lifetime of 30 Year Olds in that Year (Present + ARIMA Forecast)") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Year") + 
      ylab("Expected Life Span") + 
      xlim(currentYear - 37, currentYear + 20) +
      ylim(65, 95) + 
      theme(legend.position = "none") + 
      scale_color_manual(values = gender_colour) +
      scale_linetype_manual(values = forecast_linetype) + 
      geom_text_repel(aes(label = expectancy), data = present_and_future_values_30, fontface ="plain", color = "black", size = 3)
    
    print(Present_and_future_graph_main)
  })
  
  output$cancer_mortality_age_graph <- renderPlot({
    
    cancer_mortality_age_graph <- ggplot(data = cancer_mortality_age_df_longer, aes(x = age_range, y = deaths, fill=gender, label = deaths)) +
      geom_bar(stat="identity", position = position_dodge()) +
      xlab("Age Range") + 
      ylab("Average Number of Deaths per Year") + 
      ggtitle(paste("Average Number of Deaths per Year in Age Groups (", as.character(cancer_mortality_age_graph_year - 2), " - ", as.character(cancer_mortality_age_graph_year), ")", sep = "")) + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      geom_text_repel(aes(label = deaths))
    
    print(cancer_mortality_age_graph)
    
  })
  
  output$cancer_mortality_age_rate_graph <- renderPlot({
    
    cancer_mortality_age_rate_graph <- ggplot(data = cancer_mortality_age_df_longer, aes(x = age_range, y = rates, group = gender, color = gender)) +
      geom_line(aes(color = gender)) +
      xlab("Age Range") + 
      ylab("Mortality Rate per 100,000") + 
      ggtitle(paste("Average Mortality Rates per 100,000 Population UK (", as.character(cancer_mortality_age_graph_year - 2), " - ", as.character(cancer_mortality_age_graph_year), ")", sep = "")) + 
      theme(plot.title = element_text(hjust = 0.5))
    
    print(cancer_mortality_age_rate_graph)
    
  })
  
  
  output$cancer_mortality_age_rate_graph_main <- renderPlot({
    
    cancer_mortality_age_rate_graph_main <- ggplot(data = cancer_mortality_age_df_longer, aes(x = age_range, y = rates, group = gender, color = gender)) +
      geom_line(aes(color = gender)) +
      xlab("Age Range") + 
      ylab("Mortality Rate per 100,000") + 
      ggtitle(paste("Average Mortality Rates per 100,000 Population UK (", as.character(cancer_mortality_age_graph_year - 2), " - ", as.character(cancer_mortality_age_graph_year), ")", sep = "")) + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none")  + 
      scale_x_discrete(breaks = c("0 to 04", "25 to 29", "50 to 54", "75 to 79","90+"))
    
    print(cancer_mortality_age_rate_graph_main)
    
  })
  
  output$coronavirus_map <- renderPlot({
    
    reactive_map()}, width = 800, height = 800
    
  )
  
  output$coronavirus_cases_graph <- renderPlot({
    
    coronavirus_cases_graph <- ggplot(coronavirus_raw, aes(x=date)) + 
      geom_line(aes(y = cumCasesByPublishDate/UK_population_estimate), color = "navy") + 
      xlab("Date") + 
      ylab("% of Population")+ 
      ggtitle("% of Population Tested Positive in the past 28 days") +
      theme(plot.title = element_text(hjust = 0.5))
    
    print(coronavirus_cases_graph)
    
  })
  
  output$coronavirus_vaccines_graph <- renderPlot({
    
    coronavirus_vaccines_graph <- ggplot(coronavirus_raw, aes(x=date)) + 
      geom_line(aes(y = cumVaccinesGivenByPublishDate/UK_population_estimate), color = "green") +
      xlab("Date") + 
      ylab("% of Population")+ 
      ggtitle("% of Population Vaccined in the past 28 days") +
      theme(plot.title = element_text(hjust = 0.5))
    
    print(coronavirus_vaccines_graph)
    
  })
  
  
  output$cvd_incidence_graph <- renderPlot({
    
    cvd_incidence_graph <- ggplot(data = incidence_cvd_longer, aes(x = year, y = incidence, group = gender, color = gender)) +
      geom_line(aes(color = gender)) +
      xlab("Year") + 
      ylab("Total Incidence") + 
      ggtitle("Total Incidence from all Heart and Circulatory Diseases (CVD) for all ages in UK") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlim(cvd_year - 8, cvd_year + 2) +
      geom_text_repel(aes(label = incidence), fontface ="plain", color = "black", size = 3)
    
    print(cvd_incidence_graph)
    
  })
  
  output$cvd_mortality_graph <- renderPlot({
    
    cvd_mortality_graph <- ggplot(data = mortality_cvd_longer, aes(x = year, y = mortality, group = gender, color = gender)) +
      geom_line(aes(color = gender)) +
      xlab("Year") + 
      ylab("Mortality Rate per 100,000") + 
      ggtitle("Mortality Rate per 100,000 from all Heart and Circulatory Diseases (CVD) for all ages in UK") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlim(cvd_year - 8, cvd_year + 2) +
      geom_text_repel(aes(label = mortality), fontface ="plain", color = "black", size = 3)
    
    print(cvd_mortality_graph)
    
  })
  
  
  output$wl_insurance_main <- renderText({
    
    wl_insurance <- input$wl_insurance_amount * Axn(AM92Lt, x = input$wl_insurance_age, i = input$wl_insurance_ir)
    
    return(wl_insurance)
    
    
  })
  
  output$insurance_value <- renderText({
    
    if (reactive_insurance_type() == 1){
      
      insurance_value <- input$insurance_amount * Axn(AM92Lt, x = input$insurance_age, i = input$insurance_ir)
      
      return(insurance_value)
      
    }else if (reactive_insurance_type() == 2){
      
      insurance_value <- input$insurance_amount * Axn(AF92Lt, x = input$insurance_age, i = input$insurance_ir)
      
      return(insurance_value)
      
    }else if (reactive_insurance_type() == 3){
      
      insurance_value <- input$insurance_amount * AExn(AM92Lt, x = input$insurance_age, n = input$insured_years, i = input$insurance_ir)
      
      return(insurance_value)
      
    }else if (reactive_insurance_type() == 4){
      
      insurance_value <- input$insurance_amount * AExn(AF92Lt, x = input$insurance_age, n = input$insured_years, i = input$insurance_ir)
      
      return(insurance_value)
      
    }else if (reactive_insurance_type() == 5) {
      
      insurance_value <- input$insurance_amount * Exn(AM92Lt, x = input$insurance_age, n = input$insured_years, i = input$insurance_ir)
      
      return(insurance_value)
      
    }else {
      
      insurance_value <- input$insurance_amount * Exn(AF92Lt, x = input$insurance_age, n = input$insured_years, i = input$insurance_ir)
      
      return(insurance_value)
      
    }
    
    
  })
  
  min_ticks <- 1
  
  observe({
    
    if(length(input$gender) < min_ticks){
      updateCheckboxGroupInput(session, "gender", selected = "male")
    }
    
    
  })
  
  observe({
    
    if(length(input$gender_30) < min_ticks){
      updateCheckboxGroupInput(session, "gender_30", selected = "male")
    }
    
    
  })
  
  
  observe({
    
    if(length(input$gender_60) < min_ticks){
      updateCheckboxGroupInput(session, "gender_60", selected = "male")
    }
    
    
  })
  
  
  observeEvent(input$mortality_link, {
    updateNavbarPage(session = session, inputId = "actuarial_dashboard", selected = "Mortality")
  })
  
  
  observeEvent(input$cancer_link, {
    updateNavbarPage(session = session, inputId = "actuarial_dashboard", selected = "Critical Illness")
  })
  
  
  observeEvent(input$coronavirus_link, {
    updateNavbarPage(session = session, inputId = "actuarial_dashboard", selected = "COVID-19")
  })
  
  
  observeEvent(input$calculator_link, {
    updateNavbarPage(session = session, inputId = "actuarial_dashboard", selected = "Calculator")
  })
  
  
  
}

shinyApp(ui, server)