library(rvest)
library(xml2)
library(stringr)
library(reshape)
setwd("C:/Users/quoej/OneDrive/Desktop/MS BAnDS OSU/MSIS 5193 Programming/Data Files")
new_data = read.csv("owid-covid-data.csv")
new_data2 = read.csv("annual-number-of-births-by-world-region.csv")
new_data3 = read.csv("cleaned family planning data for married women.csv")
new_data4 = read.csv("children-per-woman-UN.csv")
names(new_data)
names(new_data2)
names(new_data3)
names(new_data4)

#new_data[is.na(new_data)] = 0
new_data$year = format(as.Date(new_data$date, format = "%Y-%m-%d"), "%Y")

all = cbind(new_data$new_cases, new_data$new_deaths, new_data$new_cases_per_million, new_data$new_deaths_per_million, 
            new_data$hosp_patients_per_million, new_data$hospital_beds_per_thousand, new_data$icu_patients_per_million)
all2 = cbind(new_data$population,new_data$gdp_per_capita)
new_datai = aggregate(all ~ year + iso_code + location, data = new_data, FUN = sum, na.rm = TRUE)
names(new_datai) = c("Year", "Iso_Code", "Location", "Cases", "Deaths", "Cases_per_Million", "Deaths_per_Million",
                    "Hosp_Patients_per_Million", "Hospital_Beds_per_Thousand", "Icu_Patients_per_Million")
new_dataii = aggregate(all2 ~ year + location, data = new_data, FUN = mean, na.rm = TRUE)
names(new_dataii) = c("Year", "Location","Population", "GDP")
baby_covid = merge(new_datai, new_dataii, by = c("Location", "Year"))
baby_fertility = new_data2[(new_data2$Year == 2019) | (new_data2$Year == 2020), ]
names(baby_fertility)[1] = "Country"
names(baby_fertility)[4] = "Annual Births per Country"
names(new_data3)[1] = "Country"
names(new_data3)[2] = "Family Planning"
names(new_data4)[1] = "Country"
names(new_data4)[4] = "Live Births per Woman per Country"

link = "https://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2020"
scrap = read_html(link)
country_selector = '#t2 > tbody > tr > td.cityOrCountryInIndicesTable.sorting_1'
find_code = html_nodes(scrap, country_selector)
CostOfLiving = html_table(scrap, fill = TRUE)
COL = data.frame(CostOfLiving[[2]])

link1 = "https://www.numbeo.com/health-care/rankings_by_country.jsp?title=2020"
scrap1 = read_html(link1)
country_selector1 = '#t2 > tbody > tr > td.cityOrCountryInIndicesTable.sorting_1'
find_code1 = html_nodes(scrap1, country_selector)
HealthCareIndex = html_table(scrap1, fill = TRUE)
HCI = data.frame(HealthCareIndex[[2]])

link2 = "https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020"
scrap2 = read_html(link2)
country_selector2 = '#t2 > tbody > tr > td.cityOrCountryInIndicesTable.sorting_1'
find_code2 = html_nodes(scrap2, country_selector)
QualityOfLife = html_table(scrap2, fill = TRUE)
QOF = data.frame(QualityOfLife[[2]])
names(COL)
names(HCI)
names(QOF)

cf = merge(x=COL, y=HCI, by="Country", all.x=TRUE)
cf = data.frame(cf)
df = merge(x=cf, y=QOF, by="Country", all.x=TRUE)
names(df)
new = merge(df, baby_fertility[(baby_fertility$Year == 2020), ], by="Country", all.x=TRUE)
new = merge(new, new_data3, by="Country", all.x=TRUE)
new = merge(new, new_data4[(new_data4$Year == 2020), ], by="Country", all.x=TRUE)

baby_covid[, c(1,2,3,11,12,4,5,6,7,8,9,10)]
new[, c(1,23,22,4,10,11,13,25,28,24)]

write.csv(baby_covid[, c(1,2,3,11,12,4,5,6,7,8,9,10)], "Baby_Covid.csv")
write.csv(baby_fertility, "Baby_Fertility.csv")
write.csv(new[, c(1,23,22,3,4,10,11,13,25,28,24)], "Baby.csv")
