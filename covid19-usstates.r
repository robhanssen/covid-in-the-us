#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#
library(tidyverse)
library(lubridate)
library(zoo)

# constant infinite
source("config.inc")
locations = read_csv("sources/USpopulationregion.csv")

#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
covid <- read_csv(covidfile) %>% select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Lat, -Long_)

# process the time series into proper dataframe
covid <- covid %>% 
            pivot_longer(!c("Admin2", "Province_State","Country_Region"), 
                           names_to = "date",
                           values_to = "infections") %>%
            rename(county = Admin2, state = Province_State, country = Country_Region) %>%
            mutate(date = as.Date(date, format="%m/%d/%y"), 
                   time = date - min(date) + 1
                     ) %>%
            left_join(locations) %>%
            mutate(location=ifelse(is.na(county), "Other", state))

lastupdated = max(covid$date)

# total spread of infections by states
spread <- covid %>% group_by(date,time, location) %>% summarise(count=sum(infections))

max_x = ceiling(max(spread$time)/10)*10
capt = paste0(source, "\nlast updated:", format(lastupdated, format="%b %d, %Y"))

# spread %>% mutate(date=as.Date("2020-01-22")-1+time) -> spread

locationlist = unique(spread$location)

# for (st in locationlist) 
# { 
#    spread %>%  filter(location==st) %>%
#                ggplot + aes(date, count) + geom_point() + geom_line() +
#                        scale_y_log10(limit=c(1e1,1e7)) + labs(caption=capt) + 
#                         xlab("Date") + ylab("Number of cases per state") + ggtitle("Spread of COVID19 in the US by state") #+ 
#    fname = paste0("graphs/",st,".pdf")
#    ggsave(fname, device="pdf")
# }

# ggsave("graphs/covid-us-spread.pdf", device="pdf")
# write_csv(spread, "data/covid-us-spread.csv")

#
# daily growth of infections
#
widespread <- spread %>% pivot_wider(names_from=location, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)

covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "location",
   values_to = "growth") #%>% filter(location!="Other")

covid_growth %>% mutate(date=as.Date("2020-01-22")+time) -> covid_growth



# for (st in locationlist) {
# covid_growth %>% filter(location==st) %>%
#                ggplot + aes(date, growth) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
#                         labs(caption=capt) + 
#                         xlab("Date") + ylab("Growth of Infections") + ggtitle(paste("Per diem growth of COVID-19 infections in ", st))
#     fname = paste0("graphs/",st,".pdf")
#     ggsave(fname, device="pdf")

# }

# ggsave("graphs/covid-us-spreadgrowth.pdf", device="pdf")
# write_csv(covid_growth, "data/covid-us-spreadgrowth.csv")
# save variable for later use
infectiongrowth <- covid_growth

#
# US deaths analysis
#
#
#

#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
covid <- read_csv(covidfile) %>% select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Lat, -Long_, -Population)

# process the time series into proper dataframe
covid <- covid %>% pivot_longer(!c("Admin2", "Province_State","Country_Region"), 
   names_to = "date",
   values_to = "deaths") %>%
               rename(county = Admin2, state = Province_State, country = Country_Region) %>%
            mutate(date = as.Date(date, format="%m/%d/%y"), 
                   time = date - min(date) + 1
                     ) %>%
            left_join(locations) %>%
            mutate(location=ifelse(is.na(county), "Other", state)) #%>% View()


# clean up column names and differentiate between different regions
# colnames(covid) = c("county","state","country","date","deaths")
# covid$date = as.Date(covid$date, format="%m/%d/%y")
# lastupdated = max(covid$date)
# covid$time = covid$date - min(covid$date) + 1

# selection of states to highlight out of the full data set
# covid <- covid %>% left_join(locations) 
# covid$location[is.na(covid$location)] = "Other"

# total spread of infections by states
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(deaths))
# View(spread)
max_x = ceiling(max(spread$time)/10)*10

# spread %>% ggplot + aes(time, count, color=location) + geom_point() + geom_line() +
#                         scale_x_continuous(limit=c(40, max_x)) + scale_y_log10(limit=c(1e1,1e6)) + labs(caption=capt) + 
#                         xlab("Days since Jan 22, 2020") + ylab("Number of casualties per state") + ggtitle("COVID19 casualties in the US by state")

# ggsave("graphs/covid-us-deaths.pdf", device="pdf")
# write_csv(spread, "data/covid-us-deaths.csv")

#
# daily increase of deaths in the US
#
#

widespread <- spread %>% pivot_wider(names_from=location, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)

covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "location",
   values_to = "growth") #%>% filter(location!="Other")

covid_growth %>% mutate(date = as.Date("2020-01-22")+time) -> covid_growth

# for(st in locationlist)
# {
# covid_growth %>% filter(location == st) %>%
#                   ggplot + aes(date, growth) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
#                         #scale_x_continuous() + 
#                         labs(caption=capt) + 
#                         xlab("Date") + ylab("Growth of deaths") + ggtitle(paste("Per diem growth of COVID-19 casualties in", st)) #+
#                         #facet_wrap(.~location)

#               fname = paste0("graphs/deaths-in-",st,".pdf")
#               ggsave(fname, device="pdf")

# }


# ggsave("graphs/covid-us-deathsgrowth.pdf", device="pdf")
# write_csv(covid_growth, "data/covid-us-deathsgrowth.csv")
#covid_growth %>% filter(time==326) %>% summarize(x=sum(growth))

# save variable for later use
deathsgrowth <- covid_growth

# View(deathsgrowth)

#
#
#
# View(deaths)
# View(cases)
#
deaths <- deathsgrowth %>% group_by(date, location) %>% summarise(deaths=sum(growth))
cases <- infectiongrowth %>% group_by(date, location) %>% summarise(cases=sum(growth))

casesdeaths <- deaths %>% inner_join(cases, by=c("location","date")) %>% filter(cases > 0, deaths > 0)



# View(casesdeaths %>% filter(location=="California"))
# correction = 10
avdays = 7

totalcases = sum(casesdeaths$cases)
totaldeaths  = sum(casesdeaths$deaths)
totalcasecomment = paste("Total cases: ", format(totalcases, big.mark=" "), "\nTotal deaths: ", format(totaldeaths, big.mark=" "), sep="")


for( st in locationlist)
{

# st="Hawaii"
casesdeaths_by_state <- casesdeaths %>% filter(location==st)

maxcases = max(casesdeaths_by_state$cases)
maxdeaths = max(casesdeaths_by_state$deaths)
correction = maxcases/maxdeaths
# correction = 10

max_deaths_order_of_magnitude = 10^(ceiling(log10(maxdeaths/(10)))+1)

max_sec_y_scale = max_deaths_order_of_magnitude
sec_scale_increment = max_deaths_order_of_magnitude %/% 100
if(sec_scale_increment <= 1) sec_scale_increment = 10


casesdeaths_by_state %>%   
                  ggplot + aes(date, cases) + geom_line(color="blue", linetype="dotted") + geom_line(aes(y=rollmean(cases,avdays, na.pad=TRUE)), size=2, color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,max_sec_y_scale,sec_scale_increment))) + #scale_y_log10(limit=c(10,100000))+ 
                        scale_x_date(date_breaks="1 month", date_labels = "%b %d") + 
                        labs(caption=capt) + xlab("Date") + ylab("Daily incremental number of confirmed cases or deaths") +
                        ggtitle(paste("US daily cases and deaths with", avdays,"days average line in ", st)) + 
                        geom_line(aes(date, correction*deaths), color="red", linetype="dotted") + geom_line(aes(y=rollmean(correction*deaths,avdays,na.pad=TRUE)), size=2, color="red") #+
                        # annotate("text",x=as.Date("2020-03-15", format="%Y-%m-%d"),y=20000,label="cases\n<-----", color="blue") + 
                        # annotate("text",x=as.Date("2020-04-10", format="%Y-%m-%d"),y=10000,label="deaths\n------>", color="red") +
                        # annotate("text",x=as.Date("2020-02-28", format="%Y-%m-%d"),y=175000,label=totalcasecomment, color="black")

    fname = paste0("graphs/overview-chart-",st,".pdf")
    ggsave(fname, device="pdf")


}

# ggsave("graphs/covid-us-daily-cases-and-deaths.pdf", device="pdf")
 write_csv(casesdeaths, "data/covid-us-daily-cases-and-deaths.csv")