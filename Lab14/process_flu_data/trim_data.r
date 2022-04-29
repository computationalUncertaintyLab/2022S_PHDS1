library(tidyverse)

d <- read.csv('COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility_US_Federal_Health_and_Human_Services__HHS_.csv')

v = d$total_patients_hospitalized_confirmed_influenza_7_day_sum

d  <- d[!is.na(v),]

v <- v[!is.na(v)]
summary(v[v>=0])

colnames(d)[colnames(d) == 'total_patients_hospitalized_confirmed_influenza_7_day_sum'] <- 'total_hospitalizations'

by_week <- d %>% 
  filter(!is.na(total_hospitalizations), total_hospitalizations >= 0) %>%  
  group_by(Collection.Week.Date) %>% 
  summarize(total_hospitalizations = sum(total_hospitalizations)) %>% 
  mutate(Collection.Week.Date = as.POSIXct(Collection.Week.Date, format = '%Y-%m-%d')) %>% 
  rename(week = Collection.Week.Date) %>% 
  arrange(week) 

lambda <- mean(by_week$total_hospitalizations)

pois <- data.frame(x = 1:(max(by_week$total_hospitalizations)))
pois$pois_density <- dpois(1:(max(by_week$total_hospitalizations)), mean(by_week$total_hospitalizations))


## plot model against data
by_week %>% 
  ggplot(aes(x = total_hospitalizations)) + 
  geom_histogram(aes(y = ..density..), color = 'black') +
  geom_line(data = pois, aes(x = x, y = pois_density), color = 'red')

CIs <- c(lambda - 1.96*sqrt(lambda/nrow(by_week)), lambda + 1.96*sqrt(lambda/nrow(by_week)))
print(paste0('lambda: ', lambda, 'CI Lower: ', CIs[1], 'CI Upper: ', CIs[2]))

by_week %>% 
  ggplot(aes(x = as.factor(week), total_hospitalizations, group = 1)) +
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90))

