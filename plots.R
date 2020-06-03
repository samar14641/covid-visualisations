library(tidyverse)

hospCSV <- paste(getwd(), "Data", "Hospitalisations.csv", sep = "/")
hospitalisations <- read_csv(hospCSV)

hospitalisations <- rename(hospitalisations, 
                           newHosp = `Total number of COVID patients in hospital today`,
                           netNewHosp = `Net new hospitalizations`,
                           fiveDayAvgNetNew = `5 day average of net new hospitalizations`,
                           netNewICU = `Net New number ICU`) %>%  # col renames when reqd
  select(-7, -8) %>%  # drop intubated cols
  mutate(Date = as.Date(Date, "%m/%d/%Y"), totHosp = cumsum(newHosp), totICU = cumsum(ICU))  # convert data, calc cumulative sums

# view(hospitalisations)

ggplot(hospitalisations, aes(x = Date, y = newHosp)) + geom_line() + geom_point() + geom_smooth() + labs(x = "Date", y = "New Hospitalisations", title = "New Hospitalisations Each Day")

sumStatsTotHosp <- summary(hospitalisations$totHosp) %>% as.vector() 
# scalePoints <- seq(from = min(sumStatsTotHosp), to = max(sumStatsTotHosp), length.out = 5)
# scalePoints <- c(min(sumStatsTotHosp), some exp distro, max(sumStatsTotHosp))
scalePoints <- c(1000, min(sumStatsTotHosp), 5000,  10000, 20000, 40000, 80000, max(sumStatsTotHosp))
ggplot(hospitalisations, aes(x = Date, y = totHosp)) + geom_line() + scale_y_log10(labels = scalePoints, breaks = scalePoints) + labs(x = "Date", y = "Cumulative Hospitalisations", title = "Cumulative Hospitalisations (log10 scale)")
