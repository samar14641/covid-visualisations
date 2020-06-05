library(tidyverse)

hospCSV <- paste(getwd(), "Data", "Hospitalisations.csv", sep = "/")
hospitalisations <- read_csv(hospCSV)

hospitalisations <- rename(hospitalisations, 
                           newHosp = `Total number of COVID patients in hospital today`,
                           netNewHosp = `Net new hospitalizations`,
                           fiveDayAvgNetNew = `5 day average of net new hospitalizations`,
                           netNewICU = `Net New number ICU`) %>%  # col renamed when reqd
  select(-7, -8) %>%  # drop intubated cols
  mutate(Date = as.Date(Date, "%m/%d/%Y"), 
         totHosp = cumsum(newHosp), 
         totICU = cumsum(ICU))  # convert data, calc cumulative sums

# view(hospitalisations)

ggplot(hospitalisations, aes(x = Date, y = newHosp)) + geom_line() + geom_point() + geom_smooth() + labs(x = "Date", y = "New Hospitalisations", title = "New Hospitalisations Each Day")

sumStatsTotHosp <- summary(hospitalisations$totHosp) %>% as.vector() 
# scalePoints <- seq(from = min(sumStatsTotHosp), to = max(sumStatsTotHosp), length.out = 5)
# scalePoints <- c(min(sumStatsTotHosp), some exp distro, max(sumStatsTotHosp))
scalePoints <- c(1000, min(sumStatsTotHosp), 5000,  10000, 20000, 40000, 80000, max(sumStatsTotHosp))
ggplot(hospitalisations, aes(x = Date, y = totHosp)) + geom_line() + scale_y_log10(labels = scalePoints, breaks = scalePoints) + labs(x = "Date", y = "Cumulative Hospitalisations", title = "Cumulative Hospitalisations (log10 scale)")




caseCSV <- paste(getwd(), "Data", "CasesByDate.csv", sep = "/")
cases <- read_csv(caseCSV) %>% filter(Date != "6/2/2020")  # last row data is incorrect, need to get updated data for this to be removed

cases <- rename(cases,
                totPos = `Positive Total`,
                newPos = `Positive New`,
                totProb = `Probable Total`,
                newProb = `Probable New`) %>%  # col renamed
  mutate(Date = as.Date(Date, "%m/%d/%Y"),
         day = weekdays(Date))

# view(cases)

scalePoints <- c(1, 10, 100, 1000, 10000, 100000)  # check options(scipen = 999) for decimal notation
ggplot(cases, aes(x = Date, y = totPos)) + geom_line() + scale_y_log10(labels = scalePoints, breaks = scalePoints) + labs(x = "Date", y = "Total Positive Cases", title = "Total Positive Cases (log10 scale)")

summed <- group_by(cases, day) %>% summarise(newCases = sum(newPos))
mutate(summed, day = factor(day, levels = rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))) %>% 
  ggplot(aes(x = day, y = newCases)) + geom_col(aes(fill = newCases <= mean(newCases))) + geom_abline(slope = 0, intercept = mean(summed$newCases)) + scale_fill_manual(values = c("blue", "maroon")) + labs(x = "Day", y = "Total New Cases", title = "The Lag in Testing on the Weekend", fill = "Lag in\ntesting?") + coord_flip()  # clear proof for testing lag on weekends
# scale_fill_discrete(labels = c("No", "Yes"))
