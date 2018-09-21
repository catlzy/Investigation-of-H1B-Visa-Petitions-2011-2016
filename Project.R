install.packages("dplyr")
library(dplyr)
library(readr)
#take out columns that are not important for the purpose of this project
notUseful = names(h1b_kaggle) %in% c("EMPLOYER_NAME", "lon", "lat", "SOC_NAME", "WORKSITE") 
cleaned_1 = h1b_kaggle[!notUseful]
#take out rows that have missing data
missing=sum(is.na(h1b_kaggle))
cleaned=na.omit(cleaned_1)
#conver to lower for JOB_TITLE
cleaned$JOB_TITLE = tolower(cleaned$JOB_TITLE)
#datasets separated by year
data_6 = cleaned[ which(cleaned$YEAR==2016),]
data_5 = cleaned[ which(cleaned$YEAR==2015),]
data_4 = cleaned[ which(cleaned$YEAR==2014),]
data_3 = cleaned[ which(cleaned$YEAR==2013),]
data_2 = cleaned[ which(cleaned$YEAR==2012),]
data_1 = cleaned[ which(cleaned$YEAR==2011),]
number_of_cases = c(dim(data_1)[1], dim(data_2)[1], dim(data_3)[1], dim(data_4)[1], dim(data_5)[1], dim(data_6)[1])
year = c(2011, 2012, 2013, 2014, 2015, 2016)
plot(year, number_of_cases, main = "Number of Applications Per Year")

#1.
#datasets with technology related jobs, data_1 can be changed to other datasets
#to find out tech related jobs in other years
toMatch = c('programmer', 'software', 'computer', 'database')
toNotMatch = c('sales', 'marketing')
tech_jobs_temp = filter(data_6, grepl(paste(toMatch,collapse='|'), JOB_TITLE))
tech_jobs = filter(tech_jobs_temp, !grepl(paste(toNotMatch,collapse='|'), JOB_TITLE))
prop = nrow(tech_jobs)/nrow(data_6)
#plot
percent = c(0.2953271, 0.3000837, 0.3017569, 0.321822, 0.3255494, 0.3278007)
year = c(2011, 2012, 2013, 2014, 2015, 2016)
plot(year, percent, main = "Percentage of Applicants with Technology Related Jobs Each Year")
#reject null hypothesis, there is difference between the two proportions 
ztest = prop.test(x = c(103990, 212348), n = c(352118, 647796), alternative = "less")

#3.
#number of certified application each year
c_6 = sum(data_6$CASE_STATUS == 'CERTIFIED'|data_6$CASE_STATUS == 'CERTIFIED-WITHDRAWN')
c_5 = sum(data_5$CASE_STATUS == 'CERTIFIED'|data_5$CASE_STATUS == 'CERTIFIED-WITHDRAWN')
c_4 = sum(data_4$CASE_STATUS == 'CERTIFIED'|data_4$CASE_STATUS == 'CERTIFIED-WITHDRAWN')
c_3 = sum(data_3$CASE_STATUS == 'CERTIFIED'|data_3$CASE_STATUS == 'CERTIFIED-WITHDRAWN')
c_2 = sum(data_2$CASE_STATUS == 'CERTIFIED'|data_2$CASE_STATUS == 'CERTIFIED-WITHDRAWN')
c_1 = sum(data_1$CASE_STATUS == 'CERTIFIED'|data_1$CASE_STATUS == 'CERTIFIED-WITHDRAWN')
#there is linear relationship 
certified = c(c_1, c_2, c_3, c_4, c_5, c_6)
qqnorm(certified)
qqline(certified)
lin_model = lm(certified~year)
plot(year, certified, main = "Certified Applications Per Year")
abline(lin_model)
#proportion is different 
ztest = prop.test(x = c(c_1, c_6), n = c(nrow(data_1), nrow(data_6)), alternative = "less")

#4.
#average wage for each year
w_6 = mean(data_6$PREVAILING_WAGE)
w_5 = mean(data_5$PREVAILING_WAGE)
w_4 = mean(data_4$PREVAILING_WAGE)
w_3 = mean(data_3$PREVAILING_WAGE)
w_2 = mean(data_2$PREVAILING_WAGE)
w_1 = mean(data_1$PREVAILING_WAGE)
wages = c(w_1, w_2, w_3, w_4, w_5, w_6)
plot(year, wages, main = "Average Wages Per Year of all Applications")
#certified average wage
c6 = data_6[data_6$CASE_STATUS == 'CERTIFIED'|data_6$CASE_STATUS == 'CERTIFIED-WITHDRAWN',]
w6 = mean(c6$PREVAILING_WAGE)
c5 = data_5[data_5$CASE_STATUS == 'CERTIFIED'|data_5$CASE_STATUS == 'CERTIFIED-WITHDRAWN',]
w5 = mean(c5$PREVAILING_WAGE)
c4 = data_4[data_4$CASE_STATUS == 'CERTIFIED'|data_4$CASE_STATUS == 'CERTIFIED-WITHDRAWN',]
w4 = mean(c4$PREVAILING_WAGE)
c3 = data_3[data_3$CASE_STATUS == 'CERTIFIED'|data_3$CASE_STATUS == 'CERTIFIED-WITHDRAWN',]
w3 = mean(c3$PREVAILING_WAGE)
c2 = data_2[data_2$CASE_STATUS == 'CERTIFIED'|data_2$CASE_STATUS == 'CERTIFIED-WITHDRAWN',]
w2 = mean(c2$PREVAILING_WAGE)
c1 = data_1[data_1$CASE_STATUS == 'CERTIFIED'|data_1$CASE_STATUS == 'CERTIFIED-WITHDRAWN',]
w1 = mean(c1$PREVAILING_WAGE)
certified_wages = c(w1, w2, w3, w4, w5, w6)
plot(year, certified_wages, main = "Average Wages Per Year of Certified Applications")
ttest = t.test(c1$PREVAILING_WAGE, c6$PREVAILING_WAGE)

#5.
#rate of denial 
d_6 = sum(data_6$CASE_STATUS == "DENIED")
d_5 = sum(data_5$CASE_STATUS == "DENIED")
d_4 = sum(data_4$CASE_STATUS == "DENIED")
d_3 = sum(data_3$CASE_STATUS == "DENIED")
d_2 = sum(data_2$CASE_STATUS == "DENIED")
d_1 = sum(data_1$CASE_STATUS == "DENIED")
denied = c(d_1, d_2, d_3, d_4, d_5, d_6)
denied_prop = c(d_1/dim(data_1)[1], d_2/dim(data_2)[1], d_3/dim(data_3)[1], d_4/dim(data_4)[1], d_5/dim(data_5)[1], d_6/dim(data_6)[1])
plot(year, denied_prop, main = "Proportion of Denied Cases Per Year")
#there are differences
ztest = prop.test(x = c(d_1, d_6), n = c(nrow(data_1), nrow(data_6)), alternative = "greater")

#6.
#what has the biggest influence on approval 
#full time
full_c = sum(data_6$FULL_TIME_POSITION == 'Y' & (data_6$CASE_STATUS == 'CERTIFIED'|data_6$CASE_STATUS == 'CERTIFIED-WITHDRAWN'))
full_d = sum(data_6$FULL_TIME_POSITION == 'Y' & data_6$CASE_STATUS == 'DENIED')
#tech jobs
toMatch = c('programmer', 'software', 'computer', 'database')
toNotMatch = c('sales', 'marketing')
tech_jobs_temp = filter(data_6, grepl(paste(toMatch,collapse='|'), JOB_TITLE))
tech_jobs = filter(tech_jobs_temp, !grepl(paste(toNotMatch,collapse='|'), JOB_TITLE))
tech_jobs_c = sum(tech_jobs$CASE_STATUS == 'CERTIFIED'|tech_jobs$CASE_STATUS == 'CERTIFIED-WITHDRAWN')     
tech_jobs_d = sum(tech_jobs$CASE_STATUS == 'DENIED')
#wages 
data_6$WAGE_IN_RANGE = 0
for (i in 1:dim(data_6)[1]){
  if ((data_6$PREVAILING_WAGE[i] > 60000) & (data_6$PREVAILING_WAGE[i] < 90000)){
    data_6$WAGE_IN_RANGE[i] = 1
  }
}  
sum(data_6$WAGE_IN_RANGE == 1) 
in_range_c = sum(data_6$WAGE_IN_RANGE == 1 & (data_6$CASE_STATUS == 'CERTIFIED'|data_6$CASE_STATUS == 'CERTIFIED-WITHDRAWN'))
in_range_d = sum(data_6$WAGE_IN_RANGE == 1 & data_6$CASE_STATUS == 'DENIED')
#create a 2 way table for the test
data = c(full_c, tech_jobs_c, in_range_c, full_d, tech_jobs_d, in_range_d)
col_names = c("full time", "tech jobs", "wages between 60k to 90k")
row_names = c("certified/certified-withdrawn", "denied")
obs = matrix(data, byrow = TRUE, nrow = 2, ncol = 3)
rownames(obs) = row_names
colnames(obs) = col_names
xsq = chisq.test(obs)

             