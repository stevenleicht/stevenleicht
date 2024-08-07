#Project: comparing rates of death by suicide for total and veteran populations between the year 2005 and 2011
#For context, 2005 was the third full year of war in Iraq, while 2011 was the final year
#I want to see if there are differences in death rates by suicide during different times of war
#I believe understanding this data can provide insight for potential future endeavors in war/combat
#Hopefully these results can help with overall military/veteran health and well-being
#Project: looking at suicide rates of soldiers both during the middle and end of war to see if there are differences
#Data is provided from the "US Veteran Suicides" dataset from Aleksey Bilogur (2017). Dataset was discovered on Kaggle

#Opening required packages
library(dplyr)
library(ggplot2)
library(knitr)
library(caret)

#Importing death by suicide data from 2005
file_path = "/Users/stevenleicht/Downloads/2005_military_suicide_data.csv"
military_suicide_data_1 = read.csv(file_path)
head(military_suicide_data_1)
summary(military_suicide_data_1)
str(military_suicide_data_1)

#Importing death by suicide data from 2011
file_path = "/Users/stevenleicht/Downloads/2011_military_suicide_data.csv"
military_suicide_data_2 = read.csv(file_path)
head(military_suicide_data_2)
summary(military_suicide_data_2)
str(military_suicide_data_2)

#Filtering/selecting the data from 2005 data
selected_data_1 = military_suicide_data_1 %>%
  select(vet_suicides)

#Filtering/selecting the data from 2011 data
selected_data_2 = military_suicide_data_2 %>%
  select(vet_suicides)

#Performing a t-test to see differences in military suicide rates between 2005 and 2011
t.test(selected_data_1, selected_data_2)
#the mean number of military suicides per state in 2005 was 136.92, while it was 146.92 in 2011. According to this data, this means that there are more deaths by suicide towards the end of war compared to the beginning. 

#Now I will be going into more specifics. First, I will look at veteran deaths by suicide rates by region of the country in 2005.
#I will separate states into four regions identified by the United States Censor Bureau (this data excludes Washington, D.C.)
#I will be finding the mean for each state by region for my data to run a hopefully successful statistical analysis of the data set(s)
#Region 1 = Northeast (Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, Vermont, New Jersey, New York, and Pennsylvania)
#Region 2 = Midwest (Illinois, Indiana, Michigan, Ohio, Wisconsin, Iowa, Kansas, Minnesota, Missouri, Nebraska, North Dakota, and South Dakota)
#Region 3 = South (Delaware, Florida, Georgia, Maryland, North Carolina, South Carolina, Virginia, West Virginia, Alabama, Kentucky, Mississippi, Tennessee, Arkansas, Louisiana, Oklahoma, and Texas)
#Region 4 = West (Arizona, Colorado, Idaho, Montana, Nevada, New Mexico, Utah, Wyoming, Alaska, California, Hawaii, Oregon, and Washington)
region_selected_data_1_05 = military_suicide_data_1[c(8, 20, 22, 30, 31, 33, 39, 40, 46), ]
region_selected_data_2_05 = military_suicide_data_1[c(14, 15, 16, 17, 23, 24, 26, 28, 35, 36, 42, 50), ]
region_selected_data_3_05 = military_suicide_data_1[c(2, 5, 9, 10, 11, 18, 19, 21, 25, 34, 37, 41, 43, 44, 47, 49), ]
region_selected_data_4_05 = military_suicide_data_1[c(3, 4, 6, 7, 12, 13, 27, 29, 32, 38, 45, 48, 51), ]
region_selected_data_1_05$Region = "Region 1"
region_selected_data_2_05$Region = "Region 2"
region_selected_data_3_05$Region = "Region 3"
region_selected_data_4_05$Region = "Region 4"
combined_region_data_05 = rbind(region_selected_data_1_05, 
                          region_selected_data_2_05, 
                          region_selected_data_3_05, 
                          region_selected_data_4_05)
print(combined_region_data_05)

#Before looking at future/predictive data, I want to find the means/averages for specific categories/variables between the 2005 and 2011 data

#Finding the mean for veteran deaths by suicide for each state by region in the year 2005
mean_vet_suicides_by_region_05 = combined_region_data_05 %>%
  group_by(Region) %>%
  summarise(Mean_Vet_Suicides = mean(vet_suicides, na.rm = TRUE))
print(mean_vet_suicides_by_region_05)
#Region 1 = 117
#Region 2 = 103
#Region 3 = 181
#Region 4 = 126

#Finding the mean population for each state of each region to compare to number of veteran deaths by suicide in 2005
combined_region_data_05 %>%
  group_by(Region) %>%
  summarise(Mean_Overall_Pop_18 = mean(overall_pop_18, na.rm = TRUE))
#Region 1 = 3,922,626
#Region 2 = 2,982,652
#Region 3 = 5,617,839
#Region 4 = 4,149,701

#Finding the mean for all deaths by suicide by state for each region in 2005
combined_region_data_05 %>%
  group_by(Region) %>%
  summarise(Mean_All_Suicides = mean(all_suicides, na.rm = TRUE))
#Region 1 = 555
#Region 2 = 534
#Region 3 = 850
#Region 4 = 588

#Finding percentage of veteran deaths by suicide compared to all deaths by suicide by state for each region
percent_suicide_vet_vs_all_by_region_05 = combined_region_data_05 %>%
  group_by(Region) %>%
  summarise(
    total_vet_suicides = sum(vet_suicides, na.rm = TRUE),
    total_all_suicides = sum(all_suicides, na.rm = TRUE)
  )
percent_suicide_vet_vs_all_by_region_05 <- percent_suicide_vet_vs_all_by_region_05 %>%
  mutate(vet_suicides_percentage = (total_vet_suicides / total_all_suicides) * 100)
print(percent_suicide_vet_vs_all_by_region_05)
#Region 1 = 21.1%
#Region 2 = 19.3%
#Region 3 = 21.3%
#Region 4 = 21.5%

#Now completing the same analysis for 2011 data
region_selected_data_1_11 = military_suicide_data_2[c(8, 20, 22, 30, 31, 33, 39, 40, 46), ]
region_selected_data_2_11 = military_suicide_data_2[c(14, 15, 16, 17, 23, 24, 26, 28, 35, 36, 42, 50), ]
region_selected_data_3_11 = military_suicide_data_2[c(2, 5, 9, 10, 11, 18, 19, 21, 25, 34, 37, 41, 43, 44, 47, 49), ]
region_selected_data_4_11 = military_suicide_data_2[c(3, 4, 6, 7, 12, 13, 27, 29, 32, 38, 45, 48, 51), ]
region_selected_data_1_11$Region = "Region 1"
region_selected_data_2_11$Region = "Region 2"
region_selected_data_3_11$Region = "Region 3"
region_selected_data_4_11$Region = "Region 4"
combined_region_data_11 = rbind(region_selected_data_1_11, 
                                region_selected_data_2_11, 
                                region_selected_data_3_11, 
                                region_selected_data_4_11)
print(combined_region_data_11)

#Finding the mean amount of veteran deaths by suicide for each state by region in the year 2011
mean_vet_suicides_by_region_11 = combined_region_data_11 %>%
  group_by(Region) %>%
  summarise(Mean_Vet_Suicides = mean(vet_suicides, na.rm = TRUE))
print(mean_vet_suicides_by_region_11)
#Region 1 = 120
#Region 2 = 120
#Region 3 = 198
#Region 4 = 125

#Finding the mean population for each state of each region to compare to number of veteran deaths by suicide in 2011
combined_region_data_11 %>%
  group_by(Region) %>%
  summarise(Mean_Overall_Pop_18 = mean(overall_pop_18, na.rm = TRUE))
#Region 1 = 4,319,811
#Region 2 = 3,235,458
#Region 3 = 6,291,976
#Region 4 = 4,509,764

#Finding the mean for total number of deaths by suicide by state for each region in 2011
combined_region_data_11 %>%
  group_by(Region) %>%
  summarise(Mean_All_Suicides = mean(all_suicides, na.rm = TRUE))
#Region 1 = 657
#Region 2 = 587
#Region 3 = 1,027
#Region 4 = 681

#Finding percentage of veteran deaths by suicide by region in 2011
percent_suicide_vet_vs_all_by_region_11 = combined_region_data_11 %>%
  group_by(Region) %>%
  summarise(
    total_vet_suicides = sum(vet_suicides, na.rm = TRUE),
    total_all_suicides = sum(all_suicides, na.rm = TRUE)
  )
percent_suicide_vet_vs_all_by_region_11 <- percent_suicide_vet_vs_all_by_region_11 %>%
  mutate(vet_suicides_percentage_11 = (total_vet_suicides / total_all_suicides) * 100)
print(percent_suicide_vet_vs_all_by_region_11)
#Region 1 = 18.3%
#Region 2 = 20.4%
#Region 3 = 19.3%
#Region 4 = 18.3%

#Conclusion: total percentage of death by suicide being a veteran slightly decreases towards the end of war compared to the middle, even though population sizes increase
#Even though population sizes increased during this six-year period, along with the total number of veteran suicides, the percentage did decrease

#Creating a table to showcase the differences in percentages by year (2005 vs. 2011):
#First, let's combine the 2005 and 2011 data
combined_region_suicide_percentage_data_05_vs_11 = bind_rows(percent_suicide_vet_vs_all_by_region_05, 
                                                             percent_suicide_vet_vs_all_by_region_11)
print(combined_region_suicide_percentage_data_05_vs_11)

#Creating a model to predict future veteran deaths by suicide by region depending on time of war and increase in population
#For example, if another war began in 2024, I'd like to see the values from 2026 and 2032. If there is a similar rate of population growth between 2005 and 2011 as there would be in 2026 and 2032, I'd like to see how many veteran suicides there could be when the percentages stay the same per region. This could help with starting initiatives to preventing deaths by suicide from veterans and help with the overall health and well-being of current and former soldiers

#First, let's look once again at the mean population for each region between 2005 and 2011 and see the average rate of growth during that time span
#2005 data
combined_region_data_05 %>%
  group_by(Region) %>%
  summarise(Mean_Overall_Pop_18 = mean(overall_pop_18, na.rm = TRUE))
#Region 1 = 3,922,626
#Region 2 = 2,982,652
#Region 3 = 5,617,839
#Region 4 = 4,149,701

#2011 data
combined_region_data_11 %>%
  group_by(Region) %>%
  summarise(Mean_Overall_Pop_18 = mean(overall_pop_18, na.rm = TRUE))
#Region 1 = 4,319,811
#Region 2 = 3,235,458
#Region 3 = 6,291,976
#Region 4 = 4,509,764

#Finding the percentage of growth for each region between these years
pop_2005 = c(3922626, 2982652, 5617839, 4149701)
pop_2011 = c(4319811, 3235458, 6291976, 4509764)
percentage_growth = ((pop_2011 - pop_2005) / pop_2005) * 100
names(percentage_growth) <- c("Region 1", "Region 2", "Region 3", "Region 4")
print(percentage_growth)
#Below are the percent growth rates for each region between 2005 and 2011
#Region 1 = 10.13%
#Region 2 = 8.48%
#Region 3 = 12.00%
#Region 4 = 8.68%

#Now that we know the percent change in growth, we can use these numbers to predict population growth between 2026 and 2032
#Let's pretend the population growth for each region increased by 10% from 2011 to 2026
pop_2011 = c(4319811, 3235458, 6291976, 4509764)
pop_2026 = pop_2011 * 1.10
names(pop_2026) = c("Region 1", "Region 2", "Region 3", "Region 4")
print(pop_2026)
#The adjusted population sizes by region for 2026 are below:
#Region 1 = 4,751,792
#Region 2 = 3,559,004
#Region 3 = 6,921,174
#Region 4 = 4,960,740

#Now, let's apply the percent changes between 2005 and 2011 and do the same comparison between 2026 and 2032
pop_2026 = c(4751792, 3559004, 6920174, 4960740)
pop_growth_rates = c(10.13, 8.48, 12.00, 8.68) / 100
pop_2032 = pop_2026 * (1 + pop_growth_rates)
names(pop_2032) = c("Region 1", "Region 2", "Region 3", "Region 4")
population_increase = pop_2032 - pop_2026
print("Projected population for 2032:")
print(pop_2032)
#The projected population for each region in 2032 is below:
#Region 1 = 5,233,149
#Region 2 = 3,860,808
#Region 3 = 7,750,595
#Region 4 = 5,391,332

#Below is the exact projected increases in population by region between 2026 and 2032
print("Population increase between 2026 and 2032:")
print(population_increase)
#Region 1 = 481,356.5 people
#Region 2 = 301,803.5 people
#Region 3 = 830,420.9 people
#Region 4 = 430,592.2 people

#Now, we must find the mean percentage of total deaths by suicide by the mean population of each region for the years 2005 and 2011, and then adjust that to the years 2026 and 2032
#2005 data:
percent_suicide_all_vs_pop_by_region_05 = combined_region_data_05 %>%
  group_by(Region) %>%
  summarise(
    total_all_suicides = sum(all_suicides, na.rm = TRUE),
    total_pop_by_region = sum(overall_pop_18, na.rm = TRUE)
  )
percent_suicide_all_vs_pop_by_region_05 <- percent_suicide_all_vs_pop_by_region_05 %>%
  mutate(all_suicides = (total_all_suicides / total_pop_by_region) * 100)
print(percent_suicide_all_vs_pop_by_region_05)
#Below are the percentages of all deaths by suicide compared to mean population by region in 2005 (the percentage of people who died from suicide by region)
#Region 1 = 0.0142%
#Region 2 = 0.0179%
#Region 3 = 0.0151%
#Region 4 = 0.0142%

#Doing the same thing with the 2011 data now:
percent_suicide_all_vs_pop_by_region_11 = combined_region_data_11 %>%
  group_by(Region) %>%
  summarise(
    total_all_suicides = sum(all_suicides, na.rm = TRUE),
    total_pop_by_region = sum(overall_pop_18, na.rm = TRUE)
  )
percent_suicide_all_vs_pop_by_region_11 <- percent_suicide_all_vs_pop_by_region_11 %>%
  mutate(all_suicides = (total_all_suicides / total_pop_by_region) * 100)
print(percent_suicide_all_vs_pop_by_region_11)
#Below are the percentages of all deaths by suicide compared to mean population by region in 2011 (the percentage of people who died from suicide by region)
#Region 1 = 0.0152%
#Region 2 = 0.0181%
#Region 3 = 0.0163%
#Region 4 = 0.0151%

#Let's see how updated population growth will result in the total number of deaths by suicide with the same percentages as 2005 and 2011 (we will use 2005 percentages for 2026 and 2011 percentages for 2032)
total_suicide_data_2026$Total_Suicides = total_suicide_data_2026$Population * total_suicide_data_2026$Total_Suicide_Percentage
print(total_suicide_data_2026)
#Below are the projected total number of deaths by suicide by region in 2026:
#Region 1 = 67,475.45 people
#Region 2 = 63,706.17 people
#Region 3 = 104,494.63 people
#Region 4 = 70,442.51 people

total_suicide_data_2032$Total_Suicides = total_suicide_data_2032$Population * 
  total_suicide_data_2032$Total_Suicide_Percentage
print(total_suicide_data_2032)
#Below are the projected total number of deaths by suicide by region in 2032:
#Region 1 = 79,543.86 people
#Region 2 = 69,880.62 people
#Region 3 = 126,334.70 people
#Region 4 = 81,409.12 people

#Continuing with this hand-calculated model to predict total and veteran death rates by suicide with a linear progression in population
#The percentage of veteran deaths by suicide for 2026 will be the same as 2005 for this example, while 2032 will be the same as 2011

#Creating a new column for predicted total deaths by suicide by year. 
#2026 data:
total_suicide_data_2026$Predicted_Total_Suicides_2026 = (total_suicide_data_2026$Population
                                                         * total_suicide_data_2026$Total_Suicide_Percentage)
print(total_suicide_data_2026)
#Below are the predicted total number of suicides in 2026:
#Region 1 = 67,475.45 people
#Region 2 = 63,706.17 people
#Region 3 = 104,494.63 people
#Region 4 = 70,442.51 people

#2032 data
total_suicide_data_2032$Predicted_Total_Suicides_2032 = (total_suicide_data_2032$Population
                                                         * total_suicide_data_2032$Total_Suicide_Percentage)
print(total_suicide_data_2032)
#Below are the predicted total number of suicides in 2032:
#Region 1 = 79,543.86 people
#Region 2 = 69,880.62 people
#Region 3 = 126,334.70 people
#Region 4 = 81,409.12 people

#Now I will include the veteran deaths by suicide percentages from 2005 and 2011 to determine how many of the predicted total deaths by suicide for 2026 and 2032 are veterans

#2026 predicted veteran death by suicide data:
region_1_predicted_vet_suicide_26 = 67475.45 * 0.211
print(region_1_predicted_vet_suicide_26)
#predicted number of veteran suicides for Region 1 in 2026 is 14,237.32 people
region_2_predicted_vet_suicide_26 = 63706.17 * 0.193
print(region_2_predicted_vet_suicide_26)
#predicted number of veteran suicides for Region 2 in 2026 is 12,295.29 people
region_3_predicted_vet_suicide_26 = 104494.63 * 0.213
print(region_3_predicted_vet_suicide_26)
#predicted number of veteran suicides for Region 3 in 2026 is 22,257.36 people
region_4_predicted_vet_suicide_26 = 70442.51 * 0.215
print(region_4_predicted_vet_suicide_26)
#predicted number of veteran suicides for Region 4 in 2026 is 15,145.14 people
total_predicted_vet_suicide_26 = region_1_predicted_vet_suicide_26 + region_2_predicted_vet_suicide_26 +
  region_3_predicted_vet_suicide_26 + region_4_predicted_vet_suicide_26
print(total_predicted_vet_suicide_26)
#the total predicted number of veteran suicides in 2026 is 63,935.11 people

#2032 predicted veteran suicide data:
region_1_predicted_vet_suicide_32 = 79543.86 * 0.183
print(region_1_predicted_vet_suicide_32)
#predicted number of veteran suicides for Region 1 in 2032 is 14,556.53 people
region_2_predicted_vet_suicide_32 = 69880.62 * 0.204
print(region_2_predicted_vet_suicide_32)
#predicted number of veteran suicides for Region 2 in 2032 is 14,255.65 people
region_3_predicted_vet_suicide_32 = 126334.7 * 0.193
print(region_3_predicted_vet_suicide_32)
#predicted number of veteran suicides for Region 3 in 2032 is 24,382.6 people
region_4_predicted_vet_suicide_32 = 81409.12 * 0.183
print(region_4_predicted_vet_suicide_32)
#predicted number of veteran suicides for Region 4 in 2032 is 14,897.87 people
total_predicted_vet_suicide_32 = region_1_predicted_vet_suicide_32 + region_2_predicted_vet_suicide_32 +
  region_3_predicted_vet_suicide_32 + region_4_predicted_vet_suicide_32
print(total_predicted_vet_suicide_32)
#the total predicted number of veteran suicides in 2032 is 68,092.64 people

#combining calculated predicted values into one
#2026 predicted data:
total_predicted_suicide_data_2026 = data.frame(
  Region = c("Region 1", "Region 2", "Region 3", "Region 4"),
  Predicted_Total_Suicides_2026 = c(67475.45, 63706.17, 104494.63, 70442.51),
  Predicted_Vet_Suicides_2026 = c(14237.32, 12295.29, 22257.36, 15145.14)
)
print(total_predicted_suicide_data_2026)

#2032 predicted data:
total_predicted_suicide_data_2032 <- data.frame(
  Region = c("Region 1", "Region 2", "Region 3", "Region 4"),
  Predicted_Total_Suicides_2032 = c(79543.86, 69880.62, 126334.70, 81409.12),
  Predicted_Vet_Suicides_2032 = c(14556.53, 14255.65, 24382.6, 14897.87)
)
print(total_predicted_suicide_data_2032)

#creating a table to show a clearer view for predicted values in 2026 and 2032
predicted_suicides_table <- merge(total_predicted_suicide_data_2026, 
                                  total_predicted_suicide_data_2032, by = "Region")
print(predicted_suicides_table)
kable(predicted_suicides_table)

#Before creating the multiple linear regression model, we will be calculating and combining mean sizes/percentages once again

#First, let's look once again at the mean population for each region between 2005 and 2011 and see the average rate of growth during that time span
#2005 data
combined_region_data_05 %>%
  group_by(Region) %>%
  summarise(Mean_Overall_Pop_18 = mean(overall_pop_18, na.rm = TRUE))
#Region 1 = 3,922,626
#Region 2 = 2,982,652
#Region 3 = 5,617,839
#Region 4 = 4,149,701

#2011 data
combined_region_data_11 %>%
  group_by(Region) %>%
  summarise(Mean_Overall_Pop_18 = mean(overall_pop_18, na.rm = TRUE))
#Region 1 = 4,319,811
#Region 2 = 3,235,458
#Region 3 = 6,291,976
#Region 4 = 4,509,764

#Finding the percentage of growth for each region between these years
pop_2005 = c(3922626, 2982652, 5617839, 4149701)
pop_2011 = c(4319811, 3235458, 6291976, 4509764)
percentage_growth = ((pop_2011 - pop_2005) / pop_2005) * 100
names(percentage_growth) <- c("Region 1", "Region 2", "Region 3", "Region 4")
print(percentage_growth)
#Below are the percent growth rates for each region between 2005 and 2011
#Region 1 = 10.13%
#Region 2 = 8.48%
#Region 3 = 12.00%
#Region 4 = 8.68%

#Now that we know the percent change in growth, we can use these numbers to predict population growth between 2026 and 2032
#Let's pretend the population growth for each region increased by 10% from 2011 to 2026
pop_2011 = c(4319811, 3235458, 6291976, 4509764)
pop_2026 = pop_2011 * 1.10
names(pop_2026) = c("Region 1", "Region 2", "Region 3", "Region 4")
print(pop_2026)
#The adjusted population sizes by region for 2026 are below:
#Region 1 = 4,751,792
#Region 2 = 3,559,004
#Region 3 = 6,921,174
#Region 4 = 4,960,740

#Now, let's apply the percent changes between 2005 and 2011 and do the same comparison between 2026 and 2032
pop_2026 = c(4751792, 3559004, 6920174, 4960740)
pop_growth_rates = c(10.13, 8.48, 12.00, 8.68) / 100
pop_2032 = pop_2026 * (1 + pop_growth_rates)
names(pop_2032) = c("Region 1", "Region 2", "Region 3", "Region 4")
population_increase = pop_2032 - pop_2026
print("Projected population for 2032:")
print(pop_2032)
#The projected population for each region in 2032 is below:
#Region 1 = 5,233,149
#Region 2 = 3,860,808
#Region 3 = 7,750,595
#Region 4 = 5,391,332

#Below is the exact projected increases in population by region between 2026 and 2032
print("Population increase between 2026 and 2032:")
print(population_increase)
#Region 1 = 481,356.5 people
#Region 2 = 301,803.5 people
#Region 3 = 830,420.9 people
#Region 4 = 430,592.2 people

#combining population data with death by suicide data from 2005
historical_suicide_data_05 = data.frame(
  Population_05 = c(3922626, 2982652, 5617839, 4149701), #mean population sizes by region in 2005
  Total_Suicides_05 = c(555, 534, 850, 588), #mean total suicides by region in 2005
  Veteran_Suicides_05 = c(117, 103, 181, 126) #mean veteran suicides by region in 2005
)
print(historical_suicide_data_05)

#combining population data with death by suicide data from 2011
historical_suicide_data_11 = data.frame(
  Population_11 = c(4319811, 3235458, 6291976, 4509764), #mean population sizes by region in 2011
  Total_Suicides_11 = c(657, 587, 1027, 681), #mean total suicides by region in 2011
  Veteran_Suicides_11 = c(120, 120, 198, 125) #mean veteran suicides by region in 2011
)
print(historical_suicide_data_11)

#Now we can use all this data to create a multiple linear regression model to determine the exact number of veteran suicides for the years 2026 and 2032. Hopefully, this can provide context and help with the prevention of veteran deaths by suicide for years to come
#This model will be different than the above, linear/hand-calculated model

#creating the multiple linear regression model to predict total suicides for the year 2026 based off 2005 data:
model_total_suicides_05_26 = lm(Total_Suicides_05 ~ pop_2005, data = historical_suicide_data_05)
summary(model_total_suicides_05_26)
#The residual standard error is 62.23 total suicides on 2 degrees of freedom
#The adjusted r-squared value is 0.8213, meaning that the model is reliable
#The p-value is 0.06147, meaning that it is not statistically significant

#creating the multiple linear regression model to predict veteran suicides for the year 2026 based off 2005 data:
model_veteran_suicides_05_26 = lm(Veteran_Suicides_05 ~ pop_2005, data = historical_suicide_data_05)
summary(model_veteran_suicides_05_26)
#The residual standard error is 8.939 veteran suicides on 2 degrees of freedom
#The adjusted r-squared value is 0.9316, meaning that it is very reliable
#The p-value is 0.02308, meaning that it is statistically significant

#creating the multiple linear regression model to predict total suicides for the year 2032 based off 2011 data:
model_total_suicides_11_32 = lm(Total_Suicides_11 ~ pop_2011, data = historical_suicide_data_11)
summary(model_total_suicides_11_32)
#The residual standard error is 61.31 suicides on 2 degrees of freedom
#The adjusted r-squared value is 0.9029, meaning that it is reliable
#The p-value is 0.03291, meaning that it is statistically significant

#creating the multiple linear regression model to predict veteran suicides for the year 2032 based off 2011 data:
model_veteran_suicides_11_32 = lm(Veteran_Suicides_11 ~ pop_2011, data = historical_suicide_data_11)
summary(model_veteran_suicides_11_32)
#The residual standard error is 19.26 veteran suicides on 2 degrees of freedom
#The adjusted r-squared value is 0.7462, meaning that it is pretty reliable
#The p-value is 0.08851, meaning that it is not statistically significant

#taking the multiple linear regression models to make predictive values for total and veteran deaths by suicide for the years 2026 and 2032

#predicted values for 2026:
predicted_total_suicides_2026 = predict(model_total_suicides_05_26, 
                                        newdata = data.frame(Population = pop_2005))
predicted_veteran_suicides_2026 = predict(model_veteran_suicides_05_26, 
                                          newdata = data.frame(Population = pop_2005))
print(predicted_total_suicides_2026)
print(predicted_veteran_suicides_2026)

#predicted values for 2032:
predicted_total_suicides_2032 = predict(model_total_suicides_11_32, newdata = 
                                          data.frame(Population = pop_2011))
predicted_veteran_suicides_2032 = predict(model_veteran_suicides_11_32, newdata = 
                                            data.frame(Population = pop_2011))
print(predicted_total_suicides_2032)
print(predicted_veteran_suicides_2032)

#combining the predictive values into one to show a clearer perspective:
predictions <- data.frame(
  Year = c(rep(2026, 4), rep(2032, 4)),
  Region = rep(c("Region 1", "Region 2", "Region 3", "Region 4"), 2),
  Predicted_Total_Suicides = c(predicted_total_suicides_2026, predicted_total_suicides_2032),
  Predicted_Vet_Suicides = c(predicted_veteran_suicides_2026, predicted_veteran_suicides_2032)
)
print(predictions)
#Below are the predictions for mean total suicides by region in 2026 based on the above model:
#Region 1 = 600.6414 people
#Region 2 = 481.5704 people
#Region 3 = 815.3821 people
#Region 4 = 629.4061 people

#Below are the predictions for mean veteran suicides by region in 2026 based on the above model:
#Region 1 = 124.23265 people
#Region 2 = 95.45932 people
#Region 3 = 176.12444 people
#Region 4 = 131.18359 people

#Below are the predictions for mean total suicides by region in 2032 based on the above model
#Region 1 = 697.5153 people
#Region 2 = 534.5864 people
#Region 3 = 993.8418 people
#Region 4 = 726.0565 people

#Below are the predictions for mean veteran suicides by region in 2032 based on the above model
#Region 1 = 133.33398 people
#Region 2 = 103.48856 people
#Region 3 = 187.61527 people
#Region 4 = 138.56219 people

#Creating a bar plot to visualize the above data:

#Converting the data to long format for better visualization/understanding
predictions_long_format = reshape2::melt(predictions, id.vars = c("Year", "Region"),
                                   measure.vars = c("Predicted_Total_Suicides", "Predicted_Vet_Suicides"),
                                   variable.name = "tot_vs_vet_suicide", value.name = "suicide_category")
print(predictions_long_format)

#Creating the bar plot:
ggplot(predictions_long_format, aes(x = Region, y = suicide_category, fill = interaction(tot_vs_vet_suicide, Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(suicide_category, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Total and Veteran Suicide Rate Prediction for 2026 and 2032",
       x = "Region", y = "Number of Suicides",
       fill = "Suicide Type and Year") +
  theme_minimal() +
  scale_fill_manual(values = c("darkblue", "lightblue", "darkred", "lightcoral"))

###Overall Conclusion:
#Based on the two created models, it is believed that population growth does have an impact on total deaths by suicide. Moreover, as population size increases, so does the number of deaths by suicide
#However, increases in population size does not necessarily correlate to increases in veteran deaths by suicide. A more important factor when determining the rate of veteran deaths by suicide is time of war. Moreover, there are more veteran deaths by suicide during the middle of war compared to the end of war
#There do not seem to be significant differences between regions or parts of the country as well. It would be interesting to look at other factors as to why certain populations may or may not be more inclined to commit different actions for future research purposes
#All of this can be important when determining proper solutions to help military members in their overall health and well-being, especially during times of war


