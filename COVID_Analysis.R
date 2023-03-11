#Loading required libraries
library(readxl)
library(tidyverse)
library(Hmisc)
library(VIM)
library(ggpubr)
library(psych)
library(knitr)
library(dplyr)

#Loading required data sets
owid_data<- read.csv("owid-covid-data (2).csv")
bcg_data<-read_excel("Bacillus Calmette–Guérin (BCG) vaccination coverage.xlsx")
urb_data<- read.csv("urbanPop.csv")

#filter and prepare data set for merging
bcg_data<-bcg_data %>% filter(YEAR == '2020' & COVERAGE_CATEGORY=='OFFICIAL')
bcg_data<- bcg_data %>% dplyr::select(CODE,COVERAGE)
names(bcg_data)[1]<-"iso_code"
names(bcg_data)[2]<-"bcg_vaccination_coverage"
owid_data<- owid_data %>% filter(date == '1/3/2022')
#merge existing datasets
owid_merged <- merge(owid_data, urb_data, by="iso_code")
owid_merged <- merge(owid_merged, bcg_data, by="iso_code")

#remove missing values by filtering NA
owid_filtered_df<- owid_merged %>% filter(continent != '')
owid_filtered_df<- owid_filtered_df %>% filter(gdp_per_capita != 'NA')
owid_filtered_df<- owid_filtered_df %>% filter( median_age!= 'NA')
owid_filtered_df<- owid_filtered_df %>% filter(urban_population != 'NA')
owid_filtered_df<-owid_filtered_df %>% filter(reproduction_rate != 'NA')
owid_filtered_df<- owid_filtered_df %>% filter(bcg_vaccination_coverage != 'NA')
#apply k nearest neighbour to impute remaining values
owid_filtered_df<-VIM::kNN(owid_filtered_df)

#filter out required columns
owid_filtered_df<- owid_filtered_df %>% dplyr::select(iso_code,continent,location,date,total_cases_per_million,median_age,gdp_per_capita,bcg_vaccination_coverage,urban_population)
owid_for_viz<- owid_filtered_df %>% dplyr::select(total_cases_per_million,median_age,gdp_per_capita,bcg_vaccination_coverage,urban_population)

#basic variable details
summary(owid_for_viz)
psych::describe(owid_for_viz)

#density graphs before transformation
total_cases_per_million_density<-ggdensity((owid_for_viz$total_cases_per_million),xlab="total_cases_per_million")+stat_overlay_normal_density(color = "red", linetype = "dashed")
median_age_density<-ggdensity((owid_for_viz$median_age),xlab="median_age")+stat_overlay_normal_density(color = "red", linetype = "dashed")
gdp_per_capita_density<-ggdensity((owid_for_viz$gdp_per_capita),xlab="gdp_per_capita")+stat_overlay_normal_density(color = "red", linetype = "dashed")
bcg_vaccination_coverage_density<-ggdensity((owid_for_viz$bcg_vaccination_coverage),xlab="bcg_vaccination_coverage")+stat_overlay_normal_density(color = "red", linetype = "dashed")
urban_population_density<-ggdensity((owid_for_viz$urban_population),xlab="urban_population")+stat_overlay_normal_density(color = "red", linetype = "dashed")
ggarrange(total_cases_per_million_density, median_age_density, gdp_per_capita_density,bcg_vaccination_coverage_density,urban_population_density ,
          ncol = 5, nrow = 1, labels = "AUTO",common.legend = TRUE)

#transforming data to normal distribution
owid_filtered_df$total_cases_per_million<-log10(owid_filtered_df$total_cases_per_million)
owid_filtered_df$gdp_per_capita<-log10(owid_filtered_df$gdp_per_capita)
owid_filtered_df$median_age<-log10(owid_filtered_df$median_age)
owid_filtered_df$bcg_vaccination_coverage<-log10(max(owid_filtered_df$bcg_vaccination_coverage+1)-owid_filtered_df$bcg_vaccination_coverage)
owid_filtered_df$urban_population<-log10(owid_filtered_df$urban_population)

#density graphs after transformation
total_cases_per_million_density<-ggdensity((owid_for_viz$total_cases_per_million),xlab="total_cases_per_million")+stat_overlay_normal_density(color = "red", linetype = "dashed")
median_age_density<-ggdensity((owid_for_viz$median_age),xlab="median_age")+stat_overlay_normal_density(color = "red", linetype = "dashed")
gdp_per_capita_density<-ggdensity((owid_for_viz$gdp_per_capita),xlab="gdp_per_capita")+stat_overlay_normal_density(color = "red", linetype = "dashed")
bcg_vaccination_coverage_density<-ggdensity((owid_for_viz$bcg_vaccination_coverage),xlab="bcg_vaccination_coverage")+stat_overlay_normal_density(color = "red", linetype = "dashed")
urban_population_density<-ggdensity((owid_for_viz$urban_population),xlab="urban_population")+stat_overlay_normal_density(color = "red", linetype = "dashed")
ggarrange(total_cases_per_million_density, median_age_density, gdp_per_capita_density,bcg_vaccination_coverage_density,urban_population_density ,
          ncol = 5, nrow = 1, labels = "AUTO",common.legend = TRUE)

#Linear regression analyses and correlation plots
#GDP per capita
gdp_lm<-lm(total_cases_per_million~ gdp_per_capita, data=owid_filtered_df)
summary(gdp_lm)
tab_model(gdp_lm)
gdp_lm_plot<-ggplot(owid_filtered_df, aes(x=gdp_per_capita, y=total_cases_per_million)) +
  geom_point() +stat_cor()+
  stat_smooth(method = lm)

#Median age
age_lm<-lm(total_cases_per_million~ median_age, data=owid_filtered_df)
summary(age_lm)
tab_model(age_lm)
age_lm_plot<-ggplot(owid_filtered_df, aes(x=median_age, y=total_cases_per_million)) +
  geom_point() +stat_cor()+
  stat_smooth(method = lm)

#BCG vaccination percentage
bcg_lm<-lm(total_cases_per_million~ bcg_vaccination_coverage, data=owid_filtered_df)
summary(bcg_lm)
tab_model(bcg_lm)
bcg_lm_plot<-ggplot(owid_filtered_df, aes(x=bcg_vaccination_coverage, y=total_cases_per_million)) +
  geom_point() +stat_cor()+
  stat_smooth(method = lm)

#Urban population density
population_lm<-lm(total_cases_per_million~ urban_population, data=owid_filtered_df)
summary(population_lm)
tab_model(population_lm)
population_lm_plot<-ggplot(owid_filtered_df, aes(x=urban_population, y=total_cases_per_million)) +
  geom_point() +stat_cor()+
  stat_smooth(method = lm)

ggarrange(gdp_lm_plot, age_lm_plot, bcg_lm_plot,population_lm_plot ,
          ncol = 2, nrow = 2, labels = "AUTO",common.legend = TRUE)

#Linear analysis for low to medium and high urban population densities
population_below70<- owid_filtered_df %>% filter( owid_filtered_df$urban_population <= log10(70))
population_above70<- owid_filtered_df %>% filter( owid_filtered_df$urban_population > log10(70))

population_above70_lm<-lm(total_cases_per_million~ urban_population, data=population_above70)
summary(population_above70_lm)
population_above70_plot<-ggplot(population_above70, aes(x=urban_population, y=total_cases_per_million),xlab="urban_population_above75") +
  geom_point() +stat_cor()+
  stat_smooth(method = lm)

population_below70_lm<-lm(total_cases_per_million~ urban_population, data=population_below70)
summary(population_below70_lm)
population_below70_plot<-ggplot(population_below70, aes(x=urban_population, y=total_cases_per_million)) +
  geom_point() +stat_cor()+
  stat_smooth(method = lm)

ggarrange(population_below70_plot, population_above70_plot,
          ncol = 2, nrow = 1, labels = "AUTO",common.legend = TRUE)

#Check correlation for different urabn densities
cor(population_below70$urban_population,population_below70$total_cases_per_million )
cor(population_above70$urban_population,population_above70$total_cases_per_million )
cor(owid_filtered_df$bcg_vaccination_coverage,owid_filtered_df$total_cases_per_million )

#MLR analysis
multiple.regression <- lm((total_cases_per_million)~(gdp_per_capita)+(median_age) +bcg_vaccination_coverage+(urban_population), data=owid_filtered_df)
summary(multiple.regression)
plot(multiple.regression)

#Plotting actual vs predicted graph
ggplot(owid_filtered_df, aes(x=predict(multiple.regression), y= total_cases_per_million)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)+
  geom_abline(intercept=0, slope=1) +
  stat_regline_equation()+stat_cor()+
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')

ggplot(data=owid_filtered_df,mapping = aes(x=(gdp_per_capita),y=(total_cases_per_million))) +geom_point()
ggplot(data=owid_filtered_df,mapping = aes(x=(urban_population),y=(total_cases_per_million))) +geom_point()+geom_line()
ggplot(data=owid_filtered_df,aes(y=total_cases_per_million))+ geom_line(aes(x=COVERAGE), color="red")+ geom_line(aes(x=(people_vaccinated_per_hundred)))
 
# World map with total cases across the world
joinData <- joinCountryowid_filtered_dfMap( data1,
                                            joinCode = "ISO3",
                                            nameJoinColumn = "iso_code")
theMap <- mapCountryData( joinData, nameColumnToPlot="total_cases", addLegend=FALSE )
do.call( addMapLegend, c(theMap, legendWidth=1, legendMar = 2))

# Bubble map with BCG vaccination across the world
data("bcg_data",envir=environment(),package="rworldmap")
sPDF <- joinCountryowid_filtered_dfMap(bcg_data,joinCode = "ISO3"
                                       ,nameJoinColumn = "iso_code")
mapBubbles(sPDF, nameZSize="POP_EST",nameZColour="bcg_vaccination_coverage"
           ,colourPalette='topo',numCats=5,catMethod="quantiles",addLegend = FALSE)
