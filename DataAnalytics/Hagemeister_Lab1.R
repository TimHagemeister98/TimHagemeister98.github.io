# The packages that we needed                                                      #### 
library(dplyr) 
library(ggplot2)
library(expss)
library(formattable)
library(magrittr)
library(qwraps2)
library(data.table)
library(Hmisc)
library(rlang)


# Primary Data and data summaries                                                  ####
GarlicMustardData = read.csv("GarlicMustardData.csv")

summary(GarlicMustardData)
summary(GarlicMustardGeo)

 
# Here's how to select only certain columns:                                       ####
myvars = c("Latitude","Longitude","Altitude")
GarlicMustardGeo = GarlicMustardData[myvars]

# Here's another way to select columns:                                            ####
GarlicMustard_subset = select(GarlicMustardData,1,3:5)
View(GarlicMustardData)


# Here's how to select only certain rows:                                          ####
#If I want rows with Total Dens > 40 :
GMfiltered = GarlicMustardData %>%
  filter(TotalDens>40)


# North American Plants Filter,This is to analyze only the North American Data     ####
NorthAmericanData = GarlicMustardData %>%
  filter(Region == "NorthAm")
View(NorthAmericanData)  

# European Plants Filter, This is to analyze only the European Data                ####
EuropeanData = GarlicMustardData %>% 
  filter(Region == "Europe")
View(EuropeanData)

# Dumb way to make a histogram: Never ended up using this data                     ####
hist(NorthAmericanData$AvgAdultHeight,breaks=20)

# North American Data Histogram                                                    #####
ggplot(NorthAmericanData, aes(x=AvgAdultHeight))+
  geom_histogram(bins = 10, fill = "blue", color = "black")+
  xlab("Average Adult Height")+
  ylab("Number of plants")+
  ggtitle("Average Adult Height of North American Garlic Mustard")

# North Americian Data analysis                                                    ####
summary(NorthAmericanData$AvgAdultHeight)
mean(NorthAmericanData$AvgAdultHeight, na.rm = TRUE) 

# European Data Histogram                                                          #####
ggplot(EuropeanData, aes(x=AvgAdultHeight))+
  geom_histogram(bins = 10, fill = "red", color = "black")+
  xlab("Average Adult Height")+
  ylab("Number of plants")+
  ggtitle("Average Adult Height of European Garlic Mustard")

# European Data analysis                                                           ####
summary(EuropeanData$AvgAdultHeight)
mean(EuropeanData$AvgAdultHeight, na.rm = TRUE) 

# Comparison between Densitoy of plants by region                                  #####
ggplot(GarlicMustardData, aes(x=TotalDens, fill= Region))+
  geom_histogram(position= "dodge", bins = 10)+
  xlab("Total Density")+
  ylab("Number of plants")+
  ggtitle("Density of Garlic Mustard plants by region")

# Analysis between density of plants by region                                     ####
mean(NorthAmericanData$TotalDens, na.rm = TRUE) 
mean(EuropeanData$TotalDens, na.rm = TRUE) 


# Dotplot with Denisty of plants by region and Rainfall                            ####
ggplot(GarlicMustardData, aes(x=AvgAdultHeight, y=bio12, color=Region))+
  geom_point()+
  xlab("Average Adult Height")+
  ylab(" Rainfall ")+
  ggtitle("Density of Garlic Mustard plants by region")+
  geom_smooth(method = "lm")

# Analysis of density of plants by region and Rainfall                             ####
mean(NorthAmericanData$bio12, na.rm = TRUE) 
mean(EuropeanData$bio12, na.rm = TRUE) 

CTN <- cor.test(NorthAmericanData$AvgAdultHeight, NorthAmericanData$bio12, method = "pearson")
CTE <- cor.test(EuropeanData$AvgAdultHeight, EuropeanData$bio12, method = "pearson")
print(CTN)
print(CTE)

# Boxplot                                                                          ####

GarlicMustardData$bioComb <- interaction(GarlicMustardData$bio1, GarlicMustardData$bio2)
View(GarlicMustardData)
# This ended up not working, yes it was combining the different data points, but instead it was creating a combined variable and not putting the different box plots next to each other


ggplot(GarlicMustardData, aes(x= bioComb, y=AvgAdultHeight))+
  geom_boxplot()
  geom_point()
# I ended up combining the data points on ggplot, grid arrage, this allowed me to add multiple box plots with different variables into one graph
  
gridExtra::grid.arrange(
  ggplot(GarlicMustardData, aes(x= bio1, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Anual Mean Temperature")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio2, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Mean Diurnal Range")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
    
  ggplot(GarlicMustardData, aes(x= bio3, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Isothermality")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio4, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Temperature Seasonality")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio5, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Max Temperature of Warmest month")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio6, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Min Temperature of Coldest month")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio7, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Temperature Annual Range")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio8, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Mean Temperature of Wettest quarter")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio9, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Mean Temperature of Driest quarter")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio10, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Mean Temperature of Warmest quarter")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio11, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Mean Temperature of Coldest quarter")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio12, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Annual Precipitation")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio13, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Precipitation of Wettest Month")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio14, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Precipitation of Driest Month")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio15, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Precipitation Seasonality")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio16, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Precipitatoin of Wettest Quarter")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio17, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Precipitatoin of Driest Quarter")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none", 
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio18, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Precipitation of Warmest Quarter")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = "none",
          axis.title = element_text(size = 9)),
  
  ggplot(GarlicMustardData, aes(x= bio19, y=AvgAdultHeight, fill = Region))+
    geom_boxplot(position = position_dodge())+
    xlab("Precipitation of Coldest Quarter")+
    ylab("Average Adult Height")+
    theme_light()+
    theme(legend.position = c(1.5,.4),
          legend.key.size = unit(1.5, "cm"),
          axis.title = element_text(size = 9))+
    scale_fill_discrete(name = "Region", labels = c("Europe","North America")),
  
  nrow=5
  )

# data table summarizing the statistical differneces within each bio ####

options(qwraps_markup = "markdown")
str(GarlicMustardData)
data(GarlicMustardData)

#bio1 data
mean_sd(GarlicMustardData$bio1, denote_sd = "paren")
mean_ci(GarlicMustardData$bio1)
median_iqr(GarlicMustardData$bio1)

args(summary_table)
dplyr::summarize

our_summary <- 
  list("Bio 1" = 
        list("min" = ~ min(.data$bio1),
            "max" = ~ max(.data$bio1),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$bio1, denote_sd = "paren")),
       "Bio 2" =
       list("min" = ~ min(.data$bio2),
            "max" = ~ max(.data$bio2),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$bio2, denote_sd = "paren"))
       )

whole <- summary_table(GarlicMustardData, our_summary)
whole
WD <- data.table(whole)
formattable(WD)

wholed<- data.table(whole)
formattable(wholed)
summary_table(wholed)


GarlicMustardData %>% 
  dplyr::select(.data$bio1, .data$bio2) %>%
  qsummary(.)




















# data table try 2 and it worked!                                                  #####



expss_output_rnotebook()

W = GarlicMustardData

W = W %>% compute({AAHmix = recode(AvgAdultHeight, lo %thru% 40 ~ 1 , 40 %thru% 80 ~ 2, 80 %thru% 120 ~ 3 , 120 %thru% 160 ~ 4)})
W = apply_labels(W, AAHmix = c("0-40" = 1, "40-80" = 2, "80-120" = 3, "120-160" = 4))
W = apply_labels(W, 
                 bio1= "Anual Mean Temperature",
                 bio2= "Mean Diurnal Range",
                 bio3= "Isothermality (Bio2/Bio7)X100",
                 bio4= "Temperature Seasonality",
                 bio5= "Max Temperature of Warmest month",
                 bio6= "Min Temperature of Coldest month",
                 bio7= "Temperature Annual Range (Bio5-Bio6)",
                 bio8= "Mean Temperature of Wettest quarter",
                 bio9= "Mean Temperature of Driest quarter",
                 bio10= "Mean Temperature of Warmest quarter",
                 bio11= "Mean Temperature of Coldest quarter",
                 bio12= "Annual Precipitation",
                 bio13= "Precipitation of Wettest Month",
                 bio14= "Precipitation of Driest Month",
                 bio15= "Precipitation Seasonality (coefficient of Variation",
                 bio16= "Precipitatoin of Wettest Quarter",
                 bio17= "Precipitatoin of Driest Quarter",
                 bio18= "Precipitation of Warmest Quarter",
                 bio19= "Precipitation of Coldest Quarter",
                 Region = "Region")





W %>% 
  tab_cells(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19) %>%
  tab_cols(total(label = "#total "), Region) %>% 
  tab_stat_fun(Mean = w_mean, "std. dev." = w_sd, "valid N" = w_n, method = list) %>% 
  tab_pivot() 


W %>% 
  tab_cells(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19) %>%
  tab_cols(total(), Region %nest% AAHmix) %>% 
  tab_stat_mean_sd_n() %>%
  tab_last_sig_means(subtable_marks = "both") %>% 
  tab_pivot() %>% 
  set_caption("Table with summary statistics and significance marks")


######## ignore #########
WD <- W %>% calc_cro_mean_sd_n(bio1, list(total(), AAHmix))
WD

W %>% 
  tab_cells(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19) %>%
  tab_cols(total(label = "#total "), Region) %>% 
  tab_stat_fun(Mean = w_mean, "std. dev." = w_sd, "valid N" = w_n, method = list) %>% 
  tab_pivot()
formattable(TestA)
  
  
W %>% 
  tab_cells(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19) %>%
  tab_cols(total(), Region %nest% AAHmix) %>% 
  tab_stat_mean_sd_n() %>%
  tab_last_sig_means(subtable_marks = "both") %>% 
  tab_pivot() %>% 
  set_caption("Table with summary statistics and significance marks.")
WT
formattable(WT)
formattable(WD)



library(expss)
expss_output_rnotebook()
formattable(cro(W$Region, W$bio1)
)




W %>% 
  tab_cells(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19) %>%
  tab_cols(total(label = "#total "), Region) %>% 
  tab_stat_fun(Mean = w_mean, "std. dev." = w_sd, "valid N" = w_n, method = list) %>% 
  tab_pivot() 

  
formattable(TestA, 
       align = c("l","c","c","c","c","c","c","c","c","c","r"),
       list("Bio Indicator" = formatter("span")))














