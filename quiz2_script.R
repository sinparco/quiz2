#Load Packages
library(tidyverse)
library(psych)
library(haven)
library(apaTables)
library(dplyr)

#Load Data
raw_data <- read_csv(file="raw_data.csv")

#Fix Data
raw_data <- read_csv(file="raw_data.csv",na=c("","NA","-999"))

#Labelling Data
categorical_variables <- select(raw_data, sex)
categorical_variables$sex <- as.factor(categorical_variables$sex)
levels(categorical_variables$sex) <- list("Male"=1, "Female"=2)
sex <- categorical_variables$sex

#Creating Item Scales
pos_affect <- select (raw_data, delighted, elated, enthusiastic, excited)
neg_affect <- select (raw_data, angry, anxious, ashamed)
Neuroticism <- raw_data$Neuroticism
Extraversion <- raw_data$Extraversion

#Fixing Bad Values
is_bad_value <- pos_affect<0 | pos_affect>3
pos_affect[is_bad_value] <- NA
is_bad_value <- neg_affect<0 | neg_affect >3
neg_affect[is_bad_value] <- NA
is_bad_value <- Neuroticism<0 | Neuroticism >24
Neuroticism[is_bad_value] <- NA
is_bad_value <- Extraversion<0 | Extraversion >24
Extraversion[is_bad_value] <- NA

#Descriptive Analysis
#psych::describe(depression_items)

#Obtaining Scale Scores
pos_affect <- psych::alpha(as.data.frame(pos_affect), check.keys=FALSE)$scores
neg_affect <- psych::alpha(as.data.frame(neg_affect), check.keys=FALSE)$scores

#Combine into analytic_data
analytic_data <- cbind(categorical_variables, pos_affect, neg_affect, Neuroticism, Extraversion)

# Creating Male and Female Subsets
analytic_data_male <-  filter(analytic_data, sex=="Male")
analytic_data_male <- select(analytic_data_male, -sex)
analytic_data_female <- filter(analytic_data, sex=="Female")
analytic_data_female <- select(analytic_data_female, -sex)
#also...analytic_data <- analytic_data %>% filter(sex=="Male") %>% select(-sex)
# %>% = and then

#Saving .RData, CSV, .SAV 
save(analytic_data,file="quiz2_analytic_data_.RData")
save(analytic_data_male,file="quiz2_analytic_data_male.RData")
save(analytic_data_female,file="quiz2_analytic_data_female.RData")
write_csv(analytic_data,path="quiz2_analytic_data_.csv")
write_csv(analytic_data_male,path="quiz2_analytic_data_male.csv")
write_csv(analytic_data_female,path="quiz2_analytic_data_female.csv")

#Correlation Tables
apa.cor.table(analytic_data, filename="Table_1_Overall.doc", table.number=1)
apa.cor.table(analytic_data_male, filename="Table_2_Male.doc", table.number=2)
apa.cor.table(analytic_data_female, filename="Table_3_Female.doc", table.number=3)

#Figures
psych::pairs.panels(analytic_data)
psych::pairs.panels(analytic_data_male)
psych::pairs.panels(analytic_data_female)

#Histograms
my.hist <- ggplot(analytic_data_female,aes(Neuroticism))
my.hist <- my.hist + geom_histogram(aes(y= ..count..), binwidth=1, fill="black", color="black")
my.hist <- my.hist + labs(title="Neuroticism Score",x="Neuroticism", y="Frequency")
my.hist <- my.hist + theme_classic()
my.hist <- my.hist + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist <- my.hist + scale_x_continuous( breaks = seq(0,25,by=5) )
my.hist <- my.hist + scale_y_continuous( breaks = seq(0,150,by=10), expand=c(0,0) )
ggsave("Figure_4_Neuroticism_Histogram_Female.tiff", plot=my.hist, width=6,height=6)
print(my.hist)

my.hist2 <- ggplot(analytic_data_female,aes(neg_affect))
my.hist2 <- my.hist2 + geom_histogram(aes(y= ..count..), binwidth=1, fill="black", color="black")
my.hist2 <- my.hist2 + labs(title="Negative Affect Score",x="Negative Affect", y="Frequency")
my.hist2 <- my.hist2 + theme_classic()
my.hist2 <- my.hist2 + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                             axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist2 <- my.hist2 + scale_x_continuous( breaks = seq(0, 3,by=0.5) )
my.hist2 <- my.hist2 + scale_y_continuous( breaks = seq(0,1600,by=400), expand=c(0,0) )
ggsave("Figure_5_NegativeAffect_Histogram_Female.tiff", plot=my.hist2, width=6,height=6)
print(my.hist2)

#Scatter Plot
my.plot <- qplot(neg_affect, Neuroticism, data=analytic_data_female)
my.plot <- my.plot + geom_smooth(method = "lm" , se = FALSE, color = "black")
my.plot <- my.plot + theme_classic(14)
my.plot <- my.plot + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype = 'solid'))
my.plot <- my.plot + labs(title="", x="Negative Affect", y="Neuroticism")
my.plot <- my.plot + coord_cartesian(xlim=c(0,3), ylim=c(0,25))

print(my.plot)

ggsave("Figure_6_NA_Neuroticism_Scatter.tiff", plot=my.plot, width=6,height=6)


