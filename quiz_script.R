#Load Packages
library(tidyverse)
library(haven)
library(apaTables)
##Tidyverse loads ggplot2 and dplyr

#Load Data
bfi_data <- psych::bfi

#Labelling Data
categorical_variables <- select(bfi_data, gender, education, age)

categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Males"=1, "Females"=2)

#Creating Item Scales
agreeableness <- select (bfi_data, A1, A2, A3, A4, A5)
extraversion <- select (bfi_data, E1, E2, E3, E4, E5)
neuroticism <- select (bfi_data, N1, N2, N3, N4, N5)

#Reversing Items
agreeableness <- mutate(agreeableness, A1=7-A1)
extraversion <- mutate(extraversion, E1=7-E1)
extraversion <- mutate(extraversion, E2=7-E2)

#Fixing Bad Values
is_bad_value <- agreeableness<0 | agreeableness>6
agreeableness[is_bad_value] <- NA
is_bad_value <- extraversion<0 | extraversion >6
extraversion[is_bad_value] <- NA
is_bad_value <- neuroticism<0 |  neuroticism >6
neuroticism[is_bad_value] <- NA

#Obtaining Scale Scores
agreeableness <- psych::alpha(as.data.frame(agreeableness), check.keys=FALSE)$scores
extraversion <- psych::alpha(as.data.frame(extraversion), check.keys=FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuroticism), check.keys=FALSE)$scores

#Combine into analytic_data
analytic_data <- cbind(agreeableness, extraversion, neuroticism, categorical_variables)
save(analytic_data,file="analytic_data.csv")

-----------------------------------------------------------------------
#### PART 2: CONDUCT ANALYSES ####
# 1 Correlation Tables
analytic_data_nogender <- select(analytic_data, -gender)
apa.cor.table(analytic_data_nogender, filename="Table1.doc", table.number=1)

#2 Correlation table for Just Men Over 40
analytic_data_men40 <- select(analytic_data, gender=="Males") %>% filter(age>40)
analytic_data_men40 <- select(analytic_data_men40, -gender)
apa.cor.table(analytic_data_men40, filename="Table2.doc", table.number=2)

# 4 Scatter Plot
my.plot <- qplot(agreeableness, extraversion, data=analytic_data_men40)
my.plot <- my.plot + theme_classic(14)
my.plot <- my.plot + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype = 'solid'))
my.plot <- my.plot + labs(title="", x="Agreeableness", y="Extraversion")
print(my.plot)
ggsave("Figure1.pdf", plot=my.plot, width=6,height=6)






