library(dplyr) #data manipulation
library(readr) #input/output
library(data.table) #data manipulation
library(stringr) #string manipulation
library(caret)  #model evaluation (confusion matrix)
library(tibble) #data wrangling
library("ROSE") #over/under sampling
library("randomForest") #random forest model building
library(pROC) #ROC plots
library("MLmetrics") #Normalized Gini
library(cowplot)

setwd("C:/Users/Architect_shwet/Desktop/New folder/insurance") 
train <- as.tibble(fread('train.csv', na.strings = c("-1", "-1.0")))
test <- as.tibble(fread('test.csv', na.strings = c("-1", "-1.0")))
dim(train);dim(test)
table(train$target)
sapply(train,class)
sapply(test,class)
str(train)
str(test)
train <- train %>%
           mutate_at(vars(ends_with("cat")), funs(as.factor)) %>%
           mutate_at(vars(ends_with("bin")), funs(as.logical)) %>%
           mutate(target = as.factor(target))
  
test <- test %>%
  mutate_at(vars(ends_with("cat")), funs(as.factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical))

sapply(train,class);sapply(test,class)
str(train);str(test)
dim(test)
test$target <- NA
test <- test[,c(1,59,2:58)]
dim(test);dim(train)
combined_data <- bind_rows(train,test)
dim(combined_data)
sapply(combined_data,class)

table(combined_data$ps_reg_01)
table(combined_data$ps_reg_02)

library(cowplot)
a1 <- combined_data %>%
      ggplot(aes(ps_ind_06_bin, fill = ps_ind_06_bin)) +
      geom_bar() +
      theme(legend.position = "none")

a2 <- combined_data %>%
       ggplot(aes(ps_ind_07_bin, fill = ps_ind_07_bin)) +
       geom_bar() +
       theme(legend.position = "none")

a3 <- combined_data %>%
       ggplot(aes(ps_ind_08_bin, fill = ps_ind_08_bin)) +
       geom_bar() +
       theme(legend.position = "none")

a4 <- combined_data %>%
        ggplot(aes(ps_ind_09_bin, fill = ps_ind_09_bin)) +
        geom_bar() +
        theme(legend.position = "none")
png("Binary_Variables_1.png")
plot_grid(a1,a2,a3,a4, ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)

a1 <- combined_data %>%
  ggplot(aes(ps_ind_10_bin, fill = ps_ind_10_bin)) +
  geom_bar() +
  theme(legend.position = "none")

a2 <- combined_data %>%
  ggplot(aes(ps_ind_11_bin, fill = ps_ind_11_bin)) +
  geom_bar() +
  theme(legend.position = "none")

a3 <- combined_data %>%
  ggplot(aes(ps_ind_12_bin, fill = ps_ind_12_bin)) +
  geom_bar() +
  theme(legend.position = "none")

a4 <- combined_data %>%
  ggplot(aes(ps_ind_13_bin, fill = ps_ind_13_bin)) +
  geom_bar() +
  theme(legend.position = "none")
png("Binary_Variables_2.png")
plot_grid(a1,a2,a3,a4, ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)

#We find that some of the binary features are very unbalanced; with “FALSE” accounting 
#for the vast majority of cases. This is particularly true for the ps_ind sequence from “10” to “13”.

a1 = combined_data %>%
  ggplot(aes(ps_ind_16_bin, fill = ps_ind_16_bin)) +
  geom_bar() +
  theme(legend.position = "none")

a2 = combined_data %>%
  ggplot(aes(ps_ind_17_bin, fill = ps_ind_17_bin)) +
  geom_bar() +
  theme(legend.position = "none")

a3 = combined_data %>%
  ggplot(aes(ps_ind_18_bin, fill = ps_ind_18_bin)) +
  geom_bar() +
  theme(legend.position = "none")

a4 = combined_data %>%
  ggplot(aes(ps_calc_15_bin, fill = ps_calc_15_bin)) +
  geom_bar() +
  theme(legend.position = "none")

png("Binary_Variables_3.png")
plot_grid(a1,a2,a3,a4 ,
          ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)

a1 = combined_data %>%
  ggplot(aes(ps_calc_16_bin, fill = ps_calc_16_bin)) +
  geom_bar() +
  theme(legend.position = "none")

a2 = combined_data %>%
  ggplot(aes(ps_calc_17_bin, fill = ps_calc_17_bin)) +
  geom_bar() +
  theme(legend.position = "none")

a3 = combined_data %>%
  ggplot(aes(ps_calc_18_bin, fill = ps_calc_18_bin)) +
  geom_bar() +
  theme(legend.position = "none")

a4 = combined_data %>%
  ggplot(aes(ps_calc_19_bin, fill = ps_calc_19_bin)) +
  geom_bar() +
  theme(legend.position = "none")

a5 = combined_data %>%
  ggplot(aes(ps_calc_20_bin, fill = ps_calc_20_bin)) +
  geom_bar() +
  theme(legend.position = "none")

png("Binary_Variables_4.png")
plot_grid(a1,a2,a3,a4,a5 ,
          ncol = 2, nrow = 3)
dev.off()
rm(a1,a2,a3,a4,a5)


a1 = combined_data %>%
  ggplot(aes(ps_ind_02_cat, fill = ps_ind_02_cat)) +
  geom_bar() +
  theme(legend.position = "none")

a2 = combined_data %>%
  ggplot(aes(ps_ind_04_cat, fill = ps_ind_04_cat)) +
  geom_bar() +
  theme(legend.position = "none")

a3 = combined_data %>%
  ggplot(aes(ps_ind_05_cat, fill = ps_ind_05_cat)) +
  geom_bar() +
  theme(legend.position = "none")

a4 = combined_data %>%
  ggplot(aes(ps_car_01_cat, fill = ps_car_01_cat)) +
  geom_bar() +
  theme(legend.position = "none")

png("Category_Variables_1.png")
plot_grid(a1,a2,a3,a4 ,
          ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)

a1 = combined_data %>%
  ggplot(aes(ps_car_02_cat, fill = ps_car_02_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

a2 = combined_data %>%
  ggplot(aes(ps_car_03_cat, fill = ps_car_03_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

a3 = combined_data %>%
  ggplot(aes(ps_car_04_cat, fill = ps_car_04_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

a4 = combined_data %>%
  ggplot(aes(ps_car_05_cat, fill = ps_car_05_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

png("Category_Variables_2.png")
plot_grid(a1,a2,a3,a4,
          ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)

a1 = combined_data %>%
  ggplot(aes(ps_car_06_cat, fill = ps_car_06_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

a2 = combined_data %>%
  ggplot(aes(ps_car_07_cat, fill = ps_car_07_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

a3 = combined_data %>%
  ggplot(aes(ps_car_08_cat, fill = ps_car_08_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

a4 = combined_data %>%
  ggplot(aes(ps_car_09_cat, fill = ps_car_09_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

a5 = combined_data %>%
  ggplot(aes(ps_car_10_cat, fill = ps_car_10_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

a6 = combined_data %>%
  ggplot(aes(ps_car_11_cat, fill = ps_car_11_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

png("Category_Variables_3.png")
plot_grid(a1,a2,a3,a4,a5,a6,
          ncol = 2, nrow = 3)
dev.off()
rm(a1,a2,a3,a4,a5,a6)


table(combined_data$ps_ind_01)
table(combined_data$ps_ind_03)
table(combined_data$ps_ind_14)
table(combined_data$ps_ind_15)
sapply(combined_data,class)
a1 = combined_data %>%
  mutate(ps_ind_01 = as.factor(ps_ind_01)) %>%
  ggplot(aes(ps_ind_01, fill = ps_ind_01)) +
  geom_bar() +
  theme(legend.position = "none")

a2 = combined_data %>%
  mutate(ps_ind_03 = as.factor(ps_ind_03)) %>%
  ggplot(aes(ps_ind_03, fill = ps_ind_03)) +
  geom_bar() +
  theme(legend.position = "none")

a3 = combined_data %>%
  mutate(ps_ind_14 = as.factor(ps_ind_14)) %>%
  ggplot(aes(ps_ind_14, fill = ps_ind_14)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

a4 = combined_data %>%
  mutate(ps_ind_15 = as.factor(ps_ind_15)) %>%
  ggplot(aes(ps_ind_15, fill = ps_ind_15)) +
  geom_bar() +
  theme(legend.position = "none")

a5 = combined_data %>%
  mutate(ps_car_11 = as.factor(ps_car_11)) %>%
  ggplot(aes(ps_car_11, fill = ps_car_11)) +
  geom_bar() +
  theme(legend.position = "none")

a6 = combined_data %>%
  mutate(ps_calc_04 = as.factor(ps_calc_04)) %>%
  ggplot(aes(ps_calc_04, fill = ps_calc_04)) +
  geom_bar() +
  theme(legend.position = "none")

png("Integer_Variables_1.png")
plot_grid(a1,a2,a3,a4,a5,a6 ,
          ncol = 2, nrow = 3)
dev.off()
rm(a1,a2,a3,a4,a5,a6)

sapply(combined_data, class)

table(combined_data$ps_calc_05)
table(combined_data$ps_calc_06)
table(combined_data$ps_calc_07)
table(combined_data$ps_calc_08)
table(combined_data$ps_calc_09)
table(combined_data$ps_calc_10)
table(combined_data$ps_calc_11)
table(combined_data$ps_calc_12)
table(combined_data$ps_calc_13)
table(combined_data$ps_calc_14)

a1 <- combined_data %>%
  mutate(ps_calc_05 = as.factor(ps_calc_05)) %>%
  ggplot(aes(ps_calc_05, fill = ps_calc_05)) +
  geom_bar() +
  theme(legend.position = "none")

a2 <- combined_data %>%
  mutate(ps_calc_06 = as.factor(ps_calc_06)) %>%
  ggplot(aes(ps_calc_06, fill = ps_calc_06)) +
  geom_bar() +
  theme(legend.position = "none")

a3 <- combined_data %>%
  mutate(ps_calc_07 = as.factor(ps_calc_07)) %>%
  ggplot(aes(ps_calc_07, fill = ps_calc_07)) +
  geom_bar() +
  theme(legend.position = "none")

a4 <- combined_data %>%
  mutate(ps_calc_08 = as.factor(ps_calc_08)) %>%
  ggplot(aes(ps_calc_08, fill = ps_calc_08)) +
  geom_bar() +
  theme(legend.position = "none")

a5 <- combined_data %>%
  mutate(ps_calc_09 = as.factor(ps_calc_09)) %>%
  ggplot(aes(ps_calc_09, fill = ps_calc_09)) +
  geom_bar() +
  theme(legend.position = "none")

a6 <- combined_data %>%
  mutate(ps_calc_10 = as.factor(ps_calc_10)) %>%
  ggplot(aes(ps_calc_10, fill = ps_calc_10)) +
  geom_bar() +
  theme(legend.position = "none")

png("Integer_Variables_2.png")
plot_grid(a1,a2,a3,a4,a5,a6 ,
          ncol = 2, nrow = 3)
dev.off()
rm(a1,a2,a3,a4,a5,a6)

# ps_calc_11 has 20 unique values so it is better to do histogram
a1 = combined_data %>%
  ggplot(aes(ps_calc_11, fill = ps_calc_11)) +
  geom_histogram(binwidth = 1) +
  theme(legend.position = "none")

a2 = combined_data %>%
  mutate(ps_calc_12 = as.factor(ps_calc_12)) %>%
  ggplot(aes(ps_calc_12, fill = ps_calc_12)) +
  geom_bar() +
  theme(legend.position = "none")

a3 = combined_data %>%
  mutate(ps_calc_13 = as.factor(ps_calc_13)) %>%
  ggplot(aes(ps_calc_13, fill = ps_calc_13)) +
  geom_bar() +
  theme(legend.position = "none")
# ps_calc_14 has 28 unique values so it is better to do histogram
a4 = combined_data %>%
  ggplot(aes(ps_calc_14, fill = ps_calc_14)) +
  geom_histogram(binwidth = 1) +
  theme(legend.position = "none")

png("Integer_Variables_3.png")
plot_grid(a1,a2,a3,a4 ,
          ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)


sapply(combined_data, class)


a1 = combined_data %>%
  ggplot(aes(ps_reg_01, fill = ps_reg_01)) +
  geom_histogram(fill = "dark green", binwidth = 0.1) +
  theme(legend.position = "none")

a2 = combined_data %>%
  ggplot(aes(ps_reg_02, fill = ps_reg_02)) +
  geom_histogram(fill = "dark green",binwidth = 0.1) +
  theme(legend.position = "none")

a3 = combined_data %>%
  ggplot(aes(ps_reg_03, fill = ps_reg_03)) +
  geom_histogram(fill = "dark green",binwidth = 0.1) +
  theme(legend.position = "none")

a4 = combined_data %>%
  ggplot(aes(ps_calc_01, fill = ps_calc_01)) +
  geom_histogram(fill = "dark green",binwidth = 0.1) +
  theme(legend.position = "none")

a5 = combined_data %>%
  ggplot(aes(ps_calc_02, fill = ps_calc_02)) +
  geom_histogram(fill = "dark green",binwidth = 0.1) +
  theme(legend.position = "none")

a6 = combined_data %>%
  ggplot(aes(ps_calc_03, fill = ps_calc_03)) +
  geom_histogram(fill = "dark green",binwidth = 0.1) +
  theme(legend.position = "none")

png("Numeric_Variables_1.png")
plot_grid(a1,a2,a3,a4,a5,a6 ,
          ncol = 2, nrow = 3)
dev.off()
rm(a1,a2,a3,a4,a5,a6)


a1 = combined_data %>%
  ggplot(aes(ps_car_12, fill = ps_car_12)) +
  geom_histogram( fill = "dark green", binwidth = 0.05) +
  theme(legend.position = "none")

a2 = combined_data %>%
  ggplot(aes(ps_car_13, fill = ps_car_13)) +
  geom_histogram(fill = "dark green",binwidth = 0.1) +
  theme(legend.position = "none")

a3 = combined_data %>%
  ggplot(aes(ps_car_14, fill = ps_car_14)) +
  geom_histogram(fill = "dark green",binwidth = 0.01) +
  theme(legend.position = "none")

a4 = combined_data %>%
  ggplot(aes(ps_car_15, fill = ps_car_15)) +
  geom_histogram(fill = "dark green",binwidth = 0.1) +
  theme(legend.position = "none")

png("Numeric_Variables_2.png")
plot_grid(a1,a2,a3,a4 ,
          ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)

table(combined_data$ps_car_15)


png("Target_Variable.png")
train %>%
  ggplot(aes(target, fill = target)) +
  geom_bar() + ggtitle("Target Variable") +
  theme(legend.position = "none")
dev.off()

sum(is.na(train)); sum(is.na(test))

missing_values <- as.data.frame(colSums(is.na(combined_data)))
missing_values["var"] <- row.names(missing_values)
missing_values =  melt( missing_values, id.vars="var", value.name="no_of_NA")
missing_values <- missing_values[missing_values$no_of_NA > 0, ]


png("Columns_with_missing_values.png")
ggplot(missing_values, aes(var, no_of_NA)) + geom_bar(stat = "identity", fill = "green") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()


missing_values_train <- as.data.frame(colSums(is.na(train)))
missing_values_train["var"] <- row.names(missing_values_train)
missing_values_train <- melt(missing_values_train, id.vars = "var", value.name = "no_of_NA")
missing_values_train <- missing_values_train[missing_values_train$no_of_NA > 0, ]
png("Columns_with_missing_values_train.png")
ggplot(missing_values_train, aes(var, no_of_NA)) + geom_bar(stat = "identity", fill = "green") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()

sum(is.na(train))/(nrow(train)*ncol(train))*100
sum(is.na(test))/(nrow(test)*ncol(test))*100
sum(is.na(combined_data))/(nrow(combined_data)*ncol(combined_data))*100



a1 <- train %>%
  ggplot(aes(ps_ind_06_bin, fill = target)) +
  geom_bar() 

a2 <- train %>%
  ggplot(aes(ps_ind_07_bin, fill = target)) +
  geom_bar() 

a3 <- train %>%
  ggplot(aes(ps_ind_08_bin, fill = target)) +
  geom_bar() 

a4 <- train %>%
  ggplot(aes(ps_ind_09_bin, fill = target)) +
  geom_bar() 
png("Binary_Variables_1_target.png")
plot_grid(a1,a2,a3,a4, ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)
table(train$ps_ind_10_bin)
a1 <- train %>%
  ggplot(aes(ps_ind_10_bin, fill = target)) +
  geom_bar() 

a2 <- train %>%
  ggplot(aes(ps_ind_11_bin, fill = target)) +
  geom_bar() 

a3 <- train %>%
  ggplot(aes(ps_ind_12_bin, fill = target)) +
  geom_bar() 

a4 <- train %>%
  ggplot(aes(ps_ind_13_bin, fill = target)) +
  geom_bar() 
png("Binary_Variables_2_target.png")
plot_grid(a1,a2,a3,a4, ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)

#We find that some of the binary features are very unbalanced; with “FALSE” accounting 
#for the vast majority of cases. This is particularly true for the ps_ind sequence from “10” to “13”.

a1 = train %>%
  ggplot(aes(ps_ind_16_bin, fill = target)) +
  geom_bar() 

a2 = train %>%
  ggplot(aes(ps_ind_17_bin, fill = target)) +
  geom_bar() 

a3 = train %>%
  ggplot(aes(ps_ind_18_bin, fill = target)) +
  geom_bar() 

a4 = train %>%
  ggplot(aes(ps_calc_15_bin, fill = target)) +
  geom_bar() 

png("Binary_Variables_3_target.png")
plot_grid(a1,a2,a3,a4 ,
          ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)

a1 = train %>%
  ggplot(aes(ps_calc_16_bin, fill = target)) +
  geom_bar() 

a2 = train %>%
  ggplot(aes(ps_calc_17_bin, fill = target)) +
  geom_bar() 

a3 = train %>%
  ggplot(aes(ps_calc_18_bin, fill = target)) +
  geom_bar() 

a4 = train %>%
  ggplot(aes(ps_calc_19_bin, fill = target)) +
  geom_bar() 

a5 = train %>%
  ggplot(aes(ps_calc_20_bin, fill = target)) +
  geom_bar() 

png("Binary_Variables_4_target.png")
plot_grid(a1,a2,a3,a4,a5 ,
          ncol = 2, nrow = 3)
dev.off()
rm(a1,a2,a3,a4,a5)


a1 = train %>%
  ggplot(aes(ps_ind_02_cat, fill = target)) +
  geom_bar() 

a2 = train %>%
  ggplot(aes(ps_ind_04_cat, fill = target)) +
  geom_bar() 

a3 = train %>%
  ggplot(aes(ps_ind_05_cat, fill = target)) +
  geom_bar() 

a4 = train %>%
  ggplot(aes(ps_car_01_cat, fill = target)) +
  geom_bar() 

png("Category_Variables_1_target.png")
plot_grid(a1,a2,a3,a4 ,
          ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)

a1 = train %>%
  ggplot(aes(ps_car_02_cat, fill = target)) +
  geom_bar() +
  scale_y_log10() 

a2 = train %>%
  ggplot(aes(ps_car_03_cat, fill = target)) +
  geom_bar() +
  scale_y_log10()

a3 = train %>%
  ggplot(aes(ps_car_04_cat, fill = target)) +
  geom_bar() +
  scale_y_log10() 

a4 = train %>%
  ggplot(aes(ps_car_05_cat, fill = target)) +
  geom_bar() +
  scale_y_log10()

png("Category_Variables_2_target.png")
plot_grid(a1,a2,a3,a4,
          ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)

a1 = train %>%
  ggplot(aes(ps_car_06_cat, fill = target)) +
  geom_bar() +
  scale_y_log10() 

a2 = train %>%
  ggplot(aes(ps_car_07_cat, fill = target)) +
  geom_bar() +
  scale_y_log10() 

a3 = train %>%
  ggplot(aes(ps_car_08_cat, fill = target)) +
  geom_bar() +
  scale_y_log10() 

a4 = train %>%
  ggplot(aes(ps_car_09_cat, fill = target)) +
  geom_bar() +
  scale_y_log10() 

a5 = train %>%
  ggplot(aes(ps_car_10_cat, fill = target)) +
  geom_bar() +
  scale_y_log10() 

a6 = train %>%
  ggplot(aes(ps_car_11_cat, fill = target)) +
  geom_bar() +
  scale_y_log10() 

png("Category_Variables_3_target.png")
plot_grid(a1,a2,a3,a4,a5,a6,
          ncol = 2, nrow = 3)
dev.off()
rm(a1,a2,a3,a4,a5,a6)



a1 = train %>%
  mutate(ps_ind_01 = as.factor(ps_ind_01)) %>%
  ggplot(aes(ps_ind_01, fill = target)) +
  geom_bar() 

a2 = train %>%
  mutate(ps_ind_03 = as.factor(ps_ind_03)) %>%
  ggplot(aes(ps_ind_03, fill = target)) +
  geom_bar() 

a3 = train %>%
  mutate(ps_ind_14 = as.factor(ps_ind_14)) %>%
  ggplot(aes(ps_ind_14, fill = target)) +
  geom_bar() + scale_y_log10()

a4 = train %>%
  mutate(ps_ind_15 = as.factor(ps_ind_15)) %>%
  ggplot(aes(ps_ind_15, fill = train)) +
  geom_bar()

a5 = train %>%
  mutate(ps_car_11 = as.factor(ps_car_11)) %>%
  ggplot(aes(ps_car_11, fill = target)) +
  geom_bar() 

a6 = train %>%
  mutate(ps_calc_04 = as.factor(ps_calc_04)) %>%
  ggplot(aes(ps_calc_04, fill = target)) +
  geom_bar()

png("Integer_Variables_1_target.png")
plot_grid(a1,a2,a3,a5,a6,
          ncol = 2, nrow = 3)
dev.off()
rm(a1,a2,a3,a5,a6)





a1 <- train %>%
  mutate(ps_calc_05 = as.factor(ps_calc_05)) %>%
  ggplot(aes(ps_calc_05, fill = target)) +
  geom_bar()

a2 <- train %>%
  mutate(ps_calc_06 = as.factor(ps_calc_06)) %>%
  ggplot(aes(ps_calc_06, fill = target)) +
  geom_bar() 

a3 <- train %>%
  mutate(ps_calc_07 = as.factor(ps_calc_07)) %>%
  ggplot(aes(ps_calc_07, fill = target)) +
  geom_bar() 

a4 <- train %>%
  mutate(ps_calc_08 = as.factor(ps_calc_08)) %>%
  ggplot(aes(ps_calc_08, fill = target)) +
  geom_bar() 

a5 <- train %>%
  mutate(ps_calc_09 = as.factor(ps_calc_09)) %>%
  ggplot(aes(ps_calc_09, fill = target)) +
  geom_bar()

a6 <- train %>%
  mutate(ps_calc_10 = as.factor(ps_calc_10)) %>%
  ggplot(aes(ps_calc_10, fill = target)) +
  geom_bar() 

png("Integer_Variables_2_target.png")
plot_grid(a1,a2,a3,a4,a5,a6 ,
          ncol = 2, nrow = 3)
dev.off()
rm(a1,a2,a3,a4,a5,a6)

# ps_calc_11 has 20 unique values so it is better to do histogram
a1 = train %>%
  ggplot(aes(ps_calc_11, fill = target)) +
  geom_histogram(binwidth = 1) 

a2 = train %>%
  mutate(ps_calc_12 = as.factor(ps_calc_12)) %>%
  ggplot(aes(ps_calc_12, fill = target)) +
  geom_bar() 

a3 = train %>%
  mutate(ps_calc_13 = as.factor(ps_calc_13)) %>%
  ggplot(aes(ps_calc_13, fill = target)) +
  geom_bar()
# ps_calc_14 has 28 unique values so it is better to do histogram
a4 = target %>%
  ggplot(aes(ps_calc_14, fill = target)) +
  geom_histogram(binwidth = 1) 
png("Integer_Variables_3_target.png")
plot_grid(a1,a2,a3,a4 ,
          ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)


a1 = train %>%
  ggplot(aes(ps_reg_01, fill = target)) +
  geom_histogram( binwidth = 0.1)

a2 = train %>%
  ggplot(aes(ps_reg_02, fill = target)) +
  geom_histogram(binwidth = 0.1) 

a3 = train %>%
  ggplot(aes(ps_reg_03, fill = target)) +
  geom_histogram(binwidth = 0.1) 

a4 = train %>%
  ggplot(aes(ps_calc_01, fill = target)) +
  geom_histogram(binwidth = 0.1) 

a5 = train %>%
  ggplot(aes(ps_calc_02, fill = target)) +
  geom_histogram(binwidth = 0.1)

a6 = train %>%
  ggplot(aes(ps_calc_03, fill = target)) +
  geom_histogram(binwidth = 0.1) 

png("Numeric_Variables_1_target.png")
plot_grid(a1,a2,a3,a4,a5,a6 ,
          ncol = 2, nrow = 3)
dev.off()
rm(a1,a2,a3,a4,a5,a6)


a1 = train %>%
  ggplot(aes(ps_car_12, fill = target)) +
  geom_histogram(  binwidth = 0.05) 

a2 = train %>%
  ggplot(aes(ps_car_13, fill = target)) +
  geom_histogram(binwidth = 0.1) 

a3 = train %>%
  ggplot(aes(ps_car_14, fill = target)) +
  geom_histogram(binwidth = 0.01) 

a4 = train %>%
  ggplot(aes(ps_car_15, fill = target)) +
  geom_histogram(binwidth = 0.1) 

png("Numeric_Variables_2_target.png")
plot_grid(a1,a2,a3,a4 ,
          ncol = 2, nrow = 2)
dev.off()
rm(a1,a2,a3,a4)


png("ps_ind_06_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_06_bin, train$target), ylab = "target", xlab = "ps_ind_06_bin",
                 las = 1,
                 border = "chocolate",
                 shade = TRUE, type = c("pearson"))
dev.off()
png("ps_ind_07_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_07_bin, train$target), ylab = "target", xlab = "ps_ind_07_bin",
                 las = 1,
                 border = "chocolate",
                 shade = TRUE, type = c("pearson"))
dev.off()
png("ps_ind_08_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_08_bin, train$target), ylab = "target", xlab = "ps_ind_08_bin",
                 las = 1,
                 border = "chocolate",
                 shade = TRUE, type = c("pearson"))
dev.off()
png("ps_ind_09_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_09_bin, train$target), ylab = "target", xlab = "ps_ind_09_bin",
                 las = 1,
                 border = "chocolate",
                 shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_10_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_10_bin, train$target), ylab = "target", xlab = "ps_ind_10_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_11_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_11_bin, train$target), ylab = "target", xlab = "ps_ind_11_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_12_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_12_bin, train$target), ylab = "target", xlab = "ps_ind_12_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_13_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_13_bin, train$target), ylab = "target", xlab = "ps_ind_13_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_16_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_16_bin, train$target), ylab = "target", xlab = "ps_ind_16_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_17_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_17_bin, train$target), ylab = "target", xlab = "ps_ind_17_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_18_bin_mosaic_target.png")
mosaicplot(table(train$ps_ind_18_bin, train$target), ylab = "target", xlab = "ps_ind_18_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off() 


png("ps_calc_15_bin_mosaic_target.png")
mosaicplot(table(train$ps_calc_15_bin, train$target), ylab = "target", xlab = "ps_calc_15_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_16_bin_mosaic_target.png")
mosaicplot(table(train$ps_calc_16_bin, train$target), ylab = "target", xlab = "ps_calc_16_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_17_bin_mosaic_target.png")
mosaicplot(table(train$ps_calc_17_bin, train$target), ylab = "target", xlab = "ps_calc_17_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_18_bin_mosaic_target.png")
mosaicplot(table(train$ps_calc_18_bin, train$target), ylab = "target", xlab = "ps_calc_18_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_19_bin_mosaic_target.png")
mosaicplot(table(train$ps_calc_19_bin, train$target), ylab = "target", xlab = "ps_calc_19_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()


png("ps_calc_20_bin_mosaic_target.png")
mosaicplot(table(train$ps_calc_20_bin, train$target), ylab = "target", xlab = "ps_calc_20_bin",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()





png("ps_ind_02_cat_mosaic_target.png")
mosaicplot(table(train$ps_ind_02_cat, train$target), ylab = "target", xlab = "ps_ind_02_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_04_cat_mosaic_target.png")
mosaicplot(table(train$ps_ind_04_cat, train$target), ylab = "target", xlab = "ps_ind_04_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_05_cat_mosaic_target.png")
mosaicplot(table(train$ps_ind_05_cat, train$target), ylab = "target", xlab = "ps_ind_05_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_01_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_01_cat, train$target), ylab = "target", xlab = "ps_car_01_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_02_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_02_cat, train$target), ylab = "target", xlab = "ps_car_02_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_03_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_03_cat, train$target), ylab = "target", xlab = "ps_car_03_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_04_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_04_cat, train$target), ylab = "target", xlab = "ps_car_04_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_05_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_05_cat, train$target), ylab = "target", xlab = "ps_car_05_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_06_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_06_cat, train$target), ylab = "target", xlab = "ps_car_06_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_07_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_07_cat, train$target), ylab = "target", xlab = "ps_car_07_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_08_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_08_cat, train$target), ylab = "target", xlab = "ps_car_08_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_09_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_09_cat, train$target), ylab = "target", xlab = "ps_car_09_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_10_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_10_cat, train$target), ylab = "target", xlab = "ps_car_10_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_11_cat_mosaic_target.png")
mosaicplot(table(train$ps_car_11_cat, train$target), ylab = "target", xlab = "ps_car_11_cat",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()


str(train)


cls <- sapply(train,class)
cls
df_fac <- train %>% select(which(cls=="factor"))
categorical_cols <- names(df_fac)
df_log <- train %>% select(which(cls == "logical"))
bin_cols <- names(df_log)
df_int <- train %>% select(which(cls == "integer"))
integer_cols <- names(df_int)
df_num <- train %>% select(which(cls == "numeric"))
numeric_cols <- names(df_num)

png("ps_ind_01_mosaic_target.png")
mosaicplot(table(train$ps_ind_01, train$target), ylab = "target", xlab = "ps_ind_01",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_03_mosaic_target.png")
mosaicplot(table(train$ps_ind_03, train$target), ylab = "target", xlab = "ps_ind_03",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_14_mosaic_target.png")
mosaicplot(table(train$ps_ind_14, train$target), ylab = "target", xlab = "ps_ind_14",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_ind_15_mosaic_target.png")
mosaicplot(table(train$ps_ind_15, train$target), ylab = "target", xlab = "ps_ind_15",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_car_11_mosaic_target.png")
mosaicplot(table(train$ps_car_11, train$target), ylab = "target", xlab = "ps_car_11",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_04_mosaic_target.png")
mosaicplot(table(train$ps_calc_04, train$target), ylab = "target", xlab = "ps_calc_04",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()


png("ps_calc_05_mosaic_target.png")
mosaicplot(table(train$ps_calc_05, train$target), ylab = "target", xlab = "ps_calc_05",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()


png("ps_calc_06_mosaic_target.png")
mosaicplot(table(train$ps_calc_06, train$target), ylab = "target", xlab = "ps_calc_06",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_07_mosaic_target.png")
mosaicplot(table(train$ps_calc_07, train$target), ylab = "target", xlab = "ps_calc_07",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_08_mosaic_target.png")
mosaicplot(table(train$ps_calc_08, train$target), ylab = "target", xlab = "ps_calc_08",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_09_mosaic_target.png")
mosaicplot(table(train$ps_calc_09, train$target), ylab = "target", xlab = "ps_calc_09",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_10_mosaic_target.png")
mosaicplot(table(train$ps_calc_10, train$target), ylab = "target", xlab = "ps_calc_10",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_11_mosaic_target.png")
mosaicplot(table(train$ps_calc_11, train$target), ylab = "target", xlab = "ps_calc_11",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_12_mosaic_target.png")
mosaicplot(table(train$ps_calc_12, train$target), ylab = "target", xlab = "ps_calc_12",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_13_mosaic_target.png")
mosaicplot(table(train$ps_calc_13, train$target), ylab = "target", xlab = "ps_calc_13",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()


table(train$ps_reg_01)

png("ps_reg_01_mosaic_target.png")
mosaicplot(table(train$ps_reg_01, train$target), ylab = "target", xlab = "ps_reg_01",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_reg_02_mosaic_target.png")
mosaicplot(table(train$ps_reg_02, train$target), ylab = "target", xlab = "ps_reg_02",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

#png("ps_reg_03_mosaic_target.png")
mosaicplot(table(train$ps_reg_03, train$target), ylab = "target", xlab = "ps_reg_03",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
#dev.off()

png("ps_calc_01_mosaic_target.png")
mosaicplot(table(train$ps_calc_01, train$target), ylab = "target", xlab = "ps_calc_01",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_02_mosaic_target.png")
mosaicplot(table(train$ps_calc_02, train$target), ylab = "target", xlab = "ps_calc_02",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

png("ps_calc_03_mosaic_target.png")
mosaicplot(table(train$ps_calc_03, train$target), ylab = "target", xlab = "ps_calc_03",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()

#png("ps_car_12_mosaic_target.png")
mosaicplot(table(train$ps_car_12, train$target), ylab = "target", xlab = "ps_car_12",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
#dev.off()

#png("ps_car_13_mosaic_target.png")
mosaicplot(table(train$ps_car_13, train$target), ylab = "target", xlab = "ps_car_13",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
#dev.off()



#png("ps_car_14_mosaic_target.png")
mosaicplot(table(train$ps_car_14, train$target), ylab = "target", xlab = "ps_car_14",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
#dev.off()



png("ps_car_15_mosaic_target.png")
mosaicplot(table(train$ps_car_15, train$target), ylab = "target", xlab = "ps_car_15",
           las = 1,
           border = "chocolate",
           shade = TRUE, type = c("pearson"))
dev.off()


#Finding correlations

cont_vars <- names(combined_data)[!grepl("_cat|_bin", names(combined_data))]

cont_vars <- cont_vars[3:length(cont_vars)]

library(corrplot)
png("correlation.png")
corrplot(cor(train[, cont_vars]), type = 'lower', 
         col = colorRampPalette(c('#feeb8c', '#5a64cd'))(50),tl.col = 'grey40',
         mar = c(0,0,1,0),
         title = 'Correlation Matrix of Continuous Features')
dev.off()


cont_vars <- names(train)[!grepl("_cat|_bin", names(train))]
png("correlation.png")
corrplot(cor(train[, cont_vars][3:length(cont_vars)]), 
         type = 'lower', 
         col = colorRampPalette(c('#feeb8c', '#5a64cd'))(50),
         tl.col = 'grey40',
         mar = c(0,0,1,0),
         title = 'Correlation Matrix of Continuous Features')

dev.off()

#The group of ind variables show some correlation amongst themselves as well as 
#with some of the reg and car features. The calc features do not appear to be correlated 
#with anything. This is tough to see. Let's break the correlations down by feature group.

ind_vars <- c('target', names(train)[grepl('_ind_[0-9]{2}$', names(train))])
ind_vars
table(train$target)

train$target <- as.integer(train$target)
png("correlation of ind variables.png")

corrplot(cor(train[, ind_vars]), 
         type = 'lower', 
         col = colorRampPalette(c('#feeb8c', '#5a64cd'))(50),
         tl.col = 'grey40',
         mar = c(0,0,1,0),
         title = 'Correlation Matrix of "ind" Continuous Features')
dev.off()


reg_vars <- c('target', names(train)[grepl('_reg_[0-9]{2}$', names(train))])
png("correlation_of_reg_variables.png")
corrplot(cor(train[,reg_vars]), 
         type = 'lower', 
         col = colorRampPalette(c('#feeb8c', '#5a64cd'))(50),
         tl.col = 'grey40',
         mar = c(0,0,1,0),
         title = 'Correlation Matrix of "reg" Continuous Features')
dev.off()


car_vars <- c('target', names(train)[grepl('_car_[0-9]{2}$', names(train))])
png("correlation_of_car_variables.png")
corrplot(cor(train[,car_vars]), 
         type = 'lower', 
         col = colorRampPalette(c('#feeb8c', '#5a64cd'))(50),
         tl.col = 'grey40',
         mar = c(0,0,1,0),
         title = 'Correlation Matrix of "car" Continuous Features')

dev.off()

calc_vars <- c('target', names(train)[grepl('_calc_[0-9]{2}$', names(train))])
png("correlation_of_calc_variables.png")
corrplot(cor(train[,calc_vars]), 
         type = 'lower', 
         col = colorRampPalette(c('#feeb8c', '#5a64cd'))(50),
         tl.col = 'grey40',
         mar = c(0,0,1,0),
         title = 'Correlation Matrix of "calc" Continuous Features')

dev.off()
































