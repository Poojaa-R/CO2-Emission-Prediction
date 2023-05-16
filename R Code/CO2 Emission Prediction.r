# Installing packages and importing the library,
install.packages("superml")
install.packages("corrplot")
install.packages("plotrix")
install.packages("dplyr")
install.packages("gridExtra")

library(corrplot)
library(superml)
library(ggplot2)
library(plotrix)
library(dplyr)
library(gridExtra)

df=read.csv(file.choose()) # Reading the dataset

# Changing the column names in the dataset,

colnames(df)[which(names(df) =="Fuel.Type")] <- "Fuel"
colnames(df)[which(names(df) =="Vehicle.Class")] <- "Vehicle_Class"
colnames(df)[which(names(df) =="Engine.Size.L.")] <- "Engine_size"
colnames(df)[which(names(df) =="Fuel.Consumption.City..L.100.km.")] <- "Fuel_Consumption_City_L"
colnames(df)[which(names(df) =="Fuel.Consumption.Hwy..L.100.km.")] <- "Fuel_Consumption_Hwy_L"
colnames(df)[which(names(df) =="Fuel.Consumption.Comb..L.100.km.")] <- "Fuel_Consumption_Comb_L"
colnames(df)[which(names(df) =="Fuel.Consumption.Comb..mpg.")] <- "Fuel_Consumption_Comb_M"
colnames(df)[which(names(df) =="CO2.Emissions.g.km.")] <- "C02_Emission"

print(head(df)) # The head of the dataset

print(nrow(df)) # The number of rows in the dataset
print(ncol(df)) # The number of columns in the dataset

df<- na.omit(df) # Dropping null values in the dataset

datatypes_col<-sapply(df,class) # To check the datatypes of the dataset
print(datatypes_col)

All_col=colnames(df) # All columns in the dataset
numeric_cols<-colnames(df[,sapply(df,is.numeric)]) # To find numeric columns in the dataset
nonnumeric_cols<-setdiff(All_col,numeric_cols) # To find non-numeric columns in the dataset

print(All_col)
print(numeric_cols)
print(nonnumeric_cols)

boxplot(df$C02_Emission~df$Cylinders) # Box plot for the Cylinders and Carbon dioxide Emission

counts<-table(df$Vehicle_Class)
print(counts)
barplot(counts, main="Frequency of Vechicle Class",xlab="Vechicle Class",horiz=TRUE)

# Outlier detection for all the numerical columns,

a=df$Fuel_Consumption_City_L
b=df$Fuel_Consumption_Hwy_L
c=df$Fuel_Consumption_Comb_L
d=df$Fuel_Consumption_Comb_M

par(mfrow = c(1, 3))
boxplot(a, main = "Boxplot",col="red")
boxplot(b, main = "Boxplot",col="green")
boxplot(c, main = "Boxplot",col="blue")
boxplot(d, main = "Boxplot",col="violet")


# Since the datatypes contains non-numerical values, the label encoding is performed to create the model.

encoder = LabelEncoder$new()
df$Make=encoder$fit_transform(df$Make)
df$Model=encoder$fit_transform(df$Model)
df$Vehicle_Class=encoder$fit_transform(df$Vehicle_Class)
df$Transmission=encoder$fit_transform(df$Transmission)
df$Fuel=encoder$fit_transform(df$Fuel)
print(head(df))

# Correlation matrix of the dataset,

M<-cor(df)
corrplot(M, method = "number")

panel.hist<-function(x, ...) {
  usr<-par("usr")
  on.exit(par(usr))
  par(usr=c(usr[1:2],0,1.5))
  his<-hist(x, plot = FALSE)
  breaks<-his$breaks
  nB<-length(breaks)
  y<-his$counts
  y<-y/max(y)
  rect(breaks[-nB],0,breaks[-1],y,col=rgb(0, 1, 1, alpha = 0.5), ...)
}

new_df=df[,c("Cylinders","Fuel_Consumption_Comb_M","Fuel_Consumption_Comb_L","Fuel_Consumption_Hwy_L",
             "Fuel_Consumption_City_L","C02_Emission")]

pairs(new_df,diag.panel = panel.hist) # Pair-wise plot for the attributes in the dataset


# Constructing the X_train and y_train
X_train=df[,names(df)!="C02_Emission"] # Creating the X_train by dropping the output column

y_train=df$C02_Emission # Creating the y_train with the output column


# Building the Multiple linear regression models,

head(df)
mlr<-lm(df$C02_Emission~Make+Model+Vehicle_Class+Engine_size+Cylinders+Transmission+Fuel+Fuel_Consumption_City_L+Fuel_Consumption_Hwy_L+Fuel_Consumption_Comb_L+Fuel_Consumption_Comb_M,data=df)
print(mlr)
summary(mlr)

# From the heat map, it is shown that the correlation of the following features are very less, 1) Make 2) Model 3)Transmission.
# Therefore, the above features can be dropped from the dataset.

df<-select(df,-c(Make,Model,Transmission))

# Creating the Multiple linear regression model after dropping the columns,

mlr_1<-lm(df$C02_Emission~Vehicle_Class+Engine_size+Cylinders+Fuel+Fuel_Consumption_City_L+Fuel_Consumption_Hwy_L+Fuel_Consumption_Comb_L+Fuel_Consumption_Comb_M,data=df)
print(mlr_1)
summary(mlr)

# The accuracy of the model is not changed since the following attributes 1) Make 2) Model 3)Transmission has no impact on the prediction.

# Anova table is found for the dataset,

a<-aov(df$C02_Emission~Vehicle_Class+Engine_size+Cylinders+Fuel+Fuel_Consumption_City_L+Fuel_Consumption_Hwy_L+Fuel_Consumption_Comb_L+Fuel_Consumption_Comb_M,data=df)
summary(a)
anova(a)


