dataset<-read.csv(choose.files())
library(dplyr)
library(WriteXLS)
library('writexl')
install.packages('data.table')

install.packages('WriteXLS')
head(dataset)
ncol(dataset)
nrow(dataset)
colnames(dataset)
data.frame(dataset)
# we are beginning with data preprocessing


# 1) checking null values in the dataset 

sum(is.na(dataset))
sum(is.na(dataset$NewCases)) 
sum(is.na(dataset$NewDeaths))
sum(is.na(dataset$NewRecovered))
#df2<-data.frame(dataset[, c("Country.Region","NewCases", "NewDeaths","NewRecovered")])
# these are features with high no of na values therefore we will split them into different dataset


sum(is.na(dataset$Population)) # since there is only one row with na value we can drop the row
dataset[-(is.na(dataset$Population)==TRUE),]

sum(is.na(dataset$TotalRecovered)) #replacing the nas with mean value
dataset$TotalRecovered[is.na(dataset$TotalRecovered)] <- mean(dataset$TotalRecovered, na.rm =TRUE)

sum(is.na(dataset$Deaths.1M.pop)) #replacing the nas with median value
dataset$Deaths.1M.pop[is.na(dataset$Deaths.1M.pop)] <- mean(dataset$Deaths.1M.pop, na.rm =TRUE)


# 2) checking anomlous (inf) values in dataset 

sum(is.infinite(dataset$Deaths.1M.pop))

#replacing inf values with mean
dataset$Deaths.1M.pop[which(!is.finite(dataset$Deaths.1M.pop))]<-variable

#mean(dataset$Deaths...100.Recovered)
variable=mean(dataset$Deaths...100.Recovered) #saved 39.47385
#variable=39.47385


# 4) since the name of continents are containing '/' we have to process them

num<-grep("/",dataset$Continent,value=FALSE)
for ( i in num)
{
  print(dataset$Continent[i])
}
#since the slash is only in one continent
for ( i in num)
{
  dataset$Continent[i]="Australia"
}


# 5) we want to know the total no of cases so far has been registered per total population of each continent
df5<-dataset %>%
  group_by(Continent)%>%
  summarise_at("Population",sum)

tot_cases<-dataset %>%
  group_by(Continent)%>%
  summarise_at("TotalCases",sum)

data.frame(df5)
df5<-cbind(df5, tot_cases[,2])
mutate(df5,cases_ratio=TotalCases/Population)

# 6) splitting the dataset using country.region as FK
df1<-data.frame(dataset[, c("Country.Region","TotalCases","TotalDeaths","TotalRecovered","ActiveCases")])
df1

df2<-data.frame(dataset[, c("Country.Region","NewCases", "NewDeaths","NewRecovered")])

df3<-data.frame(dataset[,c("Country.Region","Tot.Cases.1M.pop","Deaths.1M.pop","Tests.1M.pop")])

df4<-df5

#7) preprocessing of df2
df2
sum(is.na(df2)) #dropping


# 8) now are four datasets are preprocessed for tableau representation, so we will convert them into xlsx file


write.csv(df4, file="C:\\Users\\jaskirat singh\\Downloads\\datascienceproj\\dataset4.csv", row.names = FALSE)
write.csv(df1, file="C:\\Users\\jaskirat singh\\Downloads\\datascienceproj\\dataset1.csv", row.names = FALSE)
write.csv(df3, file="C:\\Users\\jaskirat singh\\Downloads\\datascienceproj\\dataset3.csv", row.names = FALSE)