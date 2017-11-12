#Project : Nursing Home Compare
#Group Name : DarkLooters
#Members: Ravindra Rashmi Dasappa
#          Virinchi Ande
#          Aishwarya Kesari
#          Sravani Kannelur
#          Amarnadh
#**********************************************************************************************************
#input data set Deficiencies into Data Frame "Deficiency"
deficiency <- read.csv(file="C:\\Users\\virin\\Desktop\\KDD\\Project\\deficiencies.csv", stringsAsFactors = FALSE)

#To view the dataset
View(deficiency)
names(deficiency)
str(deficiency)
attributes(deficiency)

#show what type of variable each one is
sapply(deficiency, class)

## Show the first ten records 
deficiency[1:10,]
head(deficiency)
tail(deficiency)

#To determine in which Provider of medicare has how many deficiency complaints 
w = table(deficiency$provnum)
t = as.data.frame(w)
t

#To determine in which Provider name of medicare has how many deficiency complaints 
factor(deficiency$Provname)
w = table(deficiency$Provname)
t = as.data.frame(w)
t
head(arrange(t, Freq, Var1))
tail(arrange(t, Freq, Var1))

#To determine in which Provider of medicare are distributed across the state
w = table(deficiency$address)
t = as.data.frame(w)
t

#To determine provider of medicare are distributed across which cities in the states
factor(deficiency$city)
w = table(deficiency$city)
t = as.data.frame(w)
t
arrange(t, Freq, Var1)

##To determine provider of medicare are distributed across which states
factor(deficiency$state)
barplot(table(deficiency$state))

#To identify the frequency of Zip code
table(deficiency$zip)
barplot(table(deficiency$zip))

#To determine the types of survey done and their frequency
factor(deficiency$SurveyType)
barplot(table(deficiency$SurveyType))

#To determine on which date the survey was done in the providers of medicare
factor(deficiency$survey_date_output)
w = table(deficiency$survey_date_output)
t = as.data.frame(w)
t
head(arrange(t, Freq, Var1))
tail(arrange(t, Freq, Var1))

#The determine the type of deficiencies detected which are represented by the prefixes
factor(deficiency$defpref)
table(deficiency$defpref)
pie(table(deficiency$defpref))

#To identify the distribution of deficiency tag
hist(deficiency$tag)

#To classify the type of description associated with the tag and their frequency
factor(deficiency$tag_desc)
table(deficiency$tag_desc)
library(dplyr)
arrange(deficiency , tag, tag_desc)

#Associating the tag along with tag description
deduped.data <- unique(deficiency[, 10:11])
deduped.data

#To identify the cycles
unique(deficiency$cycle)
barplot(table(deficiency$cycle))

#To determine how severe a deficiency is in all providers
unique(deficiency$scope)
barplot(table(deficiency$scope))

#determing the status of defieciency corrected by the providers
unique(deficiency$defstat)
pie(table(deficiency$defstat))

#To determine on which date the survey was whether corrected or not  by the providers of medicare
unique(deficiency$statdate)
w = table(deficiency$statdate)
t = as.data.frame(w)
t
head(arrange(t, Freq, Var1))
tail(arrange(t, Freq, Var1))

#Determing how many cycles of inspection are more
unique(deficiency$cycle)
table(deficiency$cycle)
pie(table(deficiency$cycle))

#Determining the number of standard deficiencies based on category
unique(deficiency$standard)
table(deficiency$standard)
pie(table(deficiency$standard))

#Determining the number of complaints deficiency 
unique(deficiency$complaint)
table(deficiency$complaint)
pie(table(deficiency$complaint))

#Determining on which date the processing of deficiencies has been done
#Observation: As per the dataset consider Inspection cycle attribute if it is 1 then the survey date
#and the file date fall between one year if it is 2 then it fall between 12-24 months and the correction date 
#is between the survey date and file date.

#grouping the provider names with provider number, address, city, state and zip
deduped.data <- unique(deficiency[, 1:6])
deduped.data

#lists the number of cities in which providers has deficiencies based on state
library(sqldf)
sqldf('SELECT  provname, city, state, zip from deficiency GROUP BY state, city')
#lists the no of cities based on each state
library(data.table) 
setDT(deficiency)[, .(count = uniqueN(city)), by = state]

#count of survey types based on states
def_subset <- table(deficiency$SurveyType, deficiency$state )
barplot(def_subset,
        legend = rownames(def_subset),
        col = c("blue", "red"),
        ylim = c(0, 60000),
        ylab = "Count",
        xlab = "state",
        main = "Comparison Bar Chart:
        Survey type for each state")
box(which = "plot",
    lty = "solid",
    col="black")

#normalised count of survey types based on states
library(ggplot2)
ggplot() +
  geom_bar(data = deficiency,
           aes(x = factor(deficiency$state),
               fill = factor(deficiency$SurveyType)),
           position = "fill") +
  scale_x_discrete("state") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="deficiency")) +
  scale_fill_manual(values=c("blue", "red"))

#scope serverity based on states
def_subset <- table(deficiency$scope, deficiency$state )
barplot(def_subset,
        legend = rownames(def_subset),
        col = c("blue", "red", "black", "brown", "pink", "light blue", "green", "grey", "violet", "purple", "orange"),
        ylim = c(0, 60000),
        ylab = "Count",
        xlab = "state",
        main = "Comparison Bar Chart:
        scope of severity for each state")
box(which = "plot",
    lty = "solid",
    col="black")

#normalised scope serverity based on states
library(ggplot2)
ggplot() +
  geom_bar(data = deficiency,
           aes(x = factor(deficiency$state),
               fill = factor(deficiency$scope)),
           position = "fill") +
  scale_x_discrete("state") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="scope")) +
  scale_fill_manual(values=c("blue", "red", "black", "brown", "pink", "light blue", "green", "grey", "violet", "purple", "orange"))

# no of tags along with tag_desc are in which state
library(sqldf)
sqldf('SELECT state, tag, count(tag), tag_desc from deficiency GROUP BY state, tag')

#deficiency prefix based on states
def_subset <- table(deficiency$defpref, deficiency$state )
barplot(def_subset,
        legend = rownames(def_subset),
        col = c("blue", "red"),
        ylim = c(0, 60000),
        ylab = "Count",
        xlab = "state",
        main = "Comparison Bar Chart:
        deficiency prefix for each state")
box(which = "plot",
    lty = "solid",
    col="black")

#normalised deficiency prefix based on states
library(ggplot2)
ggplot() +
  geom_bar(data = deficiency,
           aes(x = factor(deficiency$state),
               fill = factor(deficiency$defpref)),
           position = "fill") +
  scale_x_discrete("state") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="deficiency prefix")) +
  scale_fill_manual(values=c("blue", "red"))

#deficiency status based on states
def_subset <- table(deficiency$defstat, deficiency$state )
barplot(def_subset,
        legend = rownames(def_subset),
        col = c("blue", "red", "black" , "yellow", "pink", "violet"),
        ylim = c(0, 60000),
        ylab = "Count",
        xlab = "state",
        main = "Comparison Bar Chart:
        deficiency status for each state")
box(which = "plot",
    lty = "solid",
    col="black")

#normalised deficiency status based on states
library(ggplot2)
ggplot() +
  geom_bar(data = deficiency,
           aes(x = factor(deficiency$state),
               fill = factor(deficiency$defstat)),
           position = "fill") +
  scale_x_discrete("state") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="deficiency status")) +
  scale_fill_manual(values=c("blue", "red", "black" , "yellow", "pink", "violet"))

#cycle inspection based on scope
def_subset <- table(deficiency$cycle, deficiency$scope )
barplot(def_subset,
        legend = rownames(def_subset),
        col = c("blue", "red", "black"),
        ylim = c(0, 200000),
        ylab = "Count",
        xlab = "scope severity ",
        main = "Comparison Bar Chart:
        cycle inspection for each scope")
box(which = "plot",
    lty = "solid",
    col="black")

#normalised cycle inspection based on scope
library(ggplot2)
ggplot() +
  geom_bar(data = deficiency,
           aes(x = factor(deficiency$scope),
               fill = factor(deficiency$cycle)),
           position = "fill") +
  scale_x_discrete("scope") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="cycles")) +
  scale_fill_manual(values=c("blue", "red", "black"))


#density of tags
plot(density(deficiency$tag))

#Summary of the deficiencies dataset
summary(deficiency)

#The no of surveys being done in a state on a particular date
library(sqldf)
sqldf('select state,survey_date_output,count(survey_date_output) from deficiency group by state,survey_date_output')

#the no of deficiencies solved on a particular date for each state
library(sqldf)
sqldf('select state,statdate,count(statdate) from deficiency group by state,statdate')
#Determining the quantiles 
quantile(deficiency$zip)
quantile(deficiency$tag)
quantile(deficiency$cycle)

#finding the missing values in each column
sum(deficiency$statdate == "")
sum(deficiency$provnum=="")
sum(deficiency$Provname == "")
sum(deficiency$address=="")
sum(deficiency$SurveyType == "")
sum(deficiency$city=="")
sum(deficiency$zip == "")
sum(deficiency$state=="")
sum(deficiency$survey_date_output == "")
sum(deficiency$defpref=="")
sum(deficiency$tag == "")
sum(deficiency$tag_desc=="")
sum(deficiency$scope == "")
sum(deficiency$defstat=="")
sum(deficiency$standard == "")
sum(deficiency$cycle=="")
sum(deficiency$complaint == "")
sum(deficiency$filedate=="")


#Observations:
# 1. When we checked for the columns with missing values. we found that statdate has missing values.
#But based on our observation, those missing values are not due to the data being missed but to represent
#the fact that those providers has no date of correction of their deficiencies.
# 2. In the dataset we have chosen, most of the columns are character type columns and we have tag column that is int
#but those tags are predefined and are assigned to the providers based on the deficiency. so it doesn't make any sense 
#to find outliers for that tag variable.

provider_info <- read.csv(file="C:\\Users\\virin\\Desktop\\KDD\\Project\\provider_info.csv", stringsAsFactors = FALSE)
library(sqldf)

comb <- sqldf('select d.*,p.BEDCERT,p.RESTOT,p.overall_rating,p.survey_rating,p.quality_rating,p.staffing_rating
              from deficiency d, provider_info p where d.provnum = p.provnum ')
View(comb)

#determining to know the distribution of certified beds
unique(comb$BEDCERT)
par(mfrow = c(1,1))
qqnorm(comb$BEDCERT,
       datax = TRUE,
       col = "red",
       ylim = c(1,1000),
       main = "normal Q-Q Plot of certified beds of provider")
qqline(comb$BEDCERT,
       col = "blue",
       datax = TRUE)

# Simulate from a Normal distribution of certified beds
x <- rnorm(1000000,
            mean = mean(invsqrt_bedcert),
            sd = sd(invsqrt_bedcert))
par(mfrow = c(1,1))
hist(invsqrt_bedcert,
      breaks = 30,
      xlim = c(0.0268,  0.7071068),
      col = "lightblue",
      prob = TRUE,
      border = "black",
      xlab = "Inverse Square Root of Certified Beds",
      ylab = "Counts",
      main = "Histogram of Inverse Square Root of certified beds") 
box(which = "plot",
    lty = "solid",
    col="black") 
lines(density(x),
      col = "red")

#min-max normalization
mmnorm.bedcert <- (comb$BEDCERT - min(comb$BEDCERT))/(max(comb$BEDCERT) - min(comb$BEDCERT))
mmnorm.bedcert

#z-score
zscore.bedcert <- (comb$BEDCERT - mean(comb$BEDCERT))/sd(comb$BEDCERT)
zscore.bedcert

#natural log transformation
natlog_bedcert <- log(comb$BEDCERT)
natlog_bedcert

#inverse square root transformation
invsqrt_bedcert <- 1/sqrt(comb$BEDCERT)
invsqrt_bedcert

# Skewness 
# right skew mean > medium and skew is positive, 
# no skew is zero, left skew is negative
#calculate skewness with z score standardization (no effect on data)
bedcert_skew <- (3*(mean(comb$BEDCERT) -
                     median(comb$BEDCERT)))/sd(comb$BEDCERT)
zscore.bedcert_skew <- (3*(mean(zscore.bedcert) -
                            median(zscore.bedcert)))/
  sd(zscore.bedcert)
bedcert_skew; zscore.bedcert_skew

#find the skewness: 
lnbedcert_skew <- (3*(mean(natlog_bedcert) -
                       median(natlog_bedcert)))/
  sd(natlog_bedcert)
lnbedcert_skew

#side by side histograms of certified beds and z-score of certified beds
par(mfrow = c(1,2))
hist(comb$BEDCERT, breaks = 30,
     xlim = c(1,1500),
     main = "histogram of certified beds",
     xlab = "number of beds",
     ylab = "counts")
box(which = "plot",
    lty = "solid",
    col="black")
hist(zscore.bedcert,
     breaks = 30,
     xlim = c(-3,4),
     main = "histogram of z-score of certified beds",
     xlab = "z-score of certified beds",
     ylab = "counts")
box(which = "plot",
    lty = "solid",
    col = "black")

#inverse square root skewness
invsqweight_skew <-(3*(mean(invsqrt_bedcert)-median(invsqrt_bedcert)))/sd(invsqrt_bedcert)
invsqweight_skew

#boxplot for certified beds
boxplot(comb$BEDCERT)

#determining the distribution of RESTOT based on BEDSTAT
plot(comb$BEDCERT, comb$RESTOT, xlim = c(0, 1500),
     ylim = c(0, 1500),
     xlab = "BEDCERT",
     ylab = "RESTOT",
     main = "Scatterplot of RESTOT by BEDCERT", type = "p",
     pch = 16,
     col = "blue")
#Add open black circles 
points(comb$BEDCERT,
      comb$RESTOT, type = "p", col = "red")

#combining the attributes from two different datasets
library(sqldf)
predict_data <- sqldf('select provnum,PROVNAME,cmplnt_cnt,FINE_CNT,FINE_TOT,TOT_PENLTY_CNT,
                 overall_rating,survey_rating,quality_rating,staffing_rating,BEDCERT,RESTOT from provider_info')
str(predict_data)

#determing the missing values in various rating variables
library(mice)
init = mice(predict_data, maxit=0) 
meth = init$method
predM = init$predictorMatrix

predM[, c("provnum","PROVNAME","BEDCERT","RESTOT")]=0

meth[c("cmplnt_cnt","FINE_CNT","FINE_TOT","TOT_PENLTY_CNT")]=""

meth[c("overall_rating","survey_rating","quality_rating","staffing_rating")]="norm"

set.seed(103)
imputed_initial = mice(predict_data, method=meth, predictorMatrix=predM, m=5)

imputed_initial <- complete(imputed_initial)
imputed_initial[,c(-1,-2)] <-round(imputed_initial[,c(-1,-2)],0)
names(imputed_initial)
#imputed <- round(imputed_initial[3:12],digits=0)

sapply(imputed_initial, function(x) sum(is.na(x)))
library(sqldf)
final_data <- sqldf('select d.*,p.BEDCERT,p.RESTOT,p.overall_rating,p.survey_rating,p.quality_rating,p.staffing_rating,
              p.cmplnt_cnt,p.FINE_CNT,P.FINE_TOT,P.TOT_PENLTY_CNT 
                    from deficiency d, imputed_initial p where d.provnum = p.provnum')
names(final_data)
sapply(final_data, function(x) sum(is.na(x)))

#The values used to determine the boxplot are for each provider considered during all cycle inspections
boxplot(predict_data$FINE_CNT)
boxplot(predict_data$cmplnt_cnt)
boxplot(predict_data$FINE_TOT)
boxplot(predict_data$TOT_PENLTY_CNT)

#determing if the attributes have NA
sum(final_data$FINE_CNT=="NA")
sum(final_data$FINE_TOT=="NA")
sum(final_data$TOT_PENLTY_CNT=="NA")
sum(final_data$BEDCERT=="NA")
sum(final_data$RESTOT=="NA")
sum(final_data$survey_rating=="NA")
sum(final_data$overall_rating=="NA")
sum(final_data$quality_rating=="NA")
sum(final_data$staffing_rating=="NA")

dfvr<- imputed_initial[,-13]
write.csv(dfvr, file="dfvr.csv")

#We may think that the abve attributes consists of outliers from seeing the above boxplots but
#as these variabls are used in predicitng the missing values these attributes should not be altered

#dividing the dataset into training and test
imputed_initial$part <- runif(length(imputed_initial$provnum), min = 0,
                          max = 1)
training <- imputed_initial[imputed_initial$part <= 0.75,] 
testing <- imputed_initial[imputed_initial$part > 0.75,] 
#training[1:5, c(2,5,8,10,21, 29)] 
#testing[1:5, c(2,5,8,10,21, 29)]

#Remove the target variable, Rating, from the testing data
names(testing)
testing <- testing[,-7] 
names(testing)


#Remove the partitioning variable from both data sets

testing <- testing[,-12] 
names(testing)

names(training)
training <- training[,-13] 
names(training)

#For the continuous variable certified beds we can perform t-test and test for difference in means
t<-t.test(training$BEDCERT,testing$BEDCERT)
t$p.value
#Since the p-value is large, there is no evidence that the mean number of certified beds
#differs between the training data set and the test data set. For this variable at least, 
#the partition seems valid.

#For the cycle varaible perform the chi-squared test and test to  determine whether significant differences 
#exist between the multinomial proportions of the two data sets
a <- sum(training$cycle==1)
b <- sum(training$cycle==2)
c <- sum(training$cycle==3)

d <- sum(testing$cycle==1)
e <- sum(testing$cycle==2)
f <- sum(testing$cycle==3)

chi_table<- as.table(rbind(c(a, b, c),
                           c(d, e, f))) 
dimnames(chi_table) <- list(Data.Set =c("Training Set", "Test Set"),
                            Status = c("1", "2", "3"))
Xsq_data <- chisq.test(chi_table)
Xsq_data$statistic
Xsq_data$p.value
Xsq_data$expected
#Because this p-value is large, there is no evidence that the observed
#frequencies represent proportions that are significantly different for the training and test data sets.
#In other words, for this variable, the partition is valid.
#However, the data that is being partioned into training and test data varies based on the random values assigned to the
#variable part from 0 to 1. So as the values for part variable in the dataset changes, the training and test data varies which implies the coressponding results associated 
#with the statistical tests also changes.we have represented the analysis based on the training and testing data
#that arrived when we were doing this.

#Clustering based on the numerical data using K-means
clustering_data  <- sqldf('select cmplnt_cnt,FINE_CNT,FINE_TOT,TOT_PENLTY_CNT,
                 overall_rating,survey_rating,quality_rating,staffing_rating,BEDCERT,RESTOT from final_data')
n <- length(clustering_data)
#Declare number of bins and bin indicator 
nbins <- 3
whichbin <- c(rep(0, n))
kmeansclustering <- kmeans(clustering_data,
                           centers = nbins)
whichbin <- kmeansclustering$cluster;
whichbin
#clustering for mixed data i.e categorical and numerical
library(klaR)
c1 <- kmodes(final_data, 5, 1, weighted = FALSE)


data <- sqldf('select d.*,p.BEDCERT,p.RESTOT,p.overall_rating,p.survey_rating,p.quality_rating,p.staffing_rating,
              p.cmplnt_cnt,p.FINE_CNT,P.FINE_TOT,P.TOT_PENLTY_CNT 
                    from deficiency d, provider_info p where d.provnum = p.provnum')

View(data)

#determining to know the distribution of RESTOT
unique(data$RESTOT)
par(mfrow = c(1,1))
qqnorm(data$RESTOT,
       datax = TRUE,
       col = "red",
       ylim = c(1,1000),
       main = "normal Q-Q Plot of no of residents in certified beds of provider")
qqline(data$RESTOT,
       col = "blue",
       datax = TRUE)

#inverse square root transformation
invsqrt_restot <- 1/sqrt(data$RESTOT)
invsqrt_restot

#inverse square root skewness
invsqrestot_skew <-(3*(mean(invsqrt_restot)-median(invsqrt_restot)))/sd(invsqrt_restot)
invsqrestot_skew

# Simulate from a Normal distribution
x <- rnorm(1000000,
           mean = mean(invsqrt_restot),
           sd = sd(invsqrt_restot))
par(mfrow = c(1,1))
hist(invsqrt_restot,
     breaks = 30,
     xlim = c(0.03634562,  1),
     col = "lightblue",
     prob = TRUE,
     border = "black",
     xlab = "Inverse Square Root of residents occupied Certified Beds",
     ylab = "Counts",
     main = "Histogram of Inverse Square Root of residents occupied certified beds") 
box(which = "plot",
    lty = "solid",
    col="black") 
lines(density(x),
      col = "red")

#min-max normalization
mmnorm.restot <- (data$RESTOT - min(data$RESTOT))/(max(data$RESTOT) - min(data$RESTOT))
mmnorm.restot

#z-score
zscore.restot <- (data$RESTOT - mean(data$RESTOT))/sd(data$RESTOT)
zscore.restot

#natural log transformation
natlog_restot <- log(data$RESTOT)
natlog_restot



# Skewness 
# right skew mean > medium and skew is positive, 
# no skew is zero, left skew is negative
#calculate skewness with z score standardization (no effect on data)
restot_skew <- (3*(mean(data$RESTOT) -
                      median(data$RESTOT)))/sd(data$RESTOT)
zscore.restot_skew <- (3*(mean(zscore.restot) -
                             median(zscore.restot)))/
  sd(zscore.restot)
restot_skew; zscore.restot_skew

#find the skewness: 
lnrestot_skew <- (3*(mean(natlog_restot) -
                        median(natlog_restot)))/
  sd(natlog_restot)
lnrestot_skew

#side by side histograms of residents occupied certified beds and z-score of residents occupied certified beds
par(mfrow = c(1,2))
hist(data$RESTOT, breaks = 30,
     xlim = c(1,500),
     main = "histogram of residents ocuupied certified beds",
     xlab = "number of occupied beds",
     ylab = "counts")
box(which = "plot",
    lty = "solid",
    col="black")
hist(zscore.restot,
     breaks = 30,
     xlim = c(-4,4),
     main = "histogram of z-score of residents occupied certified beds",
     xlab = "z-score of residents occupied certified beds",
     ylab = "counts")
box(which = "plot",
    lty = "solid",
    col = "black")


#boxplot to identify the outliers
boxplot(data$RESTOT)

#scatterplot to identiffy the distribution of BEDCERT based on RESTOT
plot(data$RESTOT, data$BEDCERT, xlim = c(0, 500),
     ylim = c(0, 1500),
     xlab = "BEDCERT",
     ylab = "RESTOT",
     main = "Scatterplot of BEDCERT by RESTOT", type = "p",
     pch = 16,
     col = "blue")
#Add open black circles 
points(data$RESTOT,
       data$BEDCERT, type = "p", col = "red")

#no of hospitals with their respective ratings
factor(provider_info$overall_rating)
barplot(table(provider_info$overall_rating))

#non-normalised plot for each state based on overall rating
def_subset <- table( provider_info$overall_rating, provider_info$STATE)
barplot(def_subset,
        legend = rownames(def_subset),
        col = c("blue", "red", "black", "yellow", "violet"),
        ylim = c(0, 1500),
        ylab = "Count",
        xlab = "States ",
        main = "Comparison Bar Chart:
        overall rating for each state")
box(which = "plot",
    lty = "solid",
    col="black")

#normalised plot for each state based on overall rating
library(ggplot2)
ggplot() +
  geom_bar(data = provider_info,
           aes(x = factor(provider_info$STATE),
               fill = factor(provider_info$overall_rating)),
           position = "fill") +
  scale_x_discrete("state") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="provider_info")) +
  scale_fill_manual(values=c("blue", "red", "black", "yellow", "violet"))

#non-normalised plot for each survey type based on overall rating
def_subset <- table( data$overall_rating, data$SurveyType)
barplot(def_subset,
        legend = rownames(def_subset),
        col = c("blue", "red", "black", "yellow", "violet"),
        ylim = c(0, 350000),
        ylab = "Count",
        xlab = "Survey Type ",
        main = "Comparison Bar Chart:
        overall rating for each survey type")
box(which = "plot",
    lty = "solid",
    col="black")

#normalised plot for each survey type based on overall rating
library(ggplot2)
ggplot() +
  geom_bar(data = data,
           aes(x = factor(data$SurveyType),
               fill = factor(data$overall_rating)),
           position = "fill") +
  scale_x_discrete("survey type") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="data")) +
  scale_fill_manual(values=c("blue", "red", "black", "yellow", "violet"))

#non-normalised plot for each cycle based on overall rating
def_subset <- table( data$overall_rating, data$cycle)
barplot(def_subset,
        legend = rownames(def_subset),
        col = c("blue", "red", "black", "yellow", "violet"),
        ylim = c(0, 180000),
        ylab = "Count",
        xlab = "Cycle ",
        main = "Comparison Bar Chart:
        overall rating for each cycle")
box(which = "plot",
    lty = "solid",
    col="black")

#normalised plot for each cycle based on overall rating
library(ggplot2)
ggplot() +
  geom_bar(data = data,
           aes(x = factor(data$cycle),
               fill = factor(data$overall_rating)),
           position = "fill") +
  scale_x_discrete("cycle") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="data")) +
  scale_fill_manual(values=c("blue", "red", "black", "yellow", "violet"))

#non-normalised plot for each scope based on overall rating
def_subset <- table( data$overall_rating, data$scope)
barplot(def_subset,
        legend = rownames(def_subset),
        col = c("blue", "red", "black", "yellow", "violet"),
        ylim = c(0, 250000),
        ylab = "Count",
        xlab = "Scope ",
        main = "Comparison Bar Chart:
        overall rating for each Scope")
box(which = "plot",
    lty = "solid",
    col="black")

#normalised plot for each scope based on overall rating
library(ggplot2)
ggplot() +
  geom_bar(data = data,
           aes(x = factor(data$scope),
               fill = factor(data$overall_rating)),
           position = "fill") +
  scale_x_discrete("scope") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="data")) +
  scale_fill_manual(values=c("blue", "red", "black", "yellow", "violet"))

#principal component analysis*************************************
#names(imputed)
# log transform 
#log.fd <- log(imputed[, 1:10])

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
#Sample.scaled <- data.frame(apply(log.fd,2,scale))
#Sample.scaled.2 <- data.frame(t(na.omit(t(Sample.scaled))))
#pca.Sample.2 <- prcomp(Sample.scaled.2, retx=TRUE)
#pca.Sample.2
# plot method
#plot(pca.Sample.2, type = "l")

# summary method
#summary(pca.Sample.2)

#imputed.overall_rating <- imputed[, 5]
#library(devtools)
#install_github("ggbiplot", "vqv")

#library(ggbiplot)
#g <- ggbiplot(pca.Sample.2, obs.scale = 1, var.scale = 1, 
 #             groups = imputed.overall_rating, ellipse = TRUE, 
  #            circle = TRUE)
#print(g)

#require(ggplot2)

#theta <- seq(0,2*pi,length.out = 100)
#circle <- data.frame(x = cos(theta), y = sin(theta))
#p <- ggplot(circle,aes(x,y)) + geom_path()

#loadings <- data.frame(pca.Sample.2$rotation, 
 #                      .names = row.names(pca.Sample.2$rotation))
#p + geom_text(data=loadings, 
 #             mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
#  coord_fixed(ratio=1) +
 # labs(x = "PC1", y = "PC2")
#**********************************
#PCA 
#names(final_data)
#names(imputed)
#pairs(imputed1)
#selecting the variables needed into a dataframe
imputed1 <- cbind(imputed_initial$cmplnt_cnt,
                  imputed_initial$FINE_CNT,
                  imputed_initial$FINE_TOT,
                  imputed_initial$TOT_PENLTY_CNT,
                  imputed_initial$survey_rating,
                  imputed_initial$quality_rating,
                  imputed_initial$staffing_rating,
                  imputed_initial$BEDCERT,
                  imputed_initial$RESTOT)
#assigning the column names
colnames(imputed1) <- c("cmplnt_cnt","FINE_CNT","FINE_TOT","TOT_PENLTY_CNT","survey_rating","quality_rating",
                        "satffing_rating","BEDCERT","RESTOT")
#computing PCA
pc1 <- princomp(imputed1, scores=TRUE, cor=TRUE)
summary(pc1)
#plotting 
loadings(pc1)
plot(pc1)
biplot(pc1)
#Observations: 
#for two components the eigen value is greater than one. In first component FINECNT and Total penlaty count
#play significant role.
#In second component BEDCERT and RESTOT play significant role

#linear regression
lm.out <- lm(imputed_initial$overall_rating ~ imputed_initial$TOT_PENLTY_CNT)
plot(imputed_initial$overall_rating,
     imputed_initial$TOT_PENLTY_CNT,
     main = "overall Rating by total penalty count ", xlab = "total penality count",
     ylab = "Rating")
abline(lm.out)
summary(lm.out)
lm(formula =imputed_initial$overall_rating ~ imputed_initial$TOT_PENLTY_CNT)

#multiple regression
mreg.out <- lm(imputed_initial$overall_rating ~ imputed_initial$FINE_CNT+imputed_initial$TOT_PENLTY_CNT
               +imputed_initial$BEDCERT+imputed_initial$RESTOT+imputed_initial$cmplnt_cnt+imputed_initial$FINE_TOT
                 +imputed_initial$survey_rating+imputed_initial$quality_rating+imputed_initial$staffing_rating)
summary(mreg.out)
#Observation:
#The multiple regression model predicts about 89.4% data correctly
#*************
#factor(final_data$FINE_CNT)
#training$FINE_CNT.z<- (training$FINE_CNT - mean(training$FINE_CNT))/sd(training$FINE_CNT)
#training$TOT_PENLTY_CNT.z<- (training$TOT_PENLTY_CNT - mean(training$TOT_PENLTY_CNT))/sd(training$TOT_PENLTY_CNT)
#training$BEDCERT.z<- (training$BEDCERT - mean(training$BEDCERT))/sd(training$BEDCERT)
#training$RESTOT.z<- (training$RESTOT - mean(training$RESTOT))/sd(training$RESTOT)
#training$cmplnt_cnt.z<- (training$cmplnt_cnt - mean(training$cmplnt_cnt))/sd(training$cmplnt_cnt)
#training$FINE_TOT.z<- (training$FINE_TOT - mean(training$FINE_TOT))/sd(training$FINE_TOT)


 
 sum(is.na(testdf$overall_rating))
#install.packages(c("rpart", "rpart.plot", "C50"))
#library("rpart");
#library("rpart.plot");
#library("C50")
#rating<- rpart(overall_rating ~ FINE_CNT.z + TOT_PENLTY_CNT.z + BEDCERT.z + RESTOT.z + cmplnt_cnt.z + FINE_TOT.z + survey_rating + quality_rating + staffing_rating,
#                data = training,
#                method = "class")

#rating <-rpart(overall_rating ~ FINE_CNT, data= training, method="anova")
#print(rating)
#rpart.plot(rating)
#********
#Decision tree
# Create a new model `my_tree`
 decisiondf <- cbind(imputed_initial$cmplnt_cnt,
                    imputed_initial$FINE_CNT,
                    imputed_initial$FINE_TOT,
                    imputed_initial$TOT_PENLTY_CNT,
                    imputed_initial$survey_rating,
                    imputed_initial$quality_rating,
                    imputed_initial$staffing_rating,
                    imputed_initial$BEDCERT,
                    imputed_initial$RESTOT,
                    imputed_initial$overall_rating)
 
 
 colnames(decisiondf) <- c("cmplnt_cnt","FINE_CNT","FINE_TOT","TOT_PENLTY_CNT","survey_rating","quality_rating",
                          "staffing_rating","BEDCERT","RESTOT","overall_rating")
 #dividing the dataset into training and test
 apply(decisiondf,2,range)
 
 #using min-max normalisation
 
 maxValue <- apply(decisiondf, 2, max)
 minValue <- apply(decisiondf, 2, min)
 
 decisiondf <- as.data.frame(scale(decisiondf, center =minValue,scale=maxValue - minValue))

 ind <-sample(1:nrow(decisiondf),11733)
 training <- decisiondf[ind,]
 testing <- decisiondf[-ind,]
 
my_tree <- rpart(overall_rating ~ FINE_CNT+ TOT_PENLTY_CNT+ BEDCERT+ RESTOT + cmplnt_cnt + FINE_TOT + survey_rating + quality_rating + staffing_rating,
                 data = training,
                 method = "class", control=rpart.control(cp=0.0001))

summary(my_tree)

# Visualize your new decision tree
#fancyRpartPlot(my_tree)
prp(my_tree, type = 4, extra = 100)

# Make your prediction using `my_tree` and `test_new`
my_prediction <- predict(my_tree, testing, type = "class")
head(my_prediction)

vector_provnum <- testing$provnum

my_solution <- data.frame(provnum = vector_provnum, overall_rating = my_prediction)
head(my_solution)

# Write your solution to a csv file with the name decision_tree_output.csv
write.csv(my_solution, file = "decision_tree_output.csv")


#neural network
#neuraldf1 <- imputed_initial[,3:12]
#neuraldf <- neuraldf1[,-5]
neuraldf1 <- cbind(imputed_initial$cmplnt_cnt,
                  imputed_initial$FINE_CNT,
                  imputed_initial$FINE_TOT,
                  imputed_initial$TOT_PENLTY_CNT,
                  imputed_initial$survey_rating,
                  imputed_initial$quality_rating,
                  imputed_initial$staffing_rating,
                  imputed_initial$BEDCERT,
                  imputed_initial$RESTOT,
                  imputed_initial$overall_rating)


colnames(neuraldf1) <- c("cmplnt_cnt","FINE_CNT","FINE_TOT","TOT_PENLTY_CNT","survey_rating","quality_rating",
                        "satffing_rating","BEDCERT","RESTOT","overall_rating")
View(neuraldf1)
apply(neuraldf1,2,range)

#using min-max normalisation
maxValue <- apply(neuraldf1, 2, max)
minValue <- apply(neuraldf1, 2, min)

neuraldf1 <- as.data.frame(scale(neuraldf1, center =minValue,scale=maxValue - minValue))
#partioning the data set
ind <-sample(1:nrow(neuraldf1),11733)
traindf <- neuraldf1[ind,]
testdf <- neuraldf1[-ind,]

#preparing the needed columns and formulating the formula to be given to neural networks
allvars <- colnames(neuraldf1)
predictorvars <- allvars[!allvars%in%"overall_rating"]
predictorvars <-paste(predictorvars, collapse = "+")
form=as.formula(paste("overall_rating ~ ",predictorvars,collapse = "+"))

library(neuralnet)
neuralmodel <- neuralnet(formula =form,hidden = c(4,2), linear.output = T, data=traindf)

#plotting neural network
plot(neuralmodel)
#testdfremove <- testdf[,-5]
#names(testdf)
#predictions based on neural network
predictions <- compute(neuralmodel, testdf[,1:9])
str(predictions)

predictions <- predictions$net.result*(max(testdf$overall_rating)-min(testdf$overall_rating))+min(testdf$overall_rating)
actualvalues <- (testdf$overall_rating)*(max(testdf$overall_rating)-min(testdf$overall_rating))+min(testdf$overall_rating)

#error in prediction
MSE <- sum((predictions - actualvalues)^2)/nrow(testdf)
MSE
#plotting the difference in the vales of actual and predicted
plot(testdf$overall_rating,predictions,col = 'blue', main = 'Real vs Predicted', pch=1, cex=0.9,
     type = "p", xlab = "Actual", ylab = "Predicted")
abline(0,1, col ="black")
#Observation'
#In neural networks the mean squared error is about 1.3% which is very less for a very large dataset

#crossvalidation
#setseed(123)
#library(MASS, quietly= TRUE)
#library(caret)

#cvdf <- cbind(imputed_initial$cmplnt_cnt,
#                   imputed_initial$FINE_CNT,
#                   imputed_initial$FINE_TOT,
#                   imputed_initial$TOT_PENLTY_CNT,
#                   imputed_initial$survey_rating,
#                   imputed_initial$quality_rating,
#                   imputed_initial$staffing_rating,
#                   imputed_initial$BEDCERT,
#                   imputed_initial$RESTOT,
#                   imputed_initial$overall_rating)


#colnames(cvdf) <- c("cmplnt_cnt","FINE_CNT","FINE_TOT","TOT_PENLTY_CNT","survey_rating","quality_rating",
#                         "satffing_rating","BEDCERT","RESTOT","overall_rating")
#View(cvdf)

#ind1 <-createDataPartition(cvdf[[1]], p =2/3, list = FALSE)
#cvtraindf <- cvdf[ind1,]
#cvtestdf <- cvdf[-ind1,]

#controlparameters <- trainControl( method ="cv", number = 5, savePredictions = TRUE, classProbs = TRUE)
#parametergrid <- expand.grid(mtry = c(2,3,4))

#modelrandom <- train(overall_rating~.,
#                     data =cvtraindf,
#                     method ="rf",
#                     trControl = controlparameters,
#                     tuneGrid=parametergrid
#                     #preProcess = c('center','scale')
#)

#modelrandom

#predictionscv <- predict(modelrandom, cvtestdf)

#t<- table(predictions= predictionscv, actual = cvtestdf["overall_rating"])
#t

#******************

# Data Mining Methods
# Association rules
#str(training)
# Association Rule Mining
#library(arules)
# find association rules with default settings
#training  <- data.frame(sapply(training,as.factor))
#rules <- apriori(training)
#rules
#inspect(rules)

#plot(rules)

#rules <- apriori(training, parameter= list(minlen=2, supp=0.005, conf=0.8), 
#                 appearance = list(rhs=c("overall_rating=5","overall_rating=4","overall_rating=3","overall_rating=2","overall_rating=1","overall_rating=0"), default= "lhs") )
#rules.sorted <- sort(rules, by="lift")
#rules.sorted
#inspect(rules.sorted)

# find redundant rules
#subset.matrix <- is.subset(rules, rules)
#subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
#redundant <- colSums(subset.matrix, na.rm=T) >= 1
#which(redundant) */

# remove redundant rules
#rules.pruned <- rules[!redundant]
#inspect(rules.pruned)
#rules.pruned


#library(arulesViz)
#plot(rules)

#plot(rules, method="graph", control=list(type="items"))

#plot(rules, method="paracoord", control=list(reorder=TRUE))

#kmeans
#removing the categorical variables
imputed_initial_k <- imputed_initial[,-1]
z <- imputed_initial_k[,-1]
z$overall_rating <- NULL

#standardizing the data
m <- apply(z,2,mean)
s <- apply(z,2,sd)
z <- scale(z,m,s)

#applying kmeans algorithm
results <- kmeans(z, 6)
results

#table format of the actual result and the clustering result
table(imputed_initial$overall_rating, results$cluster)

#sample plot for the clustering data based on clusters
plot(imputed_initial[c("BEDCERT","RESTOT")], col = results$cluster)

#sample plot for the actual data based on overall_rating
plot(imputed_initial[c("BEDCERT","RESTOT")], col = imputed_initial$overall_rating)

#logistic regression
testdf <- imputed_initial
data(testdf$overall_rating>0 & testdf$overall_rating <3) <- 0;


testdf$overall_rating <-replace(testdf$overall_rating,testdf$overall_rating >0 & testdf$overall_rating <3,0)
testdf$overall_rating <-replace(testdf$overall_rating,testdf$overall_rating >=3 & testdf$overall_rating <7,1)

logreg <- cbind(testdf$cmplnt_cnt,
                testdf$FINE_CNT,
                testdf$FINE_TOT,
                testdf$TOT_PENLTY_CNT,
                testdf$survey_rating,
                testdf$quality_rating,
                testdf$staffing_rating,
                testdf$BEDCERT,
                testdf$RESTOT,
                testdf$overall_rating)


colnames(logreg) <- c("cmplnt_cnt","FINE_CNT","FINE_TOT","TOT_PENLTY_CNT","survey_rating","quality_rating",
                      "satffing_rating","BEDCERT","RESTOT","overall_rating")
#partioning the data set
testdf$part <- runif(length(testdf$provnum), min = 0,
                     max = 1)
training <- testdf[imputed_initial$part <= 0.75,] 
testing <- testdf[imputed_initial$part > 0.75,] 

#Remove the target variable, Rating, from the testing data
names(testing)
testing <- testing[,-13] 
names(testing)


#Remove the partitioning variable from both data sets

#testing <- testing[,-12] 
testing <- testing[,-1]
testing <- testing[,-1]
names(testing)

names(training)
training <- training[,-13] 
training <- training[,-1]
training <- training[,-1]
names(training)

model <- glm(formula=overall_rating ~ . ,data=training,family = binomial(link='logit') )
summary(model)

prediction = predict(model, testing, type ="response")

prediction1 = round(prediction,0)

combined = cbind(testing$overall_rating, prediction1)

colnames(combined) = c("Actual", "Predicted")

combined_dataFrame = data.frame(combined)

write.csv (combined_dataFrame, file="logistic_reg_output.csv");


# logistic regression confusion matrix

table(combined_dataFrame$Actual, combined_dataFrame$Predicted)


# ROCR curve for logistic regression 

library(ROCR)
preds = prediction(as.numeric(prediction), as.numeric(testing$overall_rating))

perf = performance(preds, "tpr", "fpr")

plot(perf)

#conclusion : As the curve is more towards true positive rate the accuracy of prediction by logistic regression is high

#Evaluation of Models:
#the evalluation of the models used is done in WEKA

#Decision Tree algorithm:
#Accuracy : 99.168%

#Logistic Regression algorithm:
#Accuracy : 80%

#Neural Networks:
#Accuracy : 93.17%

#kmeans:
#Accuracy : 50%

