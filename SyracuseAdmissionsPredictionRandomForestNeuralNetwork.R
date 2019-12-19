x <- read.csv(file='/Users/michaelznidarsic/Desktop/SYRACUSE/IST 687/NewAdmissionsData.csv', header = TRUE)

head(x)

# Clean/simplify data

# Removes empty rows

print(x[14030:14050,])

x <- x[1:14038,]

rownames(x) <- x[,1]

tail(x)

str(x)


# Removes Student ID, Mailing.City, and Student.Type

x <- x[,-c(1, 3, 6)]


# Choice of how to deal with empty dates.

x$NoVisit <- x$Visit.Date == 'No Visit'


# Replace No Visit with NA

x$Temperature <- as.numeric(x$Temperature)
x$Temperature[x$Visit.Date == "No Visit"] <- NA
x$Visit.Date <- as.character(x$Visit.Date)
x$Visit.Date[x$Visit.Date == "No Visit"] <- NA
x$Presenter <- as.character(x$Presenter)
x$Presenter[x$Presenter == "No Visit"] <- NA
x$Tour.Guide <- as.character(x$Tour.Guide)
x$Tour.Guide[x$Tour.Guide == "No Visit"] <- NA

# Necessary conversions to be understood by randomForest

x$Student.Stage <- as.factor(x$Student.Stage)

x$Visit.Date <- as.Date(x$Visit.Date, format="%m/%d/%y")
x$Presenter <- as.factor(x$Presenter)
x$Tour.Guide <- as.factor(x$Tour.Guide)

x$ED <- as.factor(x$ED)
x$Scholarship <- as.factor(x$Scholarship)
x$NoVisit <- as.factor(x$NoVisit)

x$First.Choice <- as.factor(x$First.Choice)


# As Logical instead of factor?

x$ED <- as.logical(x$ED)
x$Scholarship <- as.logical(x$Scholarship)

head(x)

# I shouldn't have to do this, but okay:
x$Student.Stage <- factor(x$Student.Stage)

str(x)


z <- x
# DPLYR binning Stuff, use z as playpen
library(dplyr)

z$Mailing.State.Province <- as.character(z$Mailing.State.Province)
z$Mailing.Country <- as.character(z$Mailing.Country)

z$Region <- case_when(
  z$Mailing.Country == 'United States' & z$Mailing.State.Province %in% c('CA','HI','AK','WA','OR') ~ 'West US',
  z$Mailing.Country == 'United States' & z$Mailing.State.Province %in% c('NV','UT','CO','AZ','ID','MT','NM','WY') ~ 'Mountain US',
  z$Mailing.Country == 'United States' & z$Mailing.State.Province %in% c('KS','NE','IL','MN','MS','MO','OK','WI','LA','IA','AR','AL','TX','ND','SD','TN','KY') ~ 'Central US',
  z$Mailing.Country == 'United States' & z$Mailing.State.Province %in% c('CT','DE','GA','ME','MD','MA','NH','NJ','NY','NC','OH','PA','RI','SC','VT','VA','WV','IN','FL','MI','DC') ~ 'East US',
  z$Mailing.Country %in% c('Japan','China','Korea, Republic of','Viet Nam','Thailand','Philippines','Macao','Mongolia','Taiwan','Hong Kong') ~ 'East Asia',
  z$Mailing.Country %in% c('France','United Kingdom','Italy','Spain','Germany','Ukraine','Czechia','Greece','Montenegro','Hungary','Belgium','Portugal','Switzerland','Sweden','Slovakia','Norway','Netherlands','Cyprus') ~ 'Europe',
  z$Mailing.Country %in% c('India','Bangladesh','Nepal','Myanmar','Pakistan') ~ 'South Asia',
  z$Mailing.Country %in% c('Israel','Lebanon','Turkey','Saudi Arabia','Kuwait','Kazakhstan','Oman','Qatar','United Arab Emirates','Armenia','Jordan') ~ 'West Asia',
  z$Mailing.Country %in% c('Singapore','Malaysia','Indonesia') ~ 'Oceania',
  z$Mailing.Country %in% c('Chile', 'Brazil','Argentina','Colombia', 'Peru', 'Paraguay','Venezuela, Bolivarian Republic') ~ 'South America',
  z$Mailing.Country %in% c('Mexico','Guatemala','Honduras','Ecuador','Panama', 'Dominican Republic','Bahamas','Jamaica','Costa Rica','Belize') ~ 'Central America',
  z$Mailing.Country %in% c('Egypt','Morocco','Tunisia') ~ 'North Africa',
  z$Mailing.Country %in% c('Togo','South Africa','Ghana','Kenya','Ethiopia','Nigeria','Zimbabwe') ~ 'Subsaharan Africa',
  z$Mailing.Country == 'Russian Federation' ~ 'Russia',
  z$Mailing.Country == 'Australia' ~ 'Australia',
  z$Mailing.Country == 'Canada' ~ 'Canada',
  z$Mailing.Country == 'United States' & z$Mailing.State.Province %in% c('PR','VI') ~ 'US Atlantic Territories',
  z$Mailing.Country == 'United States' & z$Mailing.State.Province %in% c('MP','GU','PW') ~ 'US Pacific Territories',
  z$Mailing.Country == 'United States' & z$Mailing.State.Province %in% c('AA','AE','AP') ~ 'US Armed Forces',
  TRUE ~ ''
)

# Check if there are unbinned rows
print(z[z$Region =='',])
length(rownames(z[z$Region=='',]))

table(z$Region)

z$Region <- as.factor(z$Region)

z$Domestic <- z$Region %in% c('US Atlantic Territories','US Pacific Territories','US Armed Forces','West US','Mountain US', 'Central US', 'East US')

z$Domestic <- as.factor(z$Domestic)

str(z)

# Check how first/second choice admits break down
choicecheck <- z[z$First.Choice==FALSE & z$Student.Stage=="Enrolled",]
print(length(choicecheck$First.Choice))
choicecheck2 <- z[z$First.Choice==FALSE & z$Student.Stage=="Not Enrolled",]
print(length(choicecheck2$First.Choice))

# USE THIS AS RELOAD BUTTON. Provisional removal of columns with too many levels. Move from cleaning phase to testing phase.

y <- z[-c(3,4,5,8)]

str(y)

# remove bad predictors
y <- y[-c(3,4,5,6,9,10,11,13)]

str(y)

# Partition

set.seed(123)

ind <- sample(2, nrow(y), replace=TRUE, prob = c(0.7,0.3))
train <- y[ind==1,]
test <- y[ind==2,]

str(train)


########## Run randomForest on 'train', testing on 'test'

install.packages('randomForest')
library(randomForest)

set.seed(222)
rf <- randomForest(Student.Stage~., data=train, na.action=na.exclude, importance=TRUE, mtry=2, ntree=500, xtest=test[complete.cases(test),-1], keep.forest= TRUE)

print(rf)

varImpPlot(rf)

rf$importance

newtestframe <- data.frame()

rfpredictions <- predict(rf, test)

rfpredictions <- as.numeric(rfpredictions == "Enrolled")

rfactuals <- test$Student.Stage

rfactuals <- as.numeric(rfactuals == "Enrolled")

sum(abs(rfactuals - rfpredictions))/length(rfactuals)

confusiondf <- data.frame(actual=rfactuals, prediction=rfpredictions)

table(confusiondf$actual, confusiondf$prediction)

# Begin visualization of Random Forest tree

treestructure <- randomForest::getTree(rf)

# Necessary to install reprtree package. I got this from https://stats.stackexchange.com/questions/41443/how-to-actually-plot-a-sample-tree-from-randomforestgettree
options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))


library(reprtree)

reprtree:::plot.getTree(rf, cex.lab = 0.01, cex.axis= 0.05, cex.main=0.1)




########### Begin XGBOOST analysis (THIS ONLY WORKS WITH NUMERICAL DATA; CONVERT ED, SCHOLARSHIP, AND DOMESTIC INTO DUMMY VARIABLES)


install.packages("xgboost")

require(xgboost)


# Prepare data for XGBOOST

str(z)

set.seed(1)
a <- z[,-c(2,3,4,5,6,7,8,13,14,15,18)]

a$Student.Stage <- a$Student.Stage == 'Enrolled'
a$ED <- a$ED == 'TRUE'
a$Scholarship <- a$Scholarship == 'TRUE'
a$NoVisit <- a$NoVisit == 'TRUE'
a$Domestic <- a$Domestic == 'TRUE'
a$First.Choice <- a$First.Choice == 'TRUE'

str(a)

a[,1] <- as.numeric(a[,1])
a[,4] <- as.numeric(a[,4])
a[,5] <- as.numeric(a[,5])
a[,6] <- as.numeric(a[,6])
a[,7] <- as.numeric(a[,7])
a[,8] <- as.numeric(a[,8])

newind <- sample(2, nrow(a), replace=TRUE, prob = c(0.7,0.3))
newtrain <- a[newind==1,]
newtest <- a[newind==2,]

str(newtrain)

xgtrain <- newtrain
xgtest <- newtest

str(xgtrain)


# RUN XGBOOST

param <- list("objective" = "binary:logistic",
              "eval_metric" = "logloss",
              "eta" = 1, "max.depth" = 2)
bst.cv = xgb.cv(param=param, data = as.matrix(xgtrain[,-1]), label = xgtrain$Student.Stage, nfold = 10, nrounds = 20)
print(bst.cv)

plot(log(bst.cv$evaluation_log$test_logloss_mean), type = 'l')

bst <- xgboost(data = as.matrix(xgtrain[,-1]), label = xgtrain$Student.Stage, max.depth = 2, eta = 1, nround = 3, nthread = 2, objective = "binary:logistic")

preds=predict(bst,as.matrix(xgtest[,-1]))
print(preds)
print(-mean(log(preds)*xgtest$Student.Stage+log(1-preds)*(1-xgtest$Student.Stage)))
trees  = xgb.model.dt.tree(dimnames(as.matrix(xgtrain[,-1]))[[2]], model = bst)


print(trees)


# This might be wrong:
names <- dimnames(xgtrain[,-1])[[2]]
importance_matrix <- xgb.importance(names,model = bst)
xgb.plot.importance(importance_matrix[1:10,])

install.packages('DiagrammeR')

xgb.plot.tree(feature_names = names, model = bst, trees = 2)


######## Begin Neural Network analysis


install.packages('neuralnet')
library(neuralnet)


# Use xgboost dataframes, inspect their contents/structure.
str(xgtrain)
str(xgtest)

# Strain out the NAs
neuraltrain <- xgtrain[complete.cases(xgtrain),]
neuraltest <- xgtest[complete.cases(xgtest),]

# Run neuralnet
admissionsneuraloutcome <- neuralnet(Student.Stage ~ ., data=neuraltrain, hidden = c(5,4,3), lifesign="minimal", linear.output=FALSE, threshold=0.2, stepmax=1e+05, rep=1)
plot(admissionsneuraloutcome)
print(admissionsneuraloutcome$result.matrix)

temptest <- subset(neuraltest, select=c('Temperature','Converted.GPA', 'ED', 'Scholarship','First.Choice', 'NoVisit', 'Domestic'))

admissionsresults <- compute(admissionsneuraloutcome, temptest)

resultsframe <- data.frame(actual=neuraltest$Student.Stage,prediction=admissionsresults$net.result)

# Artificially converts from probability to prediction
resultsframe$prediction <- round(resultsframe$prediction)

print(resultsframe)

errorfigure <- sum(abs(resultsframe$actual-resultsframe$prediction))/(length(resultsframe$actual))
print(errorfigure)


# Create crude confusion matrix
table(resultsframe$actual,resultsframe$prediction)

# Number of acceptances
sum(resultsframe$actual)
# Number of predictions
sum(resultsframe$actual)
# Test data size
length(resultsframe$actual)
# Number of rejections
length(resultsframe$actual) - sum(resultsframe$actual)

