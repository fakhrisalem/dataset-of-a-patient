#reading from csv:

data<-read.csv("c:/R Progamming/data.csv")

#1.Show the first 10 rows and the last 10 rows.

head(data,10)
tail(data,10)

#2.Using Date of Birth attribute, extracts the gender, average commuting time, and ancestry data for the oldest three.

ordered_data = data[order(data$dob),]
head(ordered_data[,c("gender","avg_commute","ancestry")],3)

#3.Identifies the gender, daily internet use, average commute time, ancestry,and diseases among those with more than two children.

filtered_data <- data[data$children > 2, ]
result <- filtered_data[, c("gender", "daily_internet_use", "avg_commute", "ancestry", "disease")]
head(filtered_data[,c("gender","avg_commute","ancestry")],2)

#4.Using a table , indicate the number of rows that have any missing values and the number that do not.

missing_values <- is.na(data)
rows_missingValue <- sum(rowSums(missing_values) > 0) # number of rows with any missing values
rows_without_missingValue <- nrow(data) - rows_missingValue    #number of rows without missing values
table <- data.frame(
  Status = c("number of rows with any missing values", "number of rows without missing values"),
  Count = c(rows_missingValue, rows_without_missingValue)
)
table

#5.Provide a summary of the data for each column, showing "Min, 1st Qu,Median Mean, 3rd Qu and Max" for each numerical column and the
#Number of each Category for categorical data.

for(i in 1:ncol(data)){
  if(class(data[,i])=="character"){
    print(names(data)[i])
    print(table(factor(data[,i])))
  }
  else{
    print(names(data)[i])
    print(summary(data[,i]))}
}

#6. Identify the columns that are having any missing values, and then remove any rows where all of the columns have missing values.

missing_columns <- colSums(is.na(data)) > 0
columns_with_missingvalue <- names(data)[missing_columns]
columns_with_missingvalue 
data_remove <- data[rowSums(is.na(data)) < ncol(data), ] #remove
data_remove
#or
colSums(is.na(data))
na.omit(data,c(names(data)))

#7. Show the average daily usage of the internet for each level of education.

data$education[data$education == "highscool"] = "highschool"
data$education[data$education == "phd/md"] = "phD/MD"

levels = levels(factor(data$education))

x <- sapply(levels, function(level) {
  mean(subset(data, data$education == level)[,"daily_internet_use"], na.rm = TRUE)
})

barplot(x, col=topo.colors(length(levels)), main="Avg Daily Internet Usage", ylab="Usage (Hours)", names.arg = levels)

#8. Show the distribution of the children count using a histogram.

hist(data$children,xlab="Children Number",ylab="Count", border = "black",main="Distribution of Children Count.",col=topo.colors(7))

#9. Utilizing line graphs, compare how men and women's avg commute distributions differ.

levels = levels(factor(data$gender))
print(levels)

par(mfrow=c(1, 1))  

for(i in 1:length(levels)) {
  avg_commute_data <- subset(data, gender == levels[i])$avg_commute
  if (i == 1) {
    plot(avg_commute_data, col=i+2, xlab="Index", ylab="Avg Commute", 
         main="Average Commute Distribution by Gender", type="b", ylim=c(0, max(data$avg_commute, na.rm=TRUE)))
  } else {
  
    lines(avg_commute_data, col=i+2, type="b")
  }
}

legend("topright", legend=levels, col=3:(2 + length(levels)), pch=1, lty=1)

#10.Make a histogram to show the gender distribution.

gender_counts <- table(data$gender)
barplot(gender_counts, 
        main = "Gender Distribution", 
        xlab = "Gender", 
        ylab = "Count", 
        col = c("lightblue", "lightgreen"), 
        border = "black")

#11. Use a histogram to show gender distribution for each disease.

myMatrix <- table(data$gender, data$disease)
print(myMatrix)
barplot(myMatrix,
        main = "Gender Diseases Dist.",
        xlab = "Diseases",
        col = c("red", "green"),
        beside = TRUE, cex.names = 0.6, ylim = c(0, max(myMatrix) + 20))


legend("topright", 
       legend = c("Female", "Male"), 
       fill = c("red", "green"))

#12.Use a chart to demonstrate whether there is a relationship between age and the type of disease.

levels_disease <- levels(factor(data$disease))
dob <- data$dob
age <- as.Date(Sys.Date()) - as.Date(dob)
age <- ceiling(age/365.25)
data$dob <- age
levels_age <- levels(factor(age))
myMatrix <- matrix(0, nrow = length(levels_disease), ncol = length(levels_age),
                   byrow = TRUE, dimnames = list(c(levels_disease), c(levels_age)))

for (i in 1:length(levels_disease)) {
  for (j in 1:length(levels_age)) {
    subsetdata <- subset(data, data$dob == levels_age[j] & data$disease == levels_disease[i])[,"dob"]
    myMatrix[i, j] <- length(subsetdata)
  }
}

barplot(myMatrix,
        main = "Age Diseases Distribution",
        xlab = "Age",
        ylab = "Number of Cases",
        col = rainbow(length(levels_disease)),
        ylim = c(0, max(myMatrix) + 100)
)

legend("topright", legend = levels_disease, fill = rainbow(length(levels_disease)))

#13.Make a chart to show the total number of children per disease.

levels_disease <- levels(factor(data$disease))
myMatrix <- matrix(0, ncol = 1, nrow = length(levels_disease), byrow = TRUE, dimnames = list(c(levels_disease), c("Children Count")))

for (i in 1:length(levels_disease)) {
  subsetdata <- subset(data, data$disease == levels_disease[i])[,"children"]
  myMatrix[i, 1] <- sum(subsetdata)
}

print(myMatrix)

barplot(myMatrix[1:13],
        ylim = c(0, max(myMatrix) + 50),
        main = "Total Children Per Disease",
        xlab = "Diseases",
        ylab = "Total Children Number",
        names.arg = levels_disease,
        col = topo.colors(length(levels_disease)),
        cex.names = 0.6
)

# 14.Make a chart to show the ancestry distribution.

levels = table(data$ancestry)
print(levels)
#OR
ancestry_counts <- table(data$ancestry)

pie(ancestry_counts, 
    main = "Ancestry Distribution",
    col = rainbow(length(ancestry_counts)),
    labels = paste(names(ancestry_counts), ": ", ancestry_counts),
    cex = 0.8,
    clockwise = TRUE
)

