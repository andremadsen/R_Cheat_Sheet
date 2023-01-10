##################################
# R TRICKS CHEAT SHEET
##################################


#Import European .csv - save as dataframe 'Data'
Data <- read.csv(file.choose(), header=T, sep=";", dec=",")


#Import American .csv - save as dataframe 'Data'
Data <- read.csv(file.choose(), header=T, sep=",", dec=".")


#Generic 'ifelse' statement to recode an existing discrete variable for 'Sex' as numbers instead of "Male" / "Female" text
Data$Sex_numeric <- as.factor(ifelse(Data$Sex == "Male", 1, 
                                  ifelse(Data$Sex == "Female", 2,NA)))


#Impute random values from rnorm() to replace NA values in df
Data <- Data %>% mutate_all(~ifelse(is.nan(.), NA, .)) # First turn NaN into NA
Data[is.na(Data)] <- runif(n = length(which(is.na(Data))), rnorm(1, mean=0, sd=1)) # Impute rnorm() to replace NA


#Aggregate 'Data' dataset to average duplicate rows for each study participant by 'Patient_ID' across several feature variables [,1:10]
Aggregate.df <- aggregate(Data[, 1:10], na.rm=TRUE, list(Data$Patient_ID), mean)



#Write dataset to .csv file
write.table(Data, "Data_backup_May2021.csv",
            na = "",
            row.names = FALSE,
            col.names = TRUE,
            append = FALSE,
            sep = ";",
            dec = ",")



#ALIGN AND MERGE TWO DATASETS BY CLOSEST AGE (df1 <Age> column 1; df2 <Age> in column 2)   
df2$temp <- NA
for(i in c(1:length(df2[,2]))) {
    df2[i,]$temp <- which.min(abs(df1[,1] - df2[i,2])) 
}

df1$ID <- c(1:length(df1[,1]))
Combined.df <- merge(x=df2, y=df1, by.x="temp", by.y="ID", all.x=TRUE, sort = FALSE)



#Merge datasets df1 & df2 by common ID variable "ID" - discarding excess rows from df2 not present in df1
Combined.df <- merge(x=df1, y=df2, by.x="ID", by.y="ID", all.x=TRUE)



#STATIFY DISCRETE CLASSES FROM A CONTINUOUS VARIABLE, E.G BMI weight classes  
Data$Weight_class <- ifelse(Data$BMI > 30, "obese", 
                     ifelse(Data$BMI > 25, "overweight",
                     ifelse(Data$BMI > 20, "normal", NA))))

plot.default(Data$Age, Data$BMI, col=Data$Weight_class) 



#VISUALIZE WEIGHT CLASS FOR BMI  
ggplot(Data, aes(x=Age, y=BMI, col=Weight_class)) +  
    geom_point() + theme_bw() + 
    labs(title="Weight classes in data", x="Age, years",y="BMI, kg/m2") +
    theme(text = element_text(size=15))

ggsave("BMI_weight_classes.jpg", dpi=600)



#Extract only obese patients from the above dataframe (Data) to a new dataframe
Obese.df <- Data[Data$Weight_class == "obese",]



#Change column names in dataframe 'Data' with 4 columns "VAR1-4"
colnames(Data) <- c("Age","BMI","ID","Cholesterol")



#REMOVE GRID IN A GGPLOT2 FIGURE: insert this argument
theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())



#Check to see if there are missing data (N/A) in Dataframe 'Data'?
sum(is.na(Data))



#FIND HIGHEST VALUE IN A ROW
Data$max <- Data[2:4][cbind(seq_len(nrow(Data)), max.col(Data[2:4]))]


#ADD "CHR-" TO VALUES IN A COLUMN '$Prediction'
Data$Prediction <- sprintf('CHR%i', Data$Prediction)



#Vertically merge two datasets with identical column setup
a <- data.frame(Data1$Age, Data1$Sex, Data1$BMI)
colnames(a) <- c("Age","Sex","BMI")
b <- data.frame(Data2$Age, Data2$Sex, Data2$BMI)
colnames(b) <- c("Age","Sex","BMI")
cols <- intersect(colnames(a), colnames(b))
c <- rbind(a[,cols], b[,cols])



#FETCH PACKAGE FROM CUSTOM PATH LOCAL LIBRARY
.libPaths()
.libPaths( c( .libPaths(), "C:/Users/----/AppData/Local/Temp/RtmpEzpYjO/downloaded_packages") )



#Eliminate all rows in a dataset for which there are missing vales (N/A) in ONE selected column - better than na.omit(Data) that eliminates any rows with any N/A in any column
Complete.obs <- Data[complete.cases(Data$BMI),]



#REPLACE N/A with "0" FOR MISSING VALUES IN PATIENT BMI
Data$BMI[is.na(Data$BMI)] <- 0



#Remove the column "BMI" from dataset 'Data'
Data <- subset(Data, select = -BMI)



#ASSIGN QUARTILES FOR BMI, COMPARTMENTALIZE Q1 AND Q3 STRATA, AND RUN T-TEST 
sum1 <- summary(Data$BMI)
sum1
Data$Quartile <- ifelse(Data$BMI < sum1[2], 1, 
                 ifelse(Data$BMI < sum1[3], 2,
                 ifelse(Data$BMI > sum1[5], 3,
                 ifelse(Data$BMI, 0, NA)))))

Q1_Data <- Data[Data$Quartile %in% c(1),]
Q3_Data <- Data[Data$Quartile %in% c(3,]


t.test(Q1_Data$BMI, Q3_Data$BMI)



#CORRELATION TEST OF TWO CONTINUOUS VARIABLES: BMI ~ Cholesterol
cor.test(Data$BMI, Data$Cholesterol, use = "pairwise.complete.obs", method = "spearman")



#TURN STANDARD DEVIATION SCORE (z-scores; 'BMI_SDS') INTO PERCENTILE EQUIVALENT
Data$BMI_percentile <- pnorm(Data$BMI_SDS)



#CALCULATE Nth PERCENTILES
quantile(Data$BMI, c(.5, .025, .975), na.rm=TRUE)  #output: p50=median; p2.5 and p97.5



#NIFTY BOXPLOT
obese <- Data[Data$Weight_class == "obese",]
overweight <- Data[Data$Weight_class == "overweight",]

boxplot(obese$BMI, overweight$BMI, horizontal = TRUE, col = c("orange", "red"), names = c("obese", "overweight"), 
        xlab = "BMI, kg/m2", border = "brown", notch = TRUE)



#3-DIMENSIONAL SCATTERPLOT
install.packages("scatterplot3d")
library(scatterplot3d)
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
x <- Data$Age
y <- Data$BMI
z <- Data$Cholesterol

#ULTRA BASIC
scatterplot3d(x, y, z, color="steelblue", pch=16)

#BASIC
scatterplot3d(x, y, z, xlab = "Age", zlab = "BMI", ylab = "Cholesterol", pch = 16, 
              color="steelblue", cex.axis=1, grid = FALSE, box = FALSE, addgrids3d(x, y, z, grid = c("xy", "xz", "yz")), cex.lab=1.5)

#DIFFERENT DOTS ACCORDING TO STAGE
shapes = c(16, 17, 18)
shapes <- shapes[as.numeric(Data$Weight_class)]
scatterplot3d(x, y, z, xlab = "Age", zlab = "BMI", ylab = "Cholesterol", pch = shapes, 
              cex.axis=1, grid = FALSE, box = FALSE, color="steelblue", addgrids3d(x, y, z, grid = c("xy", "xz", "yz")))

#DIFFERENT COLORS ACCORDING TO STAGE
colors <- c("#000000", "#FF8C00", "#FF0000")
colors <- colors[as.numeric(Data$Weight_class)]
s3d <- scatterplot3d(x, y, z, xlab = "Age", ylab = "BMI", zlab = "Cholesterol", pch = 16, 
       cex.axis = 1.5, grid = FALSE, box = FALSE, color = colors, addgrids3d(x, y, z, grid = c("xy", "xz", "yz")), cex.lab=1.5) 
       legend(s3d$xyz.convert(6, 5, 10), box.lty = 1, legend = c("Obese", "Overweight", "Normalweight"), 
       col = c("#000000", "#FF8C00", "#FF0000"), pch = 16)
       
       
       
#PLOT TIME ON X-AXIS (GGPLOT2) FROM COLUMN 'TIME' SPECIFYING EXAMINATION TIME OF DAY
Data$TIME_plot <- as.numeric(paste(as.numeric(lapply(strsplit(as.character(Data$TIME),split=":"), FUN = function(a) a[1])),
                                             gsub("\\.","",10*as.numeric(lapply(strsplit(as.character(Data$TIME),split=":"), 
                                              FUN = function(a) a[2]))/60), sep = "."))


ggplot(Data, aes(x=TIME_plot, y=Cortisol_level)) + theme(plot.title = element_text(size = 20)) + theme_bw() +
    theme(axis.text = element_text(size = 18)) +theme(axis.title = element_text(size = 20)) + geom_point() + 
    labs(title="Cohort blood sample time of draw", x="Sample time (hour of day)",y="Serum cortisol level") + 
    scale_x_continuous(breaks = seq(0,24,by=0.5), labels = sprintf("%02s:%02s",rep(0:24,each=2), rep(c("00","30"),50))[1:49]) +
    geom_smooth(method="lm", mapping=NULL, color="blue", se=FALSE) 

