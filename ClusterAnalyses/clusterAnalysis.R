library(tidyr)
library(dplyr)

#Load data and tidy it up
refdata <- read.csv(file = 'ClusterAnalyses/clusterAnalysis.csv')

row.names(refdata) <- refdata[,1]

# removing unwanted columns
refdata2 <- refdata[, -c(1, 22, 38:58)]

######### cluster analysis of all models #########
##################################################

#Using dist core package calculate distance between rows note this is for binary data
dist_survey <- dist(refdata2, method = 'binary')


#Using hclust core package this clusters rows based on the dissimilarity output
hc_complete <- hclust(dist_survey, method = "complete")


#Provide a plot of the output - there are other plotting options that might be better
plot(hc_complete, main = 'Complete Linkage')

#Put papers into groups using cutree function based on a cluster height
clust_complete <- cutree(hc_complete, h = 0.9)
segment_complete <- mutate(refdata2, cluster = clust_complete)
count(segment_complete, cluster)

subset(clust_complete, clust_complete == 1)
subset(clust_complete, clust_complete == 2)
subset(clust_complete, clust_complete == 3)
subset(clust_complete, clust_complete == 4)
subset(clust_complete, clust_complete == 5)
subset(clust_complete, clust_complete == 6)
subset(clust_complete, clust_complete == 7)
subset(clust_complete, clust_complete == 8)
subset(clust_complete, clust_complete == 9)
subset(clust_complete, clust_complete == 10)
subset(clust_complete, clust_complete == 11)


###### cluster analysis of biological models #####
##################################################

refdata3 <- subset(refdata2, Biological == 1)
Data_biological <- refdata3[, -c(1:5)]

#Using dist core package calculate distance between rows note this is for binary data
dist_biological <- dist(Data_biological, method = 'binary')


#Using hclust core package this clusters rows based on the dissimilarity output
hc_biological <- hclust(dist_biological, method = "complete")


#Provide a plot of the output - there are other plotting options that might be better
plot(hc_biological, main = 'Biological Models Linkage')

#Put papers into groups using cutree function based on a cluster height
clust_biological <- cutree(hc_biological, h = 0.9)
segment_biological <- mutate(Data_biological, cluster = clust_biological)
count(segment_biological, cluster)

subset(clust_biological, clust_biological == 1)


###### cluster analysis of biophysical models #####
##################################################

refdata4 <- subset(refdata2, Biophysical == 1)
Data_biophysical <- refdata4[, -c(1:5)]

#Using dist core package calculate distance between rows note this is for binary data
dist_biophysical <- dist(Data_biophysical, method = 'binary')


#Using hclust core package this clusters rows based on the dissimilarity output
hc_biophysical <- hclust(dist_biophysical, method = "complete")


#Provide a plot of the output - there are other plotting options that might be better
plot(hc_biophysical, main = 'Biophysical Models Linkage')

#Put papers into groups using cutree function based on a cluster height
clust_biophysical <- cutree(hc_biophysical, h = 0.9)
segment_biophysical <- mutate(Data_biophysical, cluster = clust_biophysical)
count(segment_biophysical, cluster)

subset(clust_biophysical, clust_biophysical == 1)
subset(clust_biophysical, clust_biophysical == 2)
subset(clust_biophysical, clust_biophysical == 3)
subset(clust_biophysical, clust_biophysical == 4)
subset(clust_biophysical, clust_biophysical == 5)
subset(clust_biophysical, clust_biophysical == 6)
subset(clust_biophysical, clust_biophysical == 7)


###### cluster analysis of bioeconomic models #####
##################################################

refdata5 <- subset(refdata2, Bioeconomic == 1)
Data_bioeconomic <- refdata5[, -c(1:5)]

#Using dist core package calculate distance between rows note this is for binary data
dist_bioeconomic <- dist(Data_bioeconomic, method = 'binary')


#Using hclust core package this clusters rows based on the dissimilarity output
hc_bioeconomic <- hclust(dist_bioeconomic, method = "complete")


#Provide a plot of the output - there are other plotting options that might be better
plot(hc_bioeconomic, main = 'Bioeconomic Models Linkage')


#Put papers into groups using cutree function based on a cluster height
clust_bioeconomic <- cutree(hc_bioeconomic, h = 0.9)
segment_bioeconomic <- mutate(Data_bioeconomic, cluster = clust_bioeconomic)
count(segment_bioeconomic, cluster)

subset(clust_bioeconomic, clust_bioeconomic == 1)
subset(clust_bioeconomic, clust_bioeconomic == 2)
subset(clust_bioeconomic, clust_bioeconomic == 3)
subset(clust_bioeconomic, clust_bioeconomic == 4)
subset(clust_bioeconomic, clust_bioeconomic == 5)


###### cluster analysis of agroecosystem models #####
##################################################

refdata6 <- subset(refdata2, Agroecosystem == 1)
Data_agroecosystem <- refdata6[, -c(1:5)]

#Using dist core package calculate distance between rows note this is for binary data
dist_agroecosystem <- dist(Data_agroecosystem, method = 'binary')


#Using hclust core package this clusters rows based on the dissimilarity output
hc_agroecosystem <- hclust(dist_agroecosystem, method = "complete")


#Provide a plot of the output - there are other plotting options that might be better
plot(hc_agroecosystem, main = 'Agroecosystem Models Linkage')


#Put papers into groups using cutree function based on a cluster height
clust_agroecosystem <- cutree(hc_agroecosystem, h = 0.9)
segment_agroecosystem <- mutate(Data_agroecosystem, cluster = clust_agroecosystem)
count(segment_agroecosystem, cluster)

subset(clust_agroecosystem, clust_agroecosystem == 1)
subset(clust_agroecosystem, clust_agroecosystem == 2)


###### cluster analysis of population models #####
##################################################

refdata7 <- subset(refdata2, Population == 1)
Data_population <- refdata7[, -c(1:5)]

#Using dist core package calculate distance between rows note this is for binary data
dist_population <- dist(Data_population, method = 'binary')


#Using hclust core package this clusters rows based on the dissimilarity output
hc_population <- hclust(dist_population, method = "complete")


#Provide a plot of the output - there are other plotting options that might be better
plot(hc_population, main = 'Population Models Linkage')


#Put papers into groups using cutree function based on a cluster height
clust_population <- cutree(hc_population, h = 0.9)
segment_population <- mutate(Data_population, cluster = clust_population)
count(segment_population, cluster)

subset(clust_population, clust_population == 1)
subset(clust_population, clust_population == 2)
