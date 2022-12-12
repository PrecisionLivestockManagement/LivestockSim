library(tidyr)
library(dplyr)

#Load data and tidy it up
Refdata <- read.csv(file = 'ClusterAnalyses/Dendrogram.csv')
row.names(Refdata) <- Refdata[,1]

refdata2 <- Refdata[, -1]

refdata3 <- refdata2[!(rowSums(is.na(refdata2))),]

refdata3 %>%
  rownames_to_column %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 


#Using dist core package calculate distance between rows note this is for binary data
dist_survey <- dist(refdata3, method = 'binary')


#Using hclust core package this clusters rows based on the dissimilarity output
hc_complete <- hclust(dist_survey, method = "complete")


#Provide a plot of the output - there are other plotting options that might be better
plot(hc_complete, main = 'Complete Linkage')

#Put papers into groups using cutree function based on a cluster height
clust_papers <- cutree(hc_complete, h = 0.9)
segment_papers <- mutate(refdata3, cluster = clust_papers)

# Count the number of papers that fall into each cluster
count(segment_papers, cluster)

#Create an output that you can see how each cluster is influced by the criteria used for clustering
dungeree <- segment_papers %>% 
  group_by(cluster) %>% 
  summarise_all(list(mean))

#To view the distance measure convert dist measures to as.matrix
tiger <- as.matrix(dist_survey)


