# Import KElbowVisualizer to visualize K-Means model performance
from yellowbrick.cluster import KElbowVisualizer
# Import KMeans method from sklearn to build the K-Means model
from sklearn.cluster import KMeans

cluster_df = open(r"C:\Users\awasthit\OneDrive - CQUniversity\Desktop\Main\TR_PHD\ClusterAnalyses\clusterAnalysis.csv")


# Good part of Yellowbrick
model = KMeans()
visualizer = KElbowVisualizer(model, k=(2,30), timings=True)
# Fit and show the performance with dataset
visualizer.fit(cluster_df)
visualizer.show()