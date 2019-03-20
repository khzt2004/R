# http://www.awesomestats.in/python-cluster-validation/

import umap
import time
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.datasets import fetch_mldata
import plotly.plotly as py
from sklearn.cluster import KMeans
from sklearn.cluster import MiniBatchKMeans
import sklearn.cluster as cluster
from sklearn.metrics import silhouette_score
import numpy as np
from scipy.spatial.distance import cdist,pdist
from sklearn.metrics import adjusted_rand_score, adjusted_mutual_info_score
from scipy.cluster.vq import kmeans
from matplotlib import cm
from sklearn import metrics
from sklearn import preprocessing
from sklearn.decomposition import PCA
from sklearn.preprocessing import MinMaxScaler
import pandas_profiling

hdd = pd.read_csv("C:\\Users\\User\\Documents\\SIA_clustering19Mar.csv")
hdd_new = hdd.drop(['fullVisitorId'], axis=1)
hdd_str_labels = hdd_labels = hdd['fullVisitorId']
# hdd_labels = hdd['fullVisitorId'].str[-2:].astype(int)

embedding = umap.UMAP().fit_transform(hdd_new)


# use a scree plot to determine clusters

#standard_embedding = umap.UMAP(random_state=42).fit_transform(hdd_new)
random_sample = hdd_new
random_sample = preprocessing.scale(random_sample)

# Generate a HTML report for dataframe profiling
profile = pandas_profiling.ProfileReport(random_sample)
profile.to_file(outputfile="profiling.html")


# Scree plots
def scree_plot(x):
   K = range(1,21)
   KM = [MiniBatchKMeans(n_clusters=k).fit(x) for k in K]
   centroids = [k.cluster_centers_ for k in KM]
   D_k = [cdist(x, cent, 'euclidean') for cent in centroids]
   cIdx = [np.argmin(D,axis=1) for D in D_k]
   dist = [np.min(D,axis=1) for D in D_k]
   avgWithinSS = [sum(d)/x.shape[0] for d in dist]
   
   wcss = [sum(d**2) for d in dist]
   tss = sum(pdist(x)**2)/x.shape[0]
   bss = tss-wcss
   return K, avgWithinSS, tss, bss;


def scree_benchmark(x):
    K, avgWithinSS, tss, bss = scree_plot(x)


#K, avgWithinSS, tss, bss = scree_plot(standard_embedding)
K, avgWithinSS, tss, bss = scree_plot(random_sample)


# time a function as benchmark
#def scree_plot_minibatch(x):
#   K = range(1,21)
#   KM = [MiniBatchKMeans(n_clusters=k).fit(x) for k in K]
#   centroids = [k.cluster_centers_ for k in KM]
#   D_k = [cdist(x, cent, 'euclidean') for cent in centroids]
#   cIdx = [np.argmin(D,axis=1) for D in D_k]
#   dist = [np.min(D,axis=1) for D in D_k]
#   avgWithinSS = [sum(d)/x.shape[0] for d in dist]
#   
#   wcss = [sum(d**2) for d in dist]
#   tss = sum(pdist(x)**2)/x.shape[0]
#   bss = tss-wcss
#   return K, avgWithinSS, tss, bss;


#def scree_benchmark_minibatch(x):
#    K, avgWithinSS, tss, bss = scree_plot_minibatch(x)


#n = 50
#t0 = time.time()
#for i in range(n): scree_benchmark(standard_embedding)
#t1 = time.time()

#total_n = t1-t0

#t2 = time.time()
#for i in range(n): scree_benchmark_minibatch(standard_embedding)
#t3 = time.time()

#total_n_minibatch = t3-t2

# calculate the inertias
ks = range(1, 30)
inertias = []
for k in ks:
    # Create a KMeans instance with k clusters: model
    model=MiniBatchKMeans(n_clusters=k)
    
    # Fit model to samples
    # model.fit(concatenated_df)
    model.fit(random_sample)
    
    # Append the inertia to the list of inertias
    inertias.append(model.inertia_)
    
print(model.labels_)
print(model.inertia_)

plt.plot(ks, inertias, '-o')
plt.xlabel('number of clusters, k')
plt.ylabel('inertia')
plt.xticks(ks)
plt.show()

# elbow curve
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(K, avgWithinSS, 'b*-')
plt.grid(True)
plt.xlabel('Number of clusters')
plt.ylabel('Average within-cluster sum of squares')
plt.title('Elbow for KMeans clustering')
plt.show()

plt.clf()
plt.cla()
plt.close()

# percentage of variance explained
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(K, bss/tss*100, 'b*-')
plt.grid(True)
plt.xlabel('Number of clusters')
plt.ylabel('Percentage of variance explained')
plt.title('Elbow for KMeans clustering')
plt.show()

plt.clf()
plt.cla()
plt.close()

# Silhouette score
def silhouette_score(x):
    for n_cluster in range(2, 21):
        kmeans = KMeans(n_clusters=n_cluster, n_jobs=-1).fit(x)
        label = kmeans.labels_
        sil_coeff = metrics.silhouette_score(x, label, metric='euclidean')
        print("For n_clusters={}, The Silhouette Coefficient is {}".format(n_cluster, sil_coeff))

silhouette_score(random_sample)

# perform K means clustering 

kmeans_model = KMeans(n_clusters=8, n_jobs=-1).fit(random_sample)
y_kmeans = kmeans_model.predict(random_sample)


# Do PCA
#Fitting the PCA algorithm with our Data
pca = PCA().fit(random_sample)

#Plotting the Cumulative Summation of the Explained Variance
plt.figure()
plt.plot(np.cumsum(pca.explained_variance_ratio_))
plt.xlabel('Number of Components')
plt.ylabel('Variance (%)') #for each component
plt.title('SIA Dataset Explained Variance')
plt.show()

pca = PCA(n_components=2).fit(random_sample)
pca_data = pca.transform(random_sample)
plt.scatter(pca_data[:, 0], pca_data[:,1], c=kmeans_model.labels_)
plt.xlabel('')
plt.ylabel('')
plt.title('2 Cluster K-Means')
plt.show()


centers = kmeans_model.cluster_centers_
plt.scatter(centers[:, 0], centers[:, 1], c='black', s=200, alpha=0.5);
plt.show()

plt.clf()
plt.cla()
plt.close()

# calculate silhouette score/coefficient to evaluate clustering
# The score is bounded between -1 for incorrect clustering and 
# +1 for highly dense clustering. Scores around zero indicate overlapping clusters.
labels = kmeans_model.labels_
metrics.silhouette_score(random_sample, labels, metric='euclidean')

# Calculate Calinski-Harabaz Index
# The score is higher when clusters are dense and well separated, 
# which relates to a standard concept of a cluster.
metrics.calinski_harabaz_score(random_sample, labels) 

# Map clusters back to standard embedding

hdd['cluster'] = y_kmeans


# cluster aggregation and exploration
hdd_cluster_aggregate = hdd.groupby('cluster')['Desc_00002', 
               'Desc_00003',
               'Desc_00013',
               'Prop_001'].mean()


# -----------------------------------------------------------------------------
# plot dimensionality reduction of clusterable embeddings
clusterable_embedding_df= pd.DataFrame(clusterable_embedding, index=hdd_str_labels)
clusterable_embedding_df.to_csv('clusterable_embedding_df.csv')

plt.scatter(clusterable_embedding[:, 0], clusterable_embedding[:, 1],
            c=hdd_labels, cmap='RdYlGn');
plt.colorbar()
plt.show()


