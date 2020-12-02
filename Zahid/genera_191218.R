rm(list = ls())
# setwd('~/Documents/R/Zahid_Test_Ecology/')
load('genera_191218.RData')
library(ape)
library(vegan)
library(plyr)
library(dplyr)
library(cluster)
library(mclust)
library(factoextra)
library(FactoMineR)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# df_clust is clustered relative abundance dataset, 
# where variable cluster_mclust shows data clusters in 5 groups,
# and variable cluster_dbscan shows data as group/outliers
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# This one shows key different Genera 
df_temp <- df_clust[df_clust$cluster_dbscan==0, ]
df_temp[order(rownames(df_temp)), ]

# Dissimilarity (Distance Measure) ----------------------------------------

df <- data_Genus
seq_method <- c("manhattan", "euclidean", 
                "canberra", "clark", "bray", "kulczynski", 
                "jaccard", "gower", "altGower", 
                # "morisita", 
                # "mountford", "raup", 
                # "chao", "mahalanobis",
                # "cao", "binomial",
                "horn")
# Distance Measures (Dissimilarity between Samples) via different methods
for (iter_method in seq_method) {
    cat('\n# # # # # # # # # # # # # # # # # # # # # # \n')
    print(iter_method)
    print(vegdist(x = df, method = iter_method, diag = T))
    cat('\n# # # # # # # # # # # # # # # # # # # # # # \n')
}
rm(iter_method)
# Calculate the ratios (8 values)
# of distance between different category (4, S1B1, S1B2, S2B1, S2B2)
# to distance within same category (2, S1S2, B1B2)
list_dist_ratio <- llply(
    .data = seq_method, 
    .fun = function(iter_method, temp_df) {
        iter_seq <- vegdist(x = temp_df, method = iter_method, binary = F)
        seq_same <- iter_seq[c(1,6)]
        seq_diff <- iter_seq[2:5]
        iter_seq <- rep(seq_diff, each = 2) / seq_same
        return(iter_seq)
    },
    temp_df = df
)
df_ratio <- Reduce(cbind, list_dist_ratio)
colnames(df_ratio) <- seq_method
rm(list_dist_ratio)
# If the ratios greater than 2 for all of them then 
df_ratio
boxplot(df_ratio)
df_ratio %>% psych::describe()


# PCA ---------------------------------------------------------------------
# Principal Components Analysis
df <- t(data_Genus)
# df <- data_Genus
# 
dim(df)
result_pca <- PCA(df)
result_pca$eig
result_pca$var
result_pca$ind
fviz_screeplot(result_pca)
fviz_pca_var(result_pca, col.var = 'contrib')
fviz_pca_ind(result_pca, col.ind = 'contrib',
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
{
    # cairo_pdf('Biplot_PCA.pdf')
    fviz_pca_biplot(result_pca, col.ind = 'contrib', 
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    # palette = 'jco', addEllipses = T,
                    repel = T) +
        coord_fixed()
    # dev.off()
    ggsave(filename = 'Biplot_PCA.pdf', device = 'pdf', height = 10, width = 8)
}

df[c('Legionella', 
     'Hassallia',
     'Nitrospira', 
     'Thauera'),]
fviz_contrib(result_pca, choice = 'ind', axes = 1, top = 10)
fviz_contrib(result_pca, choice = 'ind', axes = 2, top = 10)


# CA ----------------------------------------------------------------------
# Correspondence Analysis
df <- t(data_Genus)
result_ca <- CA(df)
fviz_ca_biplot(result_ca, repel = T)
fviz_contrib(result_ca, axes = 1, top = 10)
fviz_contrib(result_ca, axes = 2, top = 10)


# PCoA --------------------------------------------------------------------
# 
df <- data_Genus
# df <- t(data_Genus)
result_dist <- vegdist(df, 'jaccard')
result_pcoa <- cmdscale(result_dist, k = (nrow(df)-1), eig = T)
# pcoa(result_dist, correction = 'lingoes')
ordiplot(scores(result_pcoa, choices = 1:2), type = 't')
biplot.pcoa(pcoa(result_dist))


# Clustering --------------------------------------------------------------
# Model-Based Clustering --------------------------------------------------
df <- t(data_Genus)
result_mclust <- mclust::Mclust(data = df)
# BIC values used for choosing the number of clusters
{
    fviz_mclust(result_mclust, "BIC", palette = 'jco')
    ggsave('Model-Based Clustering Model Selection.pdf')
}
# Classification (clustering) plot with uncertainty
{
    fviz_cluster(
        result_mclust, data = df, 
        geom = 'point', what = 'uncertainty', stand = F, 
        ellipse.type = 'convex', show.clust.cent = T, ggtheme = theme_minimal(),
        main = 'Model-Based Cluster Plot', palette = "jco"
    ) +
        theme(legend.position = c(.1, .25))
    # 
    ggsave('Model-Based Clustering Results.pdf')
}

df <- data_Genus
Mclust(df) %>% fviz_mclust_bic()
Mclust(df) %>% fviz_cluster(ggtheme = theme_minimal()) + coord_fixed()


# DBSCAN ------------------------------------------------------------------
df <- t(data_Genus)
result_dbscan <- fpc::dbscan(df, eps = 1, MinPts = 3, scale = T)
fviz_cluster(object = result_dbscan,
             data = data.frame(df),
             geom = 'point', what = "uncertainty", stand = F,
             ellipse.type = 'convex', show.clust.cent = T,
             ggtheme = theme_minimal(), main = 'DBSCAN Cluster Plot',
             palette = RColorBrewer::brewer.pal(8, 'Dark2')) +
    theme(legend.position = c(.9,.8)) + 
    coord_fixed()
ggsave('DBSCAN.pdf', width = 5, height =  5)


# -------------------------------------------------------------------------
df <- t(data_Genus)
df_clust <- data.frame(df, 
                       cluster_mclust = result_mclust$classification,
                       cluster_dbscan = result_dbscan$cluster)
df_clust[order(df_clust$cluster_mclust),]
df_clust[order(df_clust$cluster_dbscan),]

# Outliers as identified by DBSCAN
df_clust[df_clust$cluster_dbscan==0, ]
