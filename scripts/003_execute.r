#!/usr/bin/Rscript

# ----------------------------------------------------------
# ..........................................................
# Execution script.
# ..........................................................
# Run by calling
# > source("./003_execute.r", echo=T)
# when within the './scripts' directory.
#
# Overview:
# - Cast cluster center data to column shape.
# - 'Cluster_fac' of 'kmdf' corresponds to a cluster identifier 'A', 'B', ...
# - Add numeric sequence 1:12 as x-axis.
# ..........................................................
# ----------------------------------------------------------


# ----------------------------------------------------------
# Initializing non-tidy data table with individual data entries.
# ..........................................................
# Note: 'n_centers' can not exceed 26, because else there are not
#       enough factor levels for the clusters.
n_centers <- 16
abspath_project <- "/Users/mzhKU_work/projects/011_Enzyme_Barrier_Clustering/"
abspath_scripts <- paste(abspath_project, "scripts/", sep="")
abspath_data <- paste(abspath_project, "/data_processed/", sep="")

source(paste(abspath_scripts, "002_library.r", sep=""))

#d <- read.table("../data_processed/merge_single_317.dat", header=T)
d <- read.table(paste(abspath_data, "merge_single_317.dat", sep=""), header=T)
library(data.table)
d <- as.data.table(d)

# Discard outliers.
dout <- d[-detect_outliers(d)]
# ----------------------------------------------------------




# ----------------------------------------------------------
# Calculate clusters from individual entries and prepare tidy data.
# Set seed for reproducibility.
set.seed(123)
# ..........................................................
# '.SD': Subset of data entries.
km <- kmeans(dout[, .SD, .SDcols=2:13], centers=n_centers,
             nstart=25, iter=25)

# Append the cluster to the entries.
dout[, cluster := km$cluster]

library(lattice)
library(latticeExtra, lib.loc="/Users/mzhKU_work/Library/R/3.0/library/")
# (see '?library' for hints on 'lib.loc' option)

# > km$centers
#           F1         F2         F3        F4        F5        F6        F7
# 1   7.485418  7.9100281  8.9218186 11.116878 12.529685 15.856554 15.907975
# 2   1.693959  2.7357355  3.9261483  6.323935  7.688947 10.581243 12.226583
# [...]
kmdt <- as.data.table(km$centers)

# > kmdt
#            F1         F2         F3        F4        F5        F6        F7
#  1:  7.485418  7.9100281  8.9218186 11.116878 12.529685 15.856554 15.907975
#  2:  1.693959  2.7357355  3.9261483  6.323935  7.688947 10.581243 12.226583
# [...]

# k-means data frame 'kmdf':
# Create data.frame from 'kmdt' transposed data.table ('t(.SD)')
# and recast such that cluster centers are in column form.
kmdf <- data.frame(Cluster_fac = rep(LETTERS[1:n_centers], each=12),
                   Barrier_Cl  = c(round(kmdt[, t(.SD)], 5))
)
kmdf$x <- rep(1:12, times=length(LETTERS[1:n_centers]))

# > head(kmdf, 20)
#    Var2 Barrier_Cl  x
# 1     A    1.27873  1
# 2     A    1.83650  2
# [...]
# 12    A    0.00000 12
# 13    B   -2.55337  1
# 14    B   -1.78579  2
# [...]
# ----------------------------------------------------------




# ----------------------------------------------------------
# Prepare individual entries data frame 'iedf' and plot.
# ..........................................................
# For 'Barrier_i', combine all individual entries into one vector.
iedf <- data.frame(x_axis    = rep(1:12, times=nrow(dout)),
                   id        = rep(1:nrow(dout), each=12, times=1),
                   Barrier_i = c(dout[, t(.SD), .SDcols=2:13]),
                   Cluster   = as.factor(c(sapply(km$cluster, rep, 12)))
)

# > head(iedf, 20)
#    x_axis id Barrier_i Cluster
# 1       1  1   6.53193       7
# 2       2  1   7.19020       7
# [...]
# 13      1  2   0.90377      24
# 14      2  2   4.33477      24
# 15      3  2   5.71287      24
# [...]
# Note: 'Cluster' could also be assigned with 'rep(km$cluster, each=12, times=1)'.

# 'group' required to prevent drawing of a continuous line connecting the last
# data point of a mutant with the first data point of the next mutant barrier.
xyplot(Barrier_Cl ~ x|Cluster_fac, data=kmdf, iedf_i=iedf,
       ylim=c(-20, 50), ylab="Reaction Energy [kcal/mol]",
       xlab="Reaction Coordinate", xlim=c(1, 12), strip=T,
       # Note:
       # 'x' and 'y' are the data from the cluster data frame 'kmdf'.
       # 'iedf_i' is the 'iedf'-data frame provided to the 'i'-nner panel function.
       panel = function(x, y, iedf_i)
       {
            panel.xyplot(x=iedf_i[iedf_i$Cluster==panel.number(), ]$x_axis,
                         y=iedf_i[iedf_i$Cluster==panel.number(), ]$Barrier_i,
                         group=iedf_i$id, subscripts=TRUE, type="o", col="blue")
            panel.xyplot(x, y, type="l", col="red", lwd=2)
       }
)
# ----------------------------------------------------------




# ----------------------------------------------------------
# Optional using 'latticeExtra::layer_' function.
# Note: Underscore at end of 'layer' name, 'layer_'!
# This solution however gives 'warnings'.
# ..........................................................

# foo <- xyplot(Barrier_Cl ~ x|Cluster_fac, group=Cluster_fac, data=kmdf,
#               type="l", col="red", lwd=2)
# 
# foo <- foo + layer_(panel.xyplot(
#        x=iedf$x_axis,
#        y=iedf[iedf$Cluster == levels(iedf$Cluster)[packet.number()], ]$Barrier_i,
#        type="o", col="blue",
#        subscripts=TRUE,
#        group=iedf$id)
# )
# 
# foo

# If in this order, cluster plots are behind individual barrier entries.
# foo <- xyplot(iedf$Barrier_i ~ iedf$x_axis|iedf$Cluster, group=iedf$id,
#               type="l", col="blue", subscripts=TRUE, lty=2)
# 
# foo <- foo + layer_(panel.xyplot(x=x,
#  y=kmdf[kmdf$Cluster_fac==levels(kmdf$Cluster_fac)[packet.number()], ]$Barrier_Cl,
#              subscripts=TRUE,
#              type="o", col="red", lwd=4,
#              group=Cluster_fac),
#              data=kmdf)
# foo
# ----------------------------------------------------------
