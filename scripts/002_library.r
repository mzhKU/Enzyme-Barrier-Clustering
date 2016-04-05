# ----------------------------------------------------------
# Generate within-sum of squares plot to determine
# appropriate number of clusters.
# See also p. 379 ff in "R in Action".
# ..........................................................
wssplot <- function(data, max_nc=30, seed=1234)
{
    wss <- (nrow(data)-1)*sum(apply(data, 2, var))
    for(i in 2:max_nc)
    {
        set.seed(seed)
        wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
    xyplot(wss~1:max_nc, type="b", xlab="Number of Clusters",
         ylab="Within gropus sum of squares")
}
# ----------------------------------------------------------



# ----------------------------------------------------------
# Detect outliers
# ..........................................................
max_interpolation_value <- 50.0

# Detect any entry in 'd' where any column has
# an entry greater than 'max_interpolation_value'.
# '.SDcols=2:13': Requesting only F1 - F2 columns.
detect_outliers <- function(d)
{
    which(rowSums(d[, .SD>max_interpolation_value, .SDcols=2:13])>0)
}

# nrow(d)-length(outliers) == nrow(d[-outliers])
# > TRUE
# ----------------------------------------------------------



# ----------------------------------------------------------
# Plot specific cluster with cluster id 'cluster_id'
# > source("./003_clustering.r", echo=T)
# > pc(<cluster_id>)
# ..........................................................
l <- list()

pc <- function(cluster_id)
{
    # Operate only on the entries of one cluster (eg. '2').
    dout_i <- get_entries_by_cluster(cluster_id)
    
    # For every entry in 'dout_i', recast the data into format:
    # x  y
    # 1  0.023
    # 2  0.321
    # [...]
    for(i in 1:nrow(dout_i))
    {
        l <- c(l, list(dout_i[i]))
    }
    # This format can be used to superimpose plots of multiple
    # data entries.
    
    # Transpose to obtain a numeric column vector.
    # First plot specifically to setup plotting environment with
    # scales and axes, superimpose following items with 'lines'.
    plot(x=1:12, y=as.numeric(t(l[[1]])[2:13]), type="l")
    if(length(l) > 1)
    {
        # Extract the numeric interpolation frame values  by
        # 'lapply'-ing the special purpose
        # function 'get_numeric_data' to each list item 'li'
        # > get_numeric_data <- function(li){as.numeric(t(li)[2:13])}
        # to each element and subsequently
        # 'lapply'-ing 'lines' to superimpose the various entries
        # of one cluster to the same plot 
        # > m <- dout_i[1]
        # > n <- dout_i[2]
        # > k <- data.frame(x=1:12, y=t(m[, .SD, .SDcols=2:13]))
        # > l <- data.frame(x=1:12, y=t(n[, .SD, .SDcols=2:13]))
        # > f <- list(k, l)
        # > lapply(lapply(f, get_y), lines)
        lapply(lapply(l[2:length(l)], get_numeric_data), lines)
        # Note, the first entry has already been plotted with 'plot'.  
    } else {
	# Handle cases where the cluster consists of only one entry.
        lapply(lapply(l[1], get_numeric_data), lines)
    }
    lines(km$centers[cluster_id, ], lwd="4", col="red")
}

# Iteration over clusters required.
get_entries_by_cluster <- function(cluster_id)
{
    dout_i <- dout[cluster==cluster_id]
    dout_i
}

# Get only numeric data of data table entry.
get_numeric_data <- function(li)
{
    # Omit the first entry in 'li' which is
    # the mutant identifier string.
    as.numeric(t(li)[2:13])
}
# ----------------------------------------------------------
