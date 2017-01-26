# This R script is used to acquire the number of clusters, and conduct cross-examination to every sample

stem.txt.hc <- hclust(dist(t(word_text_matrix))) # h is the height of classification
plot(stem.txt.hc,hang = -1) # Draw the cluster dendrogram
k <- 3
stem.txt.groupidx <- cutree(stem.txt.hc,k) # Let the number of classifications be 3 (may change)
table(stem.txt.groupidx) # Check how many contracts are there in any group

stem.txt.groups <- list()
for (i in 1:k){ # For every class
  stem.txt.groups[[i]] <- stem.txt_list[[which(stem.txt.groupidx==i)]]
}


