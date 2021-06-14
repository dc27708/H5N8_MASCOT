######################################################
######################################################
# MASCOT phylogenetic tree
# Phylogenetic tree figure in the H5N8 ms
######################################################
######################################################


require(treeio)
require(ggplot2)
require(ggtree)
require(remotes)

rm(list = ls())
# Set the directory to the directory of the file
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)
tree <- read.beast("trial_daily_2_combo1_mcc.tree")


locations = c("aveyron", "gers", "hautes-pyrenees", "landes", 
              "lot-et-garrone", "pyrenees-atlantiques", "tarn")
# cols = brewer.pal(n = 7, name = 'Dark2')

cols = c("aveyron"="#1B9E77", "gers"="#D95F02", "hautes-pyrenees"="#7570B3", 
         "landes"="#E7298A", "lot-et-garrone"="#66A61E", "pyrenees-atlantiques"="#E6AB02", 
         "tarn"="#A6761D")


tree@data$Location = c()
tree@data$Entropy = c()

for (i in seq(1,length(tree@data$aveyron))){
  prob_vals = c()
  entr = 0
  for (j in seq(1,length(locations))){
    prob_vals[[j]] = as.numeric(tree@data[i, locations[[j]]])
    entr = entr + prob_vals[[j]]*log(prob_vals[[j]])
  }
  tree@data$Location[[i]] = gsub("_", " ", locations[[which.max(prob_vals)]])
  tree@data$Entropy[[i]] = -entr
}

library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)

tree@data$Location <- unlist(tree@data$Location)
tree@data$Entropy <- unlist(tree@data$Entropy)

p_constant <- ggtree(tree, aes(color=Location, alpha=Entropy), mrsd="2017-3-22") + 
  geom_tree() +
  theme_tree2() +
  theme_minimal()+
  scale_color_manual(values=cols)+
  # theme(legend.position = "none") + 
  # coord_flip() + 
  # scale_x_reverse() + 
   ggtitle("H5N8 2016-17 France") + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
theme(plot.title = element_text(hjust = 0.5)) + 
  geom_rootedge(rootedge = 0.01)
plot(p_constant)

ggsave(plot= p_constant ,paste("H5N8Tree.png", sep=""),width= 20, height= 17, units = "cm")



