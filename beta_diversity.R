
#Install the packages, IF YOU NEED TO :)
install.packages("tidyverse")
install.packages("vegan")
install.packages("devtools")
library(devtools)
devtools::install_github("jbisanz/qiime2R")

#Load the packages. Everyone needs to do this.
library(tidyverse)
library(vegan)
library(qiime2R)


##############################################
#Set UP
#
#These are the things that  we need from Qiime:
#
#sample-metadata.tsv
#core-metrics-results/bray_curtis_pcoa_results.qza
#core-metrics-results/weighted_unifrac_pcoa_results.qza
#core-metrics-results/rarefied_table.qza
#rooted-tree.qza
#taxonomy.qza
#core-metrics-results/evenness_vector.qza
#core-metrics-results/faith_pd_vector.qza
#core-metrics-results/observed_otus_vector.qza
#core-metrics-results/shannon_vector.qza
#
# These files are already in the ANSC516-repo
##############################################

getwd()
###Set your working directory path/to/ANSC516/ANSC-repo/data/moving-pictures
setwd("C:/Users/vanwa/OneDrive/Desktop/Project 7")
getwd()

list.files()

if(!dir.exists("output"))
  dir.create("output")

#How to load a file into R
metadata2 <- read.delim("SowDataReal.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F)
metadata2[1,1:3]
metadata2[,1]
metadata2 <- metadata2[, -which(names(X))]

#Now the qiime2R method
metadata<-read_q2metadata("sowmetadata.txt")
str(metadata)
levels(metadata$`parity`)
# colnames(metadata)[4] <- "Code"
# colnames(metadata)[4] <- "cratenumber"
# colnames(metadata)[4] <- "parity"
# str(metadata)

row.names(metadata) <- metadata[,1]
row.names(metadata) <- metadata$SampleID
#metadata <- metadata[,-1]
row.names(metadata)

bc_PCoA<-read_qza("core-metrics-results/bray_curtis_pcoa_results.qza")
wUF <- read_qza("core-metrics-results/weighted_unifrac_pcoa_results.qza")

body_colors <- c("Purple", "Red", "Green", "Yellow")

#bc_meta <- bc_PCoA$data$Vectors %>%
#  select(SampleID, PC1, PC2, PC3) %>%
#  inner_join(metadata, by = c("SampleID" = "SampleID"))

bc_meta <- bc_PCoA$data$Vectors %>%
  select(SampleID, PC1, PC2, PC3)

bc_meta$SampleID = factor(bc_meta$SampleID)
str(bc_meta)
bc_meta <- inner_join(bc_meta, metadata, by = c("SampleID" = "SampleID"))

# Now we are going to make an ordination plot
ggplot(bc_meta, aes(x=PC1, y=PC2, color=parity)) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  theme_q2r() +
  xlab("PC1 (32.27%)") +
  ylab("PC2 (22.28%)") +
  scale_color_manual(values=c("Black", "Red", "Purple", "Blue", "Green"), name = "parity")+
  ggtitle("Ordination Plot - Bray-curtis - 1")




# Now we are going to make our code a little more re-usable
body_colors <- c("Purple", "Red", "Green", "Pink","Blue" )
my_column <- "parity"
#my_column <- "DietTreatment"

ggplot(bc_meta, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  theme_q2r() +
  facet_grid(~parity) +
  xlab(paste0("PC1 (", round(100*bc_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*bc_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  scale_color_manual(values=body_colors, name = my_column)
ggsave(paste0("output/BC-basic_", my_column,".tiff"), height=2, width=3, device="tiff") # save a PDF 3 inches by 4 inches

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),bc_meta,mean)
colnames(centroids)[1] <- "parity"

ggplot(bc_meta, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  geom_point(data=centroids, size = 3) +
  theme_q2r() +
  stat_ellipse(level = 0.95, type = "t") +
  xlab(paste0("PC1 (", round(100*bc_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*bc_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  scale_color_manual(values=body_colors, name = my_column)
ggsave(paste0("output/BC-ellipse_", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

ggplot(bc_meta, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(aes(shape= parity)) + #alpha controls transparency and helps when points are overlapping
  #geom_point(data=centroids, size = 3) +
  theme_q2r() +
  #stat_ellipse(level = 0.95, type = "t") +
  xlab(paste0("PC1 (", round(100*bc_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*bc_PCoA$data$ProportionExplained[2], digits = 2), "%)")) 
  #scale_color_manual(values=body_colors, name = my_column)
ggsave(paste0("output/BC-ellipse_", my_column,"-subject.pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

##################################################################################
## SAME thing but with unweighted UniFrac

read_qza("core-metrics-results/unweighted_unifrac_pcoa_results.qza")
Unwuni_PCoA<-read_qza("core-metrics-results/unweighted_unifrac_pcoa_results.qza")

#Wuni_meta <- Wuni_PCoA$data$Vectors %>%
#  select(SampleID, PC1, PC2) %>%
#  inner_join(metadata, by = c("SampleID" = "SampleID"))

Unwuni_meta <- Unwuni_PCoA$data$Vectors %>%
  select(SampleID, PC1, PC2, PC3)

Unwuni_meta$SampleID = factor(Unwuni_meta$SampleID)
str(Wuni_meta)
Unwuni_meta <- inner_join(Unwuni_meta, metadata, by = c("SampleID" = "SampleID"))

# Now we are going to make our code a little more re-usable
body_colors <- c("Purple", "Red", "Green", "Pink","Blue" )
my_column <- "parity"
#my_column <- "DietTreatment"

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),Unwuni_meta,mean)

ggplot(Unwuni_meta, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  #geom_point(data=centroids, size = 3) +
  theme_q2r() +
  stat_ellipse(level = 0.95, type = "t") +
  xlab(paste0("PC1 (", round(100*Wuni_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*Wuni_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  scale_color_manual(values=c("Black", "Green", "Pink", "Yellow", "Blue"), name = "parity")
ggsave(paste0("output/Wuni-ellipse_", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

ggplot(Unwuni_meta, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(aes(shape= parity), size = 3) + #alpha controls transparency and helps when points are overlapping
  #geom_point(data=centroids, size = 3) +
  theme_q2r() +
  stat_ellipse(level = 0.95, type = "t") +
  xlab(paste0("PC1 (", round(100*Wuni_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*Wuni_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  scale_color_manual(values=c("Green", "Pink", "Purple", "Black", "Blue"), name = "parity")
ggsave(paste0("output/Wuni-ellipse_", my_column,"-parity.pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

## SAME thing but with weighted UniFrac

read_qza("core-metrics-results/weighted_unifrac_pcoa_results.qza")
Wuni_PCoA<-read_qza("core-metrics-results/weighted_unifrac_pcoa_results.qza")

#Wuni_meta <- Wuni_PCoA$data$Vectors %>%
#  select(SampleID, PC1, PC2) %>%
#  inner_join(metadata, by = c("SampleID" = "SampleID"))

Wuni_meta <- Wuni_PCoA$data$Vectors %>%
  select(SampleID, PC1, PC2, PC3)

Wuni_meta$SampleID = factor(Wuni_meta$SampleID)
str(Wuni_meta)
Wuni_meta <- inner_join(Wuni_meta, metadata, by = c("SampleID" = "SampleID"))

# Now we are going to make our code a little more re-usable
body_colors <- c("Purple", "Red", "Green", "Pink","Blue" )
my_column <- "parity"
#my_column <- "DietTreatment"

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),Wuni_meta,mean)

ggplot(Wuni_meta, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  #geom_point(data=centroids, size = 3) +
  theme_q2r() +
  stat_ellipse(level = 0.95, type = "t") +
  xlab(paste0("PC1 (", round(100*Wuni_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*Wuni_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  scale_color_manual(values=c("Black", "Green", "Pink", "Yellow", "Blue"), name = "parity")
ggsave(paste0("output/Wuni-ellipse_", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

ggplot(Wuni_meta, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(aes(shape= parity), size = 3) + #alpha controls transparency and helps when points are overlapping
  #geom_point(data=centroids, size = 3) +
  theme_q2r() +
  stat_ellipse(level = 0.95, type = "t") +
  xlab(paste0("PC1 (", round(100*Wuni_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*Wuni_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  scale_color_manual(values=c("Green", "Pink", "Purple", "Black", "Blue"), name = "parity")
ggsave(paste0("output/Wuni-ellipse_", my_column,"-parity.pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

## SAME thing but with jaccard

read_qza("core-metrics-results/jaccard_pcoa_results.qza")
jacc_PCoA<-read_qza("core-metrics-results/jaccard_pcoa_results.qza")

#Wuni_meta <- Wuni_PCoA$data$Vectors %>%
#  select(SampleID, PC1, PC2) %>%
#  inner_join(metadata, by = c("SampleID" = "SampleID"))

jacc_meta <- jacc_PCoA$data$Vectors %>%
  select(SampleID, PC1, PC2, PC3)

jacc_meta$SampleID = factor(jacc_meta$SampleID)
str(Wuni_meta)
jacc_meta <- inner_join(jacc_meta, metadata, by = c("SampleID" = "SampleID"))

# Now we are going to make our code a little more re-usable
body_colors <- c("Purple", "Red", "Green", "Pink","Blue" )
my_column <- "parity"
#my_column <- "DietTreatment"

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),jacc_meta,mean)

ggplot(jacc_meta, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  #geom_point(data=centroids, size = 3) +
  theme_q2r() +
  stat_ellipse(level = 0.95, type = "t") +
  xlab(paste0("PC1 (", round(100*Wuni_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*Wuni_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  scale_color_manual(values=c("Black", "Green", "Pink", "Yellow", "Blue"), name = "parity")
ggsave(paste0("output/Wuni-ellipse_", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

ggplot(jacc_meta, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(aes(shape= parity), size = 3) + #alpha controls transparency and helps when points are overlapping
  #geom_point(data=centroids, size = 3) +
  theme_q2r() +
  stat_ellipse(level = 0.95, type = "t") +
  xlab(paste0("PC1 (", round(100*Wuni_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*Wuni_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  scale_color_manual(values=c("Green", "Pink", "Purple", "Black", "Blue"), name = "parity")
ggsave(paste0("output/Wuni-ellipse_", my_column,"-parity.pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches



##################################################################################
#Run some PERMANOVAs
#

bc_dist_mat<-read_qza("core-metrics-results/bray_curtis_distance_matrix.qza")
bc_dm <- as.matrix(bc_dist_mat$data) 
rownames(bc_dm) == metadata$SampleID ## all these values need to be "TRUE"
metadata_sub <- metadata[match(rownames(bc_dm),metadata$SampleID),]
rownames(bc_dm) == metadata_sub$SampleID ## all these values need to be "TRUE"

PERMANOVA_out <- adonis2(bc_dm ~ body.site, data = metadata_sub)

write.table(PERMANOVA_out,"output/Body.site_Adonis_overall.csv",sep=",", row.names = TRUE) 

######################################################################################
##  Pairwise adonis function
##  we can also performe a pairwise comparison with the function 
##  Pairwise Adonis funtion by edro Martinez Arbizu & Sylvain Monteux
##  https://github.com/pmartinezarbizu/pairwiseAdonis/blob/master/pairwiseAdonis/R/pairwise.adonis.R
#######################################################################################

pairwise.adonis2 <- function(x, data, strata = NULL, nperm=999, ... ) {
  
  #describe parent call function 
  ststri <- ifelse(is.null(strata),'Null',strata)
  fostri <- as.character(x)
  #list to store results
  
  #copy model formula
  x1 <- x
  # extract left hand side of formula
  lhs <- x1[[2]]
  # extract factors on right hand side of formula 
  rhs <- x1[[3]]
  # create model.frame matrix  
  x1[[2]] <- NULL   
  rhs.frame <- model.frame(x1, data, drop.unused.levels = TRUE) 
  
  # create unique pairwise combination of factors 
  co <- combn(unique(as.character(rhs.frame[,1])),2)
  
  # create names vector   
  nameres <- c('parent_call')
  for (elem in 1:ncol(co)){
    nameres <- c(nameres,paste(co[1,elem],co[2,elem],sep='_vs_'))
  }
  #create results list  
  res <- vector(mode="list", length=length(nameres))
  names(res) <- nameres
  
  #add parent call to res 
  res['parent_call'] <- list(paste(fostri[2],fostri[1],fostri[3],', strata =',ststri, ', permutations',nperm ))
  
  
  #start iteration trough pairwise combination of factors  
  for(elem in 1:ncol(co)){
    
    #reduce model elements  
    if(inherits(eval(lhs),'dist')){	
      xred <- as.dist(as.matrix(eval(lhs))[rhs.frame[,1] %in% c(co[1,elem],co[2,elem]),
                                           rhs.frame[,1] %in% c(co[1,elem],co[2,elem])])
    }else{
      xred <- eval(lhs)[rhs.frame[,1] %in% c(co[1,elem],co[2,elem]),]
    }
    
    mdat1 <-  data[rhs.frame[,1] %in% c(co[1,elem],co[2,elem]),] 
    
    # redefine formula
    if(length(rhs) == 1){
      xnew <- as.formula(paste('xred',as.character(rhs),sep='~'))	
    }else{
      xnew <- as.formula(paste('xred' , 
                               paste(rhs[-1],collapse= as.character(rhs[1])),
                               sep='~'))}
    
    #pass new formula to adonis
    if(is.null(strata)){
      ad <- adonis2(xnew,data=mdat1, ... )
    }else{
      perm <- how(nperm = nperm)
      setBlocks(perm) <- with(mdat1, mdat1[,ststri])
      ad <- adonis2(xnew,data=mdat1,permutations = perm, ... )}
    
    res[nameres[elem+1]] <- list(ad[1:5])
  }
  #names(res) <- names  
  class(res) <- c("pwadstrata", "list")
  return(res)
} 

body.site_Pair <- pairwise.adonis2(bc_dm ~ body.site, data = metadata_sub)
write.table(body.site_Pair,"output/Body.site_Adonis_pairwise.csv",sep=",", row.names = TRUE) 

