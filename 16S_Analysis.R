# Load Packages ----
library(ggplot2) #for plotting
library(vegan) #for a lot of common analyses
library(dplyr) #for data wrangling
library(phyloseq)  #for processing and holding our data as a phyloseq object
library(qiime2R) #for loading data from qiime
library(tidyverse) #for data wrangling
library(lme4) #for models (lme, glmer, etc)
library(car)  #for Anova
library(bbmle) #for comparing models
library(lmtest) #for comparing models with likelihood ratio test
library(ape)  #for rerooting the tree
#library(pairwiseAdonis)
#library(LDM)
library(indicspecies) #for indicator species
library(MASS) #for negative binomial
library(ecole) #pairwise permanova
library(ANCOMBC) #for differential abundance
library(emmeans)
library (mirlyn) #for rarefying


# Install Packages ----

# #This is how I installed phyloseq. if it doesn't work, restart R studio and do this as the first thing in the new session
# source("https://raw.githubusercontent.com/joey711/phyloseq/master/inst/scripts/installer.R",
#        local = TRUE)
# 
# # and this is how I installed qiime2R
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("TreeSummarizedExperiment")
# 
# install.packages("remotes")
# remotes::install_github("jbisanz/qiime2R")

#to install pairwiseAdonis
# install.packages('devtools')
# library(devtools)
# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

#to install ecole
#install.packages("remotes")
#remotes::install_github("phytomosaic/ecole")


# Load Metadata ----

###### 1. Load data. Alternative: once you run this once, you can then save it as an R file
#and load it directly. the code for this is in section 2d and 3e

### 1a. Metadata and elevation and slope/aspect
#the metadata
metadata<-readr::read_tsv("peru-16s-sample-metadata2.tsv")
#this is the elevation file
waypoints<- read.csv("waypoints.csv")
#slope and aspect file
#slope_aspect<- read.csv("latrine_geog_info.csv")
#vicugna RAI
critter<- read.csv("critter_diversity_richness_043026.csv")
#chronosequences
chrono<- read.csv('soil_chronosequence_points.csv') #using the dual Tang and Seimon method decided April 28 2026

### 1b. Other files, loaded into a phyloseq
# Load Other Data ----

#load it into a phyloseq object
#it wasn't loading the sample variables correctly so I had to open the metadata as an excel sheet and 
#just resave it as a tsv so it doesn't cut off the column names. I also added NAs in the cells for the pos/neg
#controls that were initially blank. this is just a note and only needs to be addressed if a future
#metadata file is used and chops off the header names
phy <- qza_to_phyloseq("F:\\Research\\16S_Soil\\Jan25_table.qza", 
                       "F:\\Research\\16S_Soil\\Jan25_rooted-tree.qza", 
                       "F:\\Research\\16S_Soil\\Jan25_taxonomy.qza",
                       "F:\\Research\\16S_Soil\\formatted_metadata.tsv")
#check that metadata didn't chop off names using sample_variables(phy). this should be the header names, not data
sample_variables(phy)


# Initial Processing ----
####### 2. Filtering

## some samples were redone but both samples are in the data (ex 12 and 12B)
#so we need to filter those out so we aren't dealing with duplicates

#look at the total number of ASVs each sample had
asv_all<- data.frame(otu_table(phy))
asv_tot<-data.frame(colSums(asv_all))

#filter only for the samples that had duplicates to compare
asv_tot<- asv_tot %>% 
  filter(row.names(asv_tot) %in% c('X47', 'X49','X48','X50','X19','X19B','X20','X20B','X4','X4B','X23','X23B','X24','X24B','X16','X16B','X11','X11B','X12','X12B','X8','X8B','X7','X7B', 'X15', 'X15B','X3','X3B'))
#47 and 49 are paired and 48 and 50 are paired

#drop the samples that had the lower number of ASVs out of that duplicate pair, NOTE 15 and 3 are not included here per the reason above
dup_phy<- subset_samples(phy, !row.names(phy@sam_data) %in% c('47', '50','19B','20B','4B','23B','24','16','11B','12B','8','7','15','3'))

### 2a. format the heading names
## Need to remove the heading from the kingdom names (they were k_Bacteria)
taxa_data<- as.data.frame(tax_table(dup_phy)) #pull out taxa table
taxa_data$Kingdom<- gsub("^.{0,3}", "", taxa_data$Kingdom) #remove the first three characters from Kingdom column
tax_table(dup_phy)<- as.matrix(taxa_data) #put it back into the phyloseq

#see how many mitochondria and chloroplasts there are
table(taxa_data$Family)
table(taxa_data$Order)
table(taxa_data$Kingdom)
#78 Chloroplast and 898 mitochondira

### 2b. Filter out mitochondria, chloroplasts, and non bacteria from phyloseq object
filtered_phy <- dup_phy %>%
  subset_taxa(   #the subset keeps rows only where the following operators are met
    Kingdom == "Bacteria" &  #this selects only where the kingdom is bacteria
      (Family  != "Mitochondria" | is.na(Family)) &  #this and the next line select things where the family and class are NOT mito/chlo (the ! means not)
      (Order   != "Chloroplast"| is.na(Order))
  )
dup_phy
filtered_phy
#the difference in taxa should match how many mito, chloro, and non bacteria were removed

### 2c. Filter out singletons 
pruned_filtered_phy<-prune_taxa(taxa_sums(filtered_phy)>1, filtered_phy) #this is similar to subset but it keeps only the taxa that had more than 1 occurence using the taxa sums function
pruned_filtered_phy
filtered_phy
#I printed them to compare 

### 2d. Filter out latrines that were switched AND the pos and neg controls
final_filtered_phy<-subset_samples(pruned_filtered_phy, 
                                   !latrine_trt_month %in% c("L83_latrine_wet", 'L83_control_wet', "L94_latrine_wet", 'L94_control_wet', "L62_control_wet", 'L62_latrine_wet') #the ! means not, so we are removing those latrines
                                   & !is.na(latrine)) #this removes NAs (the pos and neg controls)
final_filtered_phy #print the two to compare
pruned_filtered_phy

#save it as R file so it can be easily loaded. at this point I recommend continuing
#through the rarefying step and save the rarefied file instead
saveRDS(final_filtered_phy, file="F:\\Research\\16S_Soil\\RDS Files\\final_filtered_phy_silva") #use whatever file path for where you want to save it
final_filtered_phy<-readRDS("F:\\Research\\16S_Soil\\RDS Files\\final_filtered_phy_silva")

######## 3. Rarefying. if you want to skip to the rarefying and not do all of these steps (3a-3d) 
#that visualize the number of reads and determine the rarefy value, skip to section 3e

###3a. Plot histogram of number of reads per sample
reads_per_sample<- data.frame(sum=sample_sums(final_filtered_phy))
ggplot(reads_per_sample, aes(x=sum))+
  geom_histogram(binwidth=2500)


### 3b. Determine the minimum number of reads
smin<- min(sample_sums(final_filtered_phy))
smin #this was used in tutorials as the value to rarefy to but it's very low


### 3c. Plot the rarefaction curves to determine sampling depth

#For all the data
otu.matrix = otu_table(final_filtered_phy) #make data into data frame
otu.matrix = as.data.frame(t(otu.matrix))
sample_names = rownames(otu.matrix) #add sample names

#plot
otu.rarecurve = rarecurve(otu.matrix, step = 50, label = F)
abline(v=1559)
abline(v=2141)
abline(v=8993)
abline(v=3515)


#we have looked at the curves and now decided which value to use to rarefy

#rarefy data
mirl_object_500<- mirl(final_filtered_phy, libsize=3705, set.seed=200, trimOTUs=T, replace=F, rep=500)

#make an empty object to put the ASV tables in
mirl_otu_500 <- vector("list", length(mirl_object_500))

#extract otu tables from each rarefied phyloseq and add to the empty object above
for (i in 1:length(mirl_object_500)){
  colnames(mirl_object_500[[i]]@otu_table) <- paste0(colnames(mirl_object_500[[i]]@otu_table))
  (mirl_otu_500[[i]] <- mirl_object_500[[i]]@otu_table)
}


#make metadata file with the correct samples (remove ones dropped during filtering)
sample_id<- data.frame(final_filtered_phy@sam_data) 
sample_id$Samples<- row.names(sample_id)
sample_id<- sample_id %>% 
  filter(!Samples %in% c(5, 21, 127))

sample_id <- sample_id$Samples

#make empty list for each sample
average_counts_500 <- vector("list", length(sample_id))

#give how many reps you will do
rep_500<-1:500
#make empty list to hold 500 dataframes
iter_list_500<- vector('list', length(rep_500))

#loop to select columns from each rep, then average them and put them in new otu table
for (i in 1:length(sample_id) ){
  for (j in rep_500){
    iter_list_500[[j]]<-dplyr::select(as.data.frame(mirl_otu_500[[j]]),i) #this selects each individual iteration's otu table and 
    iter_list_500[[j]]$ASVname<- row.names(iter_list_500[[j]]) #this makes a column with asv names
  }
  
  sample_df_500<- reduce(iter_list_500[rep_500], full_join, by='ASVname') #this combines all the iterations of a sample into one dataframe
  sample_df_500[is.na(sample_df_500)]<-0 #make NAs into 0s
  row.names(sample_df_500)<- sample_df_500$ASVname #make row names the ASV names
  sample_df_500<- sample_df_500[,c(1, 3:(1+length(rep_500)))] #remove the ASV name column
  sample_average_500 <- data.frame(rowMeans(sample_df_500)) #calculate the mean of each row (which is the avg abundance on each ASV across iterations)
 #in this data frame, it has all the ASVs and each column is the same sample but each iteraiton of it. so sample 13, but 500 iterations.
  #then it averages the abundance of each ASV across each iteraion of sample 13. and then repeats this for each sample.
   colnames(sample_average_500) <- sample_id[[i]] #make the column name the sample number
  average_counts_500[[i]] <- sample_average_500 #put into list which has an element for each sample
}
average_count_df_500 <- do.call(cbind, average_counts_500)

write.csv(x=average_count_df_500, file="500rep_averaged_OTUtable.csv")

#check that they each have the rarefied number of ASVs
colSums(average_count_df_500)

#add to phyloseq
mirl_phyloseq <- final_filtered_phy
mirl_phyloseq@otu_table@.Data <- as.matrix(average_count_df_500)

rowSums(mirl_phyloseq@otu_table)==rowSums(average_count_df_500) #should be true



# Final phyloseq (filtered and rarefied) ----
#save the data as an R file so it doesn't have to be loaded each time.
#now when you start R, you can load the metadata and waypoints in step 1a. and skip
#steps 1b-3e

saveRDS(mirl_phyloseq, file="filt_rare_phy_16s.rds") #use whatever file path for where you want to save it
filt_rare_phy_16s<-readRDS("filt_rare_phy_16s.rds")


# Alpha Diversity ----
############
############

###### 4. Diversity Analysis [AGGLOMERATED GENUS LEVEL]

## NECESSARY Calculate Diversity ----
### 4a. Calculate diversity
# recode all NAs as incertae sedis so they are counted as the same
taxa_all<- data.frame(tax_table(filt_rare_phy_16s))
taxa_all$Phylum[is.na(taxa_all$Phylum)] <- "Incertae_Sedis"
taxa_all$Class[is.na(taxa_all$Class)] <- "Incertae_Sedis"
taxa_all$Order[is.na(taxa_all$Order)] <- "Incertae_Sedis"
taxa_all$Family[is.na(taxa_all$Family)] <- "Incertae_Sedis"
taxa_all$Genus[is.na(taxa_all$Genus)] <- "Incertae_Sedis"

filt_rare_phy_16s@tax_table<-tax_table(as.matrix(taxa_all))

# for now we are calculating diversity for just things IDed to genus
#this means richness is how many individual genera are in each sample and Shannon
#diversity is calculated where each individual genus is a "thing"

#filter out incertae sedis for genus level alpha diversity
taxa_noNA_genus<- taxa_all %>% 
  filter(Genus != 'Incertae_Sedis')
phy_noNA_genus<- filt_rare_phy_16s
phy_noNA_genus@tax_table<- tax_table(as.matrix(taxa_noNA_genus))

#glomerate by genus
phy_noNA_genus_glom<-tax_glom(phy_noNA_genus, taxrank='Genus', NArm=T)

#view taxa table
glom_taxa<- data.frame(tax_table(phy_noNA_genus_glom))

#take out asv table to calculate diversity
glom_asv<- data.frame(otu_table(phy_noNA_genus_glom))
glom_asv<- data.frame(t(glom_asv), check.names = F)

### All data Shannon
#calculate shannon's diversity
shan_div_glom<-data.frame(diversity(glom_asv, index='shannon'))

#rename column to Shannon
colnames(shan_div_glom)[1]<- 'Shannon'

#make column with sample id to merge with metadata
shan_div_glom$`#SampleID`<- row.names(shan_div_glom)

# #for some reason it added X to the beginning of the sample names so I removed it here:
shan_div_glom$`#SampleID`<-sub('.', '', shan_div_glom$`#SampleID`)

#merge with the metadata so we can run a model and filter out the stuff already filtered out
metadata_filt<-metadata %>%
  left_join(shan_div_glom, by='#SampleID') %>%
  filter(!is.na(Shannon))

#All Richness
richness_glom<- data.frame(specnumber(glom_asv))

#rename column to Shannon
colnames(richness_glom)[1]<- 'Observed'

#make column with sample id to merge with metadata
richness_glom$`#SampleID`<- row.names(richness_glom)

# #for some reason it added X to the beginning of the sample names so I removed it here:
richness_glom$`#SampleID`<-sub('.', '', richness_glom$`#SampleID`)

# #merge with metadata
metadata_filt<- metadata_filt %>% 
  left_join(richness_glom, by='#SampleID') 

# all inverse simpson
simp_glom<- data.frame(diversity(glom_asv, index='invsimpson'))
simp_glom$`#SampleID`<- row.names(simp_glom)

colnames(simp_glom)[1]<- 'InvSimpson'
simp_glom$`#SampleID`<-sub('.', '', simp_glom$`#SampleID`)

metadata_filt<- metadata_filt %>% 
  left_join(simp_glom, by='#SampleID')

# all Pielou evenness
metadata_filt$Pielou<- metadata_filt$Shannon/ log(637) #divide by the number of things used to calculate shannons, aka number of genera

#add elevation
#format latrine names
waypoints$latrineF<-gsub("^.{0,4}", "", waypoints$latrine)
waypoints$latrine<- paste("L", waypoints$latrineF, sep='') 

#sometimes it makes weird column names so try just highlighting this code and rerunning it
metadata_filt<- metadata_filt %>% 
  left_join(waypoints, by='latrine') 

###### 4. Diversity Analysis [AT THE ASV level]----

## NECESSARY Calculate Diversity 
### 4a. Calculate diversity
# 
taxa_all<- data.frame(tax_table(filt_rare_phy_16s))
taxa_all$Phylum[is.na(taxa_all$Phylum)] <- "Incertae_Sedis"
taxa_all$Class[is.na(taxa_all$Class)] <- "Incertae_Sedis"
taxa_all$Order[is.na(taxa_all$Order)] <- "Incertae_Sedis"
taxa_all$Family[is.na(taxa_all$Family)] <- "Incertae_Sedis"
taxa_all$Genus[is.na(taxa_all$Genus)] <- "Incertae_Sedis"

filt_rare_phy_16s@tax_table<-tax_table(as.matrix(taxa_all))

#fix metadata names 
metadata <- metadata %>%
  dplyr::rename(SampleID = `#SampleID`)

# ### All data shannon
all_shan_div<-estimate_richness(filt_rare_phy_16s, measures='Shannon')
all_shan_div$`SampleID`<- row.names(all_shan_div)

#merge with the metadata so we can run a model and filter out the stuff already filtered out

metadata_filt<-metadata %>%
  left_join(all_shan_div, by='SampleID') %>%
  filter(!is.na(Shannon))


#doesn't like non-integers from rarefaction averaging, so calc richness with vegan specnumber() 
otu_mat <- as(otu_table(filt_rare_phy_16s), "matrix") 
if(taxa_are_rows(filt_rare_phy_16s)) {otu_mat <- t(otu_mat)} #transpose

richness <- specnumber(otu_mat) #calculate richness
sample_ids <- sample_names(filt_rare_phy_16s) #grab samp IDs
all_richness <- data.frame(`SampleID` = sample_ids, Observed = richness)

# #merge with metadata
metadata_filt<- metadata_filt %>% 
  left_join(all_richness, by='SampleID') %>% 
  filter(!is.na(Observed))

# all inverse simpson
all_simpson<- estimate_richness(filt_rare_phy_16s, measures='InvSimpson')
all_simpson$`SampleID`<- row.names(all_simpson)

metadata_filt<- metadata_filt %>% 
  left_join(all_simpson, by='SampleID')

# all Pielou evenness
overall_S <- sum(colSums(otu_mat) > 0) #calc total number of ASVs
overall_S
metadata_filt$Pielou<- metadata_filt$Shannon/ log(overall_S)


#add elevation
#format latrine names
waypoints$latrineF<-gsub("^.{0,4}", "", waypoints$latrine)
waypoints$latrine<- paste("L", waypoints$latrineF, sep='') 

#sometimes it makes weird column names so try just highlighting this code and rerunning it
metadata_filt<- metadata_filt %>% 
  left_join(waypoints, by='latrine') 


#format slope and aspect data to be merged
#slope_aspect$latrine<- slope_aspect$Latrine
#join them together
#metadata_filt<- metadata_filt %>% 
#  left_join(slope_aspect, by='latrine')

#add critter stuff
critter$latrine<- critter$X

metadata_crit<- metadata_filt %>% 
  filter(treatment=='latrine') %>% 
  dplyr::select(c('latrine','Observed','Shannon','elevation','InvSimpson','Pielou','replicate','latrine_trt_month','month-collected','elevation'))

critter_filt<-critter %>% 
  full_join(metadata_crit, by='latrine') 
 #export as csv to finish combining data 

write.csv(critter_filt, file='critter_data.csv')

critter_data<- read.csv('critter_data.csv')

critter_data$elevation_sc<- scale(critter_data$elevation)

#add chronosequences
metadata_filt<- metadata_filt %>% 
  full_join(chrono, by='latrine')

############ make the things going into the models factors
## Wet Subset Models ----
### 4c. Run models wet data subset by soil age 

# Wet season metadata
metadata_wet<- metadata_filt %>% 
  filter(`month-collected`=='wet')

critter_wet<- critter_data %>% 
  filter(month.collected=='wet')

#factor the variables in the models and scale elevation
metadata_wet<-metadata_wet %>% 
  mutate(treatment=as.factor(treatment), soilAge=as.factor(soilAge), class=factor(class, levels=c('LIA', 'LIA-1931','1931-1962','1984-2024'))) %>% 
  mutate(elevation_sc= scale(elevation))
names(metadata_wet)
#when you view the data, the elevation scaled column has a weird name but R says its name is elevation_sc
# also it is centering it because that is the default
Wel_mean<-mean(metadata_wet$elevation)
Wel_sd<-sd(metadata_wet$elevation)
#to calculate an elevation's value on the scaled scale, subtract the mean and divide by the sd
(5100-Wel_mean)/Wel_sd

### Richness ----
#wet season richness, poisson model bc not overdispersed
m_wet_rich<- lmer(Observed~treatment*soilAge+elevation_sc*treatment+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
summary(m_wet_rich)
Anova(m_wet_rich, type='III')
qqnorm(residuals(m_wet_rich))
emmeans(m_wet_rich, pairwise~treatment*soilAge)

#use exp to backtransform bc on log scale
#the coefficients work exactly how we'd expect. at low elevations, controls are move diverse but than
# at higher elevations, latrines are more diverse. I calculated this by doing the math first and then
# exp() of the final answer. I added treatment and soil age terms as necessary and then for elevation you multiply
# the coefficient times the scale(elevation) value (ex. -.34408*1.89) and if it's latrines you do that AND
# the latrine:elevation coefficient times the scale(elevation) value (ex -.34408*1.89 + .25140*1.89)

#do LIA to see if treatment is significant
wet_richLIA<- lmer(Observed~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metalia2)
summary(wet_richLIA)
Anova(wet_richLIA)

#do RGM wet to see if treatment is significant
wet_richRGMw<- lmer(Observed~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metargmW2)
summary(wet_richRGMw)
Anova(wet_richRGMw)

#compare AIC to model without elevation
m_wet_rich<-lmer(Observed~treatment*soilAge+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
summary(m_wet_rich)
Anova(m_wet_rich)

#compare to soil Age null model. this tests if having soil age at all in the model makes it better
m_wet_rich_nullS<- lmer(Observed~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
lrtest(m_wet_richNB, m_wet_rich_nullS) #if p value is sig, then the regular model is better than the null model

#compare to interaction null model. this tests if the soil age interaction is significant
m_wet_rich_nullI<- lmer(Observed~treatment*elevation_ref+soilAge+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
lrtest(m_wet_richNB, m_wet_rich_nullI)

#chronosequence model
m_wet_rich_chrono<- lmer(Observed~treatment*class+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
summary(m_wet_rich_chrono)
Anova(m_wet_rich_chrono, type='III')
export<-emmeans(m_wet_rich_chrono, pairwise~treatment*class)
write.csv(export$contrasts, "wetChronoRichnessPairwise.csv", row.names = FALSE)

qqnorm(residuals(m_wet_rich_chrono))

#vicuna RAI
m_wet_vrai<- lmer(Observed~Vicuna.RAI*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vrai)
Anova(m_wet_vrai, type='III')
qqnorm(residuals(m_wet_vrai))

#split elevation RAI 
critter_wet_split <- critter_wet %>%
  mutate(elev_group = ifelse(
    elevation <= 5300,
    "Low elevation",
    "High elevation"))

m_wet_vrai_low <- lmer(
  Observed ~ Vicuna.RAI + (1 | latrine_trt_month),
  data = filter(critter_wet_split, elev_group == "Low elevation"))
summary(m_wet_vrai_low)
Anova(m_wet_vrai_low, type='III')
qqnorm(residuals(m_wet_vrai_low))

m_wet_vrai_high <- lmer(
  Observed ~ Vicuna.RAI + (1 | latrine_trt_month),
  data = filter(critter_wet_split, elev_group == "High elevation"))
summary(m_wet_vrai_high)
Anova(m_wet_vrai_high, type='III')
qqnorm(residuals(m_wet_vrai_high))

m_wet_vrai_both <- m_wet_vrai <- lmer(Observed ~ Vicuna.RAI * elev_group + (1 | latrine_trt_month),
    data = critter_wet_split)
summary(m_wet_vrai_both)
Anova(m_wet_vrai_both, type='III')
qqnorm(residuals(m_wet_vrai_high))



#all vert richness
m_wet_vertrich_rich<- lmer(Observed~Animal.Richness*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vertrich_rich)
Anova(m_wet_vertrich_rich, type='III')
qqnorm(residuals(m_wet_vertrich_rich))

#all vert shannon
m_wet_vertshan_rich<- lmer(Observed~Shannon.Index*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vertshan_rich)
Anova(m_wet_vertshan_rich, type='III')
qqnorm(residuals(m_wet_vertshan_rich))

### Shannon's Diversity ----
#Wet season Shannon's diversity using reference elevation
m_wet_shan_div<-lmer(Shannon~treatment*soilAge+elevation_sc*treatment+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
Anova(m_wet_shan_div, type='III')
summary(m_wet_shan_div)
emmeans(m_wet_shan_div, pairwise~treatment*soilAge)

#check model assumptions
qqnorm(residuals(m_wet_shan_div)) #checking normality

#compare AIC to model without elevation
m_wet_shan<-lmer(Shannon~treatment*soilAge+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
summary(m_wet_shan)
Anova(m_wet_shan)

#compare to soil Age null model
m_wet_shan_nullS<- lmer(Shannon~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
lrtest(m_wet_shan_div, m_wet_shan_nullS)

#compare to interaction null model
m_wet_shan_nullI<- lmer(Shannon~treatment*elevation_sc+soilAge+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
lrtest(m_wet_shan_div, m_wet_shan_nullI)

#chronosequence model
m_wet_shan_chrono<- lmer(Shannon~treatment*class+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
summary(m_wet_shan_chrono)
Anova(m_wet_shan_chrono, type='III')
emmeans(m_wet_shan_chrono, pairwise~treatment*class)
qqnorm(residuals(m_wet_shan_chrono))

#vicuna RAI
m_wet_vrai_shan<- lmer(Shannon~Vicuna.RAI*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vrai_shan)
Anova(m_wet_vrai_shan, type='III')
qqnorm(residuals(m_wet_vrai_shan))


#all vert richness
m_wet_vertrich_shan<- lmer(Shannon~Animal.Richness*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vertrich_shan)
Anova(m_wet_vertrich_shan, type='III')
qqnorm(residuals(m_wet_vertrich_shan))

#all vert shannon
m_wet_vertshan_shan<- lmer(Shannon~Shannon.Index*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vertshan_shan)
Anova(m_wet_vertshan_shan, type='III')
qqnorm(residuals(m_wet_vertshan_shan))


### Inv Simpson's Diversity ----
#wet season Simpson with reference elevation
m_wet_simp<-lmer(InvSimpson~treatment*soilAge+elevation_sc*treatment+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
Anova(m_wet_simp, type='III')
summary(m_wet_simp)
emmeans(m_wet_simp, pairwise~treatment*soilAge)

#check model assumptions
qqnorm(residuals(m_wet_simp)) #checking normality

#compare to soil Age null model
m_wet_simp_nullS<- lmer(InvSimpson~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
lrtest(m_wet_simp, m_wet_simp_nullS)

#compare to interaction null model
m_wet_simp_nullI<- lmer(InvSimpson~treatment*elevation_sc+soilAge+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
lrtest(m_wet_simp, m_wet_simp_nullI)

#chronosequence model
m_wet_simp_chrono<- lmer(InvSimpson~treatment*class+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
summary(m_wet_simp_chrono)
Anova(m_wet_simp_chrono, type='III')
emmeans(m_wet_simp_chrono, pairwise~treatment*class)
qqnorm(residuals(m_wet_simp_chrono))

#vicuna RAI
m_wet_vrai_simp<- lmer(InvSimpson~Vicuna.RAI*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vrai_simp)
Anova(m_wet_vrai_simp, type='III')
qqnorm(residuals(m_wet_vrai_simp))

#all vert richness
m_wet_vertrich_simp<- lmer(InvSimpson~Animal.Richness*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vertrich_simp)
Anova(m_wet_vertrich_simp, type='III')
qqnorm(residuals(m_wet_vertrich_simp))

#all vert shannon
m_wet_vertshan_simp<- lmer(InvSimpson~Shannon.Index*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vertshan_simp)
Anova(m_wet_vertshan_simp, type='III')
qqnorm(residuals(m_wet_vertshan_simp))


### Pielou evenness ----
#logit transform Pielou to use a linear model with it
m_wet_pie<- lmer(logit(Pielou)~treatment*soilAge+elevation_sc*treatment+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
Anova(m_wet_pie, type='III')
summary(m_wet_pie)
qqnorm(residuals(m_wet_pie))
emmeans(m_wet_pie, pairwise~treatment*soilAge)


#chronosequence model
m_wet_pie_chrono<- lmer(Pielou~treatment*class+(1|latrine_trt_month)+(1|latrine), data=metadata_wet)
summary(m_wet_pie_chrono)
Anova(m_wet_pie_chrono, type='III')
emmeans(m_wet_pie_chrono, pairwise~treatment*class)
qqnorm(residuals(m_wet_pie_chrono))


#vicuna RAI
m_wet_vrai_pie<- lmer(Pielou~Vicuna.RAI*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vrai_pie)
Anova(m_wet_vrai_pie, type='III')
qqnorm(residuals(m_wet_vrai_pie))

#all vert richness
m_wet_vertrich_pie<- lmer(Pielou~Animal.Richness*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vertrich_pie)
Anova(m_wet_vertrich_pie, type='III')
qqnorm(residuals(m_wet_vertrich_pie))

#all vert shannon
m_wet_vertshan_pie<- lmer(Pielou~Shannon.Index*elevation_sc+(1|latrine_trt_month), data=critter_wet)
summary(m_wet_vertshan_pie)
Anova(m_wet_vertshan_pie, type='III')
qqnorm(residuals(m_wet_vertshan_pie))


### Just RGM Wet----
metaWetRGM_both<- metadata_filt %>% 
  filter(soilAge=='rgm' & `month-collected`=='wet')

metaWetRGM_both<-metaWetRGM_both %>% 
  mutate(treatment=as.factor(treatment), `month-collected`=as.factor(`month-collected`)) %>% 
  mutate(elevation_sc=scale(elevation))

#richness
m_WetRGM_rich<- lmer(Observed~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metaWetRGM_both)
summary(m_WetRGM_rich)
Anova(m_WetRGM_rich, type='III')

#Shannon
m_WetRGM_shan<- lmer(Shannon~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metaWetRGM_both)
summary(m_WetRGM_shan)
Anova(m_WetRGM_shan, type='III')
qqnorm(residuals(m_WetRGM_shan))

#Inv Simpson
m_WetRGM_simp<- lmer(InvSimpson~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metaWetRGM_both)
summary(m_WetRGM_simp)
Anova(m_WetRGM_simp, type='III')

#RAI models
m_WetRGM_richRAI<- glmer.nb(Observed~treatment*RAI_IE_vicugna+(1|latrine_trt_month)+(1|latrine), data=metaWetRGM_both)
summary(m_WetRGM_richRAI)
Anova(m_WetRGM_richRAI)

m_WetRGM_shanRAI<- lmer(Shannon~treatment*RAI_IE_vicugna+(1|latrine_trt_month)+(1|latrine), data=metaWetRGM_both)
summary(m_WetRGM_shanRAI)
Anova(m_WetRGM_shanRAI)
qqnorm(residuals(m_WetRGM_shanRAI))

### Just LIA Wet----
metaLIA_both<- metadata_filt %>% 
  filter(soilAge=='lia' & `month-collected`=='wet')

metaLIA_both<-metaLIA_both %>% 
  mutate(treatment=as.factor(treatment), `month-collected`=as.factor(`month-collected`)) %>% 
  mutate(elevation_sc=scale(elevation))

#richness
m_LIA_richNB<- glmer(Observed~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metaLIA_both,family=poisson(link='log'))
summary(m_LIA_richNB)
Anova(m_LIA_richNB, type='III')

#Shannon
m_LIA_shan<- lmer(Shannon~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metaLIA_both)
summary(m_LIA_shan)
Anova(m_LIA_shan, type='III')
qqnorm(residuals(m_LIA_shan))

#Inv Simpson
m_LIA_simp<- lmer(InvSimpson~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metaLIA_both)
summary(m_LIA_simp)
Anova(m_LIA_simp, type='III')



## RGM Subset Models ----
### 4d. Run model for RGM data

#metadata file
metadata_RGM<-metadata_filt %>% 
  filter(soilAge=='rgm')

metadata_RGM<-metadata_RGM %>% 
  mutate(treatment=as.factor(treatment), `month-collected`=as.factor(`month-collected`)) %>% 
  mutate(elevation_sc=scale(elevation))
str(metadata_RGM)

### Richness----
#RGM model richness with scaled elevation
m_season_richNB<- glmer(Observed~treatment*`month-collected`+elevation_sc*treatment+(1|latrine_trt_month)+(1|latrine), data=metadata_RGM,family=poisson(link='log'))
summary(m_season_richNB)
Anova(m_season_richNB, type='III')
emmeans(m_season_richNB, pairwise~treatment*`month-collected`)

#compare to season null model. tests if having season at all is better than not having it
m_season_rich_nullS<- glmer(Observed~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metadata_RGM,family=poisson(link='log'))
lrtest(m_season_richNB, m_season_rich_nullS)

#compare to interaction null model. tests if interaction with season is better than non interaction
m_season_rich_nullI<- glmer(Observed~treatment*elevation_sc+`month-collected`+(1|latrine_trt_month)+(1|latrine), data=metadata_RGM,family=poisson(link='log'))
lrtest(m_season_richNB, m_season_rich_nullI) # if pvalue is sig, interaction is better than not

#plot the richness data to see interaction between treatment and elevation
ggplot(metadata_RGM, aes(x=elevation, y=Observed, color=trt_month))+
  geom_point()+
  geom_smooth(method='lm') #default 95% CI


### Shannon's Diversity----
# RGM model shannon with reference elevation
m_season_shan<- lmer(Shannon~treatment*`month-collected`+elevation_sc*treatment+(1|latrine_trt_month)+(1|latrine), data=metadata_RGM)
summary(m_season_shan)
Anova(m_season_shan)

#check model assumptions
qqnorm(residuals(m_season_shan)) #checking normality

#compare to season null model
m_season_shan_nullS<- lmer(Shannon~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metadata_RGM)
lrtest(m_season_shan, m_season_shan_nullS)

#compare to interaction null model
m_season_shan_nullI<- lmer(Shannon~treatment*elevation_sc+`month-collected`+(1|latrine_trt_month)+(1|latrine), data=metadata_RGM)
lrtest(m_season_shan, m_season_shan_nullI)

### Inv Simpson's Diversity----
#RGM simpson with reference elevation
m_RGM_simp<-lmer(InvSimpson~treatment*`month-collected`+elevation_sc*treatment+(1|latrine_trt_month)+(1|latrine), data=metadata_RGM)
Anova(m_RGM_simp)
summary(m_RGM_simp)

#check model assumptions
qqnorm(residuals(m_RGM_simp)) #checking normality


#compare to season null model
m_RGM_simp_nullS<- lmer(InvSimpson~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metadata_RGM)
lrtest(m_RGM_simp, m_RGM_simp_nullS)

#compare to interaction null model
m_RGM_simp_nullI<- lmer(InvSimpson~treatment*elevation_sc+`month-collected`+(1|latrine_trt_month)+(1|latrine), data=metadata_RGM)
lrtest(m_RGM_simp, m_RGM_simp_nullI)

### Pielou evenness ----
#logit transform Pielou to use a linear model with it
m_RGM_pie<- lmer(logit(Pielou)~treatment*`month-collected`+elevation_sc*treatment+(1|latrine_trt_month)+(1|latrine), data = metadata_RGM)
summary(m_RGM_pie)
Anova(m_RGM_pie, type='III')
qqnorm(residuals(m_RGM_pie))





### RGM Dry----
metaDryRGM_both<- metadata_filt %>% 
  filter(soilAge=='rgm' & `month-collected`=='dry')

metaDryRGM_both<-metaDryRGM_both %>% 
  mutate(treatment=as.factor(treatment), `month-collected`=as.factor(`month-collected`), class=factor(class, levels=c('LIA', 'LIA-1931','1931-1962','1984-2024'))) %>% 
  mutate(elevation_sc=scale(elevation))

critter_dry<- critter_data %>% 
  filter(month.collected=='dry')

#richness
m_dry_rich<- lmer(Observed~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metaDryRGM_both)
summary(m_dry_rich)
Anova(m_dry_rich, type='III')
qqnorm(residuals(m_dry_rich))

#chronosequence model
m_dry_rich_chrono<- lmer(Observed~treatment*class+(1|latrine_trt_month)+(1|latrine), data=metaDryRGM_both)
summary(m_dry_rich_chrono)
Anova(m_dry_rich_chrono, type='III')
export<-emmeans(m_dry_rich_chrono, pairwise~treatment*class)
qqnorm(residuals(m_dry_rich_chrono))

write.csv(export$contrasts, "dryChronoRichnessPairwise.csv", row.names = FALSE)

#vicuna RAI
m_dry_vrai_rich<- lmer(Observed~Vicuna.RAI*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vrai_rich)
Anova(m_dry_vrai_rich, type='III')
qqnorm(residuals(m_dry_vrai_rich))

#all vert richness
m_dry_vertrich_rich<- lmer(Observed~Animal.Richness*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vertrich_rich)
Anova(m_dry_vertrich_rich, type='III')
qqnorm(residuals(m_dry_vertrich_rich))

#all vert shannon
m_dry_vertshan_rich<- lmer(Observed~Shannon.Index*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vertshan_rich)
Anova(m_dry_vertshan_rich, type='III')
qqnorm(residuals(m_dry_vertshan_rich))



####Shannon
m_dry_shan<- lmer(Shannon~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metaDryRGM_both)
summary(m_dry_shan)
Anova(m_dry_shan, type='III')
qqnorm(residuals(m_dry_shan))

#chronosequence model
m_dry_shan_chrono<- lmer(Shannon~treatment*class+(1|latrine_trt_month)+(1|latrine), data=metaDryRGM_both)
summary(m_dry_shan_chrono)
Anova(m_dry_shan_chrono, type='III')
emmeans(m_dry_shan_chrono, pairwise~treatment*class)
qqnorm(residuals(m_dry_shan_chrono))

#vicuna RAI
m_dry_vrai_shan<- lmer(Shannon~Vicuna.RAI*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vrai_shan)
Anova(m_dry_vrai_shan, type='III')
qqnorm(residuals(m_dry_vrai_shan))

#all vert richness
m_dry_vertrich_shan<- lmer(Shannon~Animal.Richness*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vertrich_shan)
Anova(m_dry_vertrich_shan, type='III')
qqnorm(residuals(m_dry_vertrich_shan))

#all vert shannon
m_dry_vertshan_shan<- lmer(Shannon~Shannon.Index*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vertshan_shan)
Anova(m_dry_vertshan_shan, type='III')
qqnorm(residuals(m_dry_vertshan_shan))


####Inv Simpson
m_dry_simp<- lmer(InvSimpson~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metaDryRGM_both)
summary(m_dry_simp)
Anova(m_dry_simp, type='III')
qqnorm(residuals(m_dry_simp))

#chronosequence model
m_dry_simp_chrono<- lmer(InvSimpson~treatment*class+(1|latrine_trt_month)+(1|latrine), data=metaDryRGM_both)
summary(m_dry_simp_chrono)
Anova(m_dry_simp_chrono, type='III')
emmeans(m_dry_simp_chrono, pairwise~treatment*class)
qqnorm(residuals(m_dry_simp_chrono))

#vicuna RAI
m_dry_vrai_simp<- lmer(InvSimpson~Vicuna.RAI*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vrai_simp)
Anova(m_dry_vrai_simp, type='III')
qqnorm(residuals(m_dry_vrai_simp))

#all vert richness
m_dry_vertrich_simp<- lmer(InvSimpson~Animal.Richness*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vertrich_simp)
Anova(m_dry_vertrich_simp, type='III')
qqnorm(residuals(m_dry_vertrich_simp))

#all vert shannon
m_dry_vertshan_simp<- lmer(InvSimpson~Shannon.Index*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vertshan_simp)
Anova(m_dry_vertshan_simp, type='III')
qqnorm(residuals(m_dry_vertshan_simp))



####Pielou
m_dry_pie<- lmer(logit(Pielou)~treatment*elevation_sc+(1|latrine_trt_month)+(1|latrine), data=metaDryRGM_both)
summary(m_dry_pie)
Anova(m_dry_pie, type='III')
qqnorm(residuals(m_dry_pie))


#chronosequence model
m_dry_pie_chrono<- lmer(Pielou~treatment*class+(1|latrine_trt_month)+(1|latrine), data=metaDryRGM_both)
summary(m_dry_pie_chrono)
Anova(m_dry_pie_chrono, type='III')
emmeans(m_dry_pie_chrono, pairwise~treatment*class)
qqnorm(residuals(m_dry_pie_chrono))

#vicuna RAI
m_dry_vrai_pie<- lmer(Pielou~Vicuna.RAI*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vrai_pie)
Anova(m_dry_vrai_pie, type='III')
qqnorm(residuals(m_dry_vrai_pie))

#all vert richness
m_dry_vertrich_pie<- lmer(Pielou~Animal.Richness*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vertrich_pie)
Anova(m_dry_vertrich_pie, type='III')
qqnorm(residuals(m_dry_vertrich_pie))

#all vert shannon
m_dry_vertshan_pie<- lmer(Pielou~Shannon.Index*elevation_sc+(1|latrine_trt_month), data=critter_dry)
summary(m_dry_vertshan_pie)
Anova(m_dry_vertshan_pie, type='III')
qqnorm(residuals(m_dry_vertshan_pie))

# Beta Diversity ----
##### 5. Beta Diversity analysis ASV LEVEL

############## This step is neccessary 
### NECESSARY Reroot the tree.----
# 5a. Reroot the tree
#It has to be binary but now it is not since we trimmed it
ps_tree<- phy_tree(filt_rare_phy_16s) #put tree into an object
is.binary(ps_tree) #asking if it is binary. if false, go to next step

phy_tree(filt_rare_phy_16s)<-multi2di(ps_tree) #fix the tree and put it back in the phyloseq
is.binary(phy_tree(filt_rare_phy_16s)) #check if it's binary, should be true



## NOT NECESSARY Compare subsamples (replicates) Permanova----

###permanova for replicates 1 and 2 to see if they differ

#factor the variables
metadata_factored<- metadata_filt
metadata_factored$replicate<- as.factor(metadata_factored$replicate)
metadata_factored$latrine_trt<- as.factor(metadata_factored$latrine_trt)

#reorder the metadata to match the order of the phyloseq
sampr<- sample_data(filt_rare_phy_16s) #pull out data from phyloseq

#order metadata to match that from phyloseq
metadata_factored_rep<-metadata_factored[ order(match(metadata_factored$`SampleID`, row.names(sampr))), ]

set.seed(200)
#run permanova
#permanova<- adonis2(distance(filt_rare_phy, method='wunifrac')~replicate, data=metadata_factored_rep, by='terms')
#permanova

#new way so that it is testing replicate but within each latrine. we think this is the proper way to test replicate variation
perm_rep<- adonis2(distance(filt_rare_phy_16s, method='wunifrac')~replicate*latrine_trt, data=metadata_factored_rep, by='terms')
perm_rep



## NECESSARY Run this step to get filtered phyloseq that is used for beta diversity----
# 5b. Change ASV names 
metadata_factored<- metadata_filt
metadata_factored$treatment<- as.factor(metadata_factored$treatment)
metadata_factored$soilAge<- as.factor(metadata_factored$soilAge)
metadata_factored$`month-collected`<- as.factor(metadata_factored$`month-collected`)
metadata_factored$trt_month<- as.factor(metadata_factored$trt_month)
metadata_factored$trt_soilAge<- as.factor(metadata_factored$trt_soilAge)
metadata_factored$class<- factor(metadata_factored$class, levels=c('LIA','LIA-1931','1931-1962','1984-2024'))
metadata_factored$trt_class<- factor(paste(metadata_factored$treatment, metadata_factored$class, sep='_'), 
                                     levels=c('control_LIA','latrine_LIA','control_LIA-1931','latrine_LIA-1931','control_1931-1962','latrine_1931-1962','control_1984-2024','latrine_1984-2024'))


## they weren't different so we are going to choose just replicate 2 from the data
##because having 2 is pseudoreplication
##you could choose replicate 1, or randomly sample, whatever you want
filt_rare_rep2<- subset_samples(filt_rare_phy_16s, replicate %in% (2))

#### make a dataframe that has the original and new asv names for convenience
## the original names are a random long string of characters so this makes them easier
## to reference and the data frame saves the original and new name so we know what's what

#pull out taxa table
taxa<- as.data.frame(tax_table(filt_rare_rep2))
#pull out the tree
tree<- phy_tree(filt_rare_rep2)
#make sure tips are in same order as taxa
sum(tree$tip.label==row.names(taxa)) 
#if using replicate 2, it should print 28372 (because that's how many asvs there are
## and so that tells us that the tree tips are in the same order of the taxa table)

#put original names into df
#use both_names to look up the original qiime2 asv name
both_names<- data.frame(original=rownames(taxa))
#rename asvs in taxa table and add to df
rownames(taxa)<- paste('ASV', seq(1,28372,1), sep='_')
both_names$number<- rownames(taxa)
#take out asv table and rename that too
asvfull<- otu_table(filt_rare_rep2)
rownames(asvfull)<- paste('ASV', seq(1,28372,1), sep='_')
#convert them into matrix to put back into phyloseq
tax<- tax_table(as.matrix(taxa))
otu<- otu_table(as.matrix(asvfull), taxa_are_rows = T)
sample<- sample_data(filt_rare_rep2)
#rename the tree tips too
tree$tip.label<- paste('ASV', seq(1,28372,1), sep='_')

#put all this back into phyloseq so ASVs now have a normal number name
rep2_named_phy<- phyloseq(otu, tax, sample, tree)
#this is the phyloseq we will use


## NECESSARY Subset for Wet season ----
# 5c. Subset wet data
#make metadata. make sure that of the samples that were rarefied out, they don't belong to the replicate
# that is being chosen for the beta diversity stuff. so here, L70 control wet rep 1 was dropped when we 
# rarefied so if we choose replicate 1, there is no L70 control wet representation in our data so
#we have to make sure it is chosen
metadata_wet2<- metadata_factored %>% 
  filter(`month-collected`=='wet' & replicate==2)

#filter the phyloseq for only wet samples
filt_rare_wet2<- subset_samples(rep2_named_phy, `month.collected` %in% ('wet'))

#reorder the metadata to match the order of the phyloseq
samp<- sample_data(filt_rare_wet2) #pull out data from phyloseq

metadata_wet2<-metadata_wet2[ order(match(metadata_wet2$`SampleID`, row.names(samp))), ]


## NECESSARY Subset for RGM data----
# 5d. Subset RGM data
#metadata factored
metadata_RGM2<- metadata_factored %>% 
  filter(soilAge=='rgm', replicate==2) 

#make phyloseq for RGM only
filt_rare_RGM2<- rep2_named_phy%>% 
  subset_samples(soilAge %in% ('rgm')) %>% 
  subset_samples(replicate %in% (2)) 

#order samples
sampR<- sample_data(filt_rare_RGM2) #pull out data from phyloseq

metadata_RGM2<-metadata_RGM2[order(match(metadata_RGM2$`SampleID`, row.names(sampR))), ]



## Wet Subset Permanova ----
### 5e. Permanova test with wet season data
set.seed(200) ###VERY IMPORTANT, always keep the same

#run permanova
permanova_wet<- adonis2(distance(filt_rare_wet2, method='wunifrac')~treatment*soilAge, data=metadata_wet2, by='terms')
permanova_wet

#pairwise permanova to see which groups are different from each other
permanova_pairwise(distance(filt_rare_wet2, method='wunifrac'), grp=metadata_wet2$trt_soilAge, padj='holm')

# see Plots_16S file for code to make plots

#beta dispersion
wet_betadis<-betadisper(distance(filt_rare_wet2, method='wunifrac'), group=metadata_wet2$trt_soilAge, type='median')
boxplot(wet_betadis)
permutest(wet_betadis, permutations=999)

#pairwise to determine which differ significantly 
distances <- wet_betadis$distances
mod <- aov(distances ~ metadata_wet2$trt_soilAge)
qqnorm(residuals(mod))
shapiro.test(residuals(mod))
TukeyHSD(wet_betadis)

permutest(wet_betadis, pairwise = TRUE, permutations = 999)


##chronosequence
set.seed(200)
adonis2(distance(filt_rare_wet2, method='wunifrac')~treatment*class, data=metadata_wet2, by='terms')
exports<-permanova_pairwise(distance(filt_rare_wet2, method='wunifrac'), grp=metadata_wet2$trt_class, padj='holm')
write.csv(exports, "wetChronoPairwisePermanova.csv", row.names = FALSE)


## RGM Subset Permanova----
# 5f. RGM Permanova
set.seed(200)
#permanova
permanova_rgm<- adonis2(distance(filt_rare_RGM2, method='wunifrac')~treatment*`month-collected`, data=metadata_RGM2, by='terms')
permanova_rgm

#pairwise permanova to see which groups are different
permanova_pairwise(distance(filt_rare_RGM2, method='wunifrac'), grp=metadata_RGM2$trt_month, padj='holm')


### see Plots_16S file for code on how to make the plots

## Dry RGM Permanova----
filt_dryRGM2<- subset_samples(filt_rare_RGM2, `month.collected` %in% ('dry'))
metaDryRGM2<- metadata_RGM2 %>% 
  filter(`month-collected`=='dry') 

#order samples
sampRd<- sample_data(filt_dryRGM2) #pull out data from phyloseq

metaDryRGM2<-metaDryRGM2[order(match(metaDryRGM2$`SampleID`, row.names(sampRd))), ]

set.seed(200)
#permanova
permanova_rgmD<- adonis2(distance(filt_dryRGM2, method='wunifrac')~treatment, data=metaDryRGM2, by='terms')
permanova_rgmD

#beta dispersion
dryRGM_betadis<-betadisper(distance(filt_dryRGM2, method='wunifrac'), group=metaDryRGM2$treatment, type='median')
boxplot(dryRGM_betadis)
permutest(dryRGM_betadis)

##chronosequence
adonis2(distance(filt_dryRGM2, method='wunifrac')~treatment*class, data=metaDryRGM2, by='terms')
exports<-permanova_pairwise(distance(filt_dryRGM2, method='wunifrac'), grp=metaDryRGM2$trt_class, padj='holm')
write.csv(exports, "dryChronoPairwisePermanova.csv", row.names = FALSE)



# Homogeneity of dispersions----


###wet chronosequence: 
wet_betadis_chrono <- betadisper(distance(filt_rare_wet2, method = 'wunifrac'), group = metadata_wet2$trt_class, type = 'median') #create betadisper object with dispersion distances  
wet_permutest_chrono <- permutest(wet_betadis_chrono, permutations = 999) #test for differences in dispersions 
wet_permutest_chrono
boxplot(wet_betadis_chrono)


###rgm dry chronosequence: 
dryRGM_betadis_chrono<-betadisper(distance(filt_dryRGM2, method='wunifrac'), group=metaDryRGM2$trt_class, type='median')
permutest(dryRGM_betadis_chrono)
boxplot(dryRGM_betadis_chrono)


###separate for wet LIA and RGM: 


##LIA 
metadata_wet2_LIA <- metadata_wet2 %>%
  filter(soilAge == 'lia')
filt_rare_wet2_LIA <- subset_samples(filt_rare_wet2, soilAge == "lia")

wetLIA_betadis<-betadisper(distance(filt_rare_wet2_LIA, method='wunifrac'), group=metadata_wet2_LIA$treatment, type='median')
boxplot(wetLIA_betadis)
permutest(wetLIA_betadis)


##RGM 
metadata_wet2_RGM <- metadata_wet2 %>%
  filter(soilAge == 'rgm')
filt_rare_wet2_RGM <- subset_samples(filt_rare_wet2, soilAge == "rgm")

wetRGM_betadis<-betadisper(distance(filt_rare_wet2_RGM, method='wunifrac'), group=metadata_wet2_RGM$treatment, type='median')
boxplot(wetRGM_betadis)
permutest(wetRGM_betadis)
#post hoc to see which treatments are different 




# Simper ----
##### 7. Try Simper for testing community difference

## LIA Simper ----
# filter phyloseq and metadata for lia
filt_lia2<- subset_samples(filt_rare_wet2, soilAge %in% ('lia'))
metalia2<- metadata_wet2 %>% 
  filter(soilAge=='lia')

#extract asv table and transpose
asvLIA<- as.data.frame(otu_table(filt_lia2))
tasvLIA <- data.frame(t(asvLIA), check.names = F)

# run simper
simper_lia<- simper(tasvLIA, metalia2$treatment)
simper_lia

#see the top 10
s_lia<- summary(simper_lia)
top20_LIA<-head(s_lia$control_latrine, n = 8)
#if this is null after you run it, change the latrine_control to control_latrine. or
# view simper_lia and see what the name is at the top of the output

simpLIA_asv<- row.names(top20_LIA)

#get actual taxa info
taxa_lia <- as.data.frame(tax_table(filt_lia2)) #taxonomy
simperLIA_taxa<-taxa_lia[row.names(taxa_lia) %in% simpLIA_asv,]


## RGM Wet Simper----
#filter phyloseq and metadata for rgm
filt_wet_rgm2<- subset_samples(filt_rare_wet2, soilAge %in% ('rgm'))
metargmW2<- metadata_wet2 %>% 
  filter(soilAge=='rgm')

#extract asv table and transpose
asvWrgm<- as.data.frame(otu_table(filt_wet_rgm2))
tasvWrgm <- data.frame(t(asvWrgm), check.names = F)

# run simper
simper_Wrgm<- simper(tasvWrgm, metargmW2$treatment)
simper_Wrgm

#see the top 20
s_Wrgm<- summary(simper_Wrgm)
top10_Wrgm<-head(s_Wrgm$control_latrine, n = 8)
#if this is null after you run it, change the latrine_control to control_latrine. or
# view simper_Wrgm and see what the name is at the top of the output

simpWrgm_asv<- row.names(top10_Wrgm)

#get actual taxa info
taxa_Wrgm <- as.data.frame(tax_table(filt_wet_rgm2)) #taxonomy
simperWrgm_taxa<-taxa_Wrgm[row.names(taxa_Wrgm) %in% simpWrgm_asv,]


# RGM dry Simper----
## filter phyloseq and metadata for rgm dry
filt_dryRGM2<- subset_samples(filt_rare_RGM2, `month.collected` %in% ('dry'))
metaDryRGM2<- metadata_RGM2 %>% 
  filter(`month-collected`=='dry') 

#extract asvs and transpose
asvDrgm<- as.data.frame(otu_table(filt_dryRGM2))
tasvDrgm <- data.frame(t(asvDrgm), check.names = F)

# run simper
simper_Drgm<- simper(tasvDrgm, metaDryRGM2$treatment)

#see the top 20
s_Drgm<- summary(simper_Drgm)
top10_Drgm<-head(s_Drgm$control_latrine, n = 8)
#if this is null after you run it, change the latrine_control to control_latrine. or
# view simper_Wrgm and see what the name is at the top of the output

simpDrgm_asv<- row.names(top10_Drgm)

#get actual taxa info
taxa_dry <- as.data.frame(tax_table(filt_dryRGM2)) #taxonomy
simperDrgm_taxa<-taxa_dry[row.names(taxa_dry) %in% simpDrgm_asv,]



### Simper for all wet----
asvs_wet <- as.data.frame(otu_table(filt_rare_wet2)) #ASVs
tASV_wet <- data.frame(t(asvs_wet), check.names = F)

simper_trt<- simper(tASV_wet, metadata_wet2$trt_soilAge)
simper_trt

#see top contributing taxa
summary(simper_trt)$control_rgm_control_lia %>%
  round(3) %>%
  head()
s<- summary(simper_trt)
top20<-head(s$latrine_control, n = 20)

simpWASV<- row.names(top20)

#get actual taxa info
taxaW<- data.frame(tax_table(filt_rare_wet2))
simperW_taxa<-taxaW[row.names(taxaW) %in% simpWASV,]


#see only significant species
comparisons <- c("control_rgm_latrine_rgm" , "control_rgm_control_lia" , "control_rgm_latrine_lia" , "latrine_rgm_control_lia", "latrine_rgm_latrine_lia", "control_lia_latrine_lia")

simper.results <- c()

for(i in 1:length(comparisons)) {
  require(tidyverse)
  temp <- summary(simper_trt)[as.character(comparisons[i])] %>%
    as.data.frame()
  colnames(temp) <- gsub(
    paste(comparisons[i],".", sep = ""), "", colnames(temp))
  temp <- temp %>%
    mutate(Comparison = comparisons[i],
           Position = row_number()) %>%
    rownames_to_column(var = "Species")
  simper.results <- rbind(simper.results, temp)
}

simper.results %>%
  filter(p <= 0.05) %>%
  dplyr::select(Species, average, Comparison, Position)

#see sum for the groups
simper.results %>%
  group_by(Comparison) %>%
  summarize(sum.average = sum(average))

### See Plots_16S file for code that plots these ASVs as arrows

###Simper across chronosequence classes 
 
##wet season
asvs_wet <- as.data.frame(otu_table(filt_rare_wet2)) #ASVs
tASV_wet <- data.frame(t(asvs_wet), check.names = F)

simper_chrono_wet<- simper(tASV_wet, metadata_wet2$trt_class)
names(simper_chrono_wet)

#see only significant species
comparisons <- c(
  "control_1931-1962_latrine_1931-1962",
  "control_1931-1962_control_LIA-1931",
  "control_1931-1962_latrine_LIA-1931",
  "control_1931-1962_control_1984-2024",
  "control_1931-1962_latrine_1984-2024",
  "control_1931-1962_control_LIA",
  "control_1931-1962_latrine_LIA",
  "latrine_1931-1962_control_LIA-1931",
  "latrine_1931-1962_latrine_LIA-1931",
  "latrine_1931-1962_control_1984-2024",
  "latrine_1931-1962_latrine_1984-2024",
  "latrine_1931-1962_control_LIA",
  "latrine_1931-1962_latrine_LIA",
  "control_LIA-1931_latrine_LIA-1931",
  "control_LIA-1931_control_1984-2024",
  "control_LIA-1931_latrine_1984-2024",
  "control_LIA-1931_control_LIA",
  "control_LIA-1931_latrine_LIA",
  "latrine_LIA-1931_control_1984-2024",
  "latrine_LIA-1931_latrine_1984-2024",
  "latrine_LIA-1931_control_LIA",
  "latrine_LIA-1931_latrine_LIA",
  "control_1984-2024_latrine_1984-2024",
  "control_1984-2024_control_LIA",
  "control_1984-2024_latrine_LIA",
  "latrine_1984-2024_control_LIA",
  "latrine_1984-2024_latrine_LIA",
  "control_LIA_latrine_LIA")



simper.results <- purrr::map_dfr(comparisons, function(comp) {
  
  as.data.frame(simper_chrono_wet[[comp]]) %>%
    tibble::rownames_to_column("Species") %>%
    mutate(
      Comparison = comp,
      Position = row_number()
    )
})

#filter for significant 
sig_asvs_chronoW <- simper.results %>%
  filter(p <= 0.05) 


#create a df of significant ASVs with taxonomy 
taxachronoW <- as.data.frame(tax_table(filt_rare_wet2)) %>%
  tibble::rownames_to_column("ASV")
simper_taxa_chronoWsig <- sig_asvs_chronoW %>%
  left_join(taxachronoW, by = c("Species" = "ASV"))
#grab top 10 only 
simper_chronoW_top10 <- simper_taxa_chronoWsig %>%
  group_by(Comparison) %>%
  arrange(desc(average)) %>%
  slice_head(n = 10) %>%
  ungroup()
write.csv(simper_chronoW_top10, "simper_chronoW_top10.csv", row.names = FALSE)


##dry season (RGM)
asvDrgm<- as.data.frame(otu_table(filt_dryRGM2)) #ASVs
tasvDrgm <- data.frame(t(asvDrgm), check.names = F)

# run simper
simper_chrono_dry<- simper(tasvDrgm, metaDryRGM2$trt_class)
names(simper_chrono_dry)

#see only significant species
comparisons <- c(
  "control_LIA-1931_latrine_LIA-1931",
  "control_LIA-1931_control_1984-2024",
  "control_LIA-1931_latrine_1984-2024",
  "control_LIA-1931_control_1931-1962",
  "control_LIA-1931_latrine_1931-1962",
  "latrine_LIA-1931_control_1984-2024",
  "latrine_LIA-1931_latrine_1984-2024",
  "latrine_LIA-1931_control_1931-1962",
  "latrine_LIA-1931_latrine_1931-1962",
  "control_1984-2024_latrine_1984-2024",
  "control_1984-2024_control_1931-1962",
  "control_1984-2024_latrine_1931-1962",
  "latrine_1984-2024_control_1931-1962",
  "latrine_1984-2024_latrine_1931-1962",
  "control_1931-1962_latrine_1931-1962")


simper.results <- purrr::map_dfr(comparisons, function(comp) {
  
  as.data.frame(simper_chrono_dry[[comp]]) %>%
    tibble::rownames_to_column("Species") %>%
    mutate(
      Comparison = comp,
      Position = row_number())
})

#filter for significant 
sig_asvs_chronoDRGM <- simper.results %>%
  filter(p <= 0.05)

#create a df of significant ASVs with taxonomy 
taxachronoDRGM <- as.data.frame(tax_table(filt_dryRGM2)) %>%
  tibble::rownames_to_column("ASV")
simper_taxa_chronoDRGMsig <- sig_asvs_chronoDRGM %>%
  left_join(taxachronoDRGM, by = c("Species" = "ASV"))
#grab top 10 only 
simper_chronoDRGM_top10 <- simper_taxa_chronoDRGMsig %>%
  group_by(Comparison) %>%
  arrange(desc(average)) %>%
  slice_head(n = 10) %>%
  ungroup()
write.csv(simper_chronoDRGM_top10, "simper_chronDRGM_top10.csv", row.names = FALSE)



# Indicator Taxa----
## Wet Subset (LIA & wet RGM) ----
### 8. Indicator analysis for Wet data
#using rarefied data and just replicate 1

## 8a. separate lia and rgm samples

filt_lia2<- subset_samples(filt_rare_wet2, soilAge %in% ('lia'))
filt_wet_rgm2<- subset_samples(filt_rare_wet2, soilAge %in% ('rgm'))

## 8b. extract data from the phyloseq and format
#extract taxa table
taxa_lia <- as.data.frame(tax_table(filt_lia2)) #taxonomy
taxa_Wrgm <- as.data.frame(tax_table(filt_wet_rgm2)) #taxonomy

#extract the asvs 
asvLIA<- as.data.frame(otu_table(filt_lia2))
asvWrgm<- as.data.frame(otu_table(filt_wet_rgm2))

#transpose the asv matrix 
dim(asvLIA)
tasvLIA <- data.frame(t(asvLIA), check.names = F)
rownames(tasvLIA)
colnames(tasvLIA)

tasvWrgm <- data.frame(t(asvWrgm), check.names = F)

#make vector with treatment to use for the test
treatment_lia<- sample_data(filt_lia2)
treatment_lia<- treatment_lia$treatment
treatment_Wrgm<- sample_data(filt_wet_rgm2)
treatment_Wrgm<- treatment_Wrgm$treatment

### 8c. Run the test
set.seed(200) ### Very Important

ind_lia<- multipatt(tasvLIA, treatment_lia, func='IndVal.g')
summary(ind_lia, indvalcomp=T)

ind_Wrgm<- multipatt(tasvWrgm, treatment_Wrgm, func='IndVal.g')
summary(ind_Wrgm, indvalcomp=T)

#put output into data frame with sig and p value
output_lia<- data.frame(ind_lia$sign)
output_Wrgm<- data.frame(ind_Wrgm$sign)


### 8d. Extract the significant ASVs for each treatment
sigL_lia<-output_lia %>% 
  filter(p.value<=.05) %>% 
  filter(s.latrine==1)

sigC_lia<-output_lia %>% 
  filter(p.value<=.05) %>% 
  filter(s.control==1)

sigL_Wrgm<-output_Wrgm %>% 
  filter(p.value<=.05) %>% 
  filter(s.latrine==1)

sigC_Wrgm<-output_Wrgm %>% 
  filter(p.value<=.05) %>% 
  filter(s.control==1)

#make a dataframe with the taxonomic info of the significant asvs
ind_taxaL_lia <- taxa_lia[rownames(taxa_lia) %in% rownames(sigL_lia), ]  
ind_taxaC_lia <- taxa_lia[rownames(taxa_lia) %in% rownames(sigC_lia), ]  

ind_taxaL_Wrgm <- taxa_Wrgm[rownames(taxa_Wrgm) %in% rownames(sigL_Wrgm), ]  
ind_taxaC_Wrgm <- taxa_Wrgm[rownames(taxa_Wrgm) %in% rownames(sigC_Wrgm), ]  


#join taxonomic info to the output data 
sigL_lia$ASV<- row.names(sigL_lia)
ind_taxaL_lia$ASV<- row.names(ind_taxaL_lia)
ind_taxaL_lia<- ind_taxaL_lia %>% left_join(sigL_lia, by='ASV')

sigC_lia$ASV<- row.names(sigC_lia)
ind_taxaC_lia$ASV<- row.names(ind_taxaC_lia)
ind_taxaC_lia<- ind_taxaC_lia %>% left_join(sigC_lia, by='ASV')

sigL_Wrgm$ASV<- row.names(sigL_Wrgm)
ind_taxaL_Wrgm$ASV<- row.names(ind_taxaL_Wrgm)
ind_taxaL_Wrgm<- ind_taxaL_Wrgm %>% left_join(sigL_Wrgm, by='ASV')

sigC_Wrgm$ASV<- row.names(sigC_Wrgm)
ind_taxaC_Wrgm$ASV<- row.names(ind_taxaC_Wrgm)
ind_taxaC_Wrgm<- ind_taxaC_Wrgm %>% left_join(sigC_Wrgm, by='ASV')

# you can collapse it to just get one row for each unique ID
unique_ind_taxaL_lia<- unique(ind_taxaL_lia)
unique_ind_taxaC_lia<- unique(ind_taxaC_lia)
unique_ind_taxaL_Wrgm<- unique(ind_taxaL_Wrgm)
unique_ind_taxaC_Wrgm<- unique(ind_taxaC_Wrgm)


#find taxa that are shared and different
#this is at the family level but you can change it to be at any level by changing
#the column name
#you can also get the asvs instead of the taxonomic info by changing column name
#to the ASV column
#shared between latrine lia and rgm
unique(ind_taxaL_lia[ind_taxaL_lia$ASV %in% ind_taxaL_Wrgm$ASV,8])
#unique to lia
unique(ind_taxaL_lia[!ind_taxaL_lia$Family %in% ind_taxaL_Wrgm$Family,5])
#unique to wet rgm
unique(ind_taxaL_Wrgm[!ind_taxaL_Wrgm$Family %in% ind_taxaL_lia$Family,5])

#find taxa that are shared between wet rgm and lia controls
unique(ind_taxaC_lia[ind_taxaC_lia$Family %in% ind_taxaC_Wrgm$Family,5])
#unique to lia
unique(ind_taxaC_lia[!ind_taxaC_lia$Family %in% ind_taxaC_Wrgm$Family,5])
#unique to wet rgm
unique(ind_taxaC_Wrgm[!ind_taxaC_Wrgm$Family %in% ind_taxaC_lia$Family,5])



## Dry RGM----
### 8B. Indicator analysis for RGM data
#using rarefied data and just replicate 1

## 8a. separate dry samples
filt_dryRGM2<- subset_samples(filt_rare_RGM2, `month.collected` %in% ('dry'))

## 8b. extract data from the phyloseq and format
#extract taxa table
taxa_dry <- as.data.frame(tax_table(filt_dryRGM2)) #taxonomy

#extract the asvs 
asvDrgm<- as.data.frame(otu_table(filt_dryRGM2))

#transpose the asv matrix 
dim(asvDrgm)
tasvDrgm <- data.frame(t(asvDrgm), check.names = F)
rownames(tasvDrgm)
colnames(tasvDrgm)

#make vector with treatment
treatment_dry<- sample_data(filt_dryRGM2)
treatment_dry<- treatment_dry$treatment

### 8c. Run the test
set.seed(200) ### Very Important

ind_dry<- multipatt(tasvDrgm, treatment_dry, func='IndVal.g')
summary(ind_dry, indvalcomp=T)

output_dry<- data.frame(ind_dry$sign)


### 8d. Extract the significant ASVs for each treatment
#make data with just latrine significant species
sigL_dry<-output_dry %>% 
  filter(p.value<=.05) %>% 
  filter(s.latrine==1)

#control significant species
sigC_dry<-output_dry %>% 
  filter(p.value<=.05) %>% 
  filter(s.control==1)


#add taxanomic info
ind_taxaL_dry <- taxa_dry[rownames(taxa_dry) %in% rownames(sigL_dry), ]  
ind_taxaC_dry <- taxa_dry[rownames(taxa_dry) %in% rownames(sigC_dry), ]  


#join the taxonomy and outputs
sigL_dry$ASV<- row.names(sigL_dry)
ind_taxaL_dry$ASV<- row.names(ind_taxaL_dry)
ind_taxaL_dry<- ind_taxaL_dry %>% left_join(sigL_dry, by='ASV')

sigC_dry$ASV<- row.names(sigC_dry)
ind_taxaC_dry$ASV<- row.names(ind_taxaC_dry)
ind_taxaC_dry<- ind_taxaC_dry %>% left_join(sigC_dry, by='ASV')

#collapse it so I just get one row for each unique ID
unique_ind_taxaL_dry<- unique(ind_taxaL_dry)
unique_ind_taxaC_dry<- unique(ind_taxaC_dry)

#find taxa that are shared between wet and dry rgm latrines
unique(ind_taxaL_dry[ind_taxaL_dry$Family %in% ind_taxaL_Wrgm$Family,5])
#unique to dry
unique(ind_taxaL_dry[!ind_taxaL_dry$Family %in% ind_taxaL_Wrgm$Family,5])
#unique to wet (not in dry, doesn't take into account lia taxa)
unique(ind_taxaL_Wrgm[!ind_taxaL_Wrgm$Family %in% ind_taxaL_dry$Family,5])


unique(ind_taxaC_dry[ind_taxaC_dry$Family %in% ind_taxaC_Wrgm$Family,5])
#unique to dry
unique(ind_taxaC_dry[!ind_taxaC_dry$Family %in% ind_taxaC_Wrgm$Family,5])
#unique to wet
unique(ind_taxaC_Wrgm[!ind_taxaC_Wrgm$Family %in% ind_taxaC_dry$Family,5])







# Differential Abundance----
## Wet RGM Latrine vs Control DA----
RGM2_phy_ASV<- filt_rare_rep2%>% 
  subset_samples(soilAge %in% ('rgm')) %>% 
  subset_samples(replicate %in% (2)) 

rgm2_sampdata<- sample_data(RGM2_phy_ASV)
#factor treatment
rgm2_sampdata$treatment<- as.factor(rgm2_sampdata$treatment)
RGM2_phy_ASV@sam_data<- rgm2_sampdata
str(RGM2_phy_ASV@sam_data)

rgmW_rep2_phy<- subset_samples(RGM2_phy_ASV, month.collected=='wet')

# test with just rep 2 and no RE
rgmWetTreatmentDA<-ancombc2(data = rgmW_rep2_phy, tax_level = "Genus",
                      fix_formula = "treatment", rand_formula = NULL,
                      p_adj_method = "holm", pseudo_sens = TRUE,
                      prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                      group = "treatment", struc_zero = TRUE, neg_lb = TRUE,
                      alpha = 0.05, n_cl = 2, verbose = TRUE,
                      global = TRUE, pairwise = TRUE, dunnet = TRUE, trend = F,
                      iter_control = list(tol = 1e-2, max_iter = 20, 
                                          verbose = TRUE),
                      em_control = list(tol = 1e-5, max_iter = 100),
                      lme_control = lme4::lmerControl(),
                      mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))

#put primary results in data frame
rgmWetT_prim<-rgmWetTreatmentDA$res

#save it as an rds file, change for Genus or not genus level
saveRDS(rgmWetT_prim, file='rgmWetT_prim.rds')
rgmWetT_prim<-readRDS('rgmWetT_prim.rds') #Genus lvl or not

#filter for what's significant
rgmWetTSig<-rgmWetT_prim %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T)

#extract taxa from phyloseq
rgmWet_taxa<- data.frame(tax_table(rgmW_rep2_phy))

#filter for the first genus that is significant to see how many ASVs there are
rgmWet_taxa %>% 
  filter(Genus=='Iamia')

# Plot log fold change
rgmWetT_DAplot<- rgmWetTSig %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T) %>% 
  dplyr::arrange(desc(lfc_treatmentlatrine)) %>% 
  dplyr::mutate(direct = ifelse(lfc_treatmentlatrine> 0, "Positive LFC", "Negative LFC"))

#make taxon and direction factors
rgmWetT_DAplot$taxon<- factor(rgmWetT_DAplot$taxon, levels=rgmWetT_DAplot$taxon)
rgmWetT_DAplot$direct<- factor(rgmWetT_DAplot$direct, levels = c("Positive LFC", "Negative LFC"))


fig_rgmWetT = rgmWetT_DAplot %>%
  ggplot(aes(x = taxon, y = lfc_treatmentlatrine, fill=direct)) + 
  geom_bar(stat = "identity", width = 0.7, color = "black", 
           position = position_dodge(width = 0.4)) +
  scale_fill_manual(values=c('purple3', 'cyan3'), name=NULL, labels=c('Positive LFC (more in latrine)','Negative LFC (more in control)'))+
  geom_errorbar(aes(ymin = lfc_treatmentlatrine - se_treatmentlatrine, ymax = lfc_treatmentlatrine + se_treatmentlatrine), 
                width = 0.2, position = position_dodge(0.05), color = "black") + 
  labs(x = NULL, y = "Log fold change", 
       title = "Wet season Latrine vs Control") + 
  scale_color_discrete(name = NULL) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank())
fig_rgmWetT

#export the plot to a powerpoint to edit
fig_dml<- rvg::dml(ggobj = fig_rgmWetT)

pres<-officer::read_pptx("F:\\Research\\github\\Peru_microbial_analysis_16s\\16s_plots.pptx") %>%
  # add slide 
  officer::add_slide(layout='Blank') %>%
  # specify object and location of object 
  officer::ph_with(fig_dml, ph_location()) 
print(pres, target = "F:\\Research\\github\\Peru_microbial_analysis_16s\\16s_plots.pptx") 


#do the test at the phylum level
rgmWetTDA_phylum<-ancombc2(data = rgmW_rep2_phy, tax_level = "Phylum",
                            fix_formula = "treatment", rand_formula = NULL,
                            p_adj_method = "holm", pseudo_sens = TRUE,
                            prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                            group = "treatment", struc_zero = TRUE, neg_lb = TRUE,
                            alpha = 0.05, n_cl = 2, verbose = TRUE,
                            global = TRUE, pairwise = TRUE, dunnet = TRUE, trend = F,
                            iter_control = list(tol = 1e-2, max_iter = 20, 
                                                verbose = TRUE),
                            em_control = list(tol = 1e-5, max_iter = 100),
                            lme_control = lme4::lmerControl(),
                            mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))

rgmWet_phylum_prim<- rgmWetTDA_phylum$res

rgmWet_phylumSig<- rgmWet_phylum_prim %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T)

# Plot log fold change
rgmWetphylum_DAplot<- rgmWet_phylumSig %>% 
  dplyr::arrange(desc(lfc_treatmentlatrine)) %>% 
  dplyr::mutate(direct = ifelse(lfc_treatmentlatrine> 0, "Positive LFC", "Negative LFC"))

#make taxon and direction factors
rgmWetphylum_DAplot$taxon<- factor(rgmWetphylum_DAplot$taxon, levels=rgmWetphylum_DAplot$taxon)
rgmWetphylum_DAplot$direct<- factor(rgmWetphylum_DAplot$direct, levels = c("Positive LFC", "Negative LFC"))

fig_rgmWetphylum = rgmWetphylum_DAplot %>%
  ggplot(aes(x = taxon, y = lfc_treatmentlatrine, fill=direct)) + 
  geom_bar(stat = "identity", width = 0.7, color = "black", 
           position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lfc_treatmentlatrine - se_treatmentlatrine, ymax = lfc_treatmentlatrine + se_treatmentlatrine), 
                width = 0.2, position = position_dodge(0.05), color = "black") + 
  labs(x = NULL, y = "Log fold change", 
       title = "Log fold changes") + 
  scale_fill_discrete(name = NULL) +
  scale_color_discrete(name = NULL) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust=1, angle=90))
fig_rgmWetphylum

## Dry RGM Latrine vs Control----

#got the phyloseq from the latrine by season step

#make treatment a factor
rgm2_sampdata$treatment<- as.factor(rgm2_sampdata$treatment)
RGM2_phy_ASV@sam_data<- rgm2_sampdata
str(RGM2_phy_ASV@sam_data)

#make it just for the dry samples
rgmD_rep2_phy<- subset_samples(RGM2_phy_ASV, month.collected=='dry')

# test with just rep 2 and no RE
rgmDryTreatmentDA<-ancombc2(data = rgmD_rep2_phy, tax_level = "Genus",
                            fix_formula = "treatment", rand_formula = NULL,
                            p_adj_method = "holm", pseudo_sens = TRUE,
                            prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                            group = "treatment", struc_zero = TRUE, neg_lb = TRUE,
                            alpha = 0.05, n_cl = 2, verbose = TRUE,
                            global = TRUE, pairwise = F, dunnet = F, trend = F,
                            iter_control = list(tol = 1e-2, max_iter = 20, 
                                                verbose = TRUE),
                            em_control = list(tol = 1e-5, max_iter = 100),
                            lme_control = lme4::lmerControl(),
                            mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))

 #put primary results in data frame
rgmDryT_prim<-rgmDryTreatmentDA$res

#save it as an rds file
saveRDS(rgmDryT_prim, file='rgmDryT_prim.rds')
rgmDryT_prim<-readRDS('rgmDryT_prim.rds')

#filter for what's significant
rgmDryTSig<-rgmDryT_prim %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T)

#extract taxa from phyloseq
rgmDry_taxa<- data.frame(tax_table(rgmD_rep2_phy))

#filter for the first genus that is significant to see how many ASVs there are
rgmDry_taxa %>% 
  filter(Genus=='Flavobacterium') %>% 
  count()

# Plot log fold change
rgmDryT_DAplot<- rgmDryTSig %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T) %>% 
  dplyr::arrange(desc(lfc_treatmentlatrine)) %>% 
  dplyr::mutate(direct = ifelse(lfc_treatmentlatrine> 0, "Positive LFC", "Negative LFC"))

#make taxon and direction factors
rgmDryT_DAplot$taxon<- factor(rgmDryT_DAplot$taxon, levels=rgmDryT_DAplot$taxon)
rgmDryT_DAplot$direct<- factor(rgmDryT_DAplot$direct, levels = c("Positive LFC", "Negative LFC"))


fig_rgmDryT = rgmDryT_DAplot %>%
  ggplot(aes(x = taxon, y = lfc_treatmentlatrine, fill=direct)) + 
  geom_bar(stat = "identity", width = 0.7, color = "black", 
           position = position_dodge(width = 0.4)) +
  scale_fill_manual(values=c('purple3', 'cyan3'), name=NULL, labels=c('Positive LFC (more in latrine)','Negative LFC (more in control)'))+
  geom_errorbar(aes(ymin = lfc_treatmentlatrine - se_treatmentlatrine, ymax = lfc_treatmentlatrine + se_treatmentlatrine), 
                width = 0.2, position = position_dodge(0.05), color = "black") + 
  labs(x = NULL, y = "Log fold change", 
       title = "Dry Latrine vs Control") + 
  scale_color_discrete(name = NULL) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle=60))
fig_rgmDryT

#do the test at the phylum level
rgmDryTDA_phylum<-ancombc2(data = rgmD_rep2_phy, tax_level = "Phylum",
                           fix_formula = "treatment", rand_formula = NULL,
                           p_adj_method = "holm", pseudo_sens = TRUE,
                           prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                           group = "treatment", struc_zero = TRUE, neg_lb = TRUE,
                           alpha = 0.05, n_cl = 2, verbose = TRUE,
                           global = TRUE, pairwise = TRUE, dunnet = TRUE, trend = F,
                           iter_control = list(tol = 1e-2, max_iter = 20, 
                                               verbose = TRUE),
                           em_control = list(tol = 1e-5, max_iter = 100),
                           lme_control = lme4::lmerControl(),
                           mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))

rgmDry_phylum_prim<- rgmDryTDA_phylum$res

rgmDry_phylumSig<- rgmDry_phylum_prim %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T)

## LIA vs RGM Control but using only 4 rgm locations that we chose based on location and availability----
##51,60,56,58
#get the phyloseq with regular asv names
wet2_phy_ASV<- filt_rare_rep2%>% 
  subset_samples(month.collected %in% ('wet')) 

## make our tesfilt_rare_phy_16s## make our test variables factors
wet2_sampdata<- sample_data(wet2_phy_ASV)
wet2_sampdata$soilAge<- as.factor(wet2_sampdata$soilAge)
wet2_phy_ASV@sam_data<- wet2_sampdata
str(wet2_phy_ASV@sam_data)

#select only the 4 rgm samples we want (as well as the LIA ones)
soilAgeCDAphy<- wet2_phy_ASV %>% 
  subset_samples(latrine %in% c('L51','L56','L58','L60','L100','L101','L102','L104')) %>% 
  subset_samples(treatment=='control')

#run the test
soilAgeCDA<-ancombc2(data = soilAgeCDAphy, tax_level = "Genus",
                    fix_formula = "soilAge", rand_formula =NULL,
                    p_adj_method = "holm", pseudo_sens = TRUE,
                    prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                    group = "soilAge", struc_zero = T, neg_lb = T,
                    alpha = 0.05, n_cl = 2, verbose = TRUE,
                    global = TRUE, pairwise = F, dunnet = F, trend = F,
                    iter_control = list(tol = 1e-2, max_iter = 20, 
                                        verbose = TRUE),
                    em_control = list(tol = 1e-5, max_iter = 100),
                    lme_control = lme4::lmerControl(),
                    mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))

soilAgeC_prim<- soilAgeCDA$res

saveRDS(soilAgeC_prim, file='soilAgeC_prim.rds')
soilAgeC_prim<-readRDS('soilAgeC_prim.rds')

soilAgeC_sig<- soilAgeC_prim %>% 
  filter(q_soilAgergm<.05 & passed_ss_soilAgergm==T)

soilAgeC_zero<- soilAgeCDA$zero_ind
soilAgeC_zeroLIA<- soilAgeC_zero %>% 
  filter(`structural_zero (soilAge = lia)`==T & `structural_zero (soilAge = rgm)`==F)
soilAgeC_zeroRGM<- soilAgeC_zero %>% 
  filter(`structural_zero (soilAge = lia)`==F & `structural_zero (soilAge = rgm)`==T)

#### LIA phylum----
#get LIA samples
wet2_sampdata$treatment<- as.factor(wet2_sampdata$treatment)
wet2_phy_ASV@sam_data<- wet2_sampdata

LIA_phylum_DA<- wet2_phy_ASV %>% 
  subset_samples(soilAge== 'lia')

#run the test
soilAgeDAPhylum<-ancombc2(data = LIA_phylum_DA, tax_level = "Phylum",
                    fix_formula = "treatment", rand_formula =NULL,
                    p_adj_method = "holm", pseudo_sens = TRUE,
                    prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                    group = "treatment", struc_zero = T, neg_lb = T,
                    alpha = 0.05, n_cl = 2, verbose = TRUE,
                    global = TRUE, pairwise = TRUE, dunnet = F, trend = F,
                    iter_control = list(tol = 1e-2, max_iter = 20, 
                                        verbose = TRUE),
                    em_control = list(tol = 1e-5, max_iter = 100),
                    lme_control = lme4::lmerControl(),
                    mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))
liaDAPhylum_pair<- soilAgeDAPhylum$res
liaDAPhylumSig<- liaDAPhylum_pair %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T)

## Plot all 3 phylum tests in one figure----

#first run the 3 different phylum level tests under the Wet L vs C, Dry L vs C, and LIA L vs C

#######first process the wet season comparison
#select just the columns of interest
wet_phyl_fig <- rgmWet_phylum_prim %>%
  dplyr::select(taxon, contains('latrine')) 

#round LFC, choose only taxa that are significant and passed sensitivity test, make data tidy
wet_phyl_fig_lfc <- wet_phyl_fig %>%
  dplyr::filter(diff_treatmentlatrine == 1 & passed_ss_treatmentlatrine==T) %>%
  dplyr::mutate(lfc1 = ifelse(diff_treatmentlatrine == 1, 
                              round(lfc_treatmentlatrine, 2), 0)) %>%
  tidyr::pivot_longer(cols = lfc1, 
                      names_to = "group", values_to = "value") %>%
  dplyr::arrange(taxon) %>% 
  dplyr::select(group, value, taxon)

# recode the group so instead of lfc1 it says what the comparison is
wet_phyl_fig_lfc$group <- dplyr::recode(wet_phyl_fig_lfc$group, 
                          `lfc1` = "RGM Wet Season")

###### second process dry season comparison
dry_phyl_fig<- rgmDry_phylum_prim %>% 
  dplyr::select(taxon, contains('latrine')) 

#round LFC, choose only taxa that are significant and passed sensitivity test, make data tidy
dry_phyl_fig_lfc <- dry_phyl_fig %>%
  dplyr::filter(diff_treatmentlatrine == 1 & passed_ss_treatmentlatrine==T) %>%
  dplyr::mutate(lfc1 = ifelse(diff_treatmentlatrine == 1, 
                              round(lfc_treatmentlatrine, 2), 0)) %>%
  tidyr::pivot_longer(cols = lfc1, 
                      names_to = "group", values_to = "value") %>%
  dplyr::arrange(taxon) %>% 
  dplyr::select(group, value, taxon)

# recode the group so instead of lfc1 it says what the comparison is
dry_phyl_fig_lfc$group <- dplyr::recode(dry_phyl_fig_lfc$group, 
                                        `lfc1` = "RGM Dry Season")

##### third process the LIA comparison
lia_phyl_fig<- liaDAPhylum_pair %>% 
  dplyr::select(taxon, contains('latrine'))

#round LFC, choose only taxa that are significant and passed sensitivity test, make data tidy
lia_phyl_fig_lfc <- lia_phyl_fig %>%
  dplyr::filter(diff_treatmentlatrine == 1 & passed_ss_treatmentlatrine==T) %>%
  dplyr::mutate(lfc1 = ifelse(diff_treatmentlatrine == 1, 
                              round(lfc_treatmentlatrine, 2), 0)) %>%
  tidyr::pivot_longer(cols = lfc1, 
                      names_to = "group", values_to = "value") %>%
  dplyr::arrange(taxon) %>% 
  dplyr::select(group, value, taxon)

# recode the group so instead of lfc1 it says what the comparison is
lia_phyl_fig_lfc$group <- dplyr::recode(lia_phyl_fig_lfc$group, 
                                        `lfc1` = "LIA Wet Season")

#join all the comparisons together
phyl_fig<-full_join(lia_phyl_fig_lfc, wet_phyl_fig_lfc)
phyl_fig<- full_join(dry_phyl_fig_lfc, phyl_fig) #change this to wet to make just the rgm dry and wet plot

# make the figure
lo = floor(min(phyl_fig$value))
up = ceiling(max(phyl_fig$value))

fig_phyl = phyl_fig %>%
  ggplot(aes(x = group, y = taxon, fill = value)) + 
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "cyan3", high = "purple3", mid = "white", 
                       na.value = "white", midpoint = 0, limit = c(lo, up),
                       breaks=c(4,-2), labels=c('More abundant \
on latrines','More abundant \
on referencess')) +
  geom_text(aes(group, taxon, label = value), size = 4) +
  scale_color_identity(guide = "none") +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_classic() +
  theme(axis.text.x=element_text(),
        legend.title=element_blank())
fig_phyl

#### make that plot comparing simper and ancombc2 taxa----
## try it first with lia vs rgm control

LIA_RGM_taxa<- data.frame(tax_table(LIA_RGM_DAphy))

soilAgeCC_taxa1<-LIA_RGM_taxa %>% 
  filter(Genus=='Abditibacterium') %>% 
  row.names()

LIA_RGM_taxa %>% 
  filter(Genus=='Blastocatella') %>% 
  row.names()

soilAgeCC_taxa1names<-both_names %>% 
  filter(original %in% soilAgeCC_taxa1) 

soilAgeCC_simp<-s[["control_rgm_control_lia"]]

soilAgeCC_simp_taxa1<-soilAgeCC_simp %>% 
  filter(row.names(soilAgeCC_simp) %in% soilAgeCC_taxa1names$number)
#I looked at it to see if simper agrees that this taxa is more prevalent in control rgm
#and it does for the most part (except for 4 asvs, and about half of the asvs are 0 for all the simper stats)

sum(soilAgeCC_simp_taxa1$average)
#express this as a proportion out of the total dissimilarity (sum of average column in total simper results?)

## i need to do this for multiple taxa but im not sure how to simplify the process
# and not have to do it 1 by 1 for each taxa. and we could show multiple comparisons on 
# the plot so there is more going on but that makes the process even longer





# Beta Diversity GENUS LEVEL----
##### 5. Beta Diversity analysis

############## This step is neccessary 
### NECESSARY Reroot the tree.----
# 5a. Reroot the tree
#It has to be binary but now it is not since we trimmed it
ps_tree_genus<- phy_tree(phy_noNA_genus_glom) #put tree into an object
is.binary(ps_tree_genus) #asking if it is binary. if false, go to next step

## NOT NECESSARY Compare subsamples (replicates) Permanova----

###permanova for replicates 1 and 2 to see if they differ

#factor the variables
metadata_factored<- metadata_filt
metadata_factored$replicate<- as.factor(metadata_factored$replicate)
metadata_factored$latrine_trt<- as.factor(metadata_factored$latrine_trt)

#reorder the metadata to match the order of the phyloseq
sampr<- sample_data(phy_noNA_genus_glom) #pull out data from phyloseq

#order metadata to match that from phyloseq
metadata_factored_rep<-metadata_factored[ order(match(metadata_factored$`#SampleID`, row.names(sampr))), ]

set.seed(200)
#run permanova
#permanova<- adonis2(distance(filt_rare_phy, method='wunifrac')~replicate, data=metadata_factored_rep, by='terms')
#permanova

#new way so that it is testing replicate but within each latrine. we think this is the proper way to test replicate variation
perm_rep<- adonis2(distance(phy_noNA_genus_glom, method='wunifrac')~replicate*latrine_trt, data=metadata_factored_rep, by='terms')
perm_rep



## NECESSARY Run this step to get filtered phyloseq that is used for beta diversity----
# 5b. Change ASV names 
metadata_factored<- metadata_filt
metadata_factored$treatment<- as.factor(metadata_factored$treatment)
metadata_factored$soilAge<- as.factor(metadata_factored$soilAge)
metadata_factored$`month-collected`<- as.factor(metadata_factored$`month-collected`)
metadata_factored$trt_month<- as.factor(metadata_factored$trt_month)
metadata_factored$trt_soilAge<- as.factor(metadata_factored$trt_soilAge)

## they weren't different so we are going to choose just replicate 2 from the data
##because having 2 is pseudoreplication
##you could choose replicate 1, or randomly sample, whatever you want
filt_rare_rep2<- subset_samples(phy_noNA_genus_glom, replicate %in% (2))

#### make a dataframe that has the original and new asv names for convenience
## the original names are a random long string of characters so this makes them easier
## to reference and the data frame saves the original and new name so we know what's what

#pull out taxa table
taxa<- as.data.frame(tax_table(filt_rare_rep2))
#pull out the tree
tree<- phy_tree(filt_rare_rep2)
#make sure tips are in same order as taxa
sum(tree$tip.label==row.names(taxa)) 
#if using replicate 2, it should print 28372 (because that's how many asvs there are
## and so that tells us that the tree tips are in the same order of the taxa table)

#put original names into df
#use both_names to look up the original qiime2 asv name
both_names<- data.frame(original=rownames(taxa))
#rename asvs in taxa table and add to df
rownames(taxa)<- paste('ASV', seq(1,637,1), sep='_')
both_names$number<- rownames(taxa)
#take out asv table and rename that too
asvfull<- otu_table(filt_rare_rep2)
rownames(asvfull)<- paste('ASV', seq(1,637,1), sep='_')
#convert them into matrix to put back into phyloseq
tax<- tax_table(as.matrix(taxa))
otu<- otu_table(as.matrix(asvfull), taxa_are_rows = T)
sample<- sample_data(filt_rare_rep2)
#rename the tree tips too
tree$tip.label<- paste('ASV', seq(1,637,1), sep='_')

#put all this back into phyloseq so ASVs now have a normal number name
rep2_named_phy<- phyloseq(otu, tax, sample, tree)
#this is the phyloseq we will use


## NECESSARY Subset for Wet season ----
# 5c. Subset wet data
#make metadata. make sure that of the samples that were rarefied out, they don't belong to the replicate
# that is being chosen for the beta diversity stuff. so here, L70 control wet rep 1 was dropped when we 
# rarefied so if we choose replicate 1, there is no L70 control wet representation in our data so
#we have to make sure it is chosen
metadata_wet2<- metadata_factored %>% 
  filter(`month-collected`=='wet' & replicate==2)

#filter the phyloseq for only wet samples
filt_rare_wet2<- subset_samples(rep2_named_phy, `month.collected` %in% ('wet'))

#reorder the metadata to match the order of the phyloseq
samp<- sample_data(filt_rare_wet2) #pull out data from phyloseq

metadata_wet2<-metadata_wet2[ order(match(metadata_wet2$`#SampleID`, row.names(samp))), ]


## NECESSARY Subset for RGM data----
# 5d. Subset RGM data
#metadata factored
metadata_RGM2<- metadata_factored %>% 
  filter(soilAge=='rgm', replicate==2) 

#make phyloseq for RGM only
filt_rare_RGM2<- rep2_named_phy%>% 
  subset_samples(soilAge %in% ('rgm')) %>% 
  subset_samples(replicate %in% (2)) 

#order samples
sampR<- sample_data(filt_rare_RGM2) #pull out data from phyloseq

metadata_RGM2<-metadata_RGM2[order(match(metadata_RGM2$`#SampleID`, row.names(sampR))), ]



## Wet Subset Permanova ----
### 5e. Permanova test with wet season data
set.seed(200) ###VERY IMPORTANT, always keep the same

#run permanova
permanova_wet<- adonis2(distance(filt_rare_wet2, method='wunifrac')~treatment*soilAge, data=metadata_wet2, by='terms')
permanova_wet

#pairwise permanova to see which groups are different from each other
permanova_pairwise(distance(filt_rare_wet2, method='wunifrac'), grp=metadata_wet2$trt_soilAge)

# see Plots_16S file for code to make plots

#beta dispersion
wet_betadis<-betadisper(distance(filt_rare_wet2, method='wunifrac'), group=metadata_wet2$trt_soilAge, type='median')
adonis2(dist(wet_betadis$distances)~metadata_wet2$trt_soilAge)
boxplot(wet_betadis)

permutest(wet_betadis, permutations=999)

permanova_pairwise(dist(wet_betadis$distances), grp=metadata_wet2$trt_soilAge)

## RGM Subset Permanova----
# 5f. RGM Permanova
set.seed(200)
#permanova
permanova_rgm<- adonis2(distance(filt_rare_RGM2, method='wunifrac')~treatment*`month-collected`, data=metadata_RGM2, by='terms')
permanova_rgm

#pairwise permanova to see which groups are different
permanova_pairwise(distance(filt_rare_RGM2, method='wunifrac'), grp=metadata_RGM2$trt_month)


### see Plots_16S file for code on how to make the plots

## Dry RGM Permanova----
filt_dryRGM2<- subset_samples(filt_rare_RGM2, `month.collected` %in% ('dry'))
metaDryRGM2<- metadata_RGM2 %>% 
  filter(`month-collected`=='dry') 

#order samples
sampRd<- sample_data(filt_dryRGM2) #pull out data from phyloseq

metaDryRGM2<-metaDryRGM2[order(match(metaDryRGM2$`#SampleID`, row.names(sampRd))), ]

set.seed(200)
#permanova
permanova_rgmD<- adonis2(distance(filt_dryRGM2, method='wunifrac')~treatment, data=metaDryRGM2, by='terms')
permanova_rgmD

#beta dispersion
dryRGM_betadis<-betadisper(distance(filt_dryRGM2, method='wunifrac'), group=metaDryRGM2$treatment, type='median')
adonis2(dist(dryRGM_betadis$distances)~metaDryRGM2$treatment)
boxplot(dryRGM_betadis)
permutest(dryRGM_betadis)


# Differential Abundance----
## Wet RGM Latrine vs Control DA----

RGM2_phy_ASV<- filt_rare_rep2%>% 
  subset_samples(soilAge %in% ('rgm')) %>% 
  subset_samples(replicate %in% (2)) 

#order samples
sampR<- sample_data(RGM2_phy_ASV) #pull out data from phyloseq

metadata_RGM2<-metadata_RGM2[order(match(metadata_RGM2$`#SampleID`, row.names(sampR))), ]

#make season a factor
rgm2_sampdata<- sample_data(RGM2_phy_ASV)

#factor treatment
rgm2_sampdata$treatment<- as.factor(rgm2_sampdata$treatment)
RGM2_phy_ASV@sam_data<- rgm2_sampdata
str(RGM2_phy_ASV@sam_data)

rgmW_rep2_phy<- subset_samples(RGM2_phy_ASV, month.collected=='wet')

# test with just rep 2 and no RE
rgmWetTreatmentDA<-ancombc2(data = rgmW_rep2_phy, tax_level = "Genus",
                            fix_formula = "treatment", rand_formula = NULL,
                            p_adj_method = "holm", pseudo_sens = TRUE,
                            prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                            group = "treatment", struc_zero = TRUE, neg_lb = TRUE,
                            alpha = 0.05, n_cl = 2, verbose = TRUE,
                            global = TRUE, pairwise = TRUE, dunnet = TRUE, trend = F,
                            iter_control = list(tol = 1e-2, max_iter = 20, 
                                                verbose = TRUE),
                            em_control = list(tol = 1e-5, max_iter = 100),
                            lme_control = lme4::lmerControl(),
                            mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))

#put primary results in data frame
rgmWetT_prim<-rgmWetTreatmentDA$res

#save it as an rds file, change for Genus or not genus level
saveRDS(rgmWetT_prim, file='GenusrgmWetT_prim.rds')
rgmWetT_prim<-readRDS('GenusrgmWetT_prim.rds') #Genus lvl or not

#filter for what's significant
rgmWetTSig<-rgmWetT_prim %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T)

#extract taxa from phyloseq
rgmWet_taxa<- data.frame(tax_table(rgmW_rep2_phy))

#filter for the first genus that is significant to see how many ASVs there are
rgmWet_taxa %>% 
  filter(Genus=='Iamia')

# Plot log fold change
rgmWetT_DAplot<- rgmWetTSig %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T) %>% 
  dplyr::arrange(desc(lfc_treatmentlatrine)) %>% 
  dplyr::mutate(direct = ifelse(lfc_treatmentlatrine> 0, "Positive LFC", "Negative LFC"))

#make taxon and direction factors
rgmWetT_DAplot$taxon<- factor(rgmWetT_DAplot$taxon, levels=rgmWetT_DAplot$taxon)
rgmWetT_DAplot$direct<- factor(rgmWetT_DAplot$direct, levels = c("Positive LFC", "Negative LFC"))


fig_rgmWetT = rgmWetT_DAplot %>%
  ggplot(aes(x = taxon, y = lfc_treatmentlatrine, fill=direct)) + 
  geom_bar(stat = "identity", width = 0.7, color = "black", 
           position = position_dodge(width = 0.4)) +
  scale_fill_manual(values=c('purple3', 'cyan3'), name=NULL, labels=c('Positive LFC (more in latrine)','Negative LFC (more in control)'))+
  geom_errorbar(aes(ymin = lfc_treatmentlatrine - se_treatmentlatrine, ymax = lfc_treatmentlatrine + se_treatmentlatrine), 
                width = 0.2, position = position_dodge(0.05), color = "black") + 
  labs(x = NULL, y = "Log fold change", 
       title = "Wet season Latrine vs Control") + 
  scale_color_discrete(name = NULL) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank())
fig_rgmWetT


#do the test at the phylum level
rgmWetTDA_phylum<-ancombc2(data = rgmW_rep2_phy, tax_level = "Phylum",
                           fix_formula = "treatment", rand_formula = NULL,
                           p_adj_method = "holm", pseudo_sens = TRUE,
                           prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                           group = "treatment", struc_zero = TRUE, neg_lb = TRUE,
                           alpha = 0.05, n_cl = 2, verbose = TRUE,
                           global = TRUE, pairwise = TRUE, dunnet = TRUE, trend = F,
                           iter_control = list(tol = 1e-2, max_iter = 20, 
                                               verbose = TRUE),
                           em_control = list(tol = 1e-5, max_iter = 100),
                           lme_control = lme4::lmerControl(),
                           mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))

rgmWet_phylum_prim<- rgmWetTDA_phylum$res

rgmWet_phylumSig<- rgmWet_phylum_prim %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T)

# Plot log fold change
rgmWetphylum_DAplot<- rgmWet_phylumSig %>% 
  dplyr::arrange(desc(lfc_treatmentlatrine)) %>% 
  dplyr::mutate(direct = ifelse(lfc_treatmentlatrine> 0, "Positive LFC", "Negative LFC"))

#make taxon and direction factors
rgmWetphylum_DAplot$taxon<- factor(rgmWetphylum_DAplot$taxon, levels=rgmWetphylum_DAplot$taxon)
rgmWetphylum_DAplot$direct<- factor(rgmWetphylum_DAplot$direct, levels = c("Positive LFC", "Negative LFC"))

fig_rgmWetphylum = rgmWetphylum_DAplot %>%
  ggplot(aes(x = taxon, y = lfc_treatmentlatrine, fill=direct)) + 
  geom_bar(stat = "identity", width = 0.7, color = "black", 
           position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lfc_treatmentlatrine - se_treatmentlatrine, ymax = lfc_treatmentlatrine + se_treatmentlatrine), 
                width = 0.2, position = position_dodge(0.05), color = "black") + 
  labs(x = NULL, y = "Log fold change", 
       title = "Log fold changes") + 
  scale_fill_discrete(name = NULL) +
  scale_color_discrete(name = NULL) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust=1, angle=90))
fig_rgmWetphylum

## Dry RGM Latrine vs Control----

#got the phyloseq from the latrine by season step

#make treatment a factor
rgm2_sampdata$treatment<- as.factor(rgm2_sampdata$treatment)
RGM2_phy_ASV@sam_data<- rgm2_sampdata
str(RGM2_phy_ASV@sam_data)

#make it just for the dry samples
rgmD_rep2_phy<- subset_samples(RGM2_phy_ASV, month.collected=='dry')

# test with just rep 2 and no RE
rgmDryTreatmentDA<-ancombc2(data = rgmD_rep2_phy, tax_level = "Genus",
                            fix_formula = "treatment", rand_formula = NULL,
                            p_adj_method = "holm", pseudo_sens = TRUE,
                            prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                            group = "treatment", struc_zero = TRUE, neg_lb = TRUE,
                            alpha = 0.05, n_cl = 2, verbose = TRUE,
                            global = TRUE, pairwise = F, dunnet = F, trend = F,
                            iter_control = list(tol = 1e-2, max_iter = 20, 
                                                verbose = TRUE),
                            em_control = list(tol = 1e-5, max_iter = 100),
                            lme_control = lme4::lmerControl(),
                            mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))

#put primary results in data frame
rgmDryT_prim<-rgmDryTreatmentDA$res

#save it as an rds file
saveRDS(rgmDryT_prim, file='GenusrgmDryT_prim.rds')
rgmDryT_prim<-readRDS('GenusrgmDryT_prim.rds')

#filter for what's significant
rgmDryTSig<-rgmDryT_prim %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T)

#extract taxa from phyloseq
rgmDry_taxa<- data.frame(tax_table(rgmD_rep2_phy))

#filter for the first genus that is significant to see how many ASVs there are
rgmDry_taxa %>% 
  filter(Genus=='Flavobacterium') %>% 
  count()

# Plot log fold change
rgmDryT_DAplot<- rgmDryTSig %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T) %>% 
  dplyr::arrange(desc(lfc_treatmentlatrine)) %>% 
  dplyr::mutate(direct = ifelse(lfc_treatmentlatrine> 0, "Positive LFC", "Negative LFC"))

#make taxon and direction factors
rgmDryT_DAplot$taxon<- factor(rgmDryT_DAplot$taxon, levels=rgmDryT_DAplot$taxon)
rgmDryT_DAplot$direct<- factor(rgmDryT_DAplot$direct, levels = c("Positive LFC", "Negative LFC"))


fig_rgmDryT = rgmDryT_DAplot %>%
  ggplot(aes(x = taxon, y = lfc_treatmentlatrine, fill=direct)) + 
  geom_bar(stat = "identity", width = 0.7, color = "black", 
           position = position_dodge(width = 0.4)) +
  scale_fill_manual(values=c('purple3', 'cyan3'), name=NULL, labels=c('Positive LFC (more in latrine)','Negative LFC (more in control)'))+
  geom_errorbar(aes(ymin = lfc_treatmentlatrine - se_treatmentlatrine, ymax = lfc_treatmentlatrine + se_treatmentlatrine), 
                width = 0.2, position = position_dodge(0.05), color = "black") + 
  labs(x = NULL, y = "Log fold change", 
       title = "Dry Latrine vs Control") + 
  scale_color_discrete(name = NULL) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust=.5, angle=60))
fig_rgmDryT

#do the test at the phylum level
rgmDryTDA_phylum<-ancombc2(data = rgmD_rep2_phy, tax_level = "Phylum",
                           fix_formula = "treatment", rand_formula = NULL,
                           p_adj_method = "holm", pseudo_sens = TRUE,
                           prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                           group = "treatment", struc_zero = TRUE, neg_lb = TRUE,
                           alpha = 0.05, n_cl = 2, verbose = TRUE,
                           global = TRUE, pairwise = TRUE, dunnet = TRUE, trend = F,
                           iter_control = list(tol = 1e-2, max_iter = 20, 
                                               verbose = TRUE),
                           em_control = list(tol = 1e-5, max_iter = 100),
                           lme_control = lme4::lmerControl(),
                           mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))

rgmDry_phylum_prim<- rgmDryTDA_phylum$res

rgmDry_phylumSig<- rgmDry_phylum_prim %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T)

## LIA vs RGM Control but using only 4 rgm locations that we chose based on location and availability----
##51,60,56,58
#get the phyloseq with regular asv names
wet2_phy_ASV<- phy_noNA_genus_glom%>% 
  subset_samples(month.collected %in% ('wet')) %>% 
  subset_samples(replicate %in% (2)) 

## make our test variables factors
wet2_sampdata<- sample_data(wet2_phy_ASV)
wet2_sampdata$soilAge<- as.factor(wet2_sampdata$soilAge)
wet2_phy_ASV@sam_data<- wet2_sampdata
str(wet2_phy_ASV@sam_data)

#select only the 4 rgm samples we want (as well as the LIA ones)
soilAgeCDAphy<- wet2_phy_ASV %>% 
  subset_samples(latrine %in% c('L51','L56','L58','L60','L100','L101','L102','L104')) %>% 
  subset_samples(treatment=='control')

#run the test
soilAgeCDA<-ancombc2(data = soilAgeCDAphy, tax_level = "Genus",
                     fix_formula = "soilAge", rand_formula =NULL,
                     p_adj_method = "holm", pseudo_sens = TRUE,
                     prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                     group = "soilAge", struc_zero = T, neg_lb = T,
                     alpha = 0.05, n_cl = 2, verbose = TRUE,
                     global = TRUE, pairwise = F, dunnet = F, trend = F,
                     iter_control = list(tol = 1e-2, max_iter = 20, 
                                         verbose = TRUE),
                     em_control = list(tol = 1e-5, max_iter = 100),
                     lme_control = lme4::lmerControl(),
                     mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))

soilAgeC_prim<- soilAgeCDA$res

saveRDS(soilAgeC_prim, file='F:\\Research\\16S_Soil\\RDS Files\\soilAgeC_prim.rds')
soilAgeC_prim<-readRDS('F:\\Research\\16S_Soil\\RDS Files\\soilAgeC_prim.rds')

soilAgeC_sig<- soilAgeC_prim %>% 
  filter(q_soilAgergm<.05 & passed_ss_soilAgergm==T)

soilAgeC_zero<- soilAgeCDA$zero_ind
soilAgeC_zeroLIA<- soilAgeC_zero %>% 
  filter(`structural_zero (soilAge = lia)`==T & `structural_zero (soilAge = rgm)`==F)
soilAgeC_zeroRGM<- soilAgeC_zero %>% 
  filter(`structural_zero (soilAge = lia)`==F & `structural_zero (soilAge = rgm)`==T)

#### LIA phylum----
#get LIA samples
wet2_sampdata$treatment<- as.factor(wet2_sampdata$treatment)
wet2_phy_ASV@sam_data<- wet2_sampdata

LIA_phylum_DA<- wet2_phy_ASV %>% 
  subset_samples(soilAge== 'lia')

#run the test
soilAgeDAPhylum<-ancombc2(data = LIA_phylum_DA, tax_level = "Phylum",
                          fix_formula = "treatment", rand_formula =NULL,
                          p_adj_method = "holm", pseudo_sens = TRUE,
                          prv_cut = 0.10, lib_cut = 0, s0_perc = 0.05,
                          group = "treatment", struc_zero = T, neg_lb = T,
                          alpha = 0.05, n_cl = 2, verbose = TRUE,
                          global = TRUE, pairwise = TRUE, dunnet = F, trend = F,
                          iter_control = list(tol = 1e-2, max_iter = 20, 
                                              verbose = TRUE),
                          em_control = list(tol = 1e-5, max_iter = 100),
                          lme_control = lme4::lmerControl(),
                          mdfdr_control = list(fwer_ctrl_method = "holm", B = 100))
liaDAPhylum_pair<- soilAgeDAPhylum$res
liaDAPhylumSig<- liaDAPhylum_pair %>% 
  filter(q_treatmentlatrine<.05 & passed_ss_treatmentlatrine==T)

## Plot all 3 phylum tests in one figure----

#first run the 3 different phylum level tests under the Wet L vs C, Dry L vs C, and LIA L vs C

#######first process the wet season comparison
#select just the columns of interest
wet_phyl_fig <- rgmWet_phylum_prim %>%
  dplyr::select(taxon, contains('latrine')) 

#round LFC, choose only taxa that are significant and passed sensitivity test, make data tidy
wet_phyl_fig_lfc <- wet_phyl_fig %>%
  dplyr::filter(diff_treatmentlatrine == 1 & passed_ss_treatmentlatrine==T) %>%
  dplyr::mutate(lfc1 = ifelse(diff_treatmentlatrine == 1, 
                              round(lfc_treatmentlatrine, 2), 0)) %>%
  tidyr::pivot_longer(cols = lfc1, 
                      names_to = "group", values_to = "value") %>%
  dplyr::arrange(taxon) %>% 
  dplyr::select(group, value, taxon)

# recode the group so instead of lfc1 it says what the comparison is
wet_phyl_fig_lfc$group <- dplyr::recode(wet_phyl_fig_lfc$group, 
                                        `lfc1` = "RGM Wet Season")

###### second process dry season comparison
dry_phyl_fig<- rgmDry_phylum_prim %>% 
  dplyr::select(taxon, contains('latrine')) 

#round LFC, choose only taxa that are significant and passed sensitivity test, make data tidy
dry_phyl_fig_lfc <- dry_phyl_fig %>%
  dplyr::filter(diff_treatmentlatrine == 1 & passed_ss_treatmentlatrine==T) %>%
  dplyr::mutate(lfc1 = ifelse(diff_treatmentlatrine == 1, 
                              round(lfc_treatmentlatrine, 2), 0)) %>%
  tidyr::pivot_longer(cols = lfc1, 
                      names_to = "group", values_to = "value") %>%
  dplyr::arrange(taxon) %>% 
  dplyr::select(group, value, taxon)

# recode the group so instead of lfc1 it says what the comparison is
dry_phyl_fig_lfc$group <- dplyr::recode(dry_phyl_fig_lfc$group, 
                                        `lfc1` = "RGM Dry Season")

##### third process the LIA comparison
lia_phyl_fig<- liaDAPhylum_pair %>% 
  dplyr::select(taxon, contains('latrine'))

#round LFC, choose only taxa that are significant and passed sensitivity test, make data tidy
lia_phyl_fig_lfc <- lia_phyl_fig %>%
  dplyr::filter(diff_treatmentlatrine == 1 & passed_ss_treatmentlatrine==T) %>%
  dplyr::mutate(lfc1 = ifelse(diff_treatmentlatrine == 1, 
                              round(lfc_treatmentlatrine, 2), 0)) %>%
  tidyr::pivot_longer(cols = lfc1, 
                      names_to = "group", values_to = "value") %>%
  dplyr::arrange(taxon) %>% 
  dplyr::select(group, value, taxon)

# recode the group so instead of lfc1 it says what the comparison is
lia_phyl_fig_lfc$group <- dplyr::recode(lia_phyl_fig_lfc$group, 
                                        `lfc1` = "LIA Wet Season")

#join all the comparisons together
phyl_fig<-full_join(lia_phyl_fig_lfc, wet_phyl_fig_lfc)
phyl_fig<- full_join(dry_phyl_fig_lfc, phyl_fig) #change this to wet to make just the rgm dry and wet plot

# make the figure
lo = floor(min(phyl_fig$value))
up = ceiling(max(phyl_fig$value))

fig_phyl = phyl_fig %>%
  ggplot(aes(x = group, y = taxon, fill = value)) + 
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "cyan3", high = "purple3", mid = "white", 
                       na.value = "white", midpoint = 0, limit = c(lo, up),
                       breaks=c(4,-2), labels=c('more abundant \
on latrines','more abundant \
on controls')) +
  geom_text(aes(group, taxon, label = value), size = 4) +
  scale_color_identity(guide = "none") +
  labs(x = NULL, y = NULL, title = 'Phylum LFC Latrine-Control') +
  theme_classic() +
  theme(axis.text.x=element_text(),
        legend.title=element_blank())
fig_phyl


######extract Bacteroidetes with abundance
#filter for bacteroidota
rgm_bacter<-subset_taxa(filt_rare_RGM2, Phylum=='Bacteroidota')
#extract taxa
rgm_bacter_tax<-data.frame(rgm_bacter@tax_table)

# sum ASVs for latrines and controls 
rgmL_bacter<- subset_samples(rgm_bacter, treatment=='latrine')
rgmL_bacter_asv<- rowSums(data.frame(rgmL_bacter@otu_table))
rgmC_bacter<- subset_samples(rgm_bacter, treatment=='control')
rgmC_bacter_asv<- rowSums(data.frame(rgmC_bacter@otu_table))

#add abundance to taxa data 
rgm_bacter_tax$latrineAbun<- rgmL_bacter_asv
rgm_bacter_tax$controlAbun<- rgmC_bacter_asv

#get overall abundance for each family
rgm_bacter_tax<- rgm_bacter_tax %>% 
  group_by(Family) %>% 
  mutate(sumL=sum(latrineAbun), sumC=sum(controlAbun)) %>% 
  ungroup() %>% 
  dplyr::select(-c('latrineAbun', 'controlAbun','Genus'))
rgm_bacter_fam<-distinct(rgm_bacter_tax)

#get overall abundance for each genus
#rerun the rgm_bacter_tax lines first
rgm_bacter_tax<- rgm_bacter_tax %>% 
  group_by(Genus) %>% 
  mutate(sumL=sum(latrineAbun), sumC=sum(controlAbun)) %>% 
  ungroup() %>% 
  dplyr::select(-c('latrineAbun', 'controlAbun'))
rgm_bacter_gen<-distinct(rgm_bacter_tax)


lia_bacter<- subset_taxa(filt_lia2, Phylum=='Bacteroidota')
lia_bacter_tax<- data.frame(lia_bacter@tax_table)

liaL_bacter<- subset_samples(lia_bacter, treatment=='latrine')
liaL_bacter_asv<- rowSums(data.frame(liaL_bacter@otu_table))
liaC_bacter<- subset_samples(lia_bacter, treatment=='control')
liaC_bacter_asv<- rowSums(data.frame(liaC_bacter@otu_table))

lia_bacter_tax$latrineAbun<- liaL_bacter_asv
lia_bacter_tax$controlAbun<- liaC_bacter_asv

#get overall abundance for each family
lia_bacter_tax<- lia_bacter_tax %>% 
  group_by(Family) %>% 
  mutate(sumL=sum(latrineAbun), sumC=sum(controlAbun)) %>% 
  ungroup() %>% 
  dplyr::select(-c('latrineAbun', 'controlAbun','Genus'))
lia_bacter_fam<-distinct(lia_bacter_tax)

#get overall abundance for each genus
#rerun the lia_bacter_tax lines first
lia_bacter_tax<- lia_bacter_tax %>% 
  group_by(Genus) %>% 
  mutate(sumL=sum(latrineAbun), sumC=sum(controlAbun)) %>% 
  ungroup() %>% 
  dplyr::select(-c('latrineAbun', 'controlAbun'))
lia_bacter_gen<-distinct(lia_bacter_tax)

#save as csv
write.csv(lia_bacter_gen, file='LIA_BacteroidotaGenus.csv')
write.csv(lia_bacter_fam, file='LIA_BacteroidotaFamily.csv')
write.csv(rgm_bacter_gen, file='RGM_BacteroidotaGenus.csv')
write.csv(rgm_bacter_fam, file='RGM_BacteroidotaFamily.csv')

#second round for just flagged taxa 
target_fams <- c("Sphingobacteriaceae", "Blattabacteriaceae", "Paludibacteraceae", "Lentimicrobiaceae")

rgm_target_fams <- subset_taxa(
  filt_rare_RGM2,
  Family %in% target_fams)

rgm_target_fams_df <- psmelt(rgm_target_fams)
#for those not identified at genus call it OTU
rgm_target_fams_df$lowest_tax <- ifelse(
  is.na(rgm_target_fams_df$Genus) | rgm_target_fams_df$Genus == "",
  rgm_target_fams_df$OTU,
  rgm_target_fams_df$Genus)

rgm_target_fams_summary <- rgm_target_fams_df %>%
  group_by(Family, lowest_tax, treatment) %>%
  summarise(total_abun = sum(Abundance), .groups = "drop")

rgm_target_fams_summary_wide <- rgm_target_fams_summary %>%
  tidyr::pivot_wider(
    names_from = treatment,
    values_from = total_abun,
    values_fill = 0)

#write to CSV
write.csv(rgm_target_fams_summary_wide, "RGM_target_families_abundance.csv", row.names = FALSE)

#bar graph of bacteroidetes families 

#rgm relative abundance
rgm_bacter_rel <- transform_sample_counts(
  rgm_bacter,
  function(x) x / sum(x))
rgm_df <- psmelt(rgm_bacter_rel) #melt long format
rgm_bacter_family_avg <- rgm_df %>%
  dplyr::group_by(Family, treatment, trt_month_soilAge) %>%
  dplyr::summarise(
    mean_RA = mean(Abundance),
    sd_RA = sd(Abundance),
    .groups = "drop") #have to call dplyr specifically

#lia relative abundance 
lia_bacter_rel <- transform_sample_counts(
  lia_bacter,
  function(x) x / sum(x))
lia_df <- psmelt(lia_bacter_rel) #melt long format
lia_bacter_family_avg <- lia_df %>%
  dplyr::group_by(Family, treatment, trt_month_soilAge) %>%
  dplyr::summarise(
    mean_RA = mean(Abundance),
    sd_RA = sd(Abundance),
    .groups = "drop") #have to call dplyr specifically



#plot time 
bacter_families <- sort(unique(c(
  as.character(rgm_bacter_family_avg$Family),
  as.character(lia_bacter_family_avg$Family))))

cols <- setNames(
  colorRampPalette(brewer.pal(12, "Set3"))(length(bacter_families)),
  bacter_families)

rgm_bacter_plot <- ggplot(
  rgm_bacter_family_avg,
  aes(x = trt_month_soilAge,
      y = mean_RA,
      fill = Family)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(
    title = "RGM Bacteroidota families",
    x = "",
    y = "Avg rel abundance") +
  theme_bw() + 
  theme()
lia_bacter_plot <- ggplot(
  lia_bacter_family_avg,
  aes(x = trt_month_soilAge,
      y = mean_RA,
      fill = Family)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cols) +
  labs(
    title = "LIA Bacteroidota families",
    x = "",
    y = "Avg rel abundance") +
  theme_bw() + 
  theme()
rgm_bacter_plot + lia_bacter_plot

#combine into a single legend for plotting 
legend <- get_legend(rgm_bacter_plot)

#plot combined figure 
both_bacter_plot <- plot_grid(
  rgm_bacter_plot + theme(legend.position = "none"),
  lia_bacter_plot + theme(legend.position = "none"),
  ncol = 2)
plot_grid(both_bacter_plot, legend, rel_widths = c(4, 1))


######extract Fibrobacterota, Flavobacterium, & Pedobacter with sequences for BLAST

#read in sequences 
rep_seqs <- read_qza("Jan25_rep-seqs.qza")$data
seq_df <- data.frame(
  ASV = names(rep_seqs),
  Sequence = as.character(rep_seqs),
  stringsAsFactors = FALSE)

#RGM
rgm_ffp <- subset_taxa(
  filt_rare_RGM2,
  Phylum == "Fibrobacterota" |
    Genus == "Flavobacterium" |
    Genus == "Pedobacter")

#taxonomy
tax_df <- as.data.frame(tax_table(filt_rare_phy_16s))
tax_df$ASV <- rownames(tax_df)

#all
target_taxa <- tax_df %>%
  filter(Phylum == "Fibrobacterota" |
      Genus == "Flavobacterium" |
      Genus == "Pedobacter")

fibro_flavo_pedobac <- target_taxa %>%
  left_join(seq_df, by = "ASV")

write.csv(fibro_flavo_pedobac, "fibro_flavo_pedobac_BLAST.csv")





