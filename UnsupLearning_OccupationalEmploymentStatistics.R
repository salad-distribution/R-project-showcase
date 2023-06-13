### Preparing Data For Unsupervised Learning ###
################################################
# Libraries Needed
# dendextend for color_branches() function
library(dendextend)
# Libraries for data matrix processing
library(dplyr)    ### for mutate() function
library(tibble)   ### for rownames_to_column() function
library(tidyr)    ### for gather() function
# ggplot2 for advanced plot functionality
library(ggplot2)

# Data Source: Occupational Employment Statistics (OES) program THROUGH Data Camp
# assign dimensions to vector
oes_jobs <- c("Management",
              "Business Operations",
              "Computer Science",
              "Architecture/Engineering",
              "Life/Physical/Social Sci.",
              "Community Services",
              "Legal",
              "Education/Training/Library",
              "Arts/Design/Entertainment",
              "Healthcare Practitioners",
              "Healthcare Support",
              "Protective Service",
              "Food Preparation",
              "Grounds Cleaning & Maint.",
              "Personal Care",
              "Sales",
              "Office Administrative",
              "Farming/Fishing/Forestry",
              "Construction",
              "Installation/Repair/Maint.",
              "Production",
              "Transportation/Moving")
oes_years <- c("2001","2002","2003","2004","2005","2006","2007",
               "2008","2010","2011","2012","2013","2014","2015","2016")

# assign income values to matrix. Dimensions assigned to rows and columns
oes <- matrix(c(70800,78870,83400,87090,88450,91930,96150,100310,105440,107410,108570,110550,112490,115020,118020,
              50580,53350,56000,57120,57930,60000,62410,64720,67690,68740,69550,71020,72410,73800,75070,
              60350,61630,64150,66370,67100,69240,72190,74500,77230,78730,80180,82010,83970,86170,87880,
              56330,58020,60390,63060,63910,66190,68880,71430,75550,77120,79000,80100,81520,82980,84300,
              49710,52380,54930,57550,58030,59660,62020,64280,66390,67470,68360,69400,70070,71220,72930,
              34190,34630,35800,37050,37530,39000,40540,41790,43180,43830,44240,44710,45310,46160,47200,
              69030,77330,78590,81180,81070,85360,88450,92270,96940,98380,98570,99620,101110,103460,105980,
              39130,40160,41390,42810,43450,45320,46610,48460,50440,50870,51210,51500,52210,53000,54520,
              39770,41660,43350,43820,44310,46110,48410,50670,52290,53850,54490,55580,55790,56980,58390,
              49930,53990,56240,58310,59170,62030,65020,67890,71280,72730,73540,74740,76010,77800,79160,
              21900,22410,22960,23510,23850,24610,25600,26340,26920,27370,27780,28300,28820,29520,30470,
              32530,33330,34430,35240,35750,37040,38750,40200,42490,42730,43050,43510,43980,44610,45810,
              16720,17180,17400,17620,17840,18430,19440,20220,21240,21430,21380,21580,21980,22850,23850,
              20380,20850,21290,21670,21930,22580,23560,24370,25300,25560,25670,26010,26370,27080,28010,
              21010,21370,21570,22080,22180,22920,23980,24120,24590,24620,24550,24710,24980,25650,26510,
              28920,30610,31560,32280,32800,34350,35240,36080,36790,37520,37990,38200,38660,39320,40560,
              27230,27910,28540,29390,29710,30370,31200,32220,33470,34120,34410,34900,35530,36330,37260,
              19630,20220,20290,20670,21010,21810,22640,23560,24330,24300,24230,24330,25160,26360,27810,
              35450,36340,37000,37890,38260,39290,40620,42350,43870,44630,44960,45630,46600,47580,48900,
              34960,35780,36560,37620,38050,39060,39930,41230,42810,43390,43870,44420,45220,45990,46690,
              27600,28190,28930,29480,29890,30480,31310,32320,33770,34220,34500,34930,35490,36220,37190,
              26560,27220,27630,28250,28820,29460,30680,31450,32660,33200,33590,33860,34460,35160,36070),
              nrow = 22, byrow = TRUE, dimnames = list(oes_jobs, oes_years))

#################################
### Exploratory Data Analysis ###
#################################
summary_oes <- summary(oes) 
  ##data has no categorical features and does not need dummifying
  ##data has no missing values and does not need imputation / no observations need to be excluded
  ##features are all on same scale, no scaling needed

###############################
### Hierarchical Clustering ###
###############################

### create hclust object
# Euclidean Distances b/w occupations
dist_oes <- dist(oes)

# hierarchical clustering of OES using Average linkage method
hc_oes <- hclust(dist_oes, method = "average")

### explore potential cut height, explore potential number of clusters
# create dendrogram object from hclust object
dend_oes <- as.dendrogram(hc_oes)

## view plot of dendrogram
plot(dend_oes)

# color clusters of dendrogram branches by a cut height of 100,000
col_dend_oes <- color_branches(dend_oes, h = 100000)

## view plot of colored dendrogram with cut height of 100,000
plot(col_dend_oes)
    ## h = 100,000 creates 3 clusters

### prepare hclustering for exploration (target output is ready for ggplot input)
# move rownames into column of the data frame with the variable name, 'occupation'
df_oes <- rownames_to_column(as.data.frame(oes), var = "occupation")

# create cluster assignment vector at h = 100,000 using hclust object
cut_oes <- cutree(hc_oes, h = 100000)

# append cluster assignments to data frame as column - 'cluster'
clust_oes <- mutate(df_oes, cluster = cut_oes)

# create tidy data frame by gathering the year and values into two columns
gathered_oes <- gather(data = clust_oes,
                       key = year,
                       value = mean_salary,
                       -occupation, -cluster)

### Analyze resulting clusters
# view cluster assignments by sorting the cluster assignment vector
sorted_oes <- sort(cut_oes)

# plot relationship b/w mean_salary and year for each occupation. Lines colored by assigned cluster
oes_hclust_plot <- ggplot(gathered_oes, aes(x = year, y = mean_salary, color = factor(cluster))) +
  geom_line(aes(group = occupation))


