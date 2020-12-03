# Tomales_waterbird_trends_2020
Analysis of waterbird abundance on Tomales Bay, CA, 1989-2019

##Working title for paper:
Decadal trends of (wintering) waterbirds at a coastal California site – a 30-year perspective.

##Authors:
Nils Warnock, Scott Jennings, Emiko Condeso and David Lumpkin
Audubon Canyon Ranch, PO Box 808, Marshall, CA 94940

Please contact Scott Jennings for questions about code or data (scott.jennings@egret.org)


##Data
*Currently only including foraging guild data files*  

*Foraging guild table.csv - foraging guild assignments made by NW  

*guilds_foraging_diet_classification.csv - combination of guild assignments from Foraging guild table.csv and those of   

  +Vilchis, L. I., Johnson, C. K., Evenson, J. R., Pearson, S. F., Barry, K. L., Davidson, P., … Gaydos, J. K. (2014). Assessing ecological correlates of marine bird declines to inform marine conservation. Conservation Biology, 29(1), 154–163. https://doi.org/10.1111/cobi.12378  

  +This file is created by code/vilchis_guilds.R  


##Code
*raw_data_vizualization.R - descriptive file name. Basic plots and summaries for the raw data.  

* simulate_data.R - also descriptive file name. simulate some data that roughly looks like our actual waterbird data. Mostly used this data early on to help visualize different analysis methods.  

* vilchis_guilds.R - extract appendix table from Vilchis et al 2014. Combine with NW guild classifications.  

##RMD
*Keeping RMD files in separate folder form code files to avoid crowding the later folder.*

* foraging_guild_summary.Rmd - Summary of number of surveys per year, which species were assigned to which foraging guild by NW, and basic raw data plots for each guild.  

* how_many_birds_grouped.Rmd - Basic raw data visualization showing how many birds are left in species groups vs  how many are IDed to species

