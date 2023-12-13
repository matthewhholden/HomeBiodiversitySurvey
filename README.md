# HomeBiodiversitySurvey

This project has the data and code to replicate results in Rogers, Yong, and Holden's paper "The house of a thousand species"

You will need to download this projcet and store it in one folder. When opening R set the working directory to this folder.

You can then run Generate_Accumulation_Curves_and_Cimate_Figs.R in R. This generates Fig 2 in the paper, which uses a function in Unique_Records to convert the raw data to the unique records required to plot species accumulation curves (effectively removing species that are recorded more than one time in the given period in the data file/frame). Since all of our records for the house are unique this is not neccessary to run it for our data (although we do so to error check avoiding duplicates), but we need to run it for ALA data to remove duplicates.

The bar graphs in Fig 1 are done directly from the excel file, and the maps are done in GIS (and therefore not included in this repository). 
