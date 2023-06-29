##name of program: startup_buildenh.R
#description: program(script) starts the package 'buildenh'
v_nr = "1.2" #version number of the program package
cat("version_number= ",v_nr,"\n")
#examples: extracted buildings from land cover maps derived by classification programs
#data: ISPRS test "Vaihingen": orthoimage of areas #1, #7, #26
#author: Joachim Höhle
#publication: Automated mapping of buildings through classification of DSM-based
#ortho-images and cartographic enhancement, International Journal of Applied Earth
#Observations and Geoinformation 95(2021) 102237; https://doi.org/10.1016/j.jag.2020.102237
#instructions: use given example for getting acquainted with the programs/scripts
#instructions: change directories for input; 
#instructions: input project title 
#instructions: save your home directory
#instructions: select orthoimage 
#instructions: select the OrgClassResFilename,OrgImgPathname,OrgImgFilename,OrgGtsPathname,OrgGtsFilename   
#instructions: type 'Ctrl+A'(select all) and 'Source'
#instructions: new users may start by examples (processing mode = demo)
#instructions: The parameters (bnr, p_m_md, part, ro_rg, ref_l, cas, n_pix, c_ld, sek, c_pos, c_sek, 
#c_adj_l) must be selected. The used parameters in the example can be found in a table stored under /data.
#instructions: find supporting software in directory '/support'
#depends: R 4.2.1; BiocManager 1.78.0-0 (EBImage); spatstat 2.3-4; tiff 01-11; rpart 4.1.19; nlme 3.1-162;
#Copyright(C) 2022 Joachim Höhle
#GNU General Public License (GPL)

###################################################################################
cat("start of software package 'buildenh_jh' ","\n")
cat("first program/script 'startup_buildenh.R' ","\n")

#save your home directory
old_dir <- setwd("./")
getwd()
#
home_dir <- "C:/Users/Joachim/R_programs/buildenh_jh/v1.2/buildenh_v1.2"
home_dir2 <- "C:/Users/Joachim/R_programs/buildenh_jh/v1.2/buildenh_v1.2/R"
###################################################################################

## title of project (manual input of characteristics)

#prj_title <- "ISPRS7_LCM1" #(example1)
#orthoimage: ISPRS7
#classification method: DT/LCM1 by 17 attributes
#training by orthoimage #7
#enhancement of buildings
#

prj_title <- "ISPRS1_LCM2" #example#2
#orthoimage ISPRS1 
#classification method: DT/LCM2 by 5 attributes
#training by orthoimage #26
#enhancement of buildings
#

#prj_title <- "X" (your project)
#orthoimage NN 
#classification method: ?
#training by orthoimage: ?
#enhancement of buildings
#

##########################################

cat("project title is = ", prj_title,"\n")
setwd(home_dir)

#select orthoimage (activate manually)
#Img_name <- readline("type name of orthoimage: ") #line can be avoided when Img_name is selected
#Img_name <- "ISPRS7" #name of orthoimage to be processed (example1)
Img_name <- "ISPRS1" #name of orthoimage to be processed (example2)
#Img_name <- "NN" #name of orthoimage to be processed (orthoimage of your project)

if (Img_name == "ISPRS7") { #example1
  ##setting of path- & file-name for original data:
  OrgClassResFilename <- "ISPRS_#7_b.tiff" #extracted buildings
  OrgClassResPathname <- paste(home_dir,"/data",sep = "")
  OrgImgPathname <- paste(home_dir,"/data",sep = "")
  OrgImgFilename <- "top_mosaic_09cm_area7.tif"  #GSD=0.09m
  OrgGtsPathname <- paste(home_dir,"/data",sep = "")
  OrgGtsFilename <- "gts_top_mosaic_09cm_area7.tif" #GSD=0.09m
  #GSD=Ground Sampling Distance
} #end of image7

if (Img_name == "ISPRS1") { #example2
  ##setting of path- & file-name for original data:
  setwd(home_dir)
  OrgClassResPathname <- paste(home_dir,"/data",sep = "")
  OrgClassResFilename <- "ISPRS_#1_b.tiff" #extracted buildings
  OrgImgPathname <- paste(home_dir,"/data",sep = "")
  OrgImgFilename <- "top_mosaic_09cm_area1.tif" #GSD=0.09m
  OrgGtsPathname <- paste(home_dir,"/data",sep = "") 
  OrgGtsFilename <- "gts_top_mosaic_09cm_area1.tif" #GSD=0.09m
  #GSD=Ground Sampling Distance
} #end of image1

if (Img_name == "NN") { #your orthoimage
  ##setting of path- & file-name for original data:
  OrgClassResFilename <- "NN" #extracted buildings
  OrgClassResPathname <- paste(home_dir,"/data",sep = "")
  OrgImgPathname <- paste(home_dir,"/data",sep = "")
  OrgImgFilename <- "NN"  #GSD=?m
  OrgGtsPathname <- paste(home_dir,"/data",sep = "")
  OrgGtsFilename <- "NN" #GSD=?m
  #GSD=Ground Sampling Distance
} #end of image7

proc_mode <- "NA" #mode of processing

## install packages
# install.packages('EBImage')
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("EBImage")
# install.packages('spatstat')
# install.packages('tiff')
# install.packages('rpart')
# install.packages('nlme')

##loading of libraries 
setwd(home_dir2)
source("./func/func_loadLib_op.R") #load of other R-packages, v1.1
source("./func/func_loadLib_jh.R") #load of functions for the R-package 'buildenh', v1.1
#
loadLib_op() #call of function
loadLib_jh() #call of function

#other functions
display = function(...) if (interactive()) EBImage::display(...)

#setup for processing mode "auto"

if (Img_name == "ISPRS1") {
  y_auto <- c(4,5,7,18) #objects for automatic processing (orthoimage #1)  
}

if (Img_name == "ISPRS7") {
  y_auto <- c(20,22,23) #objects for automatic processing (orthoimage #7)  
}

n_y_auto <- length(y_auto)
k_y_auto <- 1

#setup of parameter/variables
par(mai = c(1.02,0.82,0.82,0.42)) #setup of margins/plot region [inches]

bnr2_part <- "NA" #partition of object

cat("end of 'startup_buildenh.R' - continue with 'extract_all_buildings.R' ", "\n")
#end of 'startup_buildenh.R'

##start the next program ("enhance_image.R")
setwd(home_dir2)
source(paste("enhance_image_v",v_nr,".R",sep=""))
#

