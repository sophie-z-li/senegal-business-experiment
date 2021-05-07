Hello and welcome to my replication project.

In this repository, you will find everything you need to replicate my 
replication.

Firstly, there is Replication-Project.Rmd, which is the RMarkDown file I used to
code my entire replication report. Inside, you will find significant amounts of
data cleaning (especially because I decided to run MICE models as my extension),
a PDf file of my final replication report, as well as a folder of the original
author, Abhit Bhandari,'s files, including the original dataset file (which
I used in loading the data), his own code in a file called analysis.r (please
note, however, that not all of the code done in his paper, especially with
table formatting, is available in his provided code. Also, when conducting my
replication, I realized that some of his coding was incorrect, e.g., he
would omit the `trans.enum` variable when it was the `enum` variable that
needed to be omitted). In his file folder, you will also be able to find the
codebook for this research project.

In order to run my Rmd file, you only need the data source (which I have provided
to you in the Author_Files folder) as well as the below libraries:

library(tidyverse)
library(stargazer)
library(multcomp)
library(car)
library(haven)
library(sjlabelled)
library(nnet)
library(gt)
library(kableExtra)
library(mice)

^All of these packages were up-to-date as of May 6, 2021, when I finished this
project. At the time, I was also running R Version 1.3.1073 on MacOS Catalina.

Once you run my Rmd file, you will be able to view the output, which is the
entirety of my replication (figures, tables, models, analysis, etc).