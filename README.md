# NAUKRI SCRAPER
 This R code takes in an xlsx file called "NAUKALLINDIA.xlsx" and "compsoundex" to return a file named "R.xlsx"

The sample data structure was a list of 100k sparse string rows which contained details of 10k job seekers and their profile details ie. W, XP, LOC, and CO.

Unstructured Data Manipulation, for each profile, 3 variables were in-row separated by a delimiter and 1 variable, Unnormalized Company names(CO) were in a row-below. 

To get all 4 variables in a single row I duplicated the list, lagged by 1 then pasted the 2 lists collapsed by a Delimiter aligning all 4 variables.

Next step involved subsetting rows that had all 4 variables with a regex("Lacs), then splitting strings by the preset delimiter into 4 columns.
The irrelevant text was removed from each data point, followed by rounding integers, converting strings to uppercase and setting factors.

The Hardest Problem was identifying job seekers Company by Location names which required fuzzy matching
I Used several distance algorithms and settled with Caver-phone Algorithm using Soundex function as it gave a low "false positive" rate due to the small range of codes Refined Soundex can create.

Implementing the Algorithm involved creating training file of normalized company names accompanied by 4 character Soundex code and performing left_join by Soundex Code.

Cross Validation, a  human eye was needed for validation and updating training file for future.

Final Product: A Visual of a Stacked histogram depicting sample distribution of job seekers across 7 cities, 0-4 years XP using base r graphics which was made from a table created using SQL syntax.

Benefits: My application sped up data processing 12 fold, widened sample size 10 fold, raised accuracy to 99% and improved data utilisation by 15%, hence proven business value addition.
