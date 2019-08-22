



import pandas as pd
import urllib


#"https://electionresults.dcboe.org/Downloads/Reports/November_6_2012_General_and_Special_Election_Certified_Results.csv"

# https://electionresults.dcboe.org/Downloads/Reports/November_6_2018_General_Election_Certified_Results.csv

# https://electionresults.dcboe.org/Downloads/Reports/November_8_2016_General_Election_Certified_Results.csv


#for year in [2014]:
#    urllib.urlretrieve("https://electionresults.dcboe.org/Downloads/Reports/November_4_"+str(year)+"_General_Election_Certified_Results.csv", "data/" + str(year) + ".csv")





# make anc entries readable
# CONTEST_NAME
# ContestName in 2018
# VOTES

data = pd.read_csv("data/2012.csv")
str(data)






















