

cleaned_data/election_data_for_anc_map.csv: cleaned_data/election_history_R.csv map_prep.R
	Rscript map_prep.R

cleaned_data/2018_elections_commissioners.csv: cleaned_data/election_history_R.csv merge_incumbents.R cleaned_data/current_anc_membership.csv
	Rscript merge_incumbents.R
# current_anc_membership.csv comes from Ilya's web scraping



cleaned_data/election_history_R.csv: anc_election_cleaner.R raw_data/2012.csv raw_data/2014.csv raw_data/2016.csv raw_data/2018.csv
	Rscript anc_election_cleaner.R

raw_data/2012.csv: FORCE
	curl -o raw_data/2012.csv https://electionresults.dcboe.org/Downloads/Reports/November_6_2012_General_and_Special_Election_Certified_Results.csv

raw_data/2014.csv:
	curl -o raw_data/2014.csv https://electionresults.dcboe.org/Downloads/Reports/November_4_2014_General_Election_Certified_Results.csv

raw_data/2016.csv:
	curl -o raw_data/2016.csv https://electionresults.dcboe.org/Downloads/Reports/November_8_2016_General_Election_Certified_Results.csv

raw_data/2018.csv:
	curl -o raw_data/2018.csv https://electionresults.dcboe.org/Downloads/Reports/November_6_2018_General_Election_Certified_Results.csv

FORCE:
