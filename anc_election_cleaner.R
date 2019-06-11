
library(tidyverse)


path <- getwd()
years <- c("2012", "2014", "2016", "2018")

all.data <- NULL

for(year in years){
	
	# for running snippets and ignoring the loop:
	# year <- 2018
	
	# read in data
	data <- read.table(file=paste(path, "/data/", year, ".csv", sep=""), header=TRUE, sep=",")
		
	print(paste("starting year", year))
	print(colnames(data))	
	
	### Wrangle column name inconsistencies
	
	# first, reassign colnames as lowercase
	colnames(data) <- tolower(colnames(data))
	
	# tidy style!
	# fix names
	data <- rename(data, contest_name = matches("contest_?name"), precinct = matches("precinct"),
	                ward = matches("ward"))
	
	# drop
	data <- select(data, -matches("election"), -matches("contest_?(id|number)"), -matches("party"))
	head(data)
	
	
	
	# old untidy renaming
# # 	# find index of contest name
	# cn.index <- grep("contest_?name", colnames(data))
	# # R is 1-indexed
	# # this is just for the benefit of the 2018 file which omits the underscore
	# colnames(data)[cn.index] <- "contest_name"
	# colnames(data)[grep("precinct", colnames(data))] <- "precinct"
	# # ward, candidate, votes should already be fine
	# # whoops, fix ward for 2018's sake
	# colnames(data)[grep("ward", colnames(data))] <- "ward"
	
	
	# ### Drop irrelevant columns
	
	# # things we wanna drop:
	# c <- colnames(data)
	# drop.indices <- c(grep("election", c), grep("contest_?(id|number)", c), grep("party", c))
	# # note we don't drop precinct because SMDs may cross precincts
	# colnames(data)[drop.indices] <- "drop"
	# data[grep("drop", colnames(data))] <- NULL
	# # got an extra line there bro
	# colnames(data)
	
	# add year
	data$year <- rep(year, dim(data)[1])
	# can also do this with cbind(data, year) after creating vector year
	
	print("dropped irrelevant columns (rows/cols)")
	print(dim(data))
	
	
	### Drop non-ANC obs
	# I kind of like this section and don't know if it needs tidyverse pithifying
	
	reg <- "[[:digit:]][[:upper:]][[:digit:]]{2}"
	#print(str(data$contest_name))
	#print(grep(reg, data$contest_name, fixed=FALSE))
	
	# keep ANC obs and vote / registration totals (by precinct)
	keepers <- grep(reg, data$contest_name)
	keepers <- c(keepers, grep("TOTAL", data$contest_name))
	
	data <- data[keepers,]
	
	print("dropped non-ANC obs (rows/cols)")
	print(dim(data))
	
	##### Reshape precinct-level totals to columns
	
	# ooh shoot the next code assumes that all contest names are ANC jawns.
	# reshape away totals before? they could indeed be [...]
	# or mutate?
	#mutate(data, contest_name = regmatches(contest_name, regexpr(reg, contest_name)))
	# same problem duh
	
	#test <- mutate(data, contest_totals = map(contest_name, function(x) regmatches(x, regexpr("^[[:print:]]+- TOTAL", x))))
	
	#test$.votes <- test$votes
	#x <- spread(test, contest_totals, .votes)
	# nah this is too complex for spread ATMO?
	# totals are precinct-level
	#x <- reshape(test, direction="wide", v.names="votes", idvar=c("precinct", "", timevar="contest_totals")
	# didn't work -- drops all but one precinct obs
	
	# ah, hang on. try two-step
	totals <- data[grep("- TOTAL", data$contest_name),] %>% select(contest_name, precinct, votes)
	totals <- spread(totals, contest_name, votes)
	totals <- rename(totals, registered_voters = matches("REGISTER"), ballots = matches("BALLOT"))
	
	# take totals out of data
	data <- data[-grep("- TOTAL", data$contest_name),]
	
	# merge
	data <- inner_join(data, totals, by="precinct")
	# this mostly seems to work
	# I'm seeing observations for ward 2 precinct 129 anc 6D04. weird. is in original data?
	# yes. test for this! could indicate data issues!
	
	
	# reformat contest name to be just 6B04 e.g.
	data$contest_name <- regmatches(data$contest_name, regexpr(reg, data$contest_name))
	
	
	# break out to ANC and smd fields (already have ward)
	data$anc <- regmatches(data$contest_name, regexpr("[[:alpha:]]", data$contest_name))
	data$smd <- regmatches(data$contest_name, regexpr("[[:digit:]]{2}$", data$contest_name))
	data$ward_check <- regmatches(data$contest_name, regexpr("^[[:digit:]]", data$contest_name))
	
	# some years have whitespace in candidate names
	data$candidate <- strwrap(data$candidate)


    #### Collapsing / Reshaping
    
	# ah shit, ballots etc. will vary by precinct, so collapsing away precinct won't work yet
	# I think we wanted to aggregate up?
	# we could get the vote proportions at precinct level
	# but that doesn't mean much to people
	# so next might be ward
	# do any precincts cross wards??

	# pause to analyze ward_check
	ward_test <- data$ward_check == data$ward
	print("How many SMDs are recorded as a different ward?")
	print(summary(!ward_test))
	
	# assert that no precincts cross wards
	# why? for aggregating totals from precinct to ward to make sense?
	prec_ward <- data %>% group_by(precinct) %>% summarize(x = var(ward))
	stopifnot(identical(unique(prec_ward$x), 0))
	
	# Aggregate ballots to ward level because precinct x SMD is weird
	ward_totals <- data %>% group_by(precinct) %>% summarize(ballots = unique(ballots), ward = unique(ward), anc_votes = sum(votes))
	ward_totals <- ward_totals %>% group_by(ward) %>% summarize(ward_ballots = sum(ballots), ward_anc_votes = sum(anc_votes))
	
	# drop from low-lvl data
	data <- select(data, -ballots, -registered_voters)
	# merge
	data <- inner_join(data, ward_totals, "ward")
	

	# delete anomalous ward data (SMDs crossing wards) for collapsing (defaulting to ANC identifier)
	# first, take out ward-lvl data for affected obs
	data$ward_ballots[!ward_test] <- NA
	data$ward_anc_votes[!ward_test] <- NA
	# then coerce ward to fit with ANC
	data$ward <- data$ward_check
	data <- select(data, -ward_check)
	
	# ah shit! if I 'fix' ward, we then have ballots varying within wards/SMDs (where it was fixed)
	# if we don't do that, we have ward varying within SMD
	#   replace ballots with missing where affected? that may be clearest
	
	# collapse to contest x candidate
    # propogate up NAs in ballots using max(), we don't need that data at each ANC
	data.cand <- data %>% group_by(contest_name, candidate) %>% summarize(votes = sum(votes),
	        anc=unique(anc), ward=unique(ward), year=unique(year), 
	        ward_ballots=max(ward_ballots), ward_anc_votes=max(ward_anc_votes),
	        smd=unique(smd))


	# ah snacks I forgot about OVER VOTES and UNDER VOTES in later years!
	# 1. gotta avoid treating them as candidates
	# 2. are they worth reshaping or is my own ward-level calculation of under votes better?
	# maybe worth reshaping because I was curious to have under votes at the contest level,
	#   that's cool
    # so, they should have been collapsed to candidate level okay... 

    # also consider what under and over votes mean for the votes fields. can I just sum votes here
    #   to get smd_anc_votes? no, that gives me smd_ballots
    
    # ok. deal with over/under votes. I think we don't have to condition -- should be harmless if
    #   they're absent?
    ind <- grep("^(over|under) ?votes$", tolower(data.cand$candidate))
    not_ind <- setdiff(seq(nrow(data.cand)), ind)
    over.under <- data.cand[ind,]
    # drop
    data.cand <- data.cand[not_ind,]
    # generalize strings
    over.under$candidate <- tolower(over.under$candidate)
    # reshape
    over.under <- over.under %>% spread(candidate, votes)
    # accommodate years w/o over/under data by adding the reshaped columns manually
    if(nrow(over.under) == 0) {
    	over.under$"over votes" <- integer(0)
    	over.under$"under votes" <- integer(0)
    }
    # rename; keep
    over.under <- over.under %>% rename(over_votes = "over votes", under_votes = "under votes") %>%
            select(contest_name, over_votes, under_votes)
    # pull back in
    data.cand <- left_join(data.cand, over.under, by="contest_name")
    	
	# reshape to contest
	# keep: SMD ANC votes, ward ANC votes, # official candidates, winner name, winner %, write-in %
	#   ward totals (2), anc/ward/smd/yr, 
	data.cont <- data.cand %>% group_by(contest_name) %>% summarize(smd_anc_votes = sum(votes), 
	        explicit_candidates = n() - 1, ward_ballots = unique(ward_ballots), 
	        over_votes = unique(over_votes), under_votes = unique(under_votes),
	        ward_anc_votes = unique(ward_anc_votes), anc=unique(anc), smd=unique(smd), year=unique(year),
	        ward=unique(ward), winner=candidate[which.max(votes)], winner_votes=max(votes),
	        write_in_votes=votes[grep("write.*in",tolower(candidate))])
	
    # now we can make use of over/under if they exist
    data.cont$smd_ballots <- data.cont$smd_anc_votes + data.cont$over_votes + data.cont$under_votes

	
	##### old awkward collapsing shits
	
	
	#### Collapse
	# Want to collapse down to contest level, right now just collapsing to candidate
	
	#smd.list <- unique(data$contest_name)
	
	# ooh when I try grabbing a SMD it seems like obs are dublicated. why??
	# ah. SMDs may cross precincts. so keep precinct above!
	
	# gotta sort before we get into this...
	# cause will assume candidates are sorted when reshaping
	#data <- data[order(data$contest_name, data$candidate),]
	
	# initialze new data.frame with same column names
	#wide.data <- data[0,]
	
	# collapsing to candidate level ought to be simple.
	#wide.data <- data %>% group_by(contest_name, candidate) %>% summarize(votes = sum(votes))
	# was this a false start from this round of coding? hun.
	
	
	
	
	
	
	# loop over unique SMDs, collapsing over precincts down to candidate level
	# for(smd in smd.list){
		# # find the smd
		# smd <- data[data$contest_name == smd,]
		# # handle multiple precincts
		# precincts <- unique(smd$precinct)
		# smd.new <- NULL
	    # for(pre in precincts){
	    	# smd.pre <- smd[smd$precinct == pre,]
	    	# if (is.null(smd.new)){
	    		# smd.new <- smd.pre
	    	# } else{
	    		# # assert smd.new and smd.pre have same # obs
	    		# if(dim(smd.new)[1] != dim(smd.pre)[1]){
	    			# stop(simpleError(paste("Precinct data w/in SMD ", str(pre), " differ in dimension!", sep="")))
	    		# }
	    		# smd.new$votes <- smd.new$votes + smd.pre$votes
	    	# }
	    # }
	    
	    # # boo runtime
	    # wide.data <- rbind(wide.data, smd.new)
	# }
	
	# # precinct is no longer meaningful
	# wide.data$precinct <- NULL
	
	
	print(paste("year done", year, "(rows/cols)", sep=" "))
	print(dim(data.cont))
	
	# If this is the first iteration, initialize with header
	if(is.null(all.data)) all.data <- data.cont[0,]

	# append to other years, tidystyle
	all.data <- bind_rows(all.data, data.cont)
		
}

# sort for easier sanity checking
all.data <- all.data[order(all.data$year, all.data$contest_name),]


write.table(all.data, file=paste(path, "/data/", "allyears", "_collapsed.csv", sep=""), append=FALSE, quote=FALSE, sep=",", row.names=FALSE, col.names=TRUE)



