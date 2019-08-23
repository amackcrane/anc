

library(tidyverse)
library(sf)
library(lwgeom)
library(magrittr)
# for some reason we need to get magrittr explicitly to get fancy pipes

path <- getwd()

# read in data on voter registrations and ballots cast by precinct/year
regs <- read.table(paste(path, "/cleaned_data/precinct_totals.csv", sep=""),
         header=TRUE, sep=",")

#print(head(regs))

# Make table of # ANCs corresponding to each precinct
regs %<>% mutate(anc.full = paste(as.character(ward), as.character(anc), sep=""))
count <- regs %>% group_by(precinct) %>% summarize(ancs = length(unique(anc.full)))

# print some stuff
print("How many ANCs does each precinct line in?")
print(count)
print("How many precincts cross N ANCs?")
print(count %>% group_by(ancs) %>% summarize(precincts = length(unique(precinct))))

# how many ancs have duplicitous precincts?
#count.anc <- regs %>% group_by(anc.full) %>% summarize(pcts = length(unique(anc.full)))
# not sure this is right ^^

# Merge data on 'duplicitous' precincts w/ registration data
#   and collapse from contest-level to ANC x precinct x year
collapsed <- count %>%
        mutate(duplicitous = (ancs > 1)) %>%
	inner_join(regs, by=c("precinct")) %>%
	group_by(anc.full, precinct, year) %>%
	summarize(
            duplicitous = unique(duplicitous),
	    voters = unique(registered_voters),
            ballots = unique(ballots))

print("Registration data @ precinct x ANC x year lvl")
print(collapsed)

# so at this point we could just toss precincts crossing ANCs (and lose around 40% of the data)
# or we could average them or something
# or we could give them weighted averages based on GIS data

# read in shapefiles
precinct_shapes <- st_read(paste(path, "/raw_data/precinct_shapes_2012/Voting_Precinct__2012.shp", sep=""))
anc_shapes <- st_read(paste(path, "/raw_data/anc_2013/Advisory_Neighborhood_Commissions_from_2013.shp", sep=""))

#print(anc_shapes)

# compute shape overlaps
overlap <- st_intersection(anc_shapes, precinct_shapes) %>%
        mutate(over.area = st_area(.) %>% as.numeric()) %>%
	rename(.anc = NAME, .precinct = NAME.1) %>%
	select(.anc, .precinct, over.area)


# parse ANC & precinct identifiers better
overlap %<>%
        mutate(anc.full =
	    regmatches(.anc, regexpr("[[:digit:]][[:alpha:]]$", .anc))) %>%
	mutate(precinct =
	    regmatches(.precinct, regexpr("[[:digit:]]+", .precinct)))
overlap %<>% select(anc.full, precinct, over.area)

#print(overlap)

print("how many entries do we get in the overlap dataset?")
print(nrow(overlap))
print("how many did we start with in the election data grouped by anc x pct?")
print(nrow(collapsed) / 4)

# these aren't far off, so it's clealy only including shapes with intersections -- it just might be counting some trivial ones

#print("how many of the intersections in 'overlap' are nontrivial?")
#print(nrow(overlap[overlap$area > 10,]))

# what are the units? it's like 7-digit numbers... sq meters, feet, lat/lon minutes???

#hist(overlap$area, breaks=200)

# well... we could start by restricting it to ones noted in 'collapsed'

# how do we test??
# plot shapes
#   plot questionable (small) overlaps
# cross-ref 'overlap' with 'collapsed'

# note there are different types of geometries in the 'overlap' set
# in the originals it's just polygons
# in 'overlap' -- polygon, multipolygon, geometrycollection (point; linestr...)


# 0. get relative areas of precincts in diff ANCs

# get precinct total areas
precinct.areas <- precincts %>%
            mutate(area = st_area(.) %>% as.numeric(),
                precinct = regmatches(NAME, regexpr("[[:digit:]]+", NAME)))
precinct.areas <- tibble(precinct=precinct.areas$precinct,
                            prec.area=precinct.areas$area)

# merge with overlap areas
overlap %<>% inner_join(precinct.areas, by=c("precinct"))

#print(overlap)

# compute relative area of ANCxprec as [area of overlap] / [precinct area]
overlap %<>% mutate(rel.area = over.area / prec.area)

#print(overlap)
#hist(overlap$rel.area, breaks=100)


#hist(overlap$rel.area, breaks=100)



# 1. cross-reference with reg data

overlap %<>% mutate(precinct = as.integer(precinct))
crossref <- full_join(overlap, collapsed, by=c("anc.full", "precinct"))


print(crossref)

# who's left hanging?
# rel.area represents 'overlap'; duplicitous reps 'collapsed'
# what do we expect?
#   hopefully nobody left hanging from 'collapsed'
print("Any any precinct-ANC combos from voting data missing from GIS data?")
hanging.vote <- crossref %>% filter(is.na(rel.area), year==2012)
print(nrow(hanging.vote))
print(hanging.vote)
# with no rel.area filtering, we get 3 missing, one of which is 'duplicitous'
#   why would we get non-duplicitous hanging??? funny.

#  ************************** Look into this ^^  *************************

print("Are any precinct-ANC combos from GIS data missing from voting data?")
hanging.gis <- crossref %>% filter(is.na(duplicitous))
print(nrow(hanging.gis))
#print(hanging.gis)
# there's 5 (when we drop at 10% rel.area):
# 2A prec 6; 3G prec 51; 7B prec 107; 3G prec 52; 6D prec 129


# what does relative area look like cond. on 'duplicitous'?
#crossref %>% filter(duplicitous, is.numeric(rel.area)) %>% hist(as.numeric(rel.area), breaks=100)
# ^^ not working


# 2. Make some numbers!

# 2a. Naive way -- drop crossing precincts

# 2b. Fancy way -- proportional to relative area

# drop hanging gis data
crossref %<>% filter(!is.na(duplicitous))

# fix hanging vote data, first dropping duplicitous prec's w/ missing GIS
crossref %<>% filter(!(is.na(rel.area) & duplicitous))
crossref %<>% mutate(rel.area = ifelse(is.na(rel.area), 1, rel.area))


# gotta... recompute 'duplicitous' I think?
crossref %<>% group_by(precinct, year) %>%
                mutate(duplicitous = length(unique(anc.full)))
# and then make sure nonduplicitous obs have area 1
print("this better be zero")
print(nrow(crossref %>% filter(!duplicitous & rel.area < 1)))
# which they do already! neat.

# do the thing!
crossref %<>% mutate(voters = voters * rel.area, ballots = ballots * rel.area)

# aggregate up to ANC
crossref %<>% group_by(anc.full, year) %<>%
               summarize(voters = round(sum(voters)),
	           ballots = round(sum(ballots)))
reg.fixed <- tibble(anc.full = crossref$anc.full, year=crossref$year,
                   voters=crossref$voters, ballots=crossref$ballots)

print(reg.fixed)

write.table(reg.fixed, file=paste(path, "/cleaned_data/anc_turnout.csv", sep=""), sep=",", append=FALSE, quote=FALSE, row.name=FALSE, col.names=TRUE)