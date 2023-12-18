# Data in this folder

## CenPop2010

These are centers of population of Census block groups downloaded from the US Census website: <https://www.census.gov/geographies/reference-files/2010/geo/2010-centers-population.html>. This is constructed by the Census as the center of mass of the constituient Census blocks weighted by the population of each block. It therefore differs from the unweighted centroid of the block group, i.e., the centroid that would be created by the `st_centroid` function of the R sf package, or the centroid operation of other GIS software.

## Crime Data

### Bergen

Crime data downloaded from New Jersey State Police Uniform Crime Reports:

<https://nj.gov/njsp/ucr/uniform-crime-reports.shtml>

Note that this is a human-readable multi-sheet spreadsheet not suitable for statistical analysis. The file bergen_ucr_2019.csv is extracted from the sheet named "Bergen" and reformatted as rectangular table.

### Queens

### Nassau

Crime data downloaded from New York State Open Data portal using the following API query:

<https://data.ny.gov/resource/ca8h-8gjq.csv?$query=SELECT%0A%20%20%60county%60%2C%0A%20%20%60agency%60%2C%0A%20%20%60year%60%2C%0A%20%20%60months_reported%60%2C%0A%20%20%60total_index_crimes%60%2C%0A%20%20%60violent%60%2C%0A%20%20%60murder%60%2C%0A%20%20%60forcible_rape%60%2C%0A%20%20%60robbery%60%2C%0A%20%20%60aggravated_assault%60%2C%0A%20%20%60property%60%2C%0A%20%20%60burglary%60%2C%0A%20%20%60larceny%60%2C%0A%20%20%60motor_vehicle_theft%60%2C%0A%20%20%60region%60%0AWHERE%20(%60county%60%20IN%20(%22Nassau%22%2C%20%22Queens%22))%20AND%20(%60year%60%20IN%20(%222019%22))%0AORDER%20BY%0A%20%20%60year%60%20DESC%20NULL%20FIRST%2C%0A%20%20%60county%60%20ASC%20NULL%20LAST%2C%0A%20%20%60agency%60%20ASC%20NULL%20LAST%2C%0A%20%20%60region%60%20ASC%20NULL%20LAST>

## Geocorr

All geocorr files are created using the Geocorr 2014 application on the Missouri Census Data Center website: <https://mcdc.missouri.edu/applications/geocorr.html>.

## Police Agency Crosswalks

Bergen, NJ and Nassau, NY police agency crosswalks created by the author.

For Bergen County, this is based on visual matching of police agency names. For example, the police agency "ALLENDALE PD" in the New Jersey UCR data corresponds to "Allendale borough" in the US Census Bureau place data.

For Nassau County, some police agencies serve multiple contiguous villages (and one Census-designated place, Port Washington CDP). Assignment is based on name matching and visual spatial matching of Nassau villages CDPs to a police district spatial file created by CUNY Center for Urban Research.

Note that these crosswalks only apply to areas served by small local police agencies (all of Bergen County and those areas of Nassau County not served by Nassau County Police Department). Block groups not falling in one of these groupings are spatially assigned to a matching police *precinct* of a larger agency. This includes Queens, NY, and those parts of Nassau County served by NCPD.


