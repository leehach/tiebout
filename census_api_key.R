###############################################################################
# Git should not be used for sensitive data, such as passwords or API keys. 
# If, for example, you are developing an application that uses a cloud service 
# such as AWS or Azure, you should not include your credentials in the code 
# committed to a repository. Since Git preserves the history of all files 
# committed to a repo, you cannot just delete the sensitive data! You will have 
# to rewrite the repo history to purge the file. Information on how to do this 
# is available at https://docs.github.com/en/github/authenticating-to-github/removing-sensitive-data-from-a-repository.
###############################################################################

library(tidycensus)
census_api_key("REPLACE_WITH_YOUR_CENSUS_API_KEY", install = TRUE) 
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY") # <--This is just a check
