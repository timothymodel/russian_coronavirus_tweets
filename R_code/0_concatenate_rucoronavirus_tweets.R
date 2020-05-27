##### If you do not see below Russian cyrillic,
##### file must be opened with UTF-8 Encoding

##### System Setup (WINDOWS) #####

Sys.setlocale('LC_ALL', 'russian') # Plays nice with cyrillic characters
Sys.setlocale("LC_TIME", "English") # English for time converstion

##### Load Relevant Packages #####

library(jsonlite)
library(data.table)
##### Read in .json files #####

korona_filenames <- list.files("C:/Users/model/OneDrive/Research/COVID-19/OSoME Twitter/", pattern="*.gz", full.names=TRUE)
korona_gzfiles <- lapply(korona_filenames, gzfile)
korona_json <- lapply(korona_gzfiles, stream_in)
korona_df <- rbindlist(lapply(korona_json, flatten), fill=T)

attach(korona_df)
korona_dfsub <- cbind(id_str, created_at, text, user.location)
detach(korona_df)

saveRDS(korona_dfsub, file ="C:/Users/model/OneDrive/Research/COVID-19/OSoME Twitter/koronasub.RDS")
