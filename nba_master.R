#### MASTER NBA SCRAPER
  
# Nicholas Archambault  

# This script scrapes full NBA game results for every team from every season
# between 1993 to 2016 from Goldsheet.com, a website providing game outcomes, 
# gambling results, and statistics. The scraping code accounts for the
# idiosyncrasies of the data for each year and corrects for formatting and 
# textual errors. The final result is over 59,000 game and gambling outcomes, a
# clean dataset that could be used to analyze statistical questions pertaining
# to gambling and the NBA.


rm(list = ls()) 

# Establish list of years as they will appear in document name
years <- c("93", "94", "95", "96", "97", "98", "99", "00", "01", "02", 
           "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", 
           "13", "14", "15", "16")

# Establish empty list in which to later store data for each year
master <- list(NA, length(years))

# Loop that modifies URL to appropriate format for each year
for (i in years) {
  if (i %in% c("93", "94")) {
    url1 <- paste0("https://www.goldsheet.com/historic/", i, "nbalog.html") 
  }
  else {url1 <- paste0("https://www.goldsheet.com/historic/nbalog", i, ".html")}
  
  # Scan and examine data
  x <- scan(url1, what = "", sep = "\n")
  length(x)
  head(x, 70)
  tail(x, 30)
  
  # Clean data with 'gsub()'
  x <- gsub("&#160;", "", x, fixed = TRUE)
  x <- gsub("</span>", ",", x, fixed = TRUE)
  x <- gsub("<[^<>]*>", "", x)
  
  # Eliminate extraneous rows at beginning
  starter <- grep("SUR:", x, fixed = TRUE)[1] - 1   # Data starts here
  x <- x[starter:length(x)]
  head(x)     # Check head
  
  # Further data cleaning
  x <- trimws(x, which = c("both"))
  x <- gsub("Go [tT]o List", "", x)
  x <- x[!grepl("SUR: ", x)]
  x <- x[x != ""]
  x <- x[grep("^[[:alnum:]]", x)]  
  
  # Eliminate extraneous rows at end
  games <- grep("/", x)
  last_entry <- games[length(games)]
  x <- x[1:last_entry]
  tail(x)        # Check tail to see if changes worked
  
  # More data cleaning to account for anomalies unique to certain years
  y <- gsub("  |\\*", ",", x)
  y <- gsub(",+", ";", y)
  y <- gsub("(')\\s([[:digit:]])", "\\1;\\2", y)
  y <- gsub("([[:alnum:]])\\s([12])", "\\1;\\2", y)
  y <- gsub("([[:lower:]])\\s([LWN];)", "\\1;\\2", y)
  y <- gsub("([[:upper:]])([+-])([[:digit:]])", "\\1;\\2", y)
  y <- gsub("; |'\\s|;;", ";", y)
  
  # Cut out data from rows that have double entries
  # [ This step accounts for the only loss of data (2 games) in the entire 
  # dataset, but I couldn't figure out how to keep those 2 games ] 
  doubles <- y[grepl(";[[:digit:]]{1,2}/", y)]
  y[grepl(";[[:digit:]]{1,2}/", y)] <- gsub(";.{1,2}/.*$", "", doubles)
  
  # Split data along semicolon
  z <- strsplit(y, ";")
  
  # Handle mistake data that spills onto multiple lines
  too_long <- which(lengths(z) > 7)
  if (length(too_long) != 0) {
    r <- unlist(z[too_long])
    s <- r[r != ""][1:7]
    z[too_long] <- list(s)
    if (length(r) > 8) { z <- z[-(too_long + 1)] }
  }
  
  # Eliminate rows that are not of length one (team names) or seven (game data)
  z <- z[lengths(z) %in% c(1, 7)]
  
  # Get block team names and indices
  team_indices <- which(lengths(z) == 1)
  teams <- unlist(z[team_indices])
  num_games <- c(team_indices[-1] - 1, length(z)) - team_indices
  
  # Populate a matrix will game data
  df <- matrix(unlist(z[lengths(z) == 7]), ncol = 7, byrow = TRUE)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- c("date", "team2", "result", "pointspread", "score", "location", 
                 "overUnder")
  
  # Fill team1 column
  df$team1 <- rep(unlist(z[team_indices]), times = num_games)
  
  # Account for the season of each year's data
  szn <- gsub("[^[:digit:]]", "", url1)
  if (as.numeric(szn) > 90) {
    yr <- as.numeric(paste0("19", szn))
    df$season <- paste0(as.character(yr), "-", as.character(yr + 1))
  } else {
    yr <- as.numeric(paste0("20", szn))
    df$season <- paste0(as.character(yr), "-", as.character(yr + 1))
  } # can do in one line
  
  # Attach year to 'date' column entries
  days <- grepl("^(10.*|11.*|12.*)$", df$date)
  second <- yr + 1
  df$date[days] <- paste0(as.character(yr), "/", df$date[days])
  df$date[!days] <- paste0(as.character(second), "/", df$date[!days])
  
  # Handle spread
  df <- df[df$pointspread != "NL", ]
  df$pointspread <- gsub("'", ".5", df$pointspread)   # Account for half points
  df$pointspread <- gsub("P", "0", df$pointspread)    # 'P's become zeros
  
  # Handle scores
  scores <- strsplit(df$score, "-")      # Separate into individual scores
  scores <- matrix(unlist(scores), ncol = 2, byrow = TRUE) 
  df$score1 <- scores[, 1]
  df$score2 <- scores[, 2]
  
  # Handle Over/Under
  df <- df[df$overUnder != "NL", ]
  df$overUnder <- gsub("[A-Z]+$", "", df$overUnder)
  
  # Handle overtime and location
  df$overtime <- gsub("[A-Z]", "", df$location)
  df$overtime[df$overtime == ""] <- 0
  df$location <- substr(df$location, 1, 1)

  
  # Reorder dataframe
  nba <- df[, c("season", "date", "team1", "team2", "score1", "score2", 
                "pointspread", "location", "overUnder", "overtime")]
  
  # Each entry in 'master' gets the dataframe of a given year
  master[[i]] <- nba
}

# Unlist the stored dataframes; bind them together
full_csv <- do.call(rbind, master)

# Remove first two rows and reset index
full_csv <- full_csv[-c(1, 2), ]
rownames(full_csv) <- 1:nrow(full_csv)

View(full_csv)
dim(full_csv)

# Write to CSV
write.csv(full_csv, file = "nba_master.csv")
