library("stringr")
library("dplyr")
library("tidyr")
library("magrittr")

parse.format.1 <- function(fpath) {
    fh <- file(fpath, "rt")

    gamers <- readLines(fh, n = 1) %>%
        textConnection() %>%
        read.csv(header = FALSE, stringsAsFactors = FALSE) %>%
        unlist %>%
        set_names(NULL) %>%
        extract(-seq(1,4))

    cnames <- c("Title", "Weighted.Score", "Total.Votes",
                "N1.Votes", gamers) %>%
        make.names
        
    tabulation <- readLines(fh) %>%
        textConnection() %>%
        read.csv(header = TRUE, stringsAsFactors = FALSE) %>%
        set_colnames(cnames)
    
    e <- close(fh)
    if(!is.null(e)) {
        warning("Closing ", fpath,
                " resulted in an error code of ",
                e)
    }

    return(tabulation)
    
}

parse.format.2 <- function(fpath) {
    read.csv(fpath, stringsAsFactors = FALSE) %>%
        set_colnames(c("Rank", "Title", "Score",
                       "Total.Votes", "N1.Votes",
                       "Average.Score"))
}

gamers.2011 <- parse.format.1("data/2011_raw.csv") %>%
    mutate(Year = "2011") %>%
    dplyr::select(-Weighted.Score, -Total.Votes, -N1.Votes) %>%
    gather(Gamer, GRank, -Title, -Year) %>%
    filter(!is.na(GRank)) %>%
    mutate(GRank = as.integer(GRank))

gamers.2012 <- parse.format.1("data/2012_raw.csv") %>%
    extract(,-97) %>%              # dup columns
    mutate(Year = "2012") %>%
    dplyr::select(-Weighted.Score, -Total.Votes, -N1.Votes) %>%
    gather(Gamer, GRank, -Title, -Year) %>%
    filter(!is.na(GRank)) %>%
    mutate(GRank = as.integer(GRank))

gamers.2013 <- parse.format.1("data/2013_raw.csv") %>%
    extract(,-8) %>%                    # dup columns
    mutate(Year = "2013") %>%
    dplyr::select(-Weighted.Score, -Total.Votes, -N1.Votes) %>%
    gather(Gamer, GRank, -Title, -Year) %>%
    filter(!is.na(GRank)) %>%
    mutate(GRank = as.integer(GRank))

gamers.2014 <- parse.format.1("data/2014_raw.csv") %>%
    mutate(Year = "2014") %>%
    dplyr::select(-Weighted.Score, -Total.Votes, -N1.Votes) %>%
    gather(Gamer, GRank, -Title, -Year) %>%
    filter(!is.na(GRank)) %>%
    mutate(GRank = as.integer(GRank))

gamers.df <- bind_rows(gamers.2011, gamers.2012,
                       gamers.2013, gamers.2014) %>%
    as_tibble()

all.games <- gamers.df$Title %>% unique %>% sort
all.games.adist <- adist(all.games)

nearest.matches <- lapply(seq_along(all.games),
       function(i) {
           min.dist <- min(all.games.adist[i,][-i])
           min.i <- which(all.games.adist[i,] == min.dist)
           games <- paste(all.games[min.i], collapse = " ; ")
           tibble("Game" = all.games[i],
                  "Nearest" = games)
       }) %>%
    bind_rows()

write.csv(nearest.matches,
          "data/nearest_game_name_matches.csv",
          row.names = FALSE)
