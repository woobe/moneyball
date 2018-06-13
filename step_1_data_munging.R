# ------------------------------------------------------------------------------
# Data Munging with Lahman baseball data only
# 1. Download data from GitHub
# 2. Reformat data for Moneyball demo
# 3. Save reformatted data as CSVs
#
#
# Author: Jo-fai Chow (https://twitter.com/matlabulous)
# ------------------------------------------------------------------------------


# Pre-load libaries
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(fst))
suppressPackageStartupMessages(library(caret))


# Core Parameters
filter_AB = 100 # look at batters with AB > n
filter_IPouts = 120 # look at pitchers with IPouts > n
n_latest = 2017 # latest year
n_ahead = 3 # Predict n years ahead
save_csv = TRUE
n_seed = 1234


# ------------------------------------------------------------------------------
# Download data from GitHub (if needed)
# Source: https://github.com/chadwickbureau/baseballdatabank
# Try to import CSVs locally first (in ./cache_data/lahman/)
# ------------------------------------------------------------------------------


# Paths
dir_local = "./cache_data/lahman/"
dir_github = "https://github.com/chadwickbureau/baseballdatabank/raw/master/core/"


# "People.csv" used to be "Master.csv" in previous versions
if (file.exists(paste0(dir_local, "People.csv"))) {
  d_master = fread(paste0(dir_local, "People.csv"))
} else {
  d_master = fread(paste0(dir_github, "People.csv"))
}


# Batting
if (file.exists(paste0(dir_local, "Batting.csv"))) {
  d_bat_raw = fread(paste0(dir_local, "Batting.csv"))
} else {
  d_bat_raw = fread(paste0(dir_github, "Batting.csv"))
}


# Pitching
if (file.exists(paste0(dir_local, "Pitching.csv"))) {
  d_pit_raw = fread(paste0(dir_local, "Pitching.csv"))
} else {
  d_pit_raw = fread(paste0(dir_github, "Pitching.csv"))
}




# ------------------------------------------------------------------------------
# Reformat Master
# ------------------------------------------------------------------------------


# Add Date info
d_master[, debut := as.Date(debut), by = .I]
d_master[, finalGame := as.Date(finalGame), by = .I]
d_master[, debut_year := lubridate::year(debut)]
d_master[, final_game_year := lubridate::year(finalGame)]



# ------------------------------------------------------------------------------
# Reformat Pitching data
# ------------------------------------------------------------------------------

if (TRUE) {

  # Columns to keep
  col_core = c("playerID",
               "nameFirst", "nameLast", "nameGiven",
               "weight", "height", "bats", "throws",
               "birthYear", "birthCountry", "birthState", "birthCity",
               "debut_year", "final_game_year")


  # Merge with master
  d_pit = merge(d_master[, col_core, with = FALSE],
                d_pit_raw, by = "playerID", sort = TRUE)


  # Filter (consider records with IPouts > filter only and recent players only)
  d_pit = d_pit[IPouts > filter_IPouts & debut_year >= 2000]


  # Unique list of pitchers
  list_pit = sort(unique(d_pit$playerID))


  # Define list of targets
  targets_pit = c(colnames(d_pit)[-1:-19],
                  "WHIP", "WPCT")


  # Helper Function
  reformat_pit = function(d_pit, list_pit, targets,
                          n = 1, n_ahead = 3, n_latest = 2017) {

    # Extract
    tmp_id = list_pit[n]
    # tmp_id = "salech01"
    tmp_pit = d_pit[playerID == tmp_id,]

    # Check: player has more than one year of records
    if (length(unique(tmp_pit$yearID)) > 1) {

      # Min/Max year
      min_year = min(tmp_pit$yearID)
      max_year = max(tmp_pit$yearID)

      # Display
      cat("Now working on ...",
          unique(tmp_pit$nameFirst), unique(tmp_pit$nameLast),
          "... Records from", min_year, "to", max_year, "...\n")

      # Simple annual agg (avoid multi teams stats)
      # Do a simple aggregation (because players may play in different teams in same year)
      tmp_pit_agg =
        tmp_pit %>%
        group_by(yearID) %>%
        summarise(W = sum(W, na.rm = TRUE),
                  L = sum(L, na.rm = TRUE),
                  G = sum(G, na.rm = TRUE),
                  GS = sum(GS, na.rm = TRUE),
                  CG = sum(CG, na.rm = TRUE),
                  SHO = sum(SHO, na.rm = TRUE),
                  SV = sum(SV, na.rm = TRUE),
                  IPouts = sum(IPouts, na.rm = TRUE),
                  H = sum(H, na.rm = TRUE),
                  ER = sum(ER, na.rm = TRUE),
                  HR = sum(HR, na.rm = TRUE),
                  BB = sum(BB, na.rm = TRUE),
                  SO = sum(SO, na.rm = TRUE),
                  BAOpp = mean(BAOpp, na.rm = TRUE),
                  ERA = mean(ERA, na.rm = TRUE),
                  IBB = sum(IBB, na.rm = TRUE),
                  WP = sum(WP, na.rm = TRUE),
                  HBP = sum(HBP, na.rm = TRUE),
                  BK = sum(BK, na.rm = TRUE),
                  BFP = sum(BFP, na.rm = TRUE),
                  GF = sum(GF, na.rm = TRUE),
                  R = sum(R, na.rm = TRUE),
                  SH = sum(SH, na.rm = TRUE),
                  SF = sum(SF, na.rm = TRUE),
                  GIDP = sum(GIDP, na.rm = TRUE))


      # Convert to data.table
      tmp_pit_agg = as.data.table(tmp_pit_agg)

      # To get WHIP, use WHIP="(BB+H)/(IPouts/3))"
      tmp_pit_agg[, WHIP := ((BB + H) / (IPouts / 3)), by = .I]

      # Add WPCT
      tmp_pit_agg[, WPCT := (W/(W+L)), by = .I]


      # Find the team with most games in each year
      list_year = sort(unique(tmp_pit$yearID))
      tmp_team = c()
      for (n_year in 1:length(list_year)) {

        # Find all records in that year
        tmp_pit_year = copy(tmp_pit[yearID == list_year[n_year]])
        row_max = which(tmp_pit_year$G == max(tmp_pit_year$G))
        row_max = row_max[1] # just in case there are two records

        # Store results
        tmp_team =
          rbind(tmp_team,
                data.table(yearID = list_year[n_year],
                           teamID = tmp_pit_year[row_max,]$teamID,
                           lgID = tmp_pit_year[row_max,]$lgID))

      }

      # Combine tmp_pit_agg and tmp_team
      col_keep = c("playerID", "yearID",
                   "nameFirst", "nameLast", "nameGiven",
                   "weight", "height", "bats", "throws",
                   "birthYear", "birthCountry", "birthState", "birthCity",
                   "debut_year", "teamID", "lgID")

      tmp_merge = merge(tmp_team,
                        tmp_pit[, col_keep, with = FALSE],
                        by = c("yearID", "teamID", "lgID"),
                        sort = TRUE)

      tmp_pit_new = merge(tmp_merge, tmp_pit_agg,
                          by = c("yearID"),
                          sort = TRUE)

      # Replace tmp_bat
      tmp_pit = copy(tmp_pit_new)
      rm(tmp_pit_new)


      # Add age
      tmp_pit[, age := (yearID - birthYear), by = .I]

      # Add career year
      tmp_pit[, career_year := (yearID - debut_year + 1), by = .I]


      # --------------------------------------------------------------------------
      # Sliding Window
      # --------------------------------------------------------------------------

      # Historical stats
      tmp_stats = c()

      # year by year munging
      list_year = sort(unique(tmp_pit$yearID))

      # Add extra n years (for predictions) if max(list_year) == n_latest
      if (max(list_year) == n_latest) {

        for (n_year in 1:n_ahead) {

          list_year = c(list_year, max(list_year) + 1)
          tmp_pit_extra = copy(tmp_pit[yearID == n_latest])

          # Modify yearID, age and career year
          tmp_pit_extra[, yearID := max(list_year)]
          tmp_pit_extra[, age := (max(list_year) - birthYear)]
          tmp_pit_extra[, career_year := (max(list_year) - debut_year + 1)]

          # Stack
          tmp_pit = rbind(tmp_pit, tmp_pit_extra)

        }

      }

      # Loop through all years
      for (item in list_year) {

        # Extract
        tmp_year = item
        tmp_pit_now = tmp_pit[yearID == tmp_year]

        # Historical records
        tmp_pit_hist = tmp_pit[yearID < tmp_year]
        tmp_pit_comb = rbind(tmp_pit_hist, tmp_pit_now)

        # For each target
        for (n_target in 1:length(targets)) {

          # Extract
          tmp_target = targets[n_target]

          # Current and previous years
          tmp_values = data.table(yearID = (tmp_year-5):tmp_year)
          tmp_values = merge(tmp_values,
                             tmp_pit[, c("yearID", tmp_target), with = F],
                             by = "yearID", all.x = TRUE, sort = TRUE)
          tmp_values = as.data.frame(tmp_values)

          # Simple shift and avg
          tmp_shift_avg =
            data.frame(
              last1 = tmp_values[5, 2],
              last2 = tmp_values[4, 2],
              last3 = tmp_values[3, 2],
              last4 = tmp_values[2, 2],
              last5 = tmp_values[1, 2],
              avg_last2 = mean(tmp_values[4:5, 2], na.rm = TRUE),
              avg_last3 = mean(tmp_values[3:5, 2], na.rm = TRUE),
              avg_last4 = mean(tmp_values[2:5, 2], na.rm = TRUE),
              avg_last5 = mean(tmp_values[1:5, 2], na.rm = TRUE))

          # Rename columns
          colnames(tmp_shift_avg) = paste0(colnames(tmp_shift_avg), "_", tmp_target)

          # Store
          if (n_target == 1) tmp_shift_avg_all = tmp_shift_avg
          if (n_target > 1) tmp_shift_avg_all = cbind(tmp_shift_avg_all,
                                                      tmp_shift_avg)

          # Clean up
          rm(tmp_shift_avg)


        }

        # Combine
        tmp_pit_output = cbind(tmp_pit_now, tmp_shift_avg_all)

        # Store
        tmp_stats = rbind(tmp_stats, tmp_pit_output)

      }


      # Fill future targets with NA
      tmp_stats_hist = copy(tmp_stats[yearID <= n_latest])
      tmp_stats_new = copy(tmp_stats[yearID > n_latest])

      # Remove value in target columns
      for (j in targets) {
        set(tmp_stats_new, i = NULL, j = j, value = NA)
      }

      # Stack
      tmp_stats_comb = rbind(tmp_stats_hist, tmp_stats_new)

      # Return
      return(tmp_stats_comb)


    }

  }

  # a = reformat_pit(d_pit, d_pit_ari, d_pit_da, list_pit, targets_pit, 2)


  # Reformat data in parallel mode
  cl = makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  d_pit_munged = foreach(n = 1:length(list_pit),
                         .packages = c("data.table", "dplyr"),
                         .combine = rbind) %dopar%
    reformat_pit(d_pit, list_pit, targets_pit, n, n_ahead, n_latest)
  stopCluster(cl)


  # Split train/valid/test
  d_pit_train = copy(d_pit_munged[yearID < 2015])
  d_pit_valid = copy(d_pit_munged[yearID >= 2015 & yearID <= n_latest])
  d_pit_test = copy(d_pit_munged[yearID > n_latest])


  # Save reformatted datasets as cache
  if (save_csv) {
    fwrite(d_pit_train, file = "./cache_data/munged/d_pit_train.csv")
    fwrite(d_pit_valid, file = "./cache_data/munged/d_pit_valid.csv")
    fwrite(d_pit_test, file = "./cache_data/munged/d_pit_test.csv")
  }


}


# ------------------------------------------------------------------------------
# Reformat Batting data
# ------------------------------------------------------------------------------

if (TRUE) {

  # Rename 2B and 3B to H2B (Double) and H3B (Triple)
  colnames(d_bat_raw)[10] = "H2B"
  colnames(d_bat_raw)[11] = "H3B"


  # Columns to keep
  col_core = c("playerID",
               "nameFirst", "nameLast", "nameGiven",
               "weight", "height", "bats", "throws",
               "birthYear", "birthCountry", "birthState", "birthCity",
               "debut_year", "final_game_year")


  # Merge with master
  d_bat = merge(d_master[, col_core, with = FALSE],
                d_bat_raw, by = "playerID", sort = TRUE)


  # Filter
  d_bat[, avg_AB := mean(AB, na.rm = TRUE), by = playerID]
  d_bat = d_bat[avg_AB > filter_AB & debut_year >= 2000]
  d_bat[, avg_AB := NULL]


  # Unique list of batchers
  list_bat = sort(unique(d_bat$playerID))


  # Define list of targets
  targets_bat = c(colnames(d_bat)[-1:-19],
                  "BA", "OBP", "SLG", "OPS")


  # Helper Function
  reformat_bat = function(d_bat, list_bat, targets,
                          n = 1, n_ahead = 3, n_latest = 2017) {

    # Extract
    tmp_id = list_bat[n]
    # tmp_id = "troutmi01"
    tmp_bat = d_bat[playerID == tmp_id,]

    # Check: player has more than one year of records
    if (length(unique(tmp_bat$yearID)) > 1) {

      # Min/Max year
      min_year = min(tmp_bat$yearID)
      max_year = max(tmp_bat$yearID)

      # Display
      cat("Now working on ...",
          unique(tmp_bat$nameFirst), unique(tmp_bat$nameLast),
          "... Records from", min_year, "to", max_year, "...\n")

      # Simple annual agg (avoid multi teams stats)
      # Do a simple aggregation (because players may play in different teams in same year)
      tmp_bat_agg =
        tmp_bat %>%
        group_by(yearID) %>%
        summarise(G = sum(G, na.rm = TRUE),
                  AB = sum(AB, na.rm = TRUE),
                  R = sum(R, na.rm = TRUE),
                  H = sum(H, na.rm = TRUE),
                  H2B = sum(H2B, na.rm = TRUE),
                  H3B = sum(H3B, na.rm = TRUE),
                  HR = sum(HR, na.rm = TRUE),
                  RBI = sum(RBI, na.rm = TRUE),
                  SB = sum(SB, na.rm = TRUE),
                  CS = sum(CS, na.rm = TRUE),
                  BB = sum(BB, na.rm = TRUE),
                  SO = sum(SO, na.rm = TRUE),
                  IBB = sum(IBB, na.rm = TRUE),
                  HBP = sum(HBP, na.rm = TRUE),
                  SH = sum(SH, na.rm = TRUE),
                  SF = sum(SF, na.rm = TRUE),
                  GIDP = sum(GIDP, na.rm = TRUE))

      # Convert to data.table
      tmp_bat_agg = as.data.table(tmp_bat_agg)

      # Calculate Batting Average
      tmp_bat_agg[, BA := (H / AB), by = .I]

      # Calculate On-base Percentage
      # (H+BB+HBP) / (AB+BB+HBP+SF)
      tmp_bat_agg[, OBP := (H+BB+HBP) / (AB+BB+HBP+SF), by = .I]

      # Slugging Percentage
      # Total base / At Bat
      tmp_bat_agg[, SLG := (H + H2B + 2*H3B + 3*HR) / AB, by = .I]

      # OPS On-base Percentage + Slugging
      tmp_bat_agg[, OPS := (OBP + SLG), by = .I]

      # Find the team with most games in each year
      list_year = sort(unique(tmp_bat$yearID))
      tmp_team = c()
      for (n_year in 1:length(list_year)) {

        # Find all records in that year
        tmp_bat_year = copy(tmp_bat[yearID == list_year[n_year]])
        row_max = which(tmp_bat_year$G == max(tmp_bat_year$G))
        row_max = row_max[1] # just in case there are two records

        # Store results
        tmp_team =
          rbind(tmp_team,
                data.table(yearID = list_year[n_year],
                           teamID = tmp_bat_year[row_max,]$teamID,
                           lgID = tmp_bat_year[row_max,]$lgID))

      }

      # Combine tmp_bat_agg and tmp_team
      col_keep = c("playerID", "yearID",
                   "nameFirst", "nameLast", "nameGiven",
                   "weight", "height", "bats", "throws",
                   "birthYear", "birthCountry", "birthState", "birthCity",
                   "debut_year", "teamID", "lgID")

      tmp_merge = merge(tmp_team,
                        tmp_bat[, col_keep, with = FALSE],
                        by = c("yearID", "teamID", "lgID"),
                        sort = TRUE)

      tmp_bat_new = merge(tmp_merge, tmp_bat_agg,
                          by = c("yearID"),
                          sort = TRUE)

      # Replace tmp_bat
      tmp_bat = copy(tmp_bat_new)
      rm(tmp_bat_new)


      # --------------------------------------------------------------------------
      # Calculate age and career year
      # --------------------------------------------------------------------------

      # Add age
      tmp_bat[, age := (yearID - birthYear), by = .I]

      # Add career year
      tmp_bat[, career_year := (yearID - debut_year + 1), by = .I]


      # --------------------------------------------------------------------------
      # Sliding Window
      # --------------------------------------------------------------------------

      # Historical stats
      tmp_stats = c()

      # year by year munging
      list_year = sort(unique(tmp_bat$yearID))

      # Add extra n years (for predictions) if max(list_year) == n_latest
      if (max(list_year) == n_latest) {

        for (n_year in 1:n_ahead) {

          list_year = c(list_year, max(list_year) + 1)
          tmp_bat_extra = copy(tmp_bat[yearID == n_latest])

          # Modify yearID, age and career year
          tmp_bat_extra[, yearID := max(list_year)]
          tmp_bat_extra[, age := (max(list_year) - birthYear)]
          tmp_bat_extra[, career_year := (max(list_year) - debut_year + 1)]

          # Stack
          tmp_bat = rbind(tmp_bat, tmp_bat_extra)

        }

      }

      # Loop through all years
      for (item in list_year) {

        # Extract
        tmp_year = item
        tmp_bat_now = tmp_bat[yearID == tmp_year]

        # Historical records
        tmp_bat_hist = tmp_bat[yearID < tmp_year]
        tmp_bat_comb = rbind(tmp_bat_hist, tmp_bat_now)

        # For each target
        for (n_target in 1:length(targets)) {

          # Extract
          tmp_target = targets[n_target]

          # Current and previous years
          tmp_values = data.table(yearID = (tmp_year-5):tmp_year)
          tmp_values = merge(tmp_values,
                             tmp_bat[, c("yearID", tmp_target), with = F],
                             by = "yearID", all.x = TRUE, sort = TRUE)
          tmp_values = as.data.frame(tmp_values)

          # Simple shift and avg
          tmp_shift_avg =
            data.frame(
              last1 = tmp_values[5, 2],
              last2 = tmp_values[4, 2],
              last3 = tmp_values[3, 2],
              last4 = tmp_values[2, 2],
              last5 = tmp_values[1, 2],
              avg_last2 = mean(tmp_values[4:5, 2], na.rm = TRUE),
              avg_last3 = mean(tmp_values[3:5, 2], na.rm = TRUE),
              avg_last4 = mean(tmp_values[2:5, 2], na.rm = TRUE),
              avg_last5 = mean(tmp_values[1:5, 2], na.rm = TRUE))

          # Rename columns
          colnames(tmp_shift_avg) = paste0(colnames(tmp_shift_avg), "_", tmp_target)

          # Store
          if (n_target == 1) tmp_shift_avg_all = tmp_shift_avg
          if (n_target > 1) tmp_shift_avg_all = cbind(tmp_shift_avg_all,
                                                      tmp_shift_avg)

          # Clean up
          rm(tmp_shift_avg)


        }

        # Combine
        tmp_bat_output = cbind(tmp_bat_now, tmp_shift_avg_all)

        # Store
        tmp_stats = rbind(tmp_stats, tmp_bat_output)

      }


      # Fill future targets with NA
      tmp_stats_hist = copy(tmp_stats[yearID <= n_latest])
      tmp_stats_new = copy(tmp_stats[yearID > n_latest])

      # Remove value in target columns
      for (j in targets) {
        set(tmp_stats_new, i = NULL, j = j, value = NA)
      }

      # Stack
      tmp_stats_comb = rbind(tmp_stats_hist, tmp_stats_new)

      # Return
      return(tmp_stats_comb)


    }

  }


  # Reformat data in parallel mode
  cl = makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  d_bat_munged = foreach(n = 1:length(list_bat),
                         .packages = c("data.table", "dplyr"),
                         .combine = rbind) %dopar%
    reformat_bat(d_bat, list_bat, targets_bat, n, n_ahead, n_latest)
  stopCluster(cl)


  # Split train/valid/test
  d_bat_train = copy(d_bat_munged[yearID < 2015])
  d_bat_valid = copy(d_bat_munged[yearID >= 2015 & yearID <= n_latest])
  d_bat_test = copy(d_bat_munged[yearID > n_latest])


  # Save reformatted datasets as cache
  if (save_csv) {
    fwrite(d_bat_train, file = "./cache_data/munged/d_bat_train.csv")
    fwrite(d_bat_valid, file = "./cache_data/munged/d_bat_valid.csv")
    fwrite(d_bat_test, file = "./cache_data/munged/d_bat_test.csv")
  }


}



