library(tidyverse)
library(tictoc)
#library(xlsx)

rm(list = ls())
data_name <- list.files("data")
data_name <- data_name[str_detect(data_name, "PV")]
data_name <- data_name[str_detect(data_name, "csv$")]
data_name <- data_name[!str_detect(data_name, "Yuanjiang")]
data_name2 <- paste0("data/",data_name)

#d <- #read.csv("data/PV_Bubeng.csv")

#for (k in 1:3) {
for (k in 1:length(data_name2)) {
  d <- read.csv(data_name2[k])

  d2 <- d
  d2[is.na(d2)] <- "hoge"

  tmp <- NULL
  LA <- NULL
  for (j in 1:ncol(d2))
    for (i in 1:nrow(d2)) {
      if(str_detect(d2[i, j ], "SWC") == TRUE) {
        tmp <- c(tmp, j)
        LA <- c(LA, d2[i - 2, j  + 1])
      }
  }
  LA <- as.numeric(LA)
  #LA <- LA[!is.na(LA)]

# because some data starts from the 2nd row
  sp_name <- names(d)
  #sp_name <- c(names(d), d[1,] |> as.character())
  sp_name2 <- sp_name[!str_detect(sp_name, "水势|重量|塞重|干重|湿重|面积|结束|[a-z]|[0-9]$")]
  #sp_name2 <- sp_name[!str_detect(sp_name, "X|长|宽|质量|叶重|水势|重量|塞重|干重|湿重|面积|结束|[a-z]|[0-9]$")]
  sp_name3 <- str_split_fixed(sp_name2, "[0-9]\\.\\.|[0-9]\\.|[0-9]", 3)

  sp_name4 <- NULL
  for (i in 1:nrow(sp_name3)) {
    for (j in 1:ncol(sp_name3))
      if (sp_name3[i,j] != "X" & sp_name3[i,j] != "")
      sp_name4 <- c(sp_name4, sp_name3[i, j])
  }

  sp_name5 <- str_split_fixed(sp_name4[sp_name4 != "NA"], " ", 2)
  sp_name6 <- NULL
  for (i in 1:nrow(sp_name5)) {
    for (j in 1:ncol(sp_name5))
      if (sp_name5[i,j] != "X" & sp_name5[i,j] != "")
      sp_name6 <- c(sp_name6, sp_name5[i, j])
  }

#str_split_fixed(sp_name2, "\\[0-9][0-9]\\.\\.|[0-9][0-9]\\.|[0-9]|[0-9][0-9]", 3)

  unique(tmp) |> length()


# sp name column ID for SWC 
  sp_dat <- tibble(sp = sp_name6, colID = unique(tmp))

# number of sample for each species
  tmp2 <- table(tmp)
  rep_dat <- tibble(colID = names(tmp2) |> as.integer(), n = tmp2 |> as.numeric())

  sp_dat2 <- full_join(sp_dat, rep_dat)

  sp_dat3 <- NULL
  for (i in 1:nrow(sp_dat2)) {
    tmp_dat <- NULL
    for (j in 1:unlist(sp_dat2[i, 3])) {
      tmp_dat <- rbind(tmp_dat, sp_dat2[i,])
    }
    sp_dat3 <- rbind(sp_dat3, tmp_dat)
  }

  sp_dat4 <- sp_dat3 |>
    mutate(LA)

  csv_name <- str_replace(data_name2, "PV", "LA")

  write_csv(sp_dat4, csv_name[k])
  message(paste0("created ", csv_name[k]))
}


d2 <- NULL
for (i in 1:length(csv_name)) {
#for (i in 1:3) {
  site <-  str_split_fixed(csv_name[i], "LA_|\\.csv", 3)[,2]
  if (site == "Bubeng") site <- "Mengla_bubeng"
  #if (site == "Bubeng") site <- "Mengla_bugen"
  d <- read_csv(csv_name[i])  |>
    mutate(site)
  d2 <- rbind(d, d2)
}

LA_dat1 <- d2 |>
  dplyr::select(sp, LA, site)

#LA_sp <- d2 |>
#  group_by(sp, site) |>
#  summarize(LA = mean(LA))

#d |>
#  filter(Biomes != "Yakushima") |>
#  filter(!is.na(LMAdisc)) |>
#  filter(!is.na(LMAleaf))

### Palm  ===============================================================
palm_file <- paste0("data/palm", 1:3, ".csv")
palm_dat <- NULL
for (k in 1:3) {
  #d1 <-  read.xlsx("data/palm.xlsx", sheetIndex = k)
  d1 <- read.csv(palm_file[k])
  d2 <- d1
  d2[is.na(d2)] <- "hoge"

  tmp <- NULL
  LA <- NULL
  for (j in 1:ncol(d2))
    for (i in 1:nrow(d2)) {
      if(str_detect(d2[i, j ], "SWC") == TRUE) {
        tmp <- c(tmp, j)
        LA <- c(LA, d2[i - 2, j  + 1])
      }
  }
  LA <- as.numeric(LA)
#  LA <- LA[!is.na(LA)]
  length(LA)

# because some data starts from the 2nd row
  #sp_name <- c(names(d), d[1:5,] |> as.character())
  sp_name <- names(d1)
  sp_name2 <- sp_name[!str_detect(sp_name, "质量|叶重|水势|重量|塞重|干重|湿重|面积|结束|[a-z]|[0-9]$")]
  sp_name3 <- str_split_fixed(sp_name2, "[0-9]\\.\\.|[0-9]\\.|[0-9]", 3)

  sp_name4 <- NULL
  for (i in 1:nrow(sp_name3)) {
    for (j in 1:ncol(sp_name3))
      if (sp_name3[i,j] != "X" & sp_name3[i,j] != "")
      sp_name4 <- c(sp_name4, sp_name3[i, j])
  }

  sp_name5 <- str_split_fixed(sp_name4[sp_name4 != "NA"], " ", 2)
  sp_name6 <- NULL
  for (i in 1:nrow(sp_name5)) {
    for (j in 1:ncol(sp_name5))
      if (sp_name5[i,j] != "X" & sp_name5[i,j] != "")
      sp_name6 <- c(sp_name6, sp_name5[i, j])
  }
  sp_name7 <- sp_name6[!str_detect(sp_name6, "NA|NA\\.")]
  length(sp_name7)

#str_split_fixed(sp_name2, "\\[0-9][0-9]\\.\\.|[0-9][0-9]\\.|[0-9]|[0-9][0-9]", 3)

  unique(tmp) |> length()

# sp name column ID for SWC 
  sp_dat <- tibble(sp = sp_name7, colID = unique(tmp))

# number of sample for each species
  tmp2 <- table(tmp)
  rep_dat <- tibble(colID = names(tmp2) |> as.integer(), n = tmp2 |> as.numeric())

  sp_dat2 <- full_join(sp_dat, rep_dat)

  sp_dat3 <- NULL
  for (i in 1:nrow(sp_dat2)) {
    tmp_dat <- NULL
    for (j in 1:unlist(sp_dat2[i, 3])) {
      tmp_dat <- rbind(tmp_dat, sp_dat2[i,])
    }
    sp_dat3 <- rbind(sp_dat3, tmp_dat)
  }

  sp_dat4 <- sp_dat3 |>
    mutate(LA)
#  csv_name <- str_replace(data_name2, "PV", "LA")
#  print(sp_dat4)
  message(paste0("created palm", k))
  palm_dat <- rbind(palm_dat, sp_dat4)
}


LA_dat2 <- palm_dat |>
  mutate(site = "Garden_palm") |>
  dplyr::select(sp, LA, site)

message(paste0("created Palm"))
write_csv(palm_dat, "data/LA_palm.csv")

## Yuanjiang===================================================================

d <- read.csv("data/PV_Yuanjiang.csv")
d2 <- d[, str_detect(names(d), "^叶面积")]

mean_LA <- apply(d2, 2, as.numeric) |>
  apply(2, \(x) mean(x, na.rm = TRUE))

#
sp_name <- names(d)
sp_name2 <- sp_name[!str_detect(sp_name, "X|^长\\.|^宽\\.|^长$|^宽$|质量|叶重|水势|重量|塞重|干重|湿重|面积|结束|[a-z]$")]
sp_name3 <- str_split_fixed(sp_name2, "\\.", 3)[,1]

LA_dat3 <- tibble(sp = sp_name3, LA = mean_LA) |>
  mutate(site = "Yuanjiang_savan")

message(paste0("created Yuanjing"))
write_csv(LA_dat3, "LA_Yuanjing.csv")

## Garden ===================================================================
#d1 <-  read.xlsx("data/Bamboo.xlsx", sheetIndex = 1) |>
d1 <-  read.csv("data/Bamboo.csv") |>
  dplyr::select(sp, LA) |>
  mutate(site = "Garden_bamboo")
#d2 <-  read.xlsx("data/Cycad.xlsx", sheetIndex = 1) |>
d2 <-  read.csv("data/Cycad.csv") |>
  dplyr::select(sp, LA) |>
  mutate(site = "Garden_cycad")
#d3 <-  read.xlsx("data/Orchid.xlsx", sheetIndex = 1) |>
d3 <-  read.csv("data/Orchid.csv") |>
  dplyr::select(sp, LA) |>
  mutate(site = "Garden_orchid")
#d4 <-  read.xlsx("data/Dipterocarps_leaf area.xlsx", sheetIndex = 1) |>
d4 <-  read.csv("data/Dipterocarps_leaf area.csv") |>
  dplyr::select(sp, LA) |>
  mutate(site = "Garden_dipterocarps")

LA_dat4 <- bind_rows(d1, d2, d3, d4)

# all ========================================================================

LA_all <- bind_rows(LA_dat1, LA_dat2, LA_dat3, LA_dat4)

LA_all$site  |> unique()

LA_sp <- LA_all |>
  group_by(sp, site) |>
  summarize(LA = mean(LA, na.rm = TRUE))

table(LA_sp$site)

# Sp name ====================================================================

sp_check <-  read.csv("data/sp_correction.csv")
#sp_check <-  read.xlsx("data/sp_correction.xlsx", sheetIndex = 1)

sp_check2 <- full_join(LA_sp, sp_check, c("sp" = "Wrong_chinese_name", "site" = "location")) |>
  mutate(sp = ifelse(is.na(chinese_name), sp, chinese_name)) |>
  dplyr::select(-chinese_name)

#sp_list <-  read.xlsx("data/Species_list.xlsx", sheetIndex = 1) 
sp_list <-  read.csv("data/Species_list.csv") 
sp_list <- sp_list |> 
  dplyr::select(-starts_with("NA")) |>
  mutate(Location = ifelse(Location == "Yuanjiang_savanna", "Yuanjiang_savan", Location))  


sp_list$Species |> unique() |> length()
sp_list |> dim()
names(sp_list)

dat <- left_join(sp_check2, sp_list, by = c("sp" = "Chineses.name", "site" = "Location"))


message(paste0("created leaf area data"))
write_csv(dat, "data/test.csv")

## check ====================================================================
d <- read_csv("data/LMA_Mean.csv") |>
  mutate(Location = ifelse(Location == "Ailao_underground", "Ailao_understory", Location))
 
d2 <- d |>
  filter(!is.na(LMAdisc)) |>
  filter(!is.na(LMAleaf))

d2 <- d2 |>
  group_by(Species, 
           Family,
           `Growth form`,
           `Leaf habit`,
           Location,
           Biomes,
           Garden,
           MAT,
           MAP,
           `Data contributor`
           ) |>
  summarize(
            LT = mean(LT, na.rm = TRUE),
            LTdisc = mean(LTdisc, na.rm = TRUE),
            LMAdisc = mean(LMAdisc, na.rm = TRUE),
            LDMCdisc = mean(LDMCdisc, na.rm = TRUE),
            LDdisc = mean(LDdisc, na.rm = TRUE),
            LMAleaf = mean(LMAleaf, na.rm = TRUE),
            LDMCleaf = mean(LDMCleaf, na.rm = TRUE),
            LDleaf = mean(LDleaf, na.rm = TRUE)
  ) |>
  ungroup()


dat |>
  ungroup() |>
  dplyr::select(chinese_name = sp, Species = Synonym, Location = site, LA,
                Data.contributor) |>
 # right_join(d2, by = c("Species" = "Species", "Location" = "Location", 
  full_join(d2, by = c("Species" = "Species", "Location" = "Location", 
                        "Data.contributor" = "Data contributor")) |>
  filter(is.na(LA)) |>
  arrange(Location) |>
  dplyr::select(-chinese_name) |>
  write_csv("data/name_missing.csv")

dat |>
  ungroup() |>
  dplyr::select(chinese_name = sp, Species = Synonym, Location = site, LA,
                Data.contributor) |>
 # right_join(d2, by = c("Species" = "Species", "Location" = "Location", 
  full_join(d2, by = c("Species" = "Species", "Location" = "Location", 
                        "Data.contributor" = "Data contributor")) |>
  filter(is.na(LA)) |>
  arrange(Location) |>
#  dplyr::select(-chinese_name) |>
  filter(Location != "55km") |>
  filter(Location != "Changbai") |>
  #filter(str_detect(Species, "Millettia pachyloba")) |>
#  filter(str_detect(Species, "Lindera")) |>
 # filter(Species == "Nephelium chryseum") |>
  as.data.frame()

message(paste0("created name_missing"))


dat |>
  ungroup() |>
  dplyr::select(chinese_name = sp, Species = Synonym, Location = site, LA,
                Data.contributor) |>
  right_join(d2, by = c("Species" = "Species", "Location" = "Location", 
                        "Data.contributor" = "Data contributor")) |>
 # full_join(d2, by = c("Species" = "Species", "Location" = "Location", 
 #                       "Data.contributor" = "Data contributor")) |>
  filter(!is.na(LA)) |>
  filter(Species != "Litsea dilleniifolia") |> # not whole leaf
  filter(Species != "Capparis fohaiensis") |> # not whole leaf
  filter(Species != "Syzygium megacarpum") |> # not whole leaf
  filter(Species != "Elaeagnus pungens") |> # not whole leaf
  filter(Species != "Chamaedorea seifrizii") |> # not sure
  filter(Species != "Campylotropis delavayi") |> # not sure
  filter(Species != "Carissa spinarum") |> # not sure
  filter(Species != "Haldina cordifolia") |> # not sure
  filter(Species != "Sargentodoxa cuneata") |> # not sure
  arrange(Location) |>
  dplyr::select(-chinese_name) |>
  write_csv("data/LMA_LA.csv")

message(paste0("created LMA_LA.csv"))

#### check dup

hoge <- dat |>
  ungroup() |>
  dplyr::select(chinese_name = sp, Species = Synonym, Location = site, LA,
                Data.contributor) |>
  right_join(d2, by = c("Species" = "Species", "Location" = "Location", 
                        "Data.contributor" = "Data contributor")) |>
 # full_join(d2, by = c("Species" = "Species", "Location" = "Location", 
 #                       "Data.contributor" = "Data contributor")) |>
  filter(!is.na(LA)) |>
  filter(Species != "Litsea dilleniifolia") |>
  filter(Species != "Capparis fohaiensis") |>
  filter(Species != "Syzygium megacarpum") |>
  filter(Species != "Elaeagnus pungens") |>
  filter(Species != "Chamaedorea seifrizii") |> # not sure
  filter(Species != "Campylotropis delavayi") |> # not sure
  filter(Species != "Carissa spinarum") |> # not sure
  filter(Species != "Haldina cordifolia") |> # not sure
  filter(Species != "Sargentodoxa cuneata") |> # not sure
  arrange(Location) |>
  dplyr::select(-chinese_name)


tmp <- table(hoge$Species) 

dup_sp <- tmp[tmp > 1] |> names()

hoge |>
  filter(Species %in% dup_sp) |>
  as.data.frame() |>
  arrange(Species) |>
  write_csv("data/test.csv")

#### random check

dat |>
  filter(sp == "巴豆属B种")

d2 |>
  filter(Species == "Haldina cordifolia")
dat |>
  filter(Synonym == "Haldina cordifolia")

LA_sp |>
  filter(sp == "心叶木")
