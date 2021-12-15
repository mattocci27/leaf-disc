library(tidyverse)

d0 <- read_csv("./data/full_data.csv")

d <- d0 |>
  rename(LT = LTleaf) |>
  rename(LA = LAleaf) |>
  filter(!is.na(Species)) |>
  filter(Species != "NA") |>
  filter(!is.na(LMAdisc)) |>
  filter(!is.na(LMAleaf)) |>
  filter(Location %in% c("Ailao_understory", "Mengla_Bubeng", "Yuanjiang_Savanna", "Yakushima"))

r2_LMA <- cor.test(log(d$LMAleaf), log(d$LMAdisc))$estimate^2 |> round(2)
r2_LD <- cor.test(log(d$LDleaf), log(d$LDdisc))$estimate^2 |> round(2)

tree <- read_csv("./data/tree_data.csv")

tree2 <- tree  |>
  filter(!is.na(LMAdisc)) |>
  filter(!is.na(LMAleaf)) |>
  rename(LT = LTleaf) |>
  rename(LA = LAleaf)

r2_LMA_ind <- cor.test(log(tree2$LMAleaf), log(tree2$LMAdisc))$estimate^2 |> round(2)
r2_LD_ind <- cor.test(log(tree2$LDleaf), log(tree2$LDdisc))$estimate^2 |> round(2)

d1 <- d |>
  filter(Garden == "NO") |>
  filter(Location != "Yakushima")

t1 <- table(d1$Biomes)
#t1["CTF"]

d2 <- d |>
  filter(Garden == "YES")

t2 <- table(d2$Growthform)

tmp <- d |>
  filter(!is.na(LMAdisc)) |>
  filter(!is.na(LMAleaf))

res <- cor.test(log(tmp$LT), log(tmp$LA))

#plot(LT ~ LA, tmp, log = "xy")

LA1 <- quantile(d$LA, 0.25, na.rm = TRUE) |> round(1)
LA2 <- quantile(d$LA, 0.5, na.rm = TRUE) |> round(1)
LA3 <- quantile(d$LA, 0.75, na.rm = TRUE) |> round(1)
LT1 <- quantile(d$LT, 0.25, na.rm = TRUE) |> round(2)
LT2 <- quantile(d$LT, 0.5, na.rm = TRUE) |> round(2)
LT3 <- quantile(d$LT, 0.75, na.rm = TRUE) |> round(2)
LA_mid <- median(d$LA, na.rm = TRUE) |> round(1)
LT_mid <- median(d$LT, na.rm = TRUE) |> round(2)
LA_mid2 <- median(tree2$LA, na.rm = TRUE) |> round(1)
LT_mid2 <- median(tree2$LT, na.rm = TRUE) |> round(2)

print(LA_mid)
print(LT_mid)
print(LA_mid2)
print(LT_mid2)
LA_LT_p <- res$p.value

LA_LT_p <- ifelse(LA_LT_p < 0.05, "< 0.05", LA_LT_p)


d_cv <- read_csv("./data/full_data_cv.csv") |>
  filter(!is.na(LMAdisc)) |>
  filter(!is.na(LMAleaf))

d3 <- read_csv("./data-raw/leaf_traits_for_Katabuchi.csv")

d3_2 <- d3 |>
  filter(!is.na(`LD LMA`)) |>
  filter(Growthform != "H")

pet <- d3_2  |>
  filter(!is.na(`petiole DW`))

d3_2$Species |> unique() |> length()
pet$Species |> unique() |> length()

#cor(sqrt(d_cv$LMAleaf), sqrt(d_cv$LMAdisc))^2 |> round(2)

#  values
#output <- "../values.yml"
output <- "values.yml"
out <- file(paste(output), "w") # write

writeLines(paste0("N1: ", nrow(d1)),
           out,
           sep = "\n")
writeLines(paste0("sp1: ", d1$Species |> unique() |> length()),
           out,
           sep = "\n")
writeLines(paste0("CTF: ", t1["CTF"]),
           out,
           sep = "\n")
writeLines(paste0("HDS: ", t1["HDS"]),
           out,
           sep = "\n")
writeLines(paste0("ST2: ", t1["ST2"]),
           out,
           sep = "\n")
writeLines(paste0("STF: ", t1["STF"]),
           out,
           sep = "\n")
writeLines(paste0("TRF: ", t1["TRF"]),
           out,
           sep = "\n")
writeLines(paste0("N2: ", nrow(d2)),
           out,
           sep = "\n")
writeLines(paste0("orchid: ", t2["Orchids"]),
           out,
           sep = "\n")
#writeLines(paste0("palm: ", t2["Palms"]),
#           out,
#           sep = "\n")
writeLines(paste0("tree: ", t2["Trees"]),
           out,
           sep = "\n")
writeLines(paste0("N3: ", nrow(d)),
           out,
           sep = "\n")
writeLines(paste0("sp3: ", d$Species |> unique() |> length()),
           out,
           sep = "\n")
writeLines(paste0("LA_LT_r: ",
           res$estimate %>% round(2)),
           out,
           sep = "\n")
writeLines(paste0("LA_LT_p: ",
           LA_LT_p),
           out,
           sep = "\n")
writeLines(paste0("LA_LT_n: ",
           nrow(tmp)),
           out,
           sep = "\n")
writeLines(paste0("LA1: ",
           LA1),
           out,
           sep = "\n")
writeLines(paste0("LA2: ",
           LA2),
           out,
           sep = "\n")
writeLines(paste0("LA3: ",
           LA3),
           out,
           sep = "\n")
writeLines(paste0("LT1: ",
           LT1),
           out,
           sep = "\n")
writeLines(paste0("LT2: ",
           LT2),
           out,
           sep = "\n")
writeLines(paste0("LT3: ",
           LT3),
           out,
           sep = "\n")
writeLines(paste0("LA_mid: ",
           LA_mid),
           out,
           sep = "\n")
writeLines(paste0("LT_mid: ",
           LT_mid),
           out,
           sep = "\n")
writeLines(paste0("LA_mid2: ",
           LA_mid2),
           out,
           sep = "\n")
writeLines(paste0("LT_mid2: ",
           LT_mid2),
           out,
           sep = "\n")
writeLines(paste0("r2_LMA: ",
           r2_LMA),
           out,
           sep = "\n")
writeLines(paste0("r2_LD: ",
           r2_LD),
           out,
           sep = "\n")
writeLines(paste0("r2_LMA_ind: ",
           r2_LMA_ind),
           out,
           sep = "\n")
writeLines(paste0("r2_LD_ind: ",
           r2_LD_ind),
           out,
           sep = "\n")
writeLines(paste0("tree1: " ,
           tree2 |>
            filter(Location != "Yakushima") |>
            nrow()),
           out,
           sep = "\n")
writeLines(paste0("yaku_sp: " ,
           d |>
            filter(Location == "Yakushima") |>
           pull(Species) |>
           unique() |>
           length()),
           out,
           sep = "\n")
writeLines(paste0("tree3: " ,
           tree2 |>
            filter(Location == "Yakushima") |>
            nrow()),
           out,
           sep = "\n")
writeLines(paste0("XTBG_sp: " ,
          d0 |>
            filter(Location %in% c("Garden_dipterocarps",
                                   "Garden_orchid")) |>
            nrow()),
           out,
           sep = "\n")
writeLines(paste0("all_tree: " ,
           tree2 |>
            nrow()),
           out,
           sep = "\n")
writeLines(paste0("all_sp: " ,
           d |>
           pull(Species) |>
           unique() |>
           length()),
           out,
           sep = "\n")
writeLines(paste0("cv_r2: " ,
           cor(sqrt(d_cv$LMAleaf), sqrt(d_cv$LMAdisc))^2 |> round(2)),
           out,
           sep = "\n")
writeLines(paste0("pet_sp: " ,
           pet$Species |> unique() |> length()),
           out,
           sep = "\n")
close(out)
