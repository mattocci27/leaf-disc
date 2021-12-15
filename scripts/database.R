library(tidyverse)
library(RSQLite)

if (file.exists("./data/try.sqlite3")) system("rm ./data/try.sqlite3")
db <- dbConnect(SQLite(), "./data/try.sqlite3", synchronous="off")

d <- read_delim("./data/TRY_DATA.txt", "\t")

db %>% dbWriteTable("try", d)

data_list <- dbListTables(db)

print(paste("Created:", data_list))
