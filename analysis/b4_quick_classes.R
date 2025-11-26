cla <- read.csv("data/derived-data/OSORPG_classes_v0.csv")
head(cla)

# one group = one label
table(apply(table(cla$grp_id, cla$grp_label) > 0, 1, sum))

# most label from 1 to 39 but 10, 12, 13 and 27 missing
sort(unique(cla$grp_id)) == (1:39)[-c(10, 12, 13, 27)]

cla$nom[duplicated(cla[, c("nom", "source")])]

# remove dates to make simpler code for 16 fourrage
nodate <- gsub('[0-9]{4}', '', cla$nom)

# one id per label but not the specific yearly ones
dup <- cla$new_code %in% cla$new_code[duplicated(cla$new_code)]


for (i in unique(cla$new_code[dup])) {
  who <- cla$new_code == i
  # if duplicates
  if (sum(duplicated(nodate[who])) > 1) {
    nodup <- nodate[who][!duplicated(nodate[who])]
    newcode <- i + 1:length(nodup)
    cla$new_code[who] <- newcode[match(nodate[who], nodup)]
  } else {
    cla$new_code[who] <- i + 1:sum(who)
  }
}

write.csv(cla, "data/derived-data/OSORPG_classes_v1.csv", row.names = FALSE)
