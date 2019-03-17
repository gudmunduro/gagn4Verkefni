UK_DD <- UKDriverDeaths

uk_dd_m <- matrix(UK_DD, 16, 12)
uk_dd_m
years <- c(1969:1984)
months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

rownames(uk_dd_m) <- years
colnames(uk_dd_m) <- months

above_2000 <- uk_dd_m > 2000

#above_2000 <- filter(above_2000, )

yi <- 1
mi <- 1

aa2000 <- c()

for (column in above_2000)
{
  if (yi >= length(years))
  {
    break
  }
  if (mi >= length(months))
  {
    mi <- 1
    yi <- yi + 1
  }
  
  if (above_2000[yi, mi] == TRUE)
  {
    aa2000 <- c(aa2000, paste(months[mi], years[yi]))
  }
  
  mi <- mi + 1
}
print("M�nu�ir,�r me� dau�sf�ll yfir 2000")
print(aa2000)

print("Me�altal dau�sfalla � m�nu�i yfir gefin �r:")
m_avg <- colSums(uk_dd_m) / length(years)
print(m_avg)

print("Me�altal dau�sfalla � gefnum �rum")
y_avg <- rowSums(uk_dd_m) / length(months)
print(y_avg)

pop_uk = c(55634935, 56211947, 56265475, 56466131)

for (year in years) {
  year_id <- ((round(year * 2, -1) / 2) - 1965) / 5
  year_pop <- switch(year_id,
                    pop_uk[1],
                    pop_uk[2],
                    pop_uk[3],
                    pop_uk[4])
  year_dd <- rowSums(uk_dd_m)[year - 1968]

  print(paste("Það lenntu", round(year_dd / year_pop * 100, 4), "prósent í alvarlegu umferðarslisi árið", year))
}


pop_is = c(204042, 216695, 226948, 240606)

for (year in years) {
  year_id <- ((round(year * 2, -1) / 2) - 1965) / 5
  year_dd <- rowSums(uk_dd_m)[year - 1968]

  is_dd <- year_dd / pop_uk[year_id] * pop_is[year_id]
  print(paste("Það lentu", uk_dd, "í alvarlegu umferðarslysi árið", year))
  print(paste("Það væru", is_dd, "á íslandi"))
}

