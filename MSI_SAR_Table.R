# Summarize and prorate humpback whale MSI by stock for SARs
 setwd("c:/carretta/github/HCM_SI")
 load("SeriousInjuryReport.RData")
 
# proration factors based on summer-to-winter movement probabilities (Wade 2021)
 
 CA.OR.CenAm <- 0.423 # CV = 0.23
 WA.SBC.CenAm <- 0.059 # CV = 0.935
 
 CA.OR.MainMex <- 0.577 # CV = 0.169
 WA.SBC.MainMex <- 0.254 # CV = 0.278
 
 CA.OR <- grep("CA|OR", MN$STATE)
 WA.SBC <- grep("WA|SBC", MN$STATE)
 
 MN$CenAm.Prorated.MSI <- NA
 MN$MainMex.Prorated.MSI <- NA
 
  MN$CenAm.Prorated.MSI[CA.OR] <- CA.OR.CenAm * MN$MSI.Value[CA.OR]
  MN$CenAm.Prorated.MSI[WA.SBC] <- WA.SBC.CenAm * MN$MSI.Value[WA.SBC]
  
  MN$MainMex.Prorated.MSI[CA.OR] <- CA.OR.MainMex * MN$MSI.Value[CA.OR]
  MN$MainMex.Prorated.MSI[WA.SBC] <- WA.SBC.MainMex * MN$MSI.Value[WA.SBC]

# number of cases by Interaction.Type
  Interaction.Type <- sort(unique(MN$Interaction.Type))
  cases <- as.numeric(table(MN$Interaction.Type))
  MSI.by.Interaction <- tapply(MN$MSI.Value, MN$Interaction.Type, sum)
  
  MSI.CenAm.by.Interaction <- signif(tapply(MN$CenAm.Prorated.MSI, MN$Interaction.Type, sum),2)
  MSI.MainMex.by.Interaction <- signif(tapply(MN$MainMex.Prorated.MSI, MN$Interaction.Type, sum),2)
  
  MSI.CenAm.5yr.mean <- signif(MSI.CenAm.by.Interaction/5, 2)
  MSI.MainMex.5yr.mean <- signif(MSI.MainMex.by.Interaction/5, 2)
  
  SAR.df <- cbind.data.frame(Interaction.Type, cases, MSI.by.Interaction, MSI.CenAm.by.Interaction, MSI.CenAm.5yr.mean, MSI.MainMex.by.Interaction, MSI.MainMex.5yr.mean)
  
  write.csv(SAR.df, "Humpback MSI SAR Table.csv", row.names=FALSE)
  
  