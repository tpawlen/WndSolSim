# Supporting code to analyze simulations

Yr4Sp <- list(2022,2023,2024,2025)
Yr2Sp <- list(2020,2021)

# Abbreviations for Case Studies
BC <- "Base Case"
MS  <- "Minimum Solar Constraint"
LCT <- "Low Carbon Tax"
HS <- "Hypothetical Sites"
IR <- "Incr Heat Rate"
HREB <- "Heat Rate and Extreme Bid Factors"

# Set limits for plots to be consistent
ylimit <- max(Hr$Output_MWH) + max(ZoneHour$Imports)

# Set legend variables
################################################################################
colours = c("darkslateblue", "grey", "darkslategrey", "coral4", "goldenrod4", 
            "dodgerblue", "forestgreen", "gold", "darkolivegreen1", "cyan")
colours1 = c("darkslateblue", "black", "grey", "darkslategrey", "coral4", 
             "goldenrod4", "darkcyan", "dodgerblue", "forestgreen", "gold", 
             "cyan") #11 colours
colours2 = c("black", "grey", "darkslategrey", "coral4", "goldenrod4", 
             "dodgerblue", "darkcyan", "forestgreen", "gold", "cyan") #10 colours
colours3 = c("forestgreen", "gold", "darkslategrey", "goldenrod4", "cyan", 
             "dodgerblue")
colours4 = c("forestgreen","gold","cyan","dodgerblue","darkorange1","goldenrod4",
             "darkslategrey","darkslategrey") #8 colours
colours4a = c("forestgreen","gold","cyan","dodgerblue","goldenrod4",
              "darkslategrey","darkslategrey") #7 colours
colours5 = c("darkslateblue", "black", "grey", "darkslategrey", "coral4", 
             "goldenrod4", "darkorange1", "darkcyan", "dodgerblue", 
             "forestgreen", "gold", "cyan") #12 colours
colours5a = c("black", "grey", "darkslategrey", "coral4", 
              "goldenrod4", "darkorange1", "darkcyan", "dodgerblue", 
              "forestgreen", "gold", "cyan") #11 colours
colours5b = c("black", "grey", "darkslategrey", "coral4", 
              "goldenrod4", "darkcyan", "dodgerblue", 
              "forestgreen", "gold", "cyan") #10 colours
colours5c = c("darkslateblue", "black", "grey", "darkslategrey", "coral4", 
              "goldenrod4", "darkorange1","lightsalmon", "darkcyan", "dodgerblue", 
              "forestgreen", "gold", "cyan","blue") #13 colours
colours5d = c("black", "grey", "darkslategrey", "coral4", 
              "goldenrod4", "darkorange1","lightsalmon", "darkcyan", "dodgerblue", 
              "forestgreen", "gold", "cyan","blue") #12 colours
colours6 = c("black", "grey", "darkslategrey", "coral4", 
             "goldenrod4", "darkorange1","lightsalmon", "darkcyan", "dodgerblue", 
             "forestgreen", "gold", "cyan") #12 colours
colours6a = c("black", "grey", "darkslategrey", "coral4", 
              "goldenrod4", "darkcyan", "dodgerblue", 
              "forestgreen", "gold", "cyan") #10 colours

# Custom scale fills to tie colours to specific resources
################################################################################
scale_fill_output <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("darkslateblue","black", "grey", "darkslategrey", 
                        "coral4","goldenrod4","darkorange1", "lightsalmon", 
                        "firebrick1", "darkcyan", "dodgerblue", 
                        "chartreuse","forestgreen", "gold", "cyan"), 
                      c("IMPORT","COAL","NGCONV","NGCC","COGEN","SCGT",
                        "CC_BLEND","SC_BLEND","H2",
                        "HYDRO","OTHER","UR","WIND","SOLAR","STORAGE")), 
    #labels = setNames(c("IMPORT","COAL","NGCONV","NGCC","COGEN","SCGT",
    #                    "CC_BLEND","SC_BLEND","H2",
    #                    "HYDRO","OTHER","UR","WIND","SOLAR","STORAGE"),
    #                  c("Imports","Coal","Retrofitted Coal to NG","Combined Cycle NG",
    #                    "Cogeneration","Simple Cycle")),
    ...
  )
}

scale_fill_built <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("black", 
                        "darkslategrey", "darkslategrey", 
                        "coral4","goldenrod4", "goldenrod3",
                        "darkorange1", "lightsalmon", "firebrick1", 
                        "darkcyan", "dodgerblue", 
                        "chartreuse","forestgreen", "gold", "cyan"), 
                      c("COAL", 
                        "Gas", "Gas1", 
                        "Gas0","Gas2","Gas3",
                        "GasB_CC","GasB_SC","H2",
                        "WAT","OT","UR","WND","SUN","PS")), 
    ...
  )
}