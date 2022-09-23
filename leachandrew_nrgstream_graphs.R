# Generates functions used to plot AESO generation output using code and data 
# gathered by Dr. Andrew Leach
#
# Author: Taylor Pawlenchuk
# email: pawlench@ualberta.ca
# April 2022; Last revision: April 1, 2022

library(png)
#library(gghighlight)

# Load data
################################################################################
{
  setwd("D:/Documents/Education/Masters Degree/Datasets/Market")

load("nrgstream_gen.RData") ## which is here *equivalent* to

setwd("D:/Documents/GitHub/AuroraEval")

nrgstream_gen <- nrgstream_gen %>% rename(time=Time)

errors<-nrgstream_gen %>% filter(is.na(Price),date<Sys.Date())
gen_errors<-nrgstream_gen %>% filter(is.na(gen),date<Sys.Date())

#unique(gen_errors$AESO_Name)

nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 
}

{
#make vol-weighted avg

#sub_samp<-subset(nrgstream_gen, time > as.Date("2010-01-1"))
#sub_samp<-subset(sub_samp, time < as.Date("2017-12-31"))
#sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
#sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
#sub_samp<-na.omit(sub_samp)
#sub_samp$Year<-year(sub_samp$time)
#df1 <- sub_samp %>% 
#  group_by(Plant_Type,time,Year) %>% 
#  summarise(sumcap = sum(Capacity),total_gen=sum(gen),p_mean=mean(Price))
#df1$Year_ID=as.character(df1$Year)

#exclude WECCs so that you don't double-count
#trade_excl<-c("AB - WECC Imp Hr Avg MW", "AB - WECC Exp Hr Avg MW","AB - WECC Imp/Exp Hr Avg MW")

# Filter data for desired categories
################################################################################
  
sub_samp<-filter(nrgstream_gen, time >= as.Date("2004-01-1"))

demand <- nrgstream_gen %>%
  group_by(time) %>%
  summarise(Demand = median(Demand), 
            Price = median(Price),
            AIL = median(AIL))

trade_excl<-c("AB - WECC Imp Hr Avg MW", 
              "AB - WECC Exp Hr Avg MW",
              "AB - WECC Imp/Exp Hr Avg MW")  

df1 <- sub_samp %>% 
  filter(! NRG_Stream %in% trade_excl)%>% 
  group_by(Plant_Type,time) %>% 
  summarise(meancap = mean(Cap_Fac),
            total_gen=sum(gen,na.rm = T),
            total_rev=sum(Revenue,na.rm = T),
            p_mean=mean(Price)) %>% 
  ungroup()

df1$Day <- date(df1$time)
df1$Year <- as.factor(year(df1$time))

# Identify the Plant Types
################################################################################
gen_set<-c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND")

# Base data set 
################################################################################
{
df1a <- df1 %>%
  filter(Plant_Type %in% gen_set,year(time)<2022)

df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "OTHER",after=Inf)
df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "HYDRO",after=Inf)
df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "WIND",after=Inf)
df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "SOLAR",after=Inf)
#df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "TRADE",after=Inf)
df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "IMPORT",after=Inf)
df1a$Plant_Type<-fct_relevel(df1a$Plant_Type, "EXPORT",after=Inf)
}

# Summarize data set for each year
################################################################################
{
df2 <- df1 %>% 
  filter(Plant_Type %in% gen_set,year(time)<2022) %>%
  group_by(Plant_Type,Year) %>% 
  summarise(capture = sum(total_rev)/sum(total_gen),
            avg_rev = sum(total_rev)/sum(total_gen),
            p_mean=mean(p_mean, na.rm = TRUE))

df2$Plant_Type<-fct_relevel(df2$Plant_Type, "OTHER",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "HYDRO",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "WIND",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "SOLAR",after=Inf)
#df2$Plant_Type<-fct_relevel(df2$Plant_Type, "TRADE",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "IMPORT",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "EXPORT",after=Inf)
}
}

# Establish image theme
################################################################################
slide_theme<-function(){
  return( theme(panel.border = element_blank(),
                panel.grid = element_blank(),
                panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
                axis.line.x = element_line(color = "gray"),
                axis.line.y = element_line(color = "gray"),
                axis.text = element_text(size = 16),
                axis.text.x = element_text(margin = margin(t = 10)),
                axis.title = element_text(size = 16),
                #axis.label.x = element_text(size=20,vjust=+5),
                plot.subtitle = element_text(size = 12,hjust=0.5),
                plot.caption = element_text(face="italic",size = 12,hjust=0),
                legend.key.width=unit(2,"line"),
                legend.position = "bottom",
                #legend.direction = "horizontal",
                #legend.box = "horizontal",
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 16),
                plot.title = element_text(hjust=0.5,size = 20),
                plot.margin=unit(c(1,1,1.5,1.2),"cm")
  )
  )
}

# Filter data by date
################################################################################
df6 <- df2 %>%
  #  filter(Plant_Type != "MARKET") %>%
  filter(as.character(Year) >= 2014)

# Establish colour palette
################################################################################
my_palette1<-c("grey81","coral4","goldenrod4","tan","slategray1",
              "lightskyblue","green4","lightgoldenrod2","thistle3","rosybrown")

################################################################################
################################################################################
# Plots
################################################################################
################################################################################

################################################################################
# Plots capture price difference from market mean by plant type
################################################################################
plot_b <- ggplot(df6,
                 aes(Year,capture-p_mean,colour=Plant_Type,fill=Plant_Type),
                 alpha=0.5)+
  geom_col(aes(Year,capture-p_mean,colour=Plant_Type,fill=Plant_Type),
           size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_manual("",values=my_palette1)+
  scale_fill_manual("",values=my_palette1)+
  
  slide_theme()+
  labs(x="",y="Revenue Relative to \nMean Price ($/MWh)")+#,
#       title="Energy Price Capture Differential ($/MWh, 2014-2021)")+#,
#       caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent")) 


df3 <- df1 %>% filter(Plant_Type %in% gen_set,year(time)<2022) %>%
  group_by(Year) %>% summarise(capture = sum(total_rev)/sum(total_gen),avg_rev = sum(total_rev)/sum(total_gen),p_mean=mean(p_mean))%>%
  mutate(Plant_Type="MARKET",Plant_Type=as_factor(Plant_Type))

df2<-df2 %>% bind_rows(df3)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "MARKET",after=0)


df4<-tibble(Year=seq(2010,2016),Plant_Type="SOLAR",capture=0)%>%
  mutate(Year=as_factor(Year),Plant_Type=as_factor(Plant_Type))

df2<-df2 %>% bind_rows(df4)

df5 <- df2 %>%
#  filter(Plant_Type != "MARKET") %>%
  filter(as.character(Year) >= 2014)

mark <- df2 %>%
  filter(Plant_Type == "MARKET") %>%
  filter(as.character(Year) >= 2014)

#my_palette<-c("black",colors_tableau10()[8],colors_tableau10_medium()[4],colors_tableau10()[4],colors_tableau10_light()[4],colors_tableau10()[7],colors_tableau10()[1],colors_tableau10()[3],colors_tableau10()[2],colors_tableau10()[9],colors_tableau10_light()[9])
my_palette<-c("black", "grey81","coral4","goldenrod4","tan","slategray1",
              "lightskyblue","green4","lightgoldenrod2","thistle3","rosybrown")
#my_palette<-c("black",grey.colors(10,start=0.2,end = .95))

plot_a <- ggplot(df5,
               aes(Year,capture,colour=Plant_Type,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,capture,colour=Plant_Type,fill=Plant_Type),
           size=1,position = position_dodge(width = .9),width = .6)+
  geom_line(data= mark, group=1, aes(Year,capture)) +
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  slide_theme()+
  labs(x="",y="Average Revenue \n($/MWh)")+#,
#       title="Energy Price Capture ($/MWh, 2014-2021)")+#,
  #     caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "right",
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent")) 

plot_a

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

cp_plot2 <- ggarrange(ggarrange(plot_a + theme(legend.position = "none"),
                                  NULL,
                                 plot_b + theme(legend.position = "none"),
                                 nrow=3, heights = c(1,-0.2,1)),
                     g_legend(plot_a), 
                     ncol=2, widths=c(6,1))

cp_plot <- annotate_figure(cp_plot2, 
                           fig.lab = "Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach",
                           fig.lab.pos = "bottom.right", 
                           fig.lab.face = "italic", 
                           fig.lab.size = 10)

cp_plot

ggsave(path = "images", filename = "captureprice.png", bg = "transparent")


mrk <- df2 %>%
  filter(Plant_Type == "MARKET") %>%
  filter(as.character(Year) >= 2010)

wind <- df2 %>%
  filter(Plant_Type == "WIND") %>%
  filter(as.character(Year) >= 2010)

wndVSmrk <- rbind(mrk,wind) 

MRK <- mrk %>%
  mutate(Market = capture) %>%
  subset(., select = c(Year,Market))

WND <- wind %>%
  mutate(Wind = capture) %>%
  subset(., select = c(Year,Wind))

discount <- merge(MRK,WND,by="Year") %>%
  mutate(diff = (1-Wind/Market)*100)

plot_c <- ggplot(wndVSmrk,
                 aes(Year,capture,colour=Plant_Type),alpha=1)+
  geom_line(data= mrk, group=1, aes(Year,capture), lwd = 1.5) +
  geom_line(data=wind, group =1, aes(Year,capture), lwd = 1.5) +
  scale_color_manual("",values=c("black", "forestgreen"))+
  slide_theme()+
  labs(x="",y="Average Revenue \n($/MWh)",
#         title="Energy Price Capture ($/MWh, 2010-2021)",
       caption="Source: AESO Data, accessed via NRGStream") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "right",
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        text = element_text(size= 15))

plot_c

ggsave(path = "images", filename = "wind_vs_market.png", bg = "transparent")



################################################################################
################################################################################
# Function to plot actual AESO Output data
################################################################################
################################################################################

Week_act <- function(year,month,day) {
  
  colours = c("darkslateblue", "grey", "darkslategrey", "coral4", "goldenrod4", 
              "darkcyan", "dodgerblue", "forestgreen", "gold", "cyan")
  
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+7,month,year, sep = "/"), format="%d/%m/%Y")
  
  # Select only a single week
  ##############################################################################
  WK <- df1a %>%
    filter(Plant_Type != "EXPORT" & Plant_Type != "IMPORT") %>%
    filter(time >= wk_st & time <= wk_end)
  
  WKIM <- df1a %>%
    filter(Plant_Type == "IMPORT") %>%
    filter(time >= wk_st & time <= wk_end)
  
  WKIM$total_gen <- WKIM$total_gen * -1
  
  WK <- rbind(WKIM, WK)
  
  {
  WK$Plant_Type<-fct_relevel(WK$Plant_Type, "IMPORT", after = Inf)
  WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COAL", after = Inf)
  WK$Plant_Type<-fct_relevel(WK$Plant_Type, "NGCC", after = Inf)
  WK$Plant_Type<-fct_relevel(WK$Plant_Type, "COGEN", after = Inf)
  WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SCGT", after = Inf)
  WK$Plant_Type<-fct_relevel(WK$Plant_Type, "HYDRO", after = Inf)
  WK$Plant_Type<-fct_relevel(WK$Plant_Type, "OTHER", after = Inf)
  WK$Plant_Type<-fct_relevel(WK$Plant_Type, "WIND", after = Inf)
  WK$Plant_Type<-fct_relevel(WK$Plant_Type, "SOLAR", after = Inf)
    }
  
  WK$Plant_Type <- factor(WK$Plant_Type, levels=c("IMPORT", "COAL", "NGCC", 
                                                  "COGEN", "SCGT", "HYDRO", 
                                                  "OTHER", "WIND", "SOLAR"))
  
  levels(WK$Plant_Type) <- c("Import","Coal", "NGCC", "Cogen", "SCGT", "Hydro", 
                             "Other", "Wind", "Solar")
  
  dmd <- demand %>%
    filter(time >= wk_st & time <= wk_end)
  
  MX <- plyr::round_any(rng[2], 100, f = ceiling)
  MN <- plyr::round_any(rng[1], 100, f = floor)
  
  # Plot the data    
  ##############################################################################
  ggplot() +
    geom_area(data = WK, aes(x = time, y = total_gen, fill = Plant_Type), 
              alpha=0.6, size=.5, colour="black") +
    
    # Add hourly load line
    geom_line(data = dmd, 
              aes(x = time, y = Demand), size=2, colour = "black") +
    
    scale_x_datetime(expand=c(0,0)) +
    
    # Set the theme for the plot
    ############################################################################
  theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "right",
    ) +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          text = element_text(size= 15)
    ) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x = "Date", y = "Output (MWh)", fill = "AESO Data: \nResource") +
    scale_fill_manual(values = colours)
}

################################################################################
################################################################################
# Plot the AESO pool price
################################################################################
################################################################################

wkPrice <- function(year,month,day) {
  
  wk_st <- as.POSIXct(paste(day,month,year, sep = "/"), format="%d/%m/%Y")
  wk_end <- as.POSIXct(paste(day+14,month,year, sep = "/"), format="%d/%m/%Y")
  
  price_WK <- demand %>%
    filter(time >= wk_st & time <= wk_end)

  ggplot() +
    geom_line(data = price_WK, 
              aes(x=time, y=Price), 
              size = 1.5, color="red") +
    scale_x_datetime(expand=c(0,0)) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size= 15)
    ) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x = "Date", y = "Pool Price \n($/MWh)")
}

################################################################################
################################################################################
# Plot combinations of plots
################################################################################
################################################################################

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

AESO_PrOt <- function(year,month,day) {
  plot_grid(wkPrice(year,month,day) + 
              theme(axis.title.x=element_blank(),axis.text.x=element_blank()),
            Week_act(year,month,day)+theme(legend.position ="none"), 
            ncol = 1, align="v", axis = "l",rel_heights = c(1,2.5))
}

AESO_Sim <- function(year,month,day,case) {
  SimP <- week_price(year,month,day,case)
  ActP <- wkPrice(year,month,day)
  SimO <- Week1(year,month,day,case)
  ActO <- Week_act(year,month,day)

  MXP <- plyr::round_any(
    max(layer_scales(SimP)$y$range$range,layer_scales(ActP)$y$range$range),
    10, f = ceiling)
  MNP <- plyr::round_any(
    min(layer_scales(SimP)$y$range$range,layer_scales(ActP)$y$range$range),
    10, f = floor)  
  MXO <- plyr::round_any(
    max(layer_scales(SimO)$y$range$range,layer_scales(ActO)$y$range$range),
    100, f = ceiling)
  MNO <- plyr::round_any(
    min(layer_scales(SimO)$y$range$range,layer_scales(ActO)$y$range$range),
    100, f = floor)

  ggarrange(arrangeGrob(plot_grid(week_price(year,month,day,case) + 
                                    theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank()) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                                                       breaks = seq(MNP, MXP, by = MXP/4)),
                                  Week1(year,month,day,case)+
                                    theme(legend.position ="none") + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                                                       breaks = seq(MNO, MXO, by = MXO/4)), 
                                  ncol = 1, align="v", axis = "l",
                                  rel_heights = c(1,2.5))+
                          ggtitle(paste("Simulated Data for ",year," (",DB,")", sep = ""))+
                          theme(legend.position ="none",
                                plot.title = element_text(hjust = 0.5)),
                        
                        plot_grid(wkPrice(year,month,day) + 
                                    theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank()) + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNP,MXP), 
                                                       breaks = seq(MNP, MXP, by = MXP/4)),
                                  Week_act(year,month,day)+
                                    theme(legend.position ="none") + 
                                    scale_y_continuous(expand=c(0,0), limits = c(MNO,MXO), 
                                                       breaks = seq(MNO, MXO, by = MXO/4)), 
                                  ncol = 1, align="v", axis = "l",
                                  rel_heights = c(1,2.5)) +
                          ggtitle(paste("AESO Data for ",year,sep=""))+
                          theme(legend.position ="none",
                                plot.title = element_text(hjust = 0.5)),
                        ncol=2),
            
            arrangeGrob(g_legend(Week1(year,month,day,case)),
                        g_legend(Week_act(year,month,day)),
                        nrow=2),
            ncol=2, widths = c(5,1))
}

ggsave(path = "images", filename = "simvsact.png", bg = "transparent")
