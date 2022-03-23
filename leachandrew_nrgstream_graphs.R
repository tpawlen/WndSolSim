library(png)


load("nrgstream_gen.RData") ## which is here *equivalent* to

nrgstream_gen <- nrgstream_gen %>% rename(time=Time)

errors<-nrgstream_gen %>% filter(is.na(Price),date<Sys.Date())
gen_errors<-nrgstream_gen %>% filter(is.na(gen),date<Sys.Date())

#unique(gen_errors$AESO_Name)

nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$gen),] 
nrgstream_gen<-nrgstream_gen[!is.na(nrgstream_gen$time),] 

#make vol-weighted avg

sub_samp<-subset(nrgstream_gen, time > as.Date("2010-01-1"))
#sub_samp<-subset(sub_samp, time < as.Date("2017-12-31"))
sub_samp<-subset(sub_samp, sub_samp$`Plant_Fuel`=="WIND")
sub_samp<-subset(sub_samp, sub_samp$Capacity>0)
sub_samp<-na.omit(sub_samp)
sub_samp$Year<-year(sub_samp$time)
df1 <- sub_samp %>% 
  group_by(Plant_Type,time,Year) %>% 
  summarise(sumcap = sum(Capacity),total_gen=sum(gen),p_mean=mean(Price))
df1$Year_ID=as.character(df1$Year)

#exclude WECCs so that you don't double-count
trade_excl<-c("AB - WECC Imp Hr Avg MW", "AB - WECC Exp Hr Avg MW","AB - WECC Imp/Exp Hr Avg MW")

sub_samp<-filter(nrgstream_gen, time >= as.Date("2010-01-1"))

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

gen_set<-c("COAL","COGEN","HYDRO","NGCC", "OTHER", "SCGT","SOLAR","IMPORT","EXPORT","WIND")

df2 <- df1 %>% 
  filter(Plant_Type %in% gen_set,year(time)<2022) %>%
  group_by(Plant_Type,Year) %>% 
  summarise(capture = sum(total_rev)/sum(total_gen),
            avg_rev = sum(total_rev)/sum(total_gen),
            p_mean=mean(p_mean))

df2$Plant_Type<-fct_relevel(df2$Plant_Type, "OTHER",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "HYDRO",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "WIND",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "SOLAR",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "TRADE",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "IMPORT",after=Inf)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "EXPORT",after=Inf)

df2 <-df2 %>% 
  mutate(ei=deemed_ei(Plant_Type,as.character(Year)), 
         oba=oba_type(Plant_Type,as.character(Year)), 
         ctax=ctax_year(as.character(Year)),
         ctax_net=(deemed_ei(Plant_Type,as.character(Year))-
                     oba_type(Plant_Type,as.character(Year)))*
           ctax_year(as.character(Year)),
                     ctax_net_rev=avg_rev-ctax_net,
                     policy=ifelse(as.character(Year)>="2018","CCIR","SGER"))

###set_png(file="images/price_capture_avg.png", width = 1400, height = 750)
my_palette<-c(colors_tableau10()[8],
              colors_tableau10_medium()[4],
              colors_tableau10()[4],
              colors_tableau10_light()[4],
              colors_tableau10()[7],
              colors_tableau10()[1],
              colors_tableau10()[3],
              colors_tableau10()[2],
              colors_tableau10()[9],
              colors_tableau10_light()[9])

ggplot(df2,aes(Year,capture-p_mean,colour=Plant_Type,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,capture-p_mean,colour=Plant_Type,fill=Plant_Type),
           size=1.5,position = position_dodge(width = .9),width = .6)+
  #scale_color_viridis("Plant Type",discrete=TRUE)+
  #scale_fill_viridis("Plant Type",discrete=TRUE)+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  
  slide_theme()+
  labs(x="",y="Revenue Relative to Mean Price ($/MWh)",
       title="Energy Price Capture Differential ($/MWh, 2010-2020)",
       caption="Source: AESO Data, accessed via NRGStream\nGraph by @andrew_leach")

df3 <- df1 %>% filter(Plant_Type %in% gen_set,year(time)<2022) %>%
  group_by(Year) %>% summarise(capture = sum(total_rev)/sum(total_gen),avg_rev = sum(total_rev)/sum(total_gen),p_mean=mean(p_mean))%>%
  mutate(Plant_Type="MARKET",Plant_Type=as_factor(Plant_Type))

df2<-df2 %>% bind_rows(df3)
df2$Plant_Type<-fct_relevel(df2$Plant_Type, "MARKET",after=0)


df4<-tibble(Year=seq(2010,2016),Plant_Type="SOLAR",capture=0)%>%mutate(Year=as_factor(Year),
                                                                       Plant_Type=as_factor(Plant_Type))
df2<-df2 %>% bind_rows(df4)


#my_palette<-c("black",colors_tableau10()[8],colors_tableau10_medium()[4],colors_tableau10()[4],colors_tableau10_light()[4],colors_tableau10()[7],colors_tableau10()[1],colors_tableau10()[3],colors_tableau10()[2],colors_tableau10()[9],colors_tableau10_light()[9])
my_palette<-c("black","grey81","coral4","goldenrod4","tan","slategray1",
              "dodgerblue","forestgreen","gold","indianred1","darkkhaki")
#my_palette<-c("black",grey.colors(10,start=0.2,end = .95))

plot_a <- ggplot(filter(df2,as.character(Year)>=2014),
               aes(Year,capture,colour=Plant_Type,fill=Plant_Type),alpha=0.5)+
  geom_col(aes(Year,capture,colour=Plant_Type,fill=Plant_Type),
           size=0.8,position = position_dodge(width = .9),width = .6, alpha=0.9)+
  #  scale_color_viridis("Plant Type",discrete=TRUE)+
  #  scale_fill_viridis("Plant Type",discrete=TRUE)+
  #scale_color_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10())+
  scale_color_manual("",values=my_palette)+
  scale_fill_manual("",values=my_palette)+
  slide_theme()+
  labs(x="Year",y="Average Revenue ($/MWh)",
       title="Energy Price Capture ($/MWh, 2014-2021)")+
  theme(panel.background = element_rect(fill = "transparent"),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
#        axis.title = element_blank(),
#        axis.text = element_blank(),
#        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "transparent"),
#        legend.box = NULL,
        legend.box.background = element_rect(fill = "transparent", color = "transparent")) 

plot_a

ggsave(path = "images", filename = "windfarmlocations.png", bg = "transparent")
