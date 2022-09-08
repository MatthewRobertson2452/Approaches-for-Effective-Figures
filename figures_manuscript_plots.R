

library(ggplot2)

####
##FIGURE 1 ABUNDANCE-OCCUPANCY
####

#generate random occupancy between 0 and 1
x<-seq(from=0.01, to=1, by=0.01)

#generate mean relationship for abundance following saturating function
meany<-6.5*x/(0.3^1+x^1)

#generate slightly modified relationships for species specific curves
diff_y<-7*x/(0.15^1+x^1)

diff_y2<-2+2*x

#put all data into one dataframe
ao_df<-data.frame(x=x, y=meany, diffy=diff_y, diffy2=diff_y2)

#plot mean relationship
p1<-ggplot(ao_df, aes(x=x, y=y))+
  geom_line(size=2)+
  xlab("Proportional Occupancy")+
  ylab("Log Local Abundance")+
  theme_grey(base_size = 12)+
  coord_cartesian(xlim=c(0,1),ylim=c(0,7),clip = 'off')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.margin=unit(c(3,1,1,1),"cm"))+
  annotate(geom="text",x=0.5, y=7.6, label="Is there a relationship between abundance and occupancy?", size=4)+
  annotate(geom="text",x=0.5, y=8, label="System 1", size=5)+
annotate(geom="text",x=0, y=8.1, label="a)", size=5)

#put all data into a dataframe for the lines in panel 2
ao_df_lines<-data.frame(x=rep(x,3), y=c(meany,diff_y,diff_y2), spp=rep(c("mean","bird","fish"), each=length(x)),
                        lwd=rep(c(2,1,1), each=length(x)))

#generate data for points around the lines
#random values between 0 and 1
newx<-rbeta(10, 2, 5)

#same relationships as before but with new x data
diff_y<-7*newx/(0.15^1+newx^1)+rnorm(10,0,0.3)

diff_y2<-2+2*newx+rnorm(10,0,0.3)

#setseed for sd bars since some random values are huge
set.seed(101)
#combine all point data to use in panel 2
ao_df_complex<-data.frame(x=newx, diffy=diff_y, diffy2=diff_y2, sdbirdy=rbeta(10,5,2), sdbirdx=rbeta(10,0.8,10),
                          sdfishy=rbeta(10,2,5), sdfishx=rbeta(10,0.3,10))

p2<-ggplot()+
  geom_line(data=ao_df_lines, aes(x=x, y=y, group=spp, color=spp, size=spp, linetype=spp))+
  scale_size_manual(values=c(1,1,2))+
  scale_color_manual(values=c('#006AAC','#EE6200','darkgrey'))+
  scale_linetype_manual(values=c("dashed","dashed","solid"))+
  geom_pointrange(data=ao_df_complex,aes(ymin=diffy-sdbirdy*newx, ymax=diffy+sdbirdy*newx,
                                         x=newx, y=diffy),col="#006AAC")+
  geom_pointrange(data=ao_df_complex, aes(xmin=newx-sdbirdx*newx, xmax=newx+sdbirdx*newx,
                                         x=newx, y=diffy),col="#006AAC")+
  geom_pointrange(data=ao_df_complex,aes(ymin=diffy2-sdfishy*newx, ymax=diffy2+sdfishy*newx,
                                         x=newx, y=diffy2),col="#EE6200")+
  geom_pointrange(data=ao_df_complex, aes(xmin=newx-sdfishx*newx, xmax=newx+sdfishx*newx,
                                          x=newx, y=diffy2),col="#EE6200")+
  xlab("Proportional Occupancy")+
  ylab("Log Local Abundance")+
  theme_grey(base_size = 12)+
  coord_cartesian(xlim=c(0,1),ylim=c(0,7),clip = 'off') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none")+
  theme(plot.margin=unit(c(3,2,1,0),"cm"))+
  annotate(geom="text",x=1.06, y=6.17, label="Bird", size=4,col="#006AAC")+
  annotate(geom="text",x=1.07, y=5, label="Mean", size=4,col="darkgrey")+
  annotate(geom="text",x=1.06, y=4, label="Fish", size=4,col="#EE6200")+
  annotate(geom="text",x=0.5, y=7.5, label="Does that relationship vary by species?", size=4)+
  annotate(geom="text",x=0.5, y=8, label="System 2", size=5)+
  annotate(geom="text",x=0, y=8.1, label="b)", size=5)


jpeg("AO_plot_thi_grey_mean.jpeg", units="in", res=400, width=10, height=5)
gridExtra::grid.arrange(p1, p2, nrow = 1)
dev.off()

pdf("AO_plot.pdf", width=10, height=5)
gridExtra::grid.arrange(p1, p2, nrow = 1)
dev.off()

####
##FIGURE 2 Logistic Growth Example

#x values from 1 to 50
x<-seq(from=1, to=50, by=1)

#use those x for a logistic growth curve with random variability
y<-95/(1+exp(-0.25*(x-25)))+rnorm(50,0,3)

# make sure the noise doesnt add values below 0
y<-ifelse(y<0, rnorm(1,5,1), y)

#put into data frame and change colors of points after a certain timestep
msy_df<-data.frame(effort=x, yield=y, overfished=ifelse(x>25, "red", "black"))


p1<-ggplot(msy_df, aes(x=effort, y=yield))+
  geom_point(size=1)+xlab("Time")+
  ylab("Population size")+
  theme_grey(base_size = 12)+
  xlim(0,50)+ylim(0,105)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  annotate(geom="text",x=25, y=105, label="Read data", size=5)+
  annotate(geom="text",x=2, y=105, label="a)", size=5)

p2<-ggplot(msy_df, aes(x=effort, y=yield))+
  stat_function(fun=function(x) 95/(1+exp(-0.25*(x-25))), size=1.5, linetype="dashed")+
  geom_point(colour="grey",size=1)+xlab("Time")+
  ylab("Population size")+
  theme_grey(base_size = 12)+
  xlim(0,50)+ylim(0,105)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"),
        axis.text.x = element_text(colour = "grey"), axis.text.y = element_text(colour = "grey"), axis.ticks=element_line(colour="grey"),
        axis.title = element_text(colour="grey"))+
  annotate(geom="text",x=29, y=105, label="Read between data", size=5)+
  annotate(geom="text",x=2, y=105, label="b)", size=5)

p3<-ggplot(msy_df, aes(x=effort, y=yield, colour=overfished))+
  stat_function(fun=function(x) 95/(1+exp(-0.25*(x-25))), size=1.5, linetype="dashed", colour="grey")+
  geom_segment(aes(x=0, xend=25, y=50, yend=50), colour="#EE6200", size=1.5, linetype="dotted")+
  geom_segment(aes(x=25, xend=25, y=0, yend=50), colour="#EE6200", size=1.5, linetype="dotted")+
  geom_point(size=1)+scale_colour_manual(values=c("black","#EE6200"))+
  scale_colour_manual(name="Effort", labels=c("<50", ">50"), values=c("black","#EE6200"))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  xlab("Time")+
  ylab("Population size")+
  theme_grey(base_size = 12)+
  xlim(0,50)+ylim(0,105)+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.9, 0.8))+
  #theme(legend.background = element_rect(fill="white",
                                           #size=0.5, linetype="solid", 
                                           #colour ="black"))+
  #theme(legend.key=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"),
        axis.text.x = element_text(colour = "grey"), axis.text.y = element_text(colour = "grey"), axis.ticks=element_line(colour="grey"),
        axis.title = element_text(colour="grey"))+
  #annotate(geom="text",x=25, y=55, label="MSY", size=4, colour="#d95f02")+
  annotate(geom="text",x=27, y=105, label="Read beyond data", size=5)+
  annotate(geom="text",x=2, y=105, label="c)", size=5)

jpeg("log_growth_plot_9_08.jpeg", units="in", res=400, width=8, height=3)
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
dev.off()

pdf("log_growth_plot_9_08.pdf", height=3, width=8)
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
dev.off()
