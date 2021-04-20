# obtain Pandemic historical records
pnds <- pandemics.data(tgt="pandemics")

# obtain Pandemics vaccine development times
pnds.vacs <- pandemics.data(tgt="pandemics_vaccines")

###################

# pandemic's dates
date.pnd <- as.numeric(substr(pnds$Time.period,1,4))

# time between pandemics
pnd.interval <- y <- (diff(date.pnd))


z <- date.pnd[2:length(date.pnd)]

# remove possible NAs
filter <- !is.na(pnd.interval) & !is.na(z)

w <- pnds$Death.toll[filter]	#!is.na(pnd.interval) & !is.na(z)]
#w <- as.integer((w/abs(max(w)-min(w)))*1)

# generate dataframe with date, delta-time, death toll, pandemic's name and R0
pnd.df <- cbind(as.data.frame(na.omit(cbind(Year=date.pnd,delta=pnd.interval,z=z,Death.toll=w))) ,
		Name=pnds$Name.of.Pandemic[filter], R0=pnds$R0[filter])

# R0s
Rnaught <- as.factor(pnd.df$R0)

#################

# sort by year
pnd.df <- pnds[order(pnds$Year),] 
pnd.df <- cbind(pnd.df,delta=c(NA,diff(pnd.df$Year)))

# get the latest number of deaths for covid19
x <- covid19.analytics::covid19.data()
pnd.df$Death.toll[pnd.df$Name=="COVID-19"] <- sum(x$Deaths)


##################

# Visualizations


## using ggplot
library(ggplot2)
library(ggrepel)

# limits for x-axis
min.yr <- 500
max.yr <- 2021

# basic plot
plt <- ggplot(pnd.df, aes(x=Year,y=delta)) + geom_line() + geom_smooth() + xlim(min.yr,max.yr)
ggsave("pandemics.pdf",plt)

# tilt text 45 degrees
plt0 <-	plt + scale_x_continuous(breaks = pnd.df$Year) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		xlim(min.yr,max.yr)
ggsave("pandemics0.pdf",plt0)

# Add details about R0 and deaths.toll to plot
plt1 <-	plt0 + geom_point(size=as.integer(2.5*pnd.df$R0), aes(colour=(pnd.df$Death.toll),shape=as.factor(pnd.df$R0)), alpha=0.85) +
	#geom_text(aes(label=Name),hjust=0, vjust=0) +
	labs(y="Time between pandemics") +
	labs(shape="R0", colour="R0s") +
	geom_label_repel(aes(label = paste(Name.of.Pandemic,'\n',Death.toll)),
		data = subset(pnd.df, Death.toll > 1.0e+6),
		size = 2,
		box.padding   = 0.35, fill="grey80", #alpha=.85, 
		point.padding = 0.5,
		segment.color = 'grey50') +
	scale_color_gradient2(name="#Deaths", low = "green", mid = "blue",high = "red",
		midpoint = (mean(pnd.df$Death.toll,na.rm=T)) , guide = guide_legend(direction = "horizontal") ) +
	theme(legend.position = "top") +
	#+ scale_x_continuous(trans='log2')
	xlim(min.yr,max.yr)

# log scales
plt.log <- plt1 + scale_y_continuous(trans='log2') #+ xlim(min.yr,max.yr)
plt.loglog <- plt.log + scale_x_continuous(trans='log2') #+ xlim(min.yr,max.yr)

# save plots
ggsave('pandemics_semilog.pdf',plt.log)
ggsave('pandemics_loglog.pdf',plt.loglog)
ggsave("pandemics1.pdf",plt1)

# display on screen
print(plt0)
dev.new()
print(plt1)
dev.new()
print(plt.log)
#dev.new()
#print(plt.loglog)


#####

## using R basics plottings capabilities
pnds.df <- data.frame(na.omit(cbind(y,z)))
print(pnds.df)
p5 <- poly(pnds.df$z,degree=6)
lm.p5 <- lm(y ~ p5, data=pnds.df)
yp5 <- predict(lm.p5, as.data.frame(seq(min(pnds.df$z),max(pnds.df$z),100)))

dev.new()
plot(z,y)
lines(pnds.df$z,yp5, lwd=3)
