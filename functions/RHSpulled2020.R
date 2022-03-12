library(Hmisc)
binconf(x=46, n=50, method=c("asymptotic")) # character string specifing which method to use. The "all" method only works when x and n are length 1.

?binconf
CI <- binconf(x=df4$Serocon, n=df4$N, method=c("wilson"))
CI <- data.frame(CI)
MERGE <- cbind(df4, CI)

# convert to % from proportion
MERGE$Lower <- MERGE$Lower*100
MERGE$Upper <- MERGE$Upper *100

write.csv(MERGE, "MERGE.csv")

# add error bars

p <- ggplot(MERGE, aes(Group, MERGE$"% Serocon", fill = Group))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=MERGE$Lower, ymax=MERGE$Upper), width=.1)+
  ylab("% Seroconverted")+
  scale_fill_discrete(guide = F)+ theme_bw() +
  scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

p

ggsave("Serocon_barplot3.pdf", width = 15, height = 10, units ="cm")

# try dot plot

p <- ggplot(MERGE, aes(Group, MERGE$"% Serocon", colour = Group))+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin=MERGE$Lower, ymax=MERGE$Upper), width=.1)+
  ylab("% Seroconverted")+
  scale_y_continuous(limits=c(0, 100), breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
  scale_colour_discrete(guide = F)+ theme_bw() 
p