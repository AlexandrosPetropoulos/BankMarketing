plot_characteristics_hist <- function(bank){
  #kalo einai na to kano na ektiponei ena mono
  #i na vazei oles tis eikones se ena fakelo
  
  p1 <- ggplot(bank, aes(x=bank$age)) + geom_histogram(binwidth=10,color="red", fill="red") + labs(title = "Histogram and density plot of features",x="age",caption = "(based on bank from Unimputed bank)")+geom_vline(aes(xintercept=mean(bank$age)),color="blue", linetype="dashed", size=1) +  geom_density(aes(y = ..density..*(41188*10)))
  #p1
  ggsave("age.png")
  
  p2 <- ggplot(bank, aes(x=bank$job)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="job",caption = "(based on bank from Unimputed bank)")
  #p2
  ggsave("job.png")
  
  p3 <- ggplot(bank, aes(x=bank$marital)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="marital",caption = "(based on bank from Unimputed bank)")
  #p3
  ggsave("marital.png")
  
  p4 <- ggplot(bank, aes(x=bank$education)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="education",caption = "(based on bank from Unimputed bank)")
  #p4
  ggsave("education.png")
  
  p5 <- ggplot(bank, aes(x=bank$default)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="default",caption = "(based on bank from Unimputed bank)")
  #p5
  ggsave("default.png")
  
  p6 <- ggplot(bank, aes(x=bank$housing)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="housing",caption = "(based on bank from Unimputed bank)")
  #p6
  ggsave("housing.png")
  
  p7 <- ggplot(bank, aes(x=bank$loan)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="loan",caption = "(based on bank from Unimputed bank)")
  #p7
  ggsave("loan.png")
  
  p8 <- ggplot(bank, aes(x=bank$contact)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="contact",caption = "(based on bank from Unimputed bank)")
  #p8
  ggsave("contact.png")
  
  p9 <- ggplot(bank, aes(x=bank$month)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="month",caption = "(based on bank from Unimputed bank)")
  #p9
  ggsave("month.png")
  
  p10 <- ggplot(bank, aes(x=bank$day_of_week)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="day_of_week",caption = "(based on bank from Unimputed bank)")
  #p10
  ggsave("day_of_week.png")
  
  p11 <- ggplot(bank, aes(x=bank$duration)) + geom_histogram(binwidth=10,color="red", fill="red") + labs(title = "Histogram and density plot of features",x="duration",caption = "(based on bank from Unimputed bank)")+geom_vline(aes(xintercept=mean(bank$duration)),color="blue", linetype="dashed", size=1) +  geom_density(aes(y = ..density..*(41188*10)))
  #p11
  ggsave("duration.png")
  
  p12 <- ggplot(bank, aes(x=bank$campaign)) + geom_histogram(binwidth=10,color="red", fill="red") + labs(title = "Histogram and density plot of features",x="campaign",caption = "(based on bank from Unimputed bank)")+geom_vline(aes(xintercept=mean(bank$campaign)),color="blue", linetype="dashed", size=1) +  geom_density(aes(y = ..density..*(41188*10))) 
  #p12
  ggsave("campaign.png")
  
  p13 <- ggplot(bank, aes(x=bank$pdays)) + geom_histogram(binwidth=10,color="red", fill="red") + labs(title = "Histogram and density plot of features",x="pdays",caption = "(based on bank from Unimputed bank)")+geom_vline(aes(xintercept=mean(bank$pdays)),color="blue", linetype="dashed", size=1) +  geom_density(aes(y = ..density..*(41188*10))) 
  #p13
  ggsave("pdays.png")
  
  p14 <- ggplot(bank, aes(x=bank$previous)) + geom_histogram(binwidth=10,color="red", fill="red") + labs(title = "Histogram and density plot of features",x="previous",caption = "(based on bank from Unimputed bank)")+geom_vline(aes(xintercept=mean(bank$previous)),color="blue", linetype="dashed", size=1) +  geom_density(aes(y = ..density..*(41188*10))) 
  #p14
  ggsave("previous.png")
  
  p15 <- ggplot(bank, aes(x=bank$poutcome)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="poutcome",caption = "(based on bank from Unimputed bank)")
  #p15
  ggsave("poutcome.png")
  
  p16 <- ggplot(bank, aes(x=bank$emp.var.rate)) + geom_histogram(binwidth=10,color="red", fill="red") + labs(title = "Histogram and density plot of features",x="emp.var.rate",caption = "(based on bank from Unimputed bank)")+geom_vline(aes(xintercept=mean(bank$emp.var.rate)),color="blue", linetype="dashed", size=1) +  geom_density(aes(y = ..density..*(41188*10))) 
  #p16
  ggsave("emp.var.rate.png")
  
  p17 <- ggplot(bank, aes(x=bank$cons.price.idx)) + geom_histogram(binwidth=10,color="red", fill="red") + labs(title = "Histogram and density plot of features",x="cons.price.idx",caption = "(based on bank from Unimputed bank)")+geom_vline(aes(xintercept=mean(bank$cons.price.idx)),color="blue", linetype="dashed", size=1) +  geom_density(aes(y = ..density..*(41188*10)))
  #p17
  ggsave("cons.price.idx.png")
  
  p18 <- ggplot(bank, aes(x=bank$cons.conf.idx)) + geom_histogram(binwidth=10,color="red", fill="red") + labs(title = "Histogram and density plot of features",x="cons.conf.idx",caption = "(based on bank from Unimputed bank)")+geom_vline(aes(xintercept=mean(bank$cons.conf.idx)),color="blue", linetype="dashed", size=1) +  geom_density(aes(y = ..density..*(41188*10)))
  #p18
  ggsave("cons.conf.idx.png")
  
  p19 <- ggplot(bank, aes(x=bank$euribor3m)) + geom_histogram(binwidth=10,color="red", fill="red") + labs(title = "Histogram and density plot of features",x="euribor3m",caption = "(based on bank from Unimputed bank)")+geom_vline(aes(xintercept=mean(bank$euribor3m)),color="blue", linetype="dashed", size=1) +  geom_density(aes(y = ..density..*(41188*10)))
  #p19
  ggsave("euribor3m.png")
  
  p20 <- ggplot(bank, aes(x=bank$nr.employed)) + geom_histogram(binwidth=10,color="red", fill="red") + labs(title = "Histogram and density plot of features",x="nr.employed",caption = "(based on bank from Unimputed bank)")+geom_vline(aes(xintercept=mean(bank$nr.employed)),color="blue", linetype="dashed", size=1) +  geom_density(aes(y = ..density..*(41188*10)))
  #p20
  ggsave("nr.employed.png")
  
  p21 <- ggplot(bank, aes(x=bank$y)) + geom_bar(fill="lightgreen")+ labs(title = "Histogram and density plot of features",x="y",caption = "(based on bank from Unimputed bank)")
  #p21
  ggsave("y.png")
  
  #http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  #source('multiplot.R')
  #multiplot(p1, p2, p3, p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21, cols=2)
  }