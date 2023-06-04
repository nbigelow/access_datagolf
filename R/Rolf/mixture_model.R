library(Rolf)
library(dplyr)
library(rstan)
# read in data
my_list <- readRDS("C:/Users/npbig/OneDrive/Desktop/Rolf/my_list.RData")
df <- Rolf::list_to_df(my_list)
df<-subset(df,df$date > '2022-01-01')

# Scale total points
dates<-unique(df$date)
a<-list()
for(i in 1:length(dates)){
  a[[i]]<-subset(df,df$date==dates[i])
  a[[i]]$scaled_total <- scale(a[[i]]$total_pts)
}
df<-Rolf::list_to_df(a)

# Manippulate finishing
df$fin_text.x<-gsub('T','',df$fin_text.x)
df$fin_text.x<-gsub(' ','',df$fin_text.x)

# Split cut, wd, ngf, etc into one class
df$fin_text.x[df$fin_text.x=='CU' | df$fin_text.x=='WD' | df$fin_text.x=='W/D' | df$fin_text.x=='CU']<-'CUT'
df<-df[-which(is.na(df$total_pts[df$fin_text.x=='CUT'])),]
df$cut <- rep(0)
df$cut[df$fin_text.x=='CUT']<-1
df$name_match<-switch_names(df$player_name)

## Subset large df to only players in tourn
dk_salaries<-read.csv("C:/Users/npbig/Downloads/DKSalaries.csv")
df_tourn<-subset(df,df$name_match %in% dk_salaries$Name)

names <- unique(df_tourn$name_match)
fits <- list()
for(i in 1:length(unique(df_tourn$name_match))){
  tryCatch({
    df_sub<-subset(df,df$name_==names[i])
    ## Set prior as function of population
    model <- stan_model("C:/Users/npbig/OneDrive/Desktop/Rolf/mixture_fit.stan")
    fit<-sampling(model,
                  data = list(N=length(na.omit(df_tourn$total_pts)),
                              y=c(na.omit(df_tourn$scaled_total)),
                              mu_1=mean(na.omit(df$scaled_total[df$fin_text.x=='CUT'])),
                              sigma_1=sd(na.omit(df$scaled_total[df$fin_text.x=='CUT'])),
                              mu_2=mean(na.omit(df$scaled_total[df$fin_text.x!='CUT'])),
                              sigma_2=sd(na.omit(df$scaled_total[df$fin_text.x!='CUT'])),
                              theta_0 = mean(na.omit(df_tourn$fin_text.x=='CUT')),
                              lambda = c(df_tourn$cut)
                  ),
                  iter = 1000,
                  chains = 2)
    hist(extract(fit,'y_new')[[1]],xlim=c(-5,5),breaks='fd',main = names[i])
    fits[[names[i]]]$mixture_model  <- fit
    fits[[names[i]]]$pred <- extract(fit,'y_new')[[1]]
    print(paste((i/length(unique(df_tourn$name_match)))*100))
    print(paste((i/length(unique(df_tourn$name_match)))*100))
    print(paste((i/length(unique(df_tourn$name_match)))*100))
  },
  error = function(e){
    print(e)
  })
}

name <- pts <- c()
for(i in 1:length(unique(df_tourn$name_match))){
  name[i]<-names(fits[i])
  pts[i]<-sample(fits[[i]]$pred,1)
}
sim<-data.frame(name,pts)
sim <- sim %>% arrange(desc(pts))
head(sim,25)
