df=read.csv("D:/Old_Data/math/Data science toseeh/Files/googleplaystore3-1.csv",header=TRUE, stringsAsFactors = F)


#b)

summary(df)
str(df)


#c)

#There are 1473 and 200 missing values for two variables Rating and Reviews, respectively. 

#d)
df$Category=as.factor(df$Category)
levels(df$Category)
levels(df$Category)=seq(1,33)
levels(df$Category)

#e)

unique(df$Type)

# In unique (df$Type), we have "Free" "" "Paid". Hence, there are three unique values.
# yes, I think " " must be missing values. Because each App is free or paid.



#f)

df$Type[df$Type==" "]=NA
is.na(df$Type)



#g)
df$Type=as.factor(df$Type)
levels(df$Type)
summary(df)
#Type has two levels: free and paid and there are 400 missing values for this variable.


#h)
# I could not find any function to check whether last-update is Date or not.
# But it is not character and it is a facor. Hence, time difference does not mean.
is.character(df$Last.Updated)
str(df$Last.Updated)
df$Last.Updated=as.Date(df$Last.Updated, tryFormats = c("%m/%d/%Y","%Y/%d/%m","%Y-%d-%m","%m-%d-%Y","%d/%m/%Y","%Y/%m/%d","%Y-%m-%d","%d-%m-%Y"))



#i)

df$Updates=Sys.Date()-df$Last.Updated
df$Updates
summary(df)
str(df)
df$Updates=as.numeric(df$Updates)
summary(df)
df$Updates
str(df)



#j)

head(df$Installs,5)
is.character(df$Installs)

# I want to write a function that, for each member of df$installs, first removes "+" and ",".Then replaces it as a number.
#since df$installs is not character, spinst=strsplit(df$Installs[i],"") hsd error. 


split=function(x){
  for (i in 1:length(df$Installs)){
    spinst=strsplit(df$Installs[i],"")
    spinst=unlist(spinst)
    spinst=spinst[spinst!="+" & spinst!=","]
    df$Installs[i]=paste(spinst,collapse = "")
  }
  return(df$Installs)
}
df$Installs=split(df$Installs)
df$Installs
df$Installs=as.numeric(df$Installs)
df$Installs


#the other way:

#s=function(x){
#    spinst=strsplit(x,"")
#    spinst=unlist(spinst)
#    if (spinst[length(spinst)]=="+"){
#      spinst=spinst[-length(spinst)]
#    }
#    spinst=spinst[spinst!=","]
#    #it means in the vector spinst, delete an element if it was equal to ",".
#    x=paste(spinst,collapse = "")
#  return(x)
#}
# df$Installs=apply(df$Installs,1,s) has error.  How should I write it?



#k)


library(mice)
library(VIM)
df=df[,c(-1,-2,-7,-8)]
summary(df)
impute=mice(df,2,meth=c("polyreg","sample","pmm","logreg","pmm"))
?mice

#l)

aggr<- aggr(df, col=c('black','red'), numbers=TRUE, sortVars=TRUE,labels=names(df), cex.axis=.7, gap=1, ylab=c("Barplot of missing data","Patterns"))
marginplot(df[,c(2,3)])
marginplot(df[,c(2,4)])
marginplot(df[,c(3,4)])
pbox(df[,c(2,3)])

#m)
xyplot(impute, Rating ~Category+Reviews+Updates | .imp, pch = 20, cex = 1.4)
xyplot(impute, Reviews~Category+Rating +Updates | .imp, pch = 20, cex = 1.4)
xyplot(impute, Type~Category+Rating+ Reviews+Updates | .imp, pch = 20, cex = 1.4)
stripplot(impute, pch = 20, cex = 1.2)
densityplot(impute)
#Two imputation steps in Reviews are almost similar
#But in ratings, there are differents.



#n)

com=complete(impute,1)
summary(com)

pairs(df[,c("Rating","Category","Type","Reviews","Updates")])
