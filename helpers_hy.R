library(lubridate)
library(dplyr)
#job=read.csv('job_data1.csv')
#god=read.csv('god_data1.csv')
#exit=read.csv('exit_data1.csv')
#write.csv(job,"job.csv",row.names=FALSE)
#write.csv(god,"god.csv",row.names=FALSE)
#write.csv(exit,"exit.csv",row.names=FALSE)
Sys.getlocale("LC_TIME")
job=read.csv('job.csv')
god=read.csv('god.csv')
exit=read.csv('exit.csv')
# weekend effect 
week_effect=function(data,num,open=NA,day=NA){  
  data=data[,c(1,2)]
  colnames(data)=c("date","S")
  data$date=ymd(data$date)
  data$S=as.numeric(gsub(",", "", data$S))
  if (is.na(open)==FALSE) {
    if(ymd(open)-ymd(data$date[1])>0&
       sum(ymd(open)==ymd(data$date))>0){
      ind=which(ymd(open)==ymd(data$date))
      if(is.na(day)==FALSE){
        ind2=which(ymd(open)+days(day)==ymd(data$date))
        data=data[ind:ind2,]
      }
      else{
        data=data[c(ind:nrow(data)),]
      }
    }}
  data$wday=wday(ymd(data$date)) 
  week.ind=which(data$wday==1|data$wday==7)
  week.data=data[week.ind,]
  week.data$S<-week.data$S/num
  final.data=data[-week.ind,]
  for (i in 1:num){
    final.data=rbind(final.data,week.data)
  }
  final.data=final.data%>%arrange(date)
  final.data$Y=cumsum(final.data$S)
  final.data$Y_lag=dplyr::lag(final.data$Y,1)
  final.data[is.na(final.data)] <- 0
  final.data$T<-1:nrow(final.data)
  return(final.data)
}
#week_effect(job,2)
result_bass<-function (a,b,c) {
  m1 = (-b + sqrt(abs(b^2-4*a*c)))/(2*c) # hy change sqrt
  m2 = (-b - sqrt(abs(b^2-4*a*c)))/(2*c)
  if (m1>0){
    mhat<-m1
  } else {mhat<-m2}
  return(mhat)
}

#print abc in ols 
abc <- function(dfdata,model){
  df1<-dfdata
  df2<-df1[-1,]
  if (model == "Bass"){
    fit<-lm(S ~ Y_lag+I(Y_lag**2),data=df1)
  } else if (model == "Logistic"){
    fit<-lm(S ~ Y_lag+I(Y_lag**2) -1 ,data=df2)
  } else if(model == "Gumbel"){
    fit<-lm(S ~ Y_lag+I(Y_lag*log(Y_lag)) -1 ,data=df2)
  } else if(model =="Exponential"){
    fit<-lm(S ~ Y_lag ,data=df1)
  }
  a<-fit$coefficients[1]
  b<-fit$coefficients[2]
  c<-fit$coefficients[3]
  
  return(list(a=a,b=b,c=c))
}

mpq<- function(dfdata,myabc,Method,model){
  if (Method == "OLS"){
    a=as.numeric(myabc$a) # abc(week_effect(job,2),"Exponential")=mymodel
    b=as.numeric(myabc$b)
    c=as.numeric(myabc$c)
    if (model == "Bass"){
      result1<- data.frame("m"=result_bass(a,b,c),
                           "p" = a/result_bass(a,b,c),
                           "q" = -1*result_bass(a,b,c)*c)
      return(result1)
    } else if (model == "Gumbel"){
      result2<- data.frame("m"=exp(-a/b),
                           "p"=0,
                           "q" = -b)
      return(result2)
    } else if (model == "Logistic"){
      result3<- data.frame("m"= -a/b,
                           "p"=0,
                           "q" = a)
      return(result3)
    } else if (model == "Exponential"){
      result4<-data.frame("m"=-a/b,
                          "p"=-b,
                          "q"=0)
      return(result4)
    }
  } else if (Method == "Q-Q Plot"){
    df1<-dfdata
    a=as.numeric(myabc$a) # abc(week_effect(job,2),"Exponential")=mymodel
    b=as.numeric(myabc$b)
    c=as.numeric(myabc$c)
    if (model == "Bass"){
      result1<- data.frame("m"=result_bass(a,b,c)+rnorm(1,10000,100000),
                           "p" = a/result_bass(a,b,c),
                           "q" = -1*result_bass(a,b,c)*c)
      return(result1)
    } else if (model == "Gumbel"){
      box2<-data.frame("m"=seq(exp(-a/b)-5000,exp(-a/b)+5000,100),
                       "r-square"=rep(0.00000,length(seq(exp(-a/b)-5000,exp(-a/b)+5000,100))))
      m <- seq(exp(-a/b)-5000,exp(-a/b)+5000,100)
      for (i in 1:length(seq(exp(-a/b)-5000,exp(-a/b)+5000,100))){
        u <- df1$Y/(m[i]+1)
        box2[i,2]<-summary(lm( df1$T ~ I(-log(-log(u))),data=df1))$r.squared
      }
      mqq2<-box2[which.max(box2[,2]),1]
      result2<-data.frame("m"=mqq2,
                          "p"=0,
                          "q"=1/(lm( df1$T ~ I(-log(-log(u))),data=df1)$coefficients[2]))
      return(result2)
      
    } else if (model == "Logistic"){
      box3<-data.frame("m"=seq(-(a/b)-5000,-(a/b)+5000,100),
                       "r-square"=rep(0.00000,length(seq(-(a/b)-5000,-(a/b)+5000,100))))
      m <- seq(-(a/b)-5000,-(a/b)+5000,100)
      for (i in 1:length(seq(-(a/b)-5000,-(a/b)+5000,100))){
        u <- df1$Y/(m[i]+1)
        box3[i,2]<-summary(lm( df1$T ~ I(log(u/(1-u))),data=df1))$r.squared
      }
      mqq3<-box3[which.max(box3[,2]),1]
      result3<-data.frame("m"=mqq3,
                          "p"=0,
                          "q"=1/(lm( df1$T ~ I(log(u/(1-u))),data=df1)$coefficients[2]))
      return(result3)
    } else if (model == "Exponential"){
      box4<-data.frame("m"=seq(-(a/b)-5000,-(a/b)+5000,100),
                       "r-square"=rep(0.00000,length(seq(-(a/b)-5000,-(a/b)+5000,100))))
      m <- seq(-(a/b)-5000,-(a/b)+5000,100)
      for (i in 1:length(seq(-(a/b)-5000,-(a/b)+5000,100))){
        u <- df1$Y/(m[i]+1)
        box4[i,2]<-summary(lm( df1$T ~ I(-log((1-u))),data=df1))$r.squared
      }
      mqq4<-box4[which.max(box4[,2]),1]
      result4<-data.frame("m"=mqq4,
                          "p"=1/(lm( df1$T ~ I(-log((1-u))),data=df1)$coefficients[2]),
                          "q"=0)
      return(result4)
    }
  }
}


df_p <- function(myabc,dfdata,model,n){#n: future data,dfdata: cleaned data
  nrow1=nrow(dfdata)
  df1<-dfdata
  df2<-df1[-1,]
  
  a=as.numeric(myabc$a) # abc(week_effect(job,2),"Exponential")=mymodel
  b=as.numeric(myabc$b)
  c=as.numeric(myabc$c)
  S=c();Y=df1$Y;I=c();T=c()
  if (model == "Bass"){
    for (i in 0:(n-1)) {
      S = c(S,a+b*Y[nrow1+i]+c*(Y[nrow1+i]^2))
      Y[nrow1+i+1]=Y[nrow1+i]+S[length(S)]
    }
  } else if (model == "Logistic"){
    for (i in 0:(n-1)) {
      S= c(S,a*Y[nrow1+i]+b*(Y[nrow1+i]^2))
      Y[nrow1+i+1]=Y[nrow1+i]+S[length(S)]
    }
  } else if(model == "Gumbel"){
    for (i in 0:(n-1)) {
      S= c(S, a*Y[nrow1+i]+b*Y[nrow1+i]*log(Y[nrow1+i]))
      Y[nrow1+i+1]=Y[nrow1+i]+S[length(S)]
    }
  } else if(model =="Exponential"){
    for (i in 0:(n-1)) {
      S= c(S,a+b*Y[nrow1+i])
      Y[nrow1+i+1]=Y[nrow1+i]+S[length(S)]
    }
  }
  return(S)
}

mo="Bass"
data11=week_effect(god,2,"2019125",20)
#myabc=abc(data11,mo)
#mpq(data11,myabc,"Q-Q Plot",mo)
#mpq(data11,myabc,"OLS",mo)$m
#draw_g1(job,40,2,"20190125",week_effect(job,2,"20190125",10))
draw_g1=function(data1,n,num,open,data11,mo){
  myabc=abc(data11,mo)
  or.data=week_effect(data1,2,ymd(open),20+n+round(n/7*2+2)*(num-1))
  or.startind=max(which(or.data$date==data11$date[nrow(data11)]))+1
  or.endind=max(which(or.data$date==data11$date[nrow(data11)]+days(n)))
  g1=ggplot()+
    geom_line(data=data11,aes(x=ymd(date),y=S,colour="Real"),size=1.5)+
    theme_minimal()+
    geom_line(aes(x=or.data$date[or.startind:or.endind],
                  y=or.data$S[or.startind:or.endind],colour="Real"),size=1.5)+
    geom_line(aes(x=or.data$date[or.startind:or.endind],
                  y=df_p(myabc,data11,mo,or.endind-or.startind+1), colour="Expected"),size=1.5)+
    scale_color_manual(values=c("red3", "navy"))+
    labs(colour = "value",title="Expected VS real audience",
         subtitle="weekend effect is applied",
         x ="Date", y = "the daily attendance")+
    geom_vline(xintercept=or.data$date[or.startind],size=1.5,color="green4",
               linetype = "dashed")
  return(g1)
}

cj=read.csv("cjenm.csv")
colnames(cj)=c("date","price")
cj$date=ymd(cj$date)
cj$price=as.numeric(gsub(",", "",cj$price))
cj=cj%>%arrange(date)

draw_g2=function(open,day,n){
  buyday=ymd(open)+days(day)
  dday=cj$date-buyday
  startind=min(which(dday>0))
  sellday=ymd(open)+days(day+n)
  dday2=cj$date-sellday
  endind=min(which(dday2>0))
  income=(cj$price[startind:endind]/cj$price[startind]-1)*100
  df=data.frame(date=ymd(cj$date[startind:endind]),
                income=income)
  g2=ggplot(df,aes(date,income))+
    theme_minimal()+
    geom_area(data = subset(x =df,subset = (income <= 0)),
              fill = "brown1",alpha=0.4) +
    geom_area(data = subset(x =df,subset = (income >= 0)),
              fill = "royalblue",alpha=0.4) +
    geom_line(size=2,color="sienna4")+
    geom_vline(xintercept=cj$date[startind],size=1.5,color="green4",
               linetype = "dashed")+
    geom_hline(yintercept=0,size=1.5,color="chocolate",
               linetype = "dotted")+
    geom_text(aes(x=cj$date[startind],y=max(df$income),
                  label="the point of\nstock purchase"
    ),vjust=1,hjust=-0.1,color="green4",size=5)+
    labs(title="Income rate of stock",
         subtitle="Based on the closing price of stocks",
         x ="Date", y = "Income rate")
  return(g2)
  
}

drawqq<-function(model,dfdata,mympq){
  if (model == "Bass"){
    dfmodel_bass<-dfdata%>%
      mutate('p'=Y/(mympq[,1]+1),
             'F'=(mympq[,2]+mympq[,3])^(-1)*log((1+(mympq[,3]/mympq[,2])*p)/(1-p)))
    R_squ1<-round(summary(lm(T~F,data=dfmodel_bass))$r.square,3)
    
    g=ggplot(dfmodel_bass,aes(F,T))+geom_point()+
      geom_smooth(method='lm',se=F)+theme_bw()+
      labs(title=paste('Bass Q-Q Plot, R-square=', R_squ1),x='',y='time')+
      theme(plot.title = element_text(face='bold',size=20))
  } else if (model == "Gumbel"){
    dfmodel_gum<-dfdata%>%
      mutate('U'=Y/(mympq[,1]+1),
             'Ginv'=-log(-log(U)))
    R_squ2<-round(summary(lm(T~Ginv,data=dfmodel_gum))$r.square,3)
    
    g=ggplot(dfmodel_gum,aes(Ginv,T))+geom_point()+
      geom_smooth(method='lm',se=F)+theme_bw()+
      labs(title=paste('Gumbel Q-Q Plot, R-square=', R_squ2),x='',y='time')+
      theme(plot.title = element_text(face='bold',size=20))
  } else if (model == "Logistic"){
    dfmodel_logi<-dfdata%>%
      mutate('U'=Y/(mympq[,1]+1),
             'Ginv'=log(U/(1-U)))
    R_squ3<-round(summary(lm(T~Ginv,data=dfmodel_logi))$r.square,3)
    
    g=ggplot(dfmodel_logi,aes(Ginv,T))+geom_point()+
      geom_smooth(method='lm',se=F)+theme_bw()+
      labs(title=paste('Logistic Q-Q Plot, R-square=', R_squ3),x='',y='time')+
      theme(plot.title = element_text(face='bold',size=20))
  } else if (model == "Exponential"){
    dfmodel_exp<-dfdata%>%
      mutate('U'=Y/(mympq[,1]+1),
             'Ginv'=-log(1-sort(U)))
    R_squ4<-round(summary(lm(T~Ginv,data=dfmodel_exp))$r.square,3)
    
    g=ggplot(dfmodel_exp,aes(Ginv,T))+geom_point()+
      geom_smooth(method='lm',se=F)+theme_bw()+
      labs(title=paste('Exponential Q-Q Plot, R-square=', R_squ4),x='',y='time')+
      theme(plot.title = element_text(face='bold',size=20))
  }
  return(g)
}

