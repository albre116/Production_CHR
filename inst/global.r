###Function Load and auto install for every bundle run###
if(!require("devtools"))
  (install.packages("devtools"))
if(!require("shiny"))
  install.packages('shiny', repos=c('http://rstudio.org/_packages', 'http://cran.rstudio.com'))
if(!require("shinyIncubator"))
  devtools::install_github("shiny-incubator", "rstudio")
if(!require("colorspace"))
  (install.packages("colorspace"))
if(!require("alr3"))
  (install.packages("alr3"))
if(!require("astsa"))
  (install.packages("astsa"))
if(!require("psych"))
  (install.packages("psych"))
if(!require("lars"))
  (install.packages("lars"))
if(!require("elasticnet"))
  (install.packages("elasticnet"))
if(!require("rpart"))
  (install.packages("rpart"))
if(!require("rpart.plot"))
  (install.packages("rpart.plot"))
if(!require("maptree"))
  (install.packages("maptree"))
if(!require("lattice"))
  (install.packages("lattice"))
if(!require("vars"))
  (install.packages("vars"))
if(!require("urca"))
  (install.packages("urca"))
if(!require("caret"))
  (install.packages("caret"))
if(!require("AppliedPredictiveModeling"))
  (install.packages("AppliedPredictiveModeling"))
if(!require("partykit"))
  (install.packages("partykit"))
if(!require("pls"))
  (install.packages("pls"))
if(!require("corrplot"))
  (install.packages("corrplot"))
if(!require("mgcv"))
  (install.packages("mgcv"))
if(!require("plyr"))
  (install.packages("plyr"))
if(!require("ggplot2"))
  (install.packages("ggplot2"))
if(!require("ggmap"))
  (install.packages("ggmap"))
if(!require("rjson"))
  (install.packages("rjson"))
if(!require("RCurl"))
  (install.packages("RCurl"))
if(!require("rCharts"))
  install_github('rCharts', 'ramnathv')




averages<-function(X,d_index=c(1)){###identify date index in input file
  date<-X[,d_index]
  if(class(date) %in% c("integer","numeric")){
    date<-as.POSIXlt((date-1)*24*60*60,origin="1900-01-01")
    stripped<-format(date,"%Y-%m-%d")
    date<-as.Date(stripped)
  }
  
  
  index<-order(date)
  date<-date[index]
  X<-X[index,]
  Z<-X[,as.logical(lapply(X[1,],is.numeric)),drop=F]

  
  
  ###compute day if needed (off for now to speed up computation)
#   index<-unique(date)
#   DAY<-as.data.frame(matrix(nrow=length(index),ncol=ncol(Z)))
#   HEADER<-numeric(length=length(index))
#   class(HEADER)<-"Date"
#   VOLUME<-numeric(length=length(index))
#   colnames(DAY)<-colnames(Z)
#   for (i in 1:length(index)){
#     tmp<-which(date==index[i])
#     DAY[i,]<-apply(Z[tmp,],2,mean)
#     HEADER[i]<-date[tmp][1]
#     VOLUME[i]<-length(tmp)
#   }
#   DAY<-data.frame("index"=index,"date"=HEADER,DAY,"volume"=VOLUME)

  

  year<-as.numeric(format(date,format="%Y"))
  reference<-as.Date(strptime(paste(year,"01","01",sep="-"),"%Y-%m-%d"),format="%Y-%m-%d")
  week<-as.double(floor(difftime(date,reference,units="weeks")))
  week[week<0]=0###deal with some where there are slightly negative numbers that get pushed to -1 with the floor command
  
  week[which(week==52)]=51 ##cut off last couple of days in year and group with last week
  year_week<-paste(year,week,sep="-")
  elapsed_weeks<-match(year_week,unique(year_week))-1
  index<-unique(elapsed_weeks)
  WEEK<-as.data.frame(matrix(nrow=length(index),ncol=ncol(Z)))
  HEADER<-numeric(length=length(index))
  class(HEADER)<-"Date"
  VOLUME<-numeric(length=length(index))
  colnames(WEEK)<-colnames(Z)
  for (i in 1:length(index)){
    tmp<-which(elapsed_weeks==index[i])
    WEEK[i,]<-apply(Z[tmp,,drop=F],2,mean)
    HEADER[i]<-as.Date(as.POSIXct(reference[tmp[1]],origin="1970-01-01")+week[tmp[1]]*7*24*60*60,format="%Y-%m-%d")
    VOLUME[i]<-length(tmp)
  }
  WEEK<-data.frame("index"=index,"date"=HEADER,WEEK,"volume"=VOLUME)
  ##q<-as.POSIXct(WEEK$date,origin="1970-01-01") ###convert back to date format
  
  
  bundle<-structure(list("WEEK"=WEEK))
  return(bundle)
}


align_week<-function(X,d_index,start="2008-01-01",end="2013-12-01"){###identify date index in input files
  y<-ls(X)
  min<-as.POSIXlt(start,origin="1970-01-01")
  max<-as.POSIXlt(end,origin="1970-01-01")
  series<-min+(0:difftime(max,min,units="days"))*24*60*60
  series<-as.Date(series,format="%Y-%m-%d")
  year<-as.numeric(format(series,format="%Y"))
  #year<-series$year+1900
  reference<-as.Date(strptime(paste(year,"01","01",sep="-"),"%Y-%m-%d"),format="%Y-%m-%d")
  week<-as.double(floor(difftime(series,reference,units="weeks")))
  week[week<0]=0###deal with slight float difference that causes negative values
  week[which(week==52)]=51 ##cut off last couple of days in year and group with last week
  year_week<-paste(year,week,sep="-")
  elapsed_weeks<-match(year_week,unique(year_week))-1
  index<-unique(elapsed_weeks)
  HEADER<-numeric(length=length(index)) 
  class(HEADER)<-"Date"
  for (i in 1:length(index)){
    tmp<-which(elapsed_weeks==index[i])
    HEADER[i]<-as.Date(as.POSIXct(reference[tmp[1]],origin="1970-01-01")+week[tmp[1]]*7*24*60*60,format="%Y-%m-%d")
  }
  
  
  for (i in 1:length(y)){
    row<-length(HEADER)
    tmpcmd<-paste("col=ncol(X$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("passes=nrow(X$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste(y[i],"=as.data.frame(matrix(nrow=row,ncol=col))",sep="")
    eval(parse(text=tmpcmd))
    for(j in 1:passes){
      tmpcmd<-paste(y[i],"[which(HEADER==X$",y[i],"[j,",d_index[i],"]),]=X$",y[i],"[j,]",sep="")
      eval(parse(text=tmpcmd))
    }
    
    tmpcmd<-paste("names=colnames(X$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("colnames(",y[i],")<-names",sep="")
    eval(parse(text=tmpcmd))
    
  }
  
  y<-c("Align_date",y)
  Align_date=HEADER
  tmpcmd<-paste("\"",y,"\"=",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("DATA=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  return(DATA)
}



raw_plot_data<-function(DATA,t_index=c(1)){###index identifies the time component
  y<-names(DATA)
  time=DATA[[t_index]]
  all_time<-numeric()
  class(all_time)<-"Date"
  all_group<-character()
  index<-1:length(DATA)
  index<-index[-t_index]
  i=2
  for (i in index){
    b<-data.frame(DATA[[i]])
    align_time=rep(time,ncol(b))
    group=rep(y[i],nrow(b)*ncol(b))
    all_group<-c(all_group,group)
    all_time<-c(all_time,align_time)
  }
  q<-data.frame(DATA[-t_index])
  q<-stack(q)
  for (i in index){
    q[,2]<-gsub(paste(y[i],".",sep=""),"",q[,2])
  }
  RAWPLOTDATA<-cbind("time"=all_time,"group"=all_group,q)
  return(RAWPLOTDATA)  
}



loess_fill<-function(X,folds=5,t_index=c(1),span=c(10:1/10)){###index identifies the cyclic time component to model against
  y<-ls(X)
  time=as.numeric(X[[t_index]])
  index<-1:length(X)
  index<-index[c(-t_index)]
  for (i in index){
    Z<-X[[i]]
    tmp<-colnames(Z)
    fold<-sample(rep(1:folds,nrow(Z) %/% folds + 1),nrow(Z),replace=F)
    for (j in 1:ncol(Z)){
      perform<-data.frame(matrix(ncol=2,nrow=0))
      colnames(perform)<-c("span","xval_err")
      for(w in 1:length(span)){
        xval_err=0
        flag=FALSE
        for (q in 1:folds){
          Z_fold<-Z
          Z_fold[(which(fold==q)),]=NA
          #tmpcmd<-paste("fit=loess(",tmp[j],"~time,data=Z_fold,span=",span[w],")",sep="")
          #eval(parse(text=tmpcmd))
          fit<-do.call("loess",list(formula=as.formula(paste0(tmp[j],"~time")),data=Z_fold,span=span[w]))
          t_hold<-time[which(fold==q)]
          hold=try(predict(fit,newdata<-data.frame("time"=t_hold)),silent=T)
          if (is.numeric(hold) & flag==FALSE){
            PMSE<-sum((Z[which(fold==q),j]-hold)^2,na.rm=T)
            xval_err=xval_err+PMSE
          }else
          {
            xval_err=NA
            flag=TRUE
          }
        }
        perform<-rbind(perform,data.frame("span"=span[w],"xval_err"=xval_err))
      }
      print(perform)
      i_span<-which.min(perform$xval_err)
      #tmpcmd<-paste("fit=loess(",tmp[j],"~time,data=Z,span=",span[i_span],")",sep="")
      #eval(parse(text=tmpcmd)) 
      fit<-do.call("loess",list(formula=as.formula(paste0(tmp[j],"~time")),data=Z,span=span[i_span]))
      element<-which(is.na(Z[,j]))
      Z[element,j]=predict(fit,newdata<-data.frame(time))[element]
    }
    X[[i]]<-Z
  }
  return(X)
}


GAM_fill<-function(X,t_index=c(1),gamma){###Not as good as the loess fill
  y<-ls(X)
  time=X[[t_index]]
  index<-1:length(X)
  index<-index[c(-t_index)]
  for (i in index){
    Z<-X[[i]]
    tmp<-colnames(Z)
    for (j in 1:ncol(Z)){
    tmpcmd<-paste("fit=gam(",tmp[j],"~s(time),data=Z,gamma=",gamma,")",sep="")
    eval(parse(text=tmpcmd))
    element<-which(is.na(Z[,j]))
    Z[element,j]=predict(fit,newdata<-data.frame(time))[element]
    }
    X[[i]]<-Z 
    
  }
    return(X)
}

PIECE_fill<-function(X,t_index=c(1)){###Piece wise linear fill

  y<-ls(X)
  time=X[[t_index]]
  index<-1:length(X)
  index<-index[c(-t_index)]
  for (i in index){
    Z<-X[[i]]
    #tmp<-colnames(Z)
    for (j in 1:ncol(Z)){

      ###consider piece wise linear fit versus loess
      non_empty<-which(!is.na(Z[,j]))
      for (g in 2:length(non_empty)){###piecewise linear fit
        ff<-(non_empty[g-1]+1):(non_empty[g]-1)
        dat<-data.frame(y=Z[non_empty[(g-1):g],j],x=time[non_empty[(g-1):g]])
        qf<-lm(y~x,data=dat)
        Z[ff,j]<-predict(qf,newdata=data.frame(x=time[ff]))
      }
      
    }
    X[[i]]<-Z 
    
  }
  return(X)
}




cut_variation<-function(data){
  for (i in 2:length(ls(data))){
    tmp<-data[[i]]
    remove<-nearZeroVar(tmp)
    print(remove)
    if (length(remove)>0){
      data[[i]]<-tmp[,-remove]
    }
  }
  return(data)
}





linear_detrend<-function(X,date){
  y<-colnames(X)
  time=as.numeric(date)
  tmp<-colnames(X)
  i=1
  for (i in 1:ncol(X)){
    tmpcmd<-paste(tmp[i],"=lm(",tmp[i],"~time,data=X)",sep="")
    eval(parse(text=tmpcmd))
  }
  
  tmpcmd<-paste("\"",y,"\"=",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("MODEL=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  return(MODEL)
}




detrend_linear<-function(X,MODEL){
  y<-colnames(X)
  for (i in 1:ncol(X)){
    tmpcmd<-paste("pred=predict(MODEL$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("obs=predict(MODEL$",y[i],") + residuals(MODEL$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    X[i]<-obs-pred
  }
  return(X)
}




loess_detrend<-function(Z,date,folds=5,span=1:10/10){###index identifies the cyclic time component to model against
  X<-data.frame("days"=as.numeric(format(date,"%j")),Z)
  y<-colnames(X)
  x<-colnames(X)[1]
  y<-colnames(X)[-1]
  fold<-sample(rep(1:folds,nrow(X) %/% folds + 1),nrow(X),replace=F)
  j=1
  for (j in 1:length(y)){
    perform<-data.frame(matrix(ncol=2,nrow=0))
    colnames(perform)<-c("span","xval_err")
    w=1
    for(w in 1:length(span)){
      xval_err=0
      flag=FALSE
      q=1
      for (q in 1:folds){
        X_fold<-X
        X_fold[(which(fold==q)),]=NA
        tmpcmd<-paste("fit=loess(",y[j],"~",x,",data=X_fold,span=",span[w],")",sep="")
        eval(parse(text=tmpcmd))
        t_hold<-X[which(fold==q),1]
        tmpcmd<-paste("hold=try(predict(fit,newdata<-data.frame(",x,"=t_hold)),silent=T)",sep="")
        eval(parse(text=tmpcmd))
        if (is.numeric(hold) & flag==FALSE){
          PMSE<-sum((Z[which(fold==q),j]-hold)^2,na.rm=T)
          xval_err=xval_err+PMSE
        }else
        {
          xval_err=NA
          flag=TRUE
        }
      }
      perform<-rbind(perform,data.frame("span"=span[w],"xval_err"=xval_err))
    }
    print(perform)
    i_span<-which.min(perform$xval_err)
    tmpcmd<-paste(y[j],"=loess(",y[j],"~",x,",data=X,span=",span[i_span],")",sep="")
    eval(parse(text=tmpcmd))      
  }
  
  tmpcmd<-paste("\"",y,"\"=",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("MODEL=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  return(MODEL)
}






detrend_loess<-function(X,MODEL){
  y<-colnames(X)
  for (i in 1:ncol(X)){
    tmpcmd<-paste("pred=predict(MODEL$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("obs=predict(MODEL$",y[i],") + residuals(MODEL$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    X[i]<-obs-pred
  }
  return(X)
}


#####replace all loess functions with this for future prediction####


mean_future_GAM<-function(time_ahead,date,z,gamma){###index identifies the time component
  days=as.numeric(format(date,"%j"))
  days_fwd=as.numeric(format(time_ahead,"%j"))
  date_fwd=as.numeric(time_ahead)
  GAM_DATA<-cbind(z,days,"date"=as.numeric(date))
  y<-colnames(z)
  FUTURE<-data.frame(matrix(nrow=length(date_fwd),ncol=length(y)))
  colnames(FUTURE)<-y
  for (i in 1:length(y)){
    preds<-c("s(date,sp=1000)","s(days,bs=\"cc\")")
    preds<-paste(preds,collapse="+")
    tmpcmd<-paste("fit=gam(",y[i],"~",preds,",data=GAM_DATA,gamma=",gamma,")",sep="")
    eval(parse(text=tmpcmd))
    FUTURE[i]<-predict(fit,newdata=data.frame("date"=date_fwd,"days"=days_fwd))
  }
  
  return(FUTURE)

}


#####end loess replace


plot_loess_fit<-function(MODEL){###index identifies the time component
  y<-ls(MODEL)
  cov<-MODEL[[1]]
  cov<-cov$xnames[[1]]
  for (i in 1:length(y)){
    tmpcmd<-paste("pred_",y[i],"=predict(MODEL$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("obs_",y[i],"=predict(MODEL$",y[i],") + residuals(MODEL$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("x_",y[i],"=MODEL$",y[i],"$x",sep="")
    eval(parse(text=tmpcmd))
  }
  
  tmpcmd<-paste("\"",y,"\"=pred_",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("PREDICTED=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  
  tmpcmd<-paste("\"",y,"\"=obs_",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("OBSERVED=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  
  tmpcmd<-paste("\"",y,"\"=x_",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("XVALS=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  
  
  
  b<-stack(PREDICTED)
  bb<-data.frame(XVALS)
  colnames(bb)<-ls(XVALS)
  bb<-stack(bb)
  b<-data.frame("group"="PREDICTED","x"=bb$values,b)
  b<-b[order(b$x),]
  
  q<-stack(OBSERVED)
  qq<-data.frame(XVALS)
  colnames(qq)<-ls(XVALS)
  qq<-stack(qq)
  q<-data.frame("group"="OBSERVED","x"=qq$values,q)
  q<-q[order(q$x),]
  
  data=rbind(q,b)
  xyplot(values~x|ind,group=group,data=data,scales=list(relation="free"),ylab=NULL,type=c("p","l"),
         lwd=5,pch=19,cex=0.01,col=c("gray","blue"),xlab=paste(cov),
         distribute.type=TRUE)
}





mean_future<-function(date,X,MODEL_LM,MODEL_LOESS){###index identifies the time component
  days<-as.numeric(format(date,"%j"))
  date<-as.numeric(date)
  y<-colnames(X)
  FUTURE<-data.frame(matrix(nrow=length(date),ncol=length(y)))
  colnames(FUTURE)<-y
  i=1
  for (i in 1:length(y)){
    tmpcmd<-paste("linear=predict(MODEL_LM$",y[i],",newdata=data.frame(time=date))",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("season=predict(MODEL_LOESS$",y[i],",newdata=data.frame(days=days))",sep="")
    eval(parse(text=tmpcmd))
    FUTURE[,i]<-linear+season
  }
  return(FUTURE)
}



plot_loess_unconstrained<-function(MODEL,LINEAR){###index identifies the time component
  y<-ls(MODEL)
  cov<-MODEL[[1]]
  cov<-attr(cov$terms,"variables")[[2]]
  for (i in 1:length(y)){
    tmpcmd<-paste("pred_",y[i],"=predict(MODEL$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("obs_",y[i],"=predict(MODEL$",y[i],") + residuals(MODEL$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("x_",y[i],"=MODEL$",y[i],"$x",sep="")
    eval(parse(text=tmpcmd))
  }
  
  tmpcmd<-paste("\"",y,"\"=pred_",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("PREDICTED=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  
  tmpcmd<-paste("\"",y,"\"=obs_",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("OBSERVED=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  
  tmpcmd<-paste("\"",y,"\"=x_",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("XVALS=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  
  if (LINEAR==TRUE) {XVALS[which(ls(XVALS)=="linear_time_effect")]=MODEL[[1]]$model[2]}
  
  b<-stack(PREDICTED)
  bb<-data.frame(XVALS)
  colnames(bb)<-ls(XVALS)
  if (LINEAR==TRUE) {bb$linear_time_effect<-as.numeric(bb$linear_time_effect)}
  bb<-stack(bb)
  b<-data.frame("group"="PREDICTED","x"=bb$values,b)
  b<-b[order(b$x),]
  
  q<-stack(OBSERVED)
  qq<-data.frame(XVALS)
  colnames(qq)<-ls(XVALS)
  if (LINEAR==TRUE) {qq$linear_time_effect<-as.numeric(qq$linear_time_effect)}
  qq<-stack(qq)
  q<-data.frame("group"="OBSERVED","x"=qq$values,q)
  q<-q[order(q$x),]
  
  w<-stack(PREDICTED)
  ww<-data.frame(XVALS)
  colnames(ww)<-ls(XVALS)
  if (LINEAR==TRUE) {ww$linear_time_effect<-as.numeric(ww$linear_time_effect)}
  ww<-stack(ww)
  w<-data.frame("group"="Today's Value","x"=ww$values,w)
  find<-unique(w$ind)
  tmp<-w[which(w$ind==find[1]),]
  idx=nrow(tmp)
  idx<-idx*(1:length(find))
  w<-w[idx,]
  
  
  data=rbind(q,b,w)
  xyplot(values~x|ind,group=group,data=data,scales=list(relation="free"),xlab=NULL,type=c("p","l","p"),
         lwd=c(1,5,3),pch=c(19,19,18),cex=c(0.01,0.01,3),col=c("gray","blue","red"),ylab=paste(cov),
         distribute.type=TRUE,main="Conditional Effect of Variable On Mean Response")
}


plot_loess_seasonal<-function(MODEL){###index identifies the time component
  y<-ls(MODEL)
  cov<-MODEL[[1]]
  cov<-cov$xnames[[1]]
  for (i in 1:length(y)){
    tmpcmd<-paste("pred_",y[i],"=predict(MODEL$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("obs_",y[i],"=predict(MODEL$",y[i],") + residuals(MODEL$",y[i],")",sep="")
    eval(parse(text=tmpcmd))
    tmpcmd<-paste("x_",y[i],"=MODEL$",y[i],"$x",sep="")
    eval(parse(text=tmpcmd))
  }
  
  tmpcmd<-paste("\"",y,"\"=pred_",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("PREDICTED=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  
  tmpcmd<-paste("\"",y,"\"=obs_",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("OBSERVED=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  
  tmpcmd<-paste("\"",y,"\"=x_",y,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("XVALS=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  
  b<-stack(PREDICTED)
  bb<-data.frame(XVALS)
  colnames(bb)<-ls(XVALS)
  bb<-stack(bb)
  b<-data.frame("group"="PREDICTED","x"=bb$values,b)
  b<-b[order(b$x),]
  
  q<-stack(OBSERVED)
  qq<-data.frame(XVALS)
  colnames(qq)<-ls(XVALS)
  qq<-stack(qq)
  q<-data.frame("group"="OBSERVED","x"=qq$values,q)
  q<-q[order(q$x),]
  
  data=rbind(q,b)
  xyplot(values~x|ind,group=group,data=data,scales=list(relation="free"),ylab=NULL,type=c("p","l"),
         lwd=5,pch=19,cex=0.01,col=c("gray","blue"),xlab=paste(cov),
         distribute.type=TRUE)
}

linear_fit_GAM<-function(X,date){
  q<-"linear_time_effect"
  X<-data.frame(date,X)
  y<-colnames(X)[-1]
  time=date
  i=1
  for (i in 1:length(y)){
    tmpcmd<-paste(q[i],"=lm(",y[i],"~time,data=X)",sep="")
    eval(parse(text=tmpcmd))
  }
  
  tmpcmd<-paste("\"",q,"\"=",q,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("MODEL=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  return(MODEL)
}

loess_fit_seasonal<-function(Z,date,folds=5,span=1:10/10){###index identifies the cyclic time component to model against
  nn<-"seasonal_time_effect"
  X<-data.frame("days"=as.numeric(format(date,"%j")),Z)
  y<-colnames(X)
  x<-colnames(X)[1]
  y<-colnames(X)[-1]
  fold<-sample(rep(1:folds,nrow(X) %/% folds + 1),nrow(X),replace=F)
  j=1
  for (j in 1:length(y)){
    perform<-data.frame(matrix(ncol=2,nrow=0))
    colnames(perform)<-c("span","xval_err")
    w=1
    for(w in 1:length(span)){
      xval_err=0
      flag=FALSE
      q=1
      for (q in 1:folds){
        X_fold<-X
        X_fold[(which(fold==q)),]=NA
        tmpcmd<-paste("fit=loess(",y[j],"~",x,",data=X_fold,span=",span[w],")",sep="")
        eval(parse(text=tmpcmd))
        t_hold<-X[which(fold==q),1]
        tmpcmd<-paste("hold=try(predict(fit,newdata<-data.frame(",x,"=t_hold)),silent=T)",sep="")
        eval(parse(text=tmpcmd))
        if (is.numeric(hold) & flag==FALSE){
          PMSE<-sum((Z[which(fold==q),j]-hold)^2,na.rm=T)
          xval_err=xval_err+PMSE
        }else
        {
          xval_err=NA
          flag=TRUE
        }
      }
      perform<-rbind(perform,data.frame("span"=span[w],"xval_err"=xval_err))
    }
    print(perform)
    i_span<-which.min(perform$xval_err)
    tmpcmd<-paste(nn[j],"=loess(",y[j],"~",x,",data=X,span=",span[i_span],")",sep="")
    eval(parse(text=tmpcmd))
  }
  
  tmpcmd<-paste("\"",nn,"\"=",nn,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("MODEL=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  return(MODEL)
}

loess_fit_unconstrained<-function(Z,response=1,folds=5,span=1:10/10){###index identifies the cyclic time component to model against
  y<-colnames(Z)
  index<-(1:length(y))[-response]
  x<-y[-response]
  y<-y[response]
  fold<-sample(rep(1:folds,nrow(Z) %/% folds + 1),nrow(Z),replace=F)
  j=3
  for (j in 1:length(x)){
    perform<-data.frame(matrix(ncol=2,nrow=0))
    colnames(perform)<-c("span","xval_err")
    w=1
    for(w in 1:length(span)){
      xval_err=0
      flag=FALSE
      q=1
      for (q in 1:folds){
        X_fold<-Z
        X_fold[(which(fold==q)),]=NA
        tmpcmd<-paste("fit=loess(",y,"~",x[j],",data=X_fold,span=",span[w],")",sep="")
        eval(parse(text=tmpcmd))
        t_hold<-Z[which(fold==q),index[j]]
        tmpcmd<-paste("hold=try(predict(fit,newdata<-data.frame(",x[j],"=t_hold)),silent=T)",sep="")
        eval(parse(text=tmpcmd))
        if (is.numeric(hold) & flag==FALSE){
          PMSE<-sum((Z[which(fold==q),response]-hold)^2,na.rm=T)
          xval_err=xval_err+PMSE
        }else
        {
          xval_err=NA
          flag=TRUE
        }
      }
      perform<-rbind(perform,data.frame("span"=span[w],"xval_err"=xval_err))
    }
    print(perform)
    i_span<-which.min(perform$xval_err)
    tmpcmd<-paste(x[j],"=loess(",y,"~",x[j],",data=Z,span=",span[i_span],")",sep="")
    eval(parse(text=tmpcmd))      
  }
  
  tmpcmd<-paste("\"",x,"\"=",x,",",sep="")
  tmpcmd<-paste(tmpcmd,collapse="")
  tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
  tmpcmd<-paste("MODEL=structure(list(",tmpcmd,"))",sep="")
  eval(parse(text=tmpcmd))
  return(MODEL)
}

GAM_detrend<-function(Y,MODEL){
  x<-ls(MODEL)
  Y_hat=0
  for (i in 1:length(x)){
    tmpcmd<-paste("pred=predict(MODEL$",x[i],")",sep="")
    eval(parse(text=tmpcmd))
    Y_hat=Y_hat+pred-mean(pred)
  }
  Y<-Y-Y_hat
  return(Y)
}


GAM_y_hat<-function(MODEL){
  x<-ls(MODEL)
  Y_hat=0
  for (i in 1:length(x)){
    tmpcmd<-paste("pred=predict(MODEL$",x[i],")",sep="")
    eval(parse(text=tmpcmd))
    Y_hat=Y_hat+pred
  }
  return(Y_hat)
}




future_GAM<-function(time_ahead,FUTURE,MODEL,LINEAR,SEASON){###index identifies the time component
  days<-as.numeric(format(time_ahead,"%j"))
  y_hat=0
  register=as.numeric(LINEAR==TRUE)
  if (LINEAR==TRUE) {linear=predict(MODEL[1],newdata=data.frame(time=time_ahead))
                     y_hat=y_hat+linear[[1]]}
  if (SEASON==TRUE) {season=predict(MODEL[1+register],newdata=data.frame(days=days))
                     y_hat=y_hat+season[[1]]}
  ct_ad=as.numeric(SEASON==TRUE) + as.numeric(LINEAR==TRUE)
  for (i in 1:(length(ls(MODEL))-ct_ad)){
    b=i+ct_ad
    tmpcmd<-paste("add=predict(MODEL[b],newdata=FUTURE$",ls(MODEL[b]),")",sep="")
    eval(parse(text=tmpcmd))
    y_hat=y_hat+add[[1]]
  }
  return(y_hat)
}



GAM_effects<-function(time_ahead,FUTURE,MODEL,LINEAR,SEASON){###index identifies the time component
  days<-as.numeric(format(time_ahead,"%j"))
  register=as.numeric(LINEAR==TRUE)
  out=data.frame()
  if (LINEAR==TRUE) {linear=predict(MODEL[1],newdata=data.frame(time=time_ahead))
                     out=data.frame(linear)}
  if (SEASON==TRUE) {season=predict(MODEL[1+register],newdata=data.frame(days=days))
                     if(LINEAR==FALSE){out=data.frame(season)}else{
                       out=data.frame(out,season)}}
  ct_ad=as.numeric(SEASON==TRUE) + as.numeric(LINEAR==TRUE)
  for (i in 1:(length(ls(MODEL))-ct_ad)){
    b=i+ct_ad
    tmpcmd<-paste("add=predict(MODEL[b],newdata=FUTURE$",ls(MODEL[b]),")",sep="")
    eval(parse(text=tmpcmd))
    if(ct_ad==0 & i==1){out=data.frame(add)}else{
      out=data.frame(out,add)}
  }
  return(out)
}



Lead_lag<-function(x,var,mvmt){###mvmt is positive for lead
  for (i in 1:length(var)){
    swap<-x[var[i]]
    if (mvmt[i]>0){
      swap[(mvmt[i]+1):nrow(swap),1]=swap[1:(nrow(swap)-mvmt[i]),1]
      swap[1:(mvmt[i]),1]=NA
      x[var[i]]=swap
      colnames(x)[var[i]]<-paste(colnames(x)[var[i]],"_lead_",mvmt[i],sep="")
    } 
    if (mvmt[i]<0){
      swap[1:(nrow(swap)+mvmt[i]),1]=swap[(-mvmt[i]+1):nrow(swap),1]
      swap[(nrow(swap)+mvmt[i]+1):nrow(swap),1]=NA
      x[var[i]]=swap
      colnames(x)[var[i]]<-paste(colnames(x)[var[i]],"_lag_",-mvmt[i],sep="")
    }
  }
  return(x)
}


EPA_API<-function(series="PET.EMD_EPD2D_PTE_NUS_DPG.W",key="A9BCC61DA44BA0C0ECA4A42D622D7D44"){
  host <- 'http://api.eia.gov/series/?'
  url <- paste(host, paste0("api_key=",key),paste0("series_id=",series), sep="&")
  jsonResponse <- getURL(url)
  resultdf <- fromJSON(jsonResponse)
  return(resultdf)
}






CENSUS_API<-function(series,key="cf2bc020b12d020f8ee3155f74198a21dc585845"){
  out<-vector("list",nrow(series))
  for (i in 1:nrow(series)){
  host<-"http://api.census.gov/data/eits/"
  url<-paste(paste0(host,series$path[i],"?","get=cell_value"),"for=us:*",paste0("category_code=",series$category_code[i]),
           paste0("data_type_code=",series$data_type[i]),"time=from+2000",paste0("key=",key),sep="&")
  jsonResponse <- getURL(url)
  out[[i]] <- fromJSON(jsonResponse)
  } 
  return(out)
}





#' @import shiny


#' Matrix input
#' 
#' Creates an adjustable-length matrix input.
#' 
#' @param inputId Input variable to assign the control's value to.
#' @param label Display label for the control.
#' @param data The initial values to use for the matrix.
#' 
#' @export
matrixCustom <- function(inputId, label, data) {
  addResourcePath(
    prefix='tableinput', 
    directoryPath=system.file('tableinput', 
                              package='shinyIncubator'))
  
  tagList(
    singleton(
      tags$head(
        tags$link(rel = 'stylesheet',
                  type = 'text/css',
                  href = 'tableinput/tableinput.css'),
        tags$script(src = 'tableinput/tableinput.js')
      )
    ),
    
    tags$div(
      class = 'control-group tableinput-container',
      tags$label(
        class = "control-label",
        label,
        tags$div(
          class = 'tableinput-buttons',
          tags$button(
            type = 'button', class = 'btn btn-mini tableinput-settings hide',
            tags$i(class = 'icon-cog')
          ),
          HTML('<a href="#" class="tableinput-plusrow"><i class="icon-plus-sign"></i></a>'),
          HTML('<a href="#" class="tableinput-minusrow"><i class="icon-minus-sign"></i></a>')
        )
      ),
      tags$table(
        id = inputId,
        class = 'tableinput data table table-bordered table-condensed',
        tags$colgroup(
          lapply(names(data), function(name) {
            tags$col('data-name' = name,
                     'data-field' = name,
                     'data-type' = 'character')
          })
        ),
        tags$thead(
          class = 'hide',
          tags$tr(
            lapply(names(data), function(name) {
              tags$th(name)
            })
          )
        ),
        tags$tbody(
          lapply(1:nrow(data), function(i) {
            tags$tr(
              lapply(names(data), function(name) {
                tags$td(
                  div(tabindex=0, as.character(data[i,name]))
                )
              })
            )
          })
        )
      ),
      tags$div(
        class = 'tableinput-editor modal hide fade',
        tags$div(
          class = 'modal-header',
          HTML('<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>'),
          tags$h3(label)
        ),
        tags$div(
          class = 'modal-body',
          
          HTML('
               <form class="form-horizontal">
               <div class="control-group">
               <label class="control-label">Rows</label>
               <div class="controls">
               <input type="number" class="tableinput-rowcount">
               </div>
               </div>
               <div class="control-group">
               <label class="control-label">Columns</label>
               <div class="controls">
               <input type="number" class="tableinput-colcount">
               </div>
               </div>
               </form>'
          )
          ),
        tags$div(
          class = 'modal-footer',
          tags$a(href = '#', class = 'btn btn-primary tableinput-edit', 'OK'),
          tags$a(href = '#',
                 class = 'btn',
                 'data-dismiss' = 'modal',
                 'aria-hidden' = 'true',
                 'Cancel')
        )
          )
      )
    )
}

chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices)
        )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)

