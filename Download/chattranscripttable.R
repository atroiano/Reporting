library(httr)
library(base64enc)
library(jsonlite)
library(data.table)
library(svGUI)
library(svDialogs)
library(qdap)
library(plyr)
#allow the key to cache, if this lines doesn't exist the function won't run
options(httr_oauth_cache=T)

#set up inputs for our function to query the EH API#
consumerKey= "<value>"
consumerSecret= "<value>"
apptoken="<value>"
appsecret= "<value>"
accountnumber="<value>"
#URL up to the /account part, the rest is built within our function
uri<-"<value"

ehquery=function(a,b,c,d,e,f,g,h){
  key <- a
  secret <- b
  mytoken<- c
  mysecret<-d
  tokenURL <- 'null'
  accessTokenURL <- 'null'
  authorizeURL <- 'null'
  #oauth_app is defining a service (we are defining our own LP service), lpR makes the token unique
  ehapi <- oauth_app('ehapi',key,secret)
  #getting our tokens, we aren't doing anything with it because we already have the token
  EHAPI <- oauth_endpoint(tokenURL,authorizeURL,accessTokenURL)
  #token <- oauth1.0_token(fitbit,fbr), this is generating the token, we are hard coding the token information

  sig <- sign_oauth1.0(ehapi,
                       token=mytoken,
                       token_secret=mysecret
  )
  #JSON formatted body for the resonse, if you need two quotes, use the opposite for the first set
  urlstart<-f
  myaccountnumber <- e
  remainingurl<-"/interactions/search?offset="
  coreurl <- paste(urlstart,myaccountnumber,remainingurl,sep="")
  #offset and count are used in our while loop to make sure we pull all that data for the date range we defined
  #the EH API has a limit of 100 records at a time
  offset = 0
  count = 1
  while (offset<count){
    posturl =paste(coreurl,offset,"&limit=100", sep="")
    bodyopen <- '{\"start\":{\"from\":'
    bodystart <-g
    bodymid<-',"to":'
    bodyend<-h
    bodyclose<-'}}'
    body<-paste(bodyopen,bodystart,bodymid,bodyend,bodyclose, sep="")
    print(body)
    ehdata <- POST(posturl, 
                  sig,
                  body=body,
                  add_headers (
                    "Content-Type" = "application/json"))
    ehdata2<-content(ehdata, "parsed")
    count = ehdata2$`_metadata`$count
    offset = offset+100
  
  #Begin chatinfo loop
    for (i in 1:length(ehdata2$interactionHistoryRecords)){
        if (exists("chatinfodt")==F){
        chatinfo=ehdata2$interactionHistoryRecords[[i]]
        chatinfodt = as.data.table(t(sapply(chatinfo$info, unlist)))
        setkeyv(chatinfodt, colnames(chatinfodt))
        } else {
          chatinfo=ehdata2$interactionHistoryRecords[[i]]
          newchatinfodatadt = as.data.table(t(sapply(chatinfo$info, unlist)))
          engagementId=chatinfo$info$engagementId
          setkeyv(chatinfodt, colnames(chatinfodt))
          setkeyv(newchatinfodatadt, colnames(newchatinfodatadt))
          chatinfodt<-merge(chatinfodt,newchatinfodatadt, all=T)
        }
      #End Chat Info Loop
      
      #Begin Prechat Survey Loop
      for (j in 1:length(chatinfo$surveys$preChat)){
        if (exists("surveydatadt")==F){
          surveydatalist=c(chatinfo$info$engagementId,
                           chatinfo$surveys$preChat[[j]]$displayName, 
                           chatinfo$surveys$preChat[[j]]$value)
          surveynameslist=c("engagementId",paste(chatinfo$surveys$preChat[[j]]$displayName, sep=" "),paste(chatinfo$surveys$preChat[[j]]$displayName, chatinfo$surveys$preChat[[j]]$questionID,sep=" "))
          names(surveydatalist)=surveynameslist
          surveydatadt<-as.data.table(t(melt(surveydatalist)))
          setkeyv(surveydatadt, colnames(surveydatadt))
        } else {
          surveydatalist=c(chatinfo$info$engagementId,
                           chatinfo$surveys$preChat[[j]]$displayName, 
                           chatinfo$surveys$preChat[[j]]$value)
          surveynameslist=c("engagementId",paste(chatinfo$surveys$preChat[[j]]$displayName, sep=" "),paste(chatinfo$surveys$preChat[[j]]$displayName, chatinfo$surveys$preChat[[j]]$questionID,sep=" "))
          names(surveydatalist)=surveynameslist
          newsurveydatadt<-as.data.table(t(melt(surveydatalist)))
          setkeyv(surveydatadt,colnames(surveydatadt))
          setkeyv(newsurveydatadt, colnames(newsurveydatadt))
          surveydatadt=merge(surveydatadt,newsurveydatadt,all=T)
        }
        chatinfodt<-chatinfodt[,setdiff(colnames(surveydatadt),colnames(chatinfodt)) := as.character('NA')]
        engagementrow=which(chatinfodt$engagementId==chatinfo$info$engagementId)
        setcolorder(surveydatadt,intersect(colnames(chatinfodt),colnames(surveydatadt)))
        #fullchatinfodt<-chatinfodt[,setdiff(colnames(surveydatadt),colnames(chatinfodt)) := as.character('NA')]
        setcolorder(surveydatadt,intersect(colnames(chatinfodt),colnames(surveydatadt)))
        chatinfodt<-chatinfodt[engagementrow,intersect(colnames(chatinfodt),colnames(surveydatadt)):=surveydatadt]
        rm(surveydatadt)
        rm(newsurveydatadt)
      }
    #End preChat Survey loop
      
  #Begin postChat Survey boiler plate code
      for (j in 1:length(chatinfo$surveys$postChat)){
        if (exists("surveydatadt")==F){
          surveydatalist=c(chatinfo$info$engagementId,
                           chatinfo$surveys$postChat[[j]]$displayName, 
                           chatinfo$surveys$postChat[[j]]$value)
          surveynameslist=c("engagementId",paste(chatinfo$surveys$postChat[[j]]$displayName, sep=""),paste(chatinfo$surveys$postChat[[j]]$displayName,chatinfo$surveys$postChat[[j]]$questionID, sep=" "))
          names(surveydatalist)=surveynameslist
          surveydatadt<-as.data.table(t(melt(surveydatalist)))
          setkeyv(surveydatadt, colnames(surveydatadt))
        } else {
          surveydatalist=c(chatinfo$info$engagementId,
                           chatinfo$surveys$postChat[[j]]$displayName, 
                           chatinfo$surveys$postChat[[j]]$value)
          surveynameslist=c("engagementId",paste(chatinfo$surveys$postChat[[j]]$displayName, sep=""),paste(chatinfo$surveys$postChat[[j]]$displayName,chatinfo$surveys$postChat[[j]]$questionID, sep=" "))
          names(surveydatalist)=surveynameslist
          newsurveydatadt<-as.data.table(t(melt(surveydatalist)))
          setkeyv(surveydatadt,colnames(surveydatadt))
          setkeyv(newsurveydatadt, colnames(newsurveydatadt))
          surveydatadt=merge(surveydatadt,newsurveydatadt, all=T)
        }
        chatinfodt<-chatinfodt[,setdiff(colnames(surveydatadt),colnames(chatinfodt)) := as.character('NA')]
        engagementrow=which(chatinfodt$engagementId==chatinfo$info$engagementId)
        setcolorder(surveydatadt,intersect(colnames(chatinfodt),colnames(surveydatadt)))
        #fullchatinfodt<-chatinfodt[,setdiff(colnames(surveydatadt),colnames(chatinfodt)) := as.character('NA')]
        setcolorder(surveydatadt,intersect(colnames(chatinfodt),colnames(surveydatadt)))
        chatinfodt<-chatinfodt[engagementrow,intersect(colnames(chatinfodt),colnames(surveydatadt)):=surveydatadt]
        rm(surveydatadt)
        rm(newsurveydatadt)
      }
      #End  postChat Survey Loop
      
      #Begin operator Survey boiler plate
      for (j in 1:length(chatinfo$surveys$operator)){
        if (exists("surveydatadt")==F){
          surveydatalist=c(chatinfo$info$engagementId,
                           chatinfo$surveys$operator[[j]]$displayName, 
                           chatinfo$surveys$operator[[j]]$value)
          surveynameslist=c("engagementId",paste(chatinfo$surveys$operator[[j]]$displayName, sep=""),paste(chatinfo$surveys$operator[[j]]$displayName, chatinfo$surveys$operator[[j]]$questionID, sep=" "))
          names(surveydatalist)=surveynameslist
          surveydatadt<-as.data.table(t(melt(surveydatalist)))
          setkeyv(surveydatadt, colnames(surveydatadt))
        } else {
          surveydatalist=c(chatinfo$info$engagementId,
                           chatinfo$surveys$operator[[j]]$displayName, 
                           chatinfo$surveys$operator[[j]]$value)
          surveynameslist=c("engagementId",paste(chatinfo$surveys$operator[[j]]$displayName, sep=" "),paste(chatinfo$surveys$operator[[j]]$displayName, chatinfo$surveys$operator[[j]]$questionID, sep=" "))
          names(surveydatalist)=surveynameslist
          newsurveydatadt<-as.data.table(t(melt(surveydatalist)))
          setkeyv(surveydatadt,colnames(surveydatadt))
          setkeyv(newsurveydatadt, colnames(newsurveydatadt))
          surveydatadt=merge(surveydatadt,newsurveydatadt, all=T)
        }
        chatinfodt<-chatinfodt[,setdiff(colnames(surveydatadt),colnames(chatinfodt)) := as.character('NA')]
        engagementrow=which(chatinfodt$engagementId==chatinfo$info$engagementId)
        setcolorder(surveydatadt,intersect(colnames(chatinfodt),colnames(surveydatadt)))
        #fullchatinfodt<-chatinfodt[,setdiff(colnames(surveydatadt),colnames(chatinfodt)) := as.character('NA')]
        setcolorder(surveydatadt,intersect(colnames(chatinfodt),colnames(surveydatadt)))
        chatinfodt<-chatinfodt[engagementrow,intersect(colnames(chatinfodt),colnames(surveydatadt)):=surveydatadt]
        rm(surveydatadt)
        rm(newsurveydatadt)
      }
      #End operator Survey loop
      
      #Begin Transcript Loop
      for (j in 1:length(chatinfo$transcript$lines)){
        if (exists("transcriptdatalist")==F){
          transcriptdatalist=c(chatinfo$info$engagementId,
                               chatinfo$transcript$lines[[j]]$text)
          transcrpitnamelist=c("engagementId","chatlines")
        } else {
          transcriptdatalist=c(chatinfo$info$engagementId,
                               paste(transcriptdatalist[[2]],chatinfo$transcript$lines[[j]]$text, sep=" "))
          names(transcriptdatalist)=transcrpitnamelist
        }}
        transcriptdt<-as.data.table(t(melt(transcriptdatalist)))
        setkeyv(transcriptdt,colnames(transcriptdt))
        engagementrow=which(chatinfodt$engagementId==chatinfo$info$engagementId)
        chatinfodt<-chatinfodt[,setdiff(colnames(transcriptdt),colnames(chatinfodt)) := as.character('NA')]
        setcolorder(transcriptdt,intersect(colnames(chatinfodt),colnames(transcriptdt)))
        chatinfodt<-chatinfodt[engagementrow,intersect(colnames(chatinfodt),colnames(transcriptdt)):=transcriptdt]
        rm(transcriptdt)
        rm(transcriptdatalist)
      #End Transcript code
      
      
      #Being UDE import
      udeinfodt<-as.data.table(unlist(t(sapply(chatinfo$sdes, unlist))))
      if (length(udeinfodt)>0){
      udeinfodt<-udeinfodt[,unique(names(udeinfodt)), with=F]
      udeinfodt<-udeinfodt[,"engagementId":= chatinfo$info$engagementId]
      chatinfodt<-chatinfodt[,setdiff(colnames(udeinfodt),colnames(chatinfodt)) := as.character('NA')]
      setcolorder(udeinfodt,intersect(colnames(chatinfodt),colnames(udeinfodt)))
      engagementrow=which(chatinfodt$engagementId==chatinfo$info$engagementId)
      chatinfodt<-chatinfodt[engagementrow,intersect(colnames(chatinfodt),colnames(udeinfodt)):=udeinfodt]
      }
       }
    
    #strip out HTML from text column of our DF
    chatinfodt$chatlines=bracketX(chatinfodt[,chatlines],"angle")

  }
  return(chatinfodt)
  }
 
mstrchatdf<-ehquery(consumerKey,consumerSecret,apptoken,appsecret, accountnumber)