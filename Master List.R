library(httr)
library(base64enc)
library(jsonlite)
library(data.table)
##########################################
###Set up keys for Engagement History API#
##########################################

key <-'[key]'
secret <-'[secret]'
tokenURL <- 'null'
accessTokenURL <- 'null'
authorizeURL <- 'null'
#oauth_app is defining a service (we are defining our own LP service), lpR makes the token unique
ehapi <- oauth_app('lpR',key,secret)
#getting our tokens, we aren't doing anything with it because we already have the token
EHAPI <- oauth_endpoint(ehtokenURL,ehauthorizeURL,ehaccessTokenURL)

#token <- oauth1.0_token(fitbit,fbr), this is generating the token, we are hard coding the token information
sig <- sign_oauth1.0(ehapi,
                     token="[token]",
                     token_secret="[tokensecret]"
)
#JSON formatted body for the resonse, if you need two quotes, use the opposite for the first set
coreurl <- "https://va.enghist.liveperson.net/interaction_history/api/account/[siteid]/interactions/search?offset="
#offset and count are used in our while loop to make sure we pull all that data for the date range we defined.
#offset will tell the return which record to start pulling from in the call
#the EH API has a limit of 100 records at a time
offset = 0
count = 1
#append flag is used down below to determine if we should create a dataframe for the chatinfo or append to an existing df
appendflag= FALSE
#set up masterdf which will contain all the lines
mstrchatdf=data.frame()
#flag used in for loop to set the DF column names
columnflag=1
while (offset<count){
  posturl =paste(coreurl,offset,"&limit=100", sep="")
  body <- '{"start":{"from":1447218000000,"to":1447390799000}}'
  test2 <- POST(posturl, 
                sig,
                body=body,
                add_headers (
                  "Content-Type" = "application/json"))
  ehdata<-content(test2, "parsed")
  count = ehdata$`_metadata`$count
  offset = offset+100
  #create baseline number of records to compare against in the code below, compare unique engagementIds between chatinfo and mstrchatdf
  if (appendflag== FALSE){
    df2=rbindlist((lapply(ehdata$interactionHistoryRecords, function(x){
      x$info})), fill=TRUE)
    appendflag=TRUE
  } else {
    dftemp= rbindlist((lapply(ehdata$interactionHistoryRecords, function(x){
      x$info})), fill=TRUE)
    df2=merge.data.frame(dftemp,df2, all=TRUE)
    rm(dftemp)
  }
  #loop through the engagement history record API and set up a sublist of values we will use to create the mstrchatdf
  for (i in 1:length(ehdata$interactionHistoryRecords)){
    chatinfo=ehdata$interactionHistoryRecords[[i]]
    info = rbind.data.frame(chatinfo$info)
    #set variable to copy engagementid, visitorId and agentID to all columns in the dataframe. Engagement and visitor Ids are used
    #to determine the unique records in the dataframe and agentID will be used for us to merge agent info with the records
    engid=info[1,'engagementId']
    visid=info[1,"visitorId"]
    agentid=info[1,"agentId"]
    clines=rbindlist((lapply(chatinfo$transcript$lines, function(x){
      x})), fill=TRUE)
    prechat= rbindlist((lapply(chatinfo$surveys$preChat, function(x){
      x})), fill=TRUE)
    postchat= rbindlist((lapply(chatinfo$surveys$postChat, function(x){
      x})), fill=TRUE)
    operator=rbindlist((lapply(chatinfo$surveys$operator, function(x){
      x})), fill=TRUE)
    #merge dataframes together if there is data in it, I always expect there to be data in the info and lines table
    if(length(info)>=1){
      chatinfodf2=merge.data.frame(clines,info,all=T)}
    if(length(postchat)>=1){
      chatinfodf2=merge.data.frame(postchat,chatinfodf2, all=T)}
    if(length(prechat)>=1){
      chatinfodf2=merge.data.frame(prechat,chatinfodf2, all=T)}
    if(length(operator)>=1){
      chatinfodf2=merge.data.frame(operator,chatinfodf2, all=T)
    }
    chatinfodf2=data.table(chatinfodf2)
    #append engagementid, visitorid, and agentID to all columns in the dataframe
    chatinfodf2[,"engagementId"]<-engid
    chatinfodf2[,"visitorId"]<-visid
    chatinfodf2[,"agentId"]<-agentid
    chatinfodf2=data.frame(chatinfodf2)
    if(columnflag==1){
      mstrchatdf=merge.data.frame(chatinfodf2,mstrchatdf, all=TRUE)
      columnflag=2}
    mstrchatdf=merge.data.frame(chatinfodf2,mstrchatdf, all=TRUE)
    print(i) 
  }
}
#Start code to get the operator name dataframe created
key2 <-'[key]'
secret2 <-'[secret]'
tokenURL <- 'null'
accessTokenURL <- 'null'
authorizeURL <- 'null'
#oauth_app is defining a service (we are defining our own LP service), lpR makes the token unique
lpopapi <- oauth_app('lpopR',key2,secret2)
#getting our tokens, we aren't doing anything with it because we already have the token
OPAPI <- oauth_endpoint(ehtokenURL,ehauthorizeURL,ehaccessTokenURL)

sig <- sign_oauth1.0(lpopapi,
                     token="[token]",
                     token_secret="[secret]"
)

posturl <- "https://sales.liveperson.net/api/account/[siteId]/operators?v=1"
body <- ''
opinfo <- GET(posturl, 
              sig,
              body=body,
              add_headers (
                "Content-Type" = "application/json"))
operatorinfo<-content(opinfo, "parsed")
#collapse operatorinfo list into a df
operatorinfodf=do.call(rbind,lapply(operatorinfo,as.data.frame.list,stringsAsFactors=FALSE))
#change agentID to a format that can be used to join the operatorinfodf to it to compile the agent IDs
mstrchatdf$agentId=as.integer(as.character(mstrchatdf$agentId))
#df2 is used to get the chatinfodf to contain the agent information
df2$agentId= as.integer(as.character(df2$agentId))
#convert dataframes to datatables and set a key value for joining
mstrchatdf=data.table(mstrchatdf, key="agentId")
df2t= data.table(df2, key="agentId")
operatorinfot2=data.table(operatorinfodf, key="id")
#joining the data together and create a new datafame
chatinfodf=operatorinfot2[df2t]
#test=operatorinfot2[mstrchatdf]
mstrchatdf=operatorinfot2[mstrchatdf]
mstrchatdf<-mstrchatdf[order(mstrchatdf$engagementId,mstrchatdf$duration,mstrchatdf$timeL),]

#remove all dataframes
rm(df2)
rm(df2t)
rm(operatorinfot2)
rm(operatorinfodf)
rm(clines)
rm(info)
rm(operator)
rm(postchat)
rm(prechat)
rm(chatinfodf2)
