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
                     token_secret="[token_secret]"
)

posturl <- "https://sales.liveperson.net/api/account/[siteid]/operators?v=1"
body <- ''
opinfo <- GET(posturl, 
              sig,
              body=body,
              add_headers (
                "Content-Type" = "application/json"))
agentinfo<-content(opinfo, "parsed")

agentinfodf=do.call(rbind,lapply(agentinfo,as.data.frame.list,stringsAsFactors=FALSE))
linksinfo=data.frame()  

#get the operator information details from the API, based on the results from our first query
operatorinformationdf= do.call(rbind.fill,lapply(agentinfodf$links.href,function(x){
  key2 <-'[key]'
  secret2 <-'[secret]'
  tokenURL <- 'null'
  accessTokenURL <- 'null'
  authorizeURL <- 'null'
  #oauth_app is defining a service (we are defining our own LP service), lpR makes the token unique
  lpopapi <- oauth_app('lpopR',key2,secret2)
  #getting our tokens, we aren't doing anything with it because we already have the token
  #OPAPI <- oauth_endpoint(ehtokenURL,ehauthorizeURL,ehaccessTokenURL)
  
  sig <- sign_oauth1.0(lpopapi,
                       token="[token]",
                       token_secret="[token secret]")
  posturl <- paste(x,"?v=1",sep="")
  body <- ''
  opinfo <- GET(posturl, 
                sig,
                body=body,
                add_headers (
                  "Content-Type" = "application/json"))
  agentinfodetails<-content(opinfo, "parsed")
  agentinfodetails=data.table(t(unlist(agentinfodetails)))
}))

#start skill API
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
                     token_secret="[token secret]"
)

posturl <- "https://sales.liveperson.net/api/account/[siteid]/skills?v=1"
body <- ''
opinfo <- GET(posturl, 
              sig,
              body=body,
              add_headers (
                "Content-Type" = "application/json"))
skillinfo<-content(opinfo, "parsed")
skillinfodf=do.call(rbind,lapply(skillinfo,as.data.frame.list,stringsAsFactors=FALSE))

#go through and match the skill names with the IDs
operatorinformationdf$skills1=skillinfodf$name[match(operatorinformationdf$skills1,skillinfodf$id)]
operatorinformationdf$skills2=skillinfodf$name[match(operatorinformationdf$skills2,skillinfodf$id)]
operatorinformationdf$skills3=skillinfodf$name[match(operatorinformationdf$skills3,skillinfodf$id)]
operatorinformationdf$skills4=skillinfodf$name[match(operatorinformationdf$skills4,skillinfodf$id)]
operatorinformationdf$skills5=skillinfodf$name[match(operatorinformationdf$skills5,skillinfodf$id)]
operatorinformationdf$skills6=skillinfodf$name[match(operatorinformationdf$skills6,skillinfodf$id)]
operatorinformationdf$skills7=skillinfodf$name[match(operatorinformationdf$skills7,skillinfodf$id)]
operatorinformationdf$skills8=skillinfodf$name[match(operatorinformationdf$skills8,skillinfodf$id)]
operatorinformationdf$skills9=skillinfodf$name[match(operatorinformationdf$skills9,skillinfodf$id)]
operatorinformationdf$skills10=skillinfodf$name[match(operatorinformationdf$skills10,skillinfodf$id)]
operatorinformationdf$skills11=skillinfodf$name[match(operatorinformationdf$skills11,skillinfodf$id)]
operatorinformationdf$skills12=skillinfodf$name[match(operatorinformationdf$skills12,skillinfodf$id)]
operatorinformationdf$skills13=skillinfodf$name[match(operatorinformationdf$skills13,skillinfodf$id)]
operatorinformationdf$skills14=skillinfodf$name[match(operatorinformationdf$skills14,skillinfodf$id)]
operatorinformationdf$skills15=skillinfodf$name[match(operatorinformationdf$skills15,skillinfodf$id)]

#write to a csv file
write.csv(file="/Outfile4.csv", x=operatorinformationdf)
