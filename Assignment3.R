#source("packages.r")
library(stringr)

mystring <- "Pasito a pasito, suave suavecito \nNos vamos pegando poquito a poquito"
str_extract_all(mystring,".+") # in breaks by lines only
str_extract_all(mystring,".*") # in breaks by lines counting spaces

mystring <- "1203 .rgnargn 667 1203$"
str_extract_all(mystring,"[0-9]+\\$")


mystring <- "Paso a paso, suave suavecito \nNos vamos pegando poco a poco"
str_extract_all(mystring,"\\b[a-z]{1,4}\\b")


mystring <- "Pasito a pasito, suave suavecito \nNos vamos pegando poquito a poquito.txt"
str_extract_all(mystring,".*?\\.txt$")

mystring <- "12323/35/8351iwuhth dhdgh/23893/24132/"
str_extract(mystring,"\\d{2}/\\d{2}/\\d{4}")

mystring <- c("18/07/1984","1","Two","<tan>.</tan>","<a>whateever I want will show</a>"," <crazytag>This is an HTML element</crazytag>")
str_extract(mystring,"<(.+?)>.+?</\\1>")

email <- "datalover89[at]aol[dot]com"
email <- sub(pattern = "\\[at\\]", replacement = "@", x = email)
email <- sub(pattern = "\\[dot\\]", replacement = "\\.", x = email)
email



unlist(str_extract_all(email, ".+(?=@)"))
unlist(str_extract_all(email, "(?<=@).+"))
unlist(str_extract(unlist(str_extract_all(email, "(?<=@).+")), "[a-z]{3}"))
unlist(str_extract(unlist(str_extract_all(email, "(?<=@).+")), "(?<=\\.).+"))
unlist(str_extract(unlist(str_extract_all(email, "(?<=@).+")), ".+(?=\\.)"))
unlist(str_extract_all(email, ".+(?=@)|[[:alpha:]].+(?=\\.)"))


secret <- "clcopCow1zmstc0d87wnkig7OvdicpNuggvhryn92Gjuwczi8hqrfpRxs5Aj5dwpn0TanwoUwisdij7Lj8kpf03AT5Idr3coc0bt7yczjatOaootj55t3Nj3ne6c4Sfek.r1w1YwwojigOd6vrfUrbz2.2bkAnbhzgv4R9i05zEcrop.wAgnb.RqoE65fGEa1otfb7wXm24k.6t3sH9zqe5fy89n6Ed5t9kc4fR905gmc4Ogxo5nhk!gr"
unlist(str_extract_all(secret, "[:upper:]|[:punct:]"))

library(readr)
trump_df <- read_csv("https://raw.githubusercontent.com/Currie32/Tweet-Like-Trump/master/tweets.csv")

trump_df <- trump_df[,3]

str(trump_df)
trump_df <- na.omit(trump_df)
trump_df[["Tweet_Text"]][1:10]
trump_df$Tweet_Text[1:10]

str_detect(trump_df$Tweet_Text[1:10], "(?<=@).+")
#When he is tagging someone
taggedtweet<- filter(trump_df, str_detect(trump_df$Tweet_Text, "(?<=@).+"))
taggedHillary<- filter(taggedtweet, str_detect(taggedtweet$Tweet_Text, "(?<=@)HillaryClinton"))
taggedSons<- filter(taggedtweet, str_detect(taggedtweet$Tweet_Text, "(?<=@)EricTrump|(?<=@)DonaldJTrumpJr|(?<=@)IvankaTrump"))
taggedHimself<- filter(taggedtweet, str_detect(taggedtweet$Tweet_Text, "(?<=@)realDonaldTrump"))

totaltweets <- nrow(trump_df)
taggs <- nrow(taggedtweet)
TagHillary <- nrow(taggedHillary)
TagSons <- nrow(taggedSons)
TagHimself <- nrow(taggedHimself)

taggs/totaltweets
TagHimself/totaltweets
TagHimself/taggs
TagHillary/totaltweets
TagHillary
#Clinton <- as.data.frame(str_match(trump_df$Tweet_Text, "HillaryClinton|Hillary Clinton" ))


Clinton <- as.data.frame(str_match(trump_df$Tweet_Text, "[Hh][Ii][a-zA-Z]{1,}[Rr][Yy][Cc][Ll][a-zA-Z]{1,}[Oo][Nn]|[Hh][Ii][a-zA-Z]{1,}[Rr][Yy] [Cc][Ll][a-zA-Z]{1,}[Oo][Nn]" ))

HCmentions <- nrow (na.omit(str_match(trump_df$Tweet_Text, "[Hh][Ii][a-zA-Z]{1,}[Rr][Yy][Cc][Ll][a-zA-Z]{1,}[Oo][Nn]|[Hh][Ii][a-zA-Z]{1,}[Rr][Yy] [Cc][Ll][a-zA-Z]{1,}[Oo][Nn]" )))

HCmentions
TagHillary

TagHillary/HCmentions
1/(TagHillary/HCmentions)

wordlength <-str_count(unlist(str_extract_all(trump_df$Tweet_Text,"\\w+")))
summary(wordlength)
sd(wordlength)

hist(wordlength, 
     main="Trump's words", 
     xlab="Word Lenght", 
     border="black", 
     col="purple",
     xlim=c(1,15),
     breaks=50)
# failed attempts - I wasnt able to filter things that didn't have clinton in there.
# Hillary <- as.data.frame(str_match(trump_df$Tweet_Text, "[Hh][Ii][a-zA-Z]{1,}[Rr][Yy]" ))
# 
# Clinton <- na.omit(Clinton)
# filter(Clinton, str_detect(Clinton$V1,"[^[Cc][Ll][a-zA-Z]{1,}[Oo][Nn]]"))
# 
# Hillary <- na.omit(Hillary)
# Hillary
# 
# 
# nrow(Clinton)
# 
# head(Clinton,50)
