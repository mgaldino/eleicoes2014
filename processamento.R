## Modelo eleitoral



library("arm")
library("foreign")


setwd("D:\\2014\\modelo eleicoes\\exemplo")
#read in megapoll and attach
marriage.data <- read.dta("gay_marriage_megapoll.dta", convert.underscore = TRUE) 
head(marriage.data)
tail(marriage.data)

tmp <- subset(marriage.data , poll == "Gall2005Aug22")

#read in state-level dataset

Statelevel <- read.dta("state_level_update.dta",convert.underscore = TRUE)
Statelevel <- Statelevel[order(Statelevel$sstate.initnum),]
head(Statelevel)
#read in Census data
Census <- read.dta("poststratification 2000.dta",convert.underscore = TRUE)
Census <- Census[order(Census$cstate),]
Census$cstate.initnum <-  match(Census$cstate, Statelevel$sstate)
head(Census)

#Create index variables

#At level of megapoll

marriage.data$race.female <- (marriage.data$female *3) + marriage.data$race.wbh# from 1 for white males to 6 for hispanic females
marriage.data$age.edu.cat <- 4 * (marriage.data$age.cat -1) + marriage.data$edu.cat# from 1 for 18-29 with low edu to 16 for 65+ with high edu
marriage.data$p.evang.full <- Statelevel$p.evang[marriage.data$state.initnum]# proportion of evangelicals in respondent's state
marriage.data$p.mormon.full <-Statelevel$p.mormon[marriage.data$state.initnum]# proportion of mormon's in respondent's state
marriage.data$p.relig.full <- marriage.data$p.evang.full + marriage.data$p.mormon.full# combined evangelical + mormom proportions
marriage.data$p.kerry.full <- Statelevel$kerry.04[marriage.data$state.initnum]# kerry's % of 2-party vote in respondent's state in 2004

#At census level (same coding as above for all variables)

Census$crace.female <- (Census$cfemale *3) + Census$crace.WBH 
Census$cage.edu.cat <- 4 * (Census$cage.cat -1) + Census$cedu.cat 
Census$cp.evang.full<-  Statelevel$p.evang[Census$cstate.initnum]
Census$cp.mormon.full <- Statelevel$p.mormon[Census$cstate.initnum]
Census$cp.relig.full <- Census$cp.evang.full + Census$cp.mormon.full
Census$cp.kerry.full <-  Statelevel$kerry.04[Census$cstate.initnum]



#run individual-level opinion model

individual.model <- glmer
(formula = yes.of.all ~ (1|race.female) + (1|age.cat) 
                          + (1|edu.cat) + (1|age.edu.cat) + (1|state) + (1|region) + (1|poll) + p.relig.full 
                          + p.kerry.full,data=marriage.data, family=binomial(link="logit"))
display(individual.model)

#Full Bayes no run individual-level opinion model
## dropping NA for now
marriage.data1 <- marriage.data
marriage.data <- subset(marriage.data, !is.na(race.female))
marriage.data <- subset(marriage.data, !is.na(age.cat))
marriage.data <- subset(marriage.data, !is.na(edu.cat))

y <- marriage.data$yes.of.all
race.female <- marriage.data$race.female
age.cat <-  marriage.data$age.cat
edu.cat <-  marriage.data$edu.cat
age.edu.cat <-  marriage.data$age.edu.cat
state <-  as.numeric(as.factor(marriage.data$state)) -1
region <-  as.numeric(as.factor(marriage.data$region)) -1
poll <-  as.numeric(as.factor(marriage.data$poll)) -1
p.relig.full <-  marriage.data$p.relig.full
p.kerry.full <-  marriage.data$p.kerry.full

n.race.famle <- max(race.female)
n.age.cat <- max(age.cat)
n.edu.cat <- max(edu.cat)
n.age.edu.cat <- max(age.edu.cat )
n.state <- max(state )
n.region <- max(region )
n.poll <- max(poll )

dataList <- list(
  N = length(y) ,
  n.poll,
  n.state,  
  y,
  race.female,
  age.cat,
  edu.cat,
  age.edu.cat,
  state,
  region,
  poll,
  p.relig.full,
  p.kerry.full
)

setwd("D:/2014/modelo eleicoes/scripts/eleicoes2014")
foo <- jags.model(file='mrp1.bug',
                  data=dataList)
nstore <- 5e3
thin <- 10
fun <- coda.samples(foo,
                    n.iter=nstore*thin,
                    thin=thin,
                    variable.names=c('y','a', 'b'))

#examine random effects and standard errors for race-female
ranef(individual.model)$race.female
se.ranef(individual.model)$race.female

#create vector of state ranefs and then fill in missing ones
state.ranefs <- array(NA,c(51,1))
dimnames(state.ranefs) <- list(c(Statelevel$sstate),"effect")
for(i in Statelevel$sstate){
  state.ranefs[i,1] <- ranef(individual.model)$state[i,1]
}
state.ranefs[,1][is.na(state.ranefs[,1])] <- 0 #set states with missing REs (b/c not in data) to zero


#create a prediction for each cell in Census data
cellpred <- invlogit(fixef(individual.model)["(Intercept)"]
                     +ranef(individual.model)$race.female[Census$crace.female,1]
                     +ranef(individual.model)$age.cat[Census$cage.cat,1]
                     +ranef(individual.model)$edu.cat[Census$cedu.cat,1]
                     +ranef(individual.model)$age.edu.cat[Census$cage.edu.cat,1]
                     +state.ranefs[Census$cstate,1]
                     +ranef(individual.model)$region[Census$cregion,1]   
                     +(fixef(individual.model)["p.relig.full"] *Census$cp.relig.full)
                     +(fixef(individual.model)["p.kerry.full"] *Census$cp.kerry.full)
)

P(y = 1) = invlogit(a + bx)
a = 10
b= 2

#weights the prediction by the freq of cell                                       
cellpredweighted <- cellpred * Census$cpercent.state

#calculates the percent within each state (weighted average of responses)
statepred <- 100* as.vector(tapply(cellpredweighted,Census$cstate,sum))
statepred


### Processando dados

library("foreign")

setwd("D:\\2014\\modelo eleicoes\\dados")

## data primeiro poll Data: 17 e 18/09/98
df1 <- read.spss("00870.SAV", to.data.frame=TRUE, reencode='UTF-8')
head(df1)
names(df1)[6:13] <- c("votoEspont", "votoEstim", "NumCandidato", "votoEstim2Turno", 
                         "rejeicao1", "rejeicao2", "rejeicao3", "rejeicao4")

# df1[duplicated(df1$NQUEST), ]
# 
# subset(df1, NQUEST == 1431)

names(df1)[21] <- "avaliacaoQualiIncumbente"
names(df1)[22] <- "avaliacaoQuantIncumbente"
df1$instituto <- "DataFolha"
df1$dataPesquisaInit <- as.Date("17/09/98", "%d/%m/%y")
df1$dataPesquisaFim <- as.Date("18/09/98", "%d/%m/%y")
df1$pesquisa <- "DataFolha9817Set"
df1$anoPesquisa <- 1998
df1$id <- 1:nrow(df1)
head(df1)

df1Final <- df1[ , c(colnames(df1[3:13]), "avaliacaoQualiIncumbente", "avaliacaoQuantIncumbente",
                       "PARTIDO", "ESCOLA", "RENDAF", "REGIAO", "PESOEST",
                       "instituto" , "dataPesquisaInit", "pesquisa", "anoPesquisa", "id")]

df2 <- read.spss("00873.SAV", to.data.frame=TRUE, reencode='UTF-8')
head(df2)
names(df2)[c(6:8, 10:14)] <- c("votoEspont", "votoEstim", "NumCandidato", "votoEstim2Turno", 
                              "rejeicao1", "rejeicao2", "rejeicao3", "rejeicao4")

names(df2)[21] <- "avaliacaoQualiIncumbente"
names(df2)[22] <- "avaliacaoQuantIncumbente"
df2$instituto <- "DataFolha"
df2$pesquisa <- "DataFolha98ddMes"
df2$dataPesquisaInit <- NA
df2$dataPesquisaFim <- NA
df2$anoPesquisa <- 1998
df2$id <- 1:nrow(df2)

df2Final <- df2[, c(colnames(df2[1:5]), "votoEspont", "votoEstim",
                             "NumCandidato", "votoEstim2Turno", 
                    "rejeicao1", "rejeicao2", "rejeicao3", "rejeicao4",
                    "avaliacaoQualiIncumbente", "avaliacaoQuantIncumbente",
                    "PARTIDO", "ESCOLA", "RENDAF", "REGIAO", 
                    "instituto", "dataPesquisaInit", "pesquisa", "anoPesquisa", "id")]

df3 <- read.spss("BD_CIS0156.sav", to.data.frame=TRUE, reencode='UTF-8')
df3$instituto <- "DataFolha"
df3$dataPesquisaInit <- as.Date("08/08/98", "%d/%m/%y")
df3$dataPesquisaFim <- as.Date("09/08/98", "%d/%m/%y")
df3$pesquisa <- "DataFolha9808Ago"
df3$anoPesquisa <- 1998
df3$id <- 1:nrow(df3)
names(df3)[ c(3:6, 9,16, 17,18,19, 20)  ] <- c("SEXO","IDADE1", "IDADE", "votoEspont", "votoEstim",  "votoEstim2Turno", 
                                           "rejeicao1", "rejeicao2", "rejeicao3", "rejeicao4")
names(df3)[39] <- "avaliacaoQualiIncumbente"
names(df3)[40] <- "avaliacaoQuantIncumbente"

names(df3)[77] <- "ESCOLA"
names(df3)[81] <- "RENDAF_ESPONT"
names(df3)[82]<- "RENDAF"
names(df3)[84]<- "REGIAO"

df3Final <- df3[, c("SEXO","IDADE1", "IDADE", "votoEspont", "votoEstim", "votoEstim2Turno", 
                    "rejeicao1", "rejeicao2", "rejeicao3", "rejeicao4",
                    "avaliacaoQualiIncumbente", "avaliacaoQuantIncumbente",
                    "ESCOLA", "RENDAF", "REGIAO", "instituto",
                    "dataPesquisaInit", "pesquisa", "anoPesquisa", "id") ]


# df4 <- read.spss("BD_CIS0154_Brasil.sav", to.data.frame=TRUE, reencode='UTF-8')
# df4$instituto <- "DataFolha"
# df4$dataPesquisaInit <- as.Date("17/09/98", "%d/%m/%y")
# df4$dataPesquisaFim <- as.Date("17/09/98", "%d/%m/%y")


df5 <- read.spss("BD_CIS0149.sav", to.data.frame=TRUE, reencode='UTF-8')
df5$instituto <- "DataFolha"
df5$dataPesquisaInit <- as.Date("14/09/98", "%d/%m/%y")
df5$dataPesquisaFim <- as.Date("15/09/98", "%d/%m/%y")
df5$pesquisa <- "DataFolha9814Set"


## PRecisamos criarum banco único
## cada linha é uma resposta
## e transformar categóricas em colunas
## pra usar no Jags
## Banco fica poll-respondente

head(df1)
dataPoll98 <- rbind(df1, df2, df3, df5)
