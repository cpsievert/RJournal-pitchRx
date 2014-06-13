library(pitchRx)
library(XML2R)
if (sessionInfo()$otherPkgs$XML2R$Version != "0.0.4") message("Please install the newest version of XML2R")
if (sessionInfo()$otherPkgs$pitchRx$Version < 1) message("Please install the newest version of pitchRx")
#load other necessary packages
library(dplyr)
library(DBI)
library(parallel)
library(rgl)
library(animation)
library(mgcv)

###########################################################################################
#This is my recommended way to collect and store all data from 2008 to 2013 in a database:
#Note that by writing to database, memory will be less of an issue (thus, better run time)!
###########################################################################################
my_db <- src_sqlite("GamedayDB.sqlite3", create=TRUE)
#This collects and stores all PITCHf/x
scrape(start="2008-01-01", end="2014-01-01", connect=my_db$con)
#Now grab other 'complementary' files
files <- c("inning/inning_hit.xml", "miniscoreboard.xml", "players.xml")
scrape(start="2008-01-01", end="2014-01-01", suffix=files, connect=my_db$con)

###########################################################################################
#Here is a function that creates an SQLite database in the current directory, then
#collects and stores all data for a particular year in that database
#NOTE: If your machine has a lot of memory, you save some time by running store() in multiple R sessions concurrently!
#DISCLAIMER: If you do this, you will have 'annual' databases, and combining them into one database is not trivial.
###########################################################################################
store <- function(year="2008"){
  my_db <<- src_sqlite(paste0("Gameday", year, ".sqlite3"), create=TRUE)
  files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
              "miniscoreboard.xml", "players.xml")
  scrape(start=paste0(year, "-01-01"), end=paste0(year, "-12-31"), suffix=files, connect=my_db$con)
}
#Collects and stores data for 2008
store()
#Collects and stores data for 2009
store("2009")

###########################################################################################
#Generate example dataset from 'my_db' which is local and has data from 2008 to 2013
###########################################################################################
#First create some table indices for faster queries
dbSendQuery(my_db$con, "CREATE INDEX url_atbat ON atbat(url)")
dbSendQuery(my_db$con, "CREATE INDEX url_pitch ON pitch(url)") 
dbSendQuery(my_db$con, "CREATE INDEX pitcher_index ON atbat(pitcher_name)")
dbSendQuery(my_db$con, "CREATE INDEX des_index ON pitch(des)")
#Now build actual query
pitch11 <- tbl(my_db, sql("SELECT * FROM pitch WHERE pitch.url LIKE '%year_2011%'"))
atbat11 <- tbl(my_db, sql("SELECT * FROM atbat WHERE atbat.url LIKE '%year_2011%'"))
bats <- filter(atbat11, pitcher_name == "Mariano Rivera" | pitcher_name == "Phil Hughes")
FBs <- filter(pitch11, pitch_type == "FF" | pitch_type == "FC")
pitches <- collect(inner_join(FBs, bats))

###########################################################################################
#Grab all 'decisions' made from 2008 to 2013 (used to create figures)
###########################################################################################
pitch <- tbl(my_db, "pitch")
dez <- filter(select(pitch, px, pz, des, num, url), 
              des == "Called Strike" | des == "Ball")
atbat <- tbl(my_db, "atbat")
atbats <- select(atbat, inning_side, stand, p_throws, b_height, num, url)
decisions <- collect(inner_join(dez, atbats))
#write.csv(decisions, file="~/Desktop/RJournal-pitchRx/decisions.csv", row.names=FALSE)
#decisions <- read.csv("~/Desktop/RJournal-pitchRx/decisions.csv", header=TRUE, stringsAsFactors=FALSE)


#Formatting needed for fitting models
decisions$strike <- as.numeric(decisions$des %in% "Called Strike")
decisions$stance <- factor(decisions$stance)
decisions$p_throws <- factor(decisions$p_throws)
decisions$inning_side <- factor(decisions$inning_side)

################################################################################################
# Density plots

#function to create better labels for stand
relabel <- function(variable, value) {
  value <- sub("^R$", "Right-Handed Batter", value)
  sub("^L$", "Left-Handed Batter", value)
}

#figure 1
pdf(file="strikes.pdf", width=6.5, height=3.5)
strikes <- subset(decisions, strike == 1)
strikeFX(strikes, geom = "tile", layer = facet_grid(.~stand, labeller = relabel))+coord_equal()
dev.off()

#figure 2
pdf(file="strikesVSballs.pdf", width=6.5, height=3.5)
strikeFX(decisions, geom="tile", density1 = list(des="Called Strike"), density2 = list(des="Ball"), 
         layer=facet_grid(.~stand, labeller = relabel))+coord_equal()
dev.off()

################################################################################################
# GAMs
# Use multiple cores to fit gams. Code altered from Brian Mills' work - http://princeofslides.blogspot.com/2013/07/advanced-sab-r-metrics-parallelization.html
cl <- makeCluster(detectCores()-1)
m <- bam(strike ~ interaction(stand, p_throws) + 
              s(px, pz, by=interaction(stand, p_throws)), 
            data=decisions, family = binomial(link='logit'))
var_summary <- m$var.summary
#Save the model since it takes a while
#save(m, file="~/Desktop/RJournal-pitchRx/decisions3rdOrder.rda")
#save(var_summary, file="~/Desktop/RJournal-pitchRx/varSummary.rda")
#load("~/Desktop/RJournal-pitchRx/decisions3rdOrder.rda")
#load("~/Desktop/RJournal-pitchRx/varSummary.rda")

#figure 3
away <- list(inning_side = factor("bottom", levels=c("top", "bottom")))
m$var.summary <- modifyList(m$var.summary, away)
pdf(file="prob-strike.pdf", width=6.5, height=3.5)
strikeFX(decisions, model=m, layer=facet_grid(.~stand, labeller = relabel))+coord_equal()
dev.off()

#function to create better labels for both stand and p_throws
relabel2 <- function(variable, value) { 
  if (variable %in% "stand") 
    return(sub("^L$", "Left-Handed Batter", 
               sub("^R$", "Right-Handed Batter", value)))
  if (variable %in% "p_throws") 
    return(sub("^L$", "Left-Handed Pitcher", 
               sub("^R$", "Right-Handed Pitcher", value)))
}

#figure 4
pdf(file="prob-diff.pdf", width=6.5, height=3.5)
strikeFX(decisions, model = m, layer = facet_grid(p_throws~stand, labeller = relabel2),
         density1 = list(inning_side="top"), density2 = list(inning_side="bottom"))+coord_equal()
dev.off()

################################################################################################
# animation

data(pitches)

#figure 5 (saveHTML is for convenient viewing platform)
saveHTML(
  animateFX(pitches, layer=list(facet_grid(pitcher_name~stand, labeller = relabel), theme_bw(), coord_equal())),
  img.name = "ani1"
)

#figure 6
saveHTML(
  animateFX(pitches, avg.by="pitch_types", layer=list(facet_grid(pitcher_name~stand, labeller = relabel), theme_bw(), coord_equal())),
  img.name = "ani2"
)
################################################################################################
# interactive

Rivera <- subset(pitches, pitcher_name=="Mariano Rivera")
interactiveFX(Rivera, avg.by="pitch_types")
#save rgl parameters
#par3d("userMatrix", "windowRect", "zoom", "FOV")

#figure 7
viewa <- matrix(c(0.996876895427704, -0.0232012420892715, -0.0754866600036621, 0, 
                  -0.0762057304382324, -0.0318569354712963, -0.996583223342896, 0, 
                  0.0207172110676765, 0.999223232269287, -0.0335255265235901, 0, 
                  0, 0, 0, 1), nrow=4, ncol=4)
par3d(FOV=30, userMatrix=viewa, windowRect=c(0, 44, 920, 546), zoom=0.1227046)
rgl.snapshot(filename="rgl_a") 

#figure 8
viewb <- matrix(c(-0.997436463832855, 0.0657184273004532, -0.0283127501606941, 0,
                  -0.0372704453766346, -0.139356434345245, 0.989540934562683, 0, 
                  0.0610855147242546, 0.988059282302856, 0.141448378562927, 0, 
                  0, 0, 0, 1), nrow=4, ncol=4)
par3d(FOV=30, userMatrix=viewb, windowRect=c(0, 44, 213, 756), zoom=0.09614231)
rgl.snapshot(filename="rgl_b") 




