library(pitchRx)
# This script requires pitchRx 1.5
if (packageVersion("pitchRx") < 1.5) { devtools::install_github("cpsievert/pitchRx"); library(pitchRx) }
library(dplyr)
library(DBI)
library(magrittr)

# This database was created using the technique described here -- http://baseballwithr.wordpress.com/2014/03/24/422/
db <- src_sqlite("~/pitchfx/pitchRx.sqlite3")

dbSendQuery(db$con, "CREATE INDEX des_index ON pitch(des)")
dbSendQuery(db$con, "CREATE INDEX pitcher_index ON atbat(pitcher_name)")
dbSendQuery(db$con, "CREATE INDEX date_atbat ON atbat(date)") 
dbSendQuery(db$con, 'CREATE INDEX pitch_join ON pitch(gameday_link, num)')
dbSendQuery(db$con, 'CREATE INDEX atbat_join ON atbat(gameday_link, num)')

# Grad first 'example' data set 
at.bat <- tbl(db, "atbat") %>%   
  filter(pitcher_name == "Mariano Rivera" | pitcher_name == "Phil Hughes")
  
fbs <- tbl(db, "pitch") %>%   
  filter(pitch_type == "FF" | pitch_type == "FC")
  
pitches <- inner_join(fbs, at.bat) %>% 
  filter(date >= "2011_01_01" & date <= "2012_01_01") %>%
  collect()
  
# Every 'decision' made from 2008 to 2013
pitch <- tbl(db, "pitch") %>%
  filter(des == "Called Strike" | des == "Ball") %>%
# Keep pitch location, descriptions 
  select(px, pz, des, gameday_link, num) %>%
# 0-1 indicator of strike/ball
  mutate(strike = as.numeric(des == "Called Strike"))
  
atbat <- tbl(db, "atbat") %>%
# Most of these variables will be used as covariates in probabilistic models
  select(b_height, p_throws, stand, inning_side, date, gameday_link, num) %>%
  filter(date <= "2014_01_01")
decisions <- collect(inner_join(pitch, atbat))

#Formatting needed for fitting models
#decisions$strike <- as.numeric(decisions$des %in% "Called Strike")
decisions$stand <- factor(decisions$stand)
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
strikeFX(strikes, geom = "raster", n = 25, layer = facet_grid(. ~ stand, labeller = relabel)) + coord_equal()
dev.off()

#figure 2
pdf(file="strikesVSballs.pdf", width=6.5, height=3.5)
strikeFX(decisions, geom = "raster", density1 = list(des = "Called Strike"), 
         density2 = list(des = "Ball"), layer = facet_grid(. ~ stand, labeller = relabel)) + coord_equal()
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
# Older version used top_inning
#strikeFX(decisions, model = m, layer = facet_grid(p_throws~stand, labeller = relabel2),
#         density1 = list(top_inning="Y"), density2 = list(top_inning="N"))+coord_equal()
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