setwd("//home.ansatt.ntnu.no/lcgarcia/Documents/R")

#install.packages("pvclust")
#install.packages("vcd")
#install.packages("boot")

library(boot)
library(janitor)
library(tibble)
library(stringr)
library(dplyr)
library(data.table)
library(sf)
library(s2)
library(stars)
library(ggplot2)
library(wdpar)
library(vegan)

#Scripts from Gardeners book - Example with 0_500 1900_49
#PENDING TO FILTER THE ANNOMALOUS HIGH DIVERSITY DATA??? see time too (where more than x algorithm that is somewhere, maybe in extra file l84. Do a regression?)???
##########AUTOMATED ANALYSES ######Incorporated/used already

#1. Calculate species richness using R

pd = read.csv(file.choose())
pd.t = ftable(Site ~ Species, data = pd)
pd.t

colSums(pd.t)
levels(pd$Site)
pd.sr = colSums(pd.t)
names(pd.sr) = levels(pd$Site)
pd.sr

#######

#1b. Correlate species richness and time (PENDING!)
#one column for years, another one for species richness, to copy and paste each
year = scan()
bfr = scan()
plot(year, bfr)
cor.test(year, bfr, method = "spearman")

#10. Correlating diversity index to a variable. p263

library(vegan)
head(bf)
comm = xtabs(Qty~Yr + Spp, data = bf)
rownames(comm)
pv = as.numeric(rownames(comm))
pv
H = diversity(comm, index = "shannon") #Shannon diversity as resp var
H

plot(H~pv, ylab = "Shannon diversity, H", xlab = "Year")
cor.test(~H + pv, method = "spearman")
abline(lm(H~pv), lty = "dashed")
lines(lowess(pv, H), lty = 1)

##########

#2. Standarize a community dataset using rarefaction

gb.biol = read.csv(file.choose(), row.names = 1)
names(gb.biol)
row.names(gb.biol)
library(vegan)
range(rowSums(gb.biol))
rrarefy(gb.biol, sample = 100)
rowSums(gb.biol)
rrarefy(gb.biol, sample = row.sums(gb.biol))
gb.biol[1:3, c("Aba.par", "Neb.bre", "Pte.mad", "Pte.str")]
rrarefy(gb.biol, sample = 100)[1:3, c("Aba.par", "Neb.bre", "Pte.mad", "Pte.str")]

#3. Use R to calculate Simpson's diversity

a = c(10, 2, 1, 1, 1)
b = c(3, 3, 3, 3, 3)
divc = rbind(a, b) #joins items together in rows
divc
rm(a,b)

prop.table(divc, margin = 1)
row.sums(prop.table(divc, margin = 1)^2)
1-rowSums(prop.table(divc, margin = 1)^2)
gb.biol = read.csv(file.choose(), row.names = 1)
gb.site = read.csv(file.choose(), row.names = 1)

names(gb.biol)
row.names(gb.biol)
gb.site

library(BiodiversityR)
diversity(gb.biol, index = "simpson")
1 - diversity(gb.biol, index = "simpson")
diversitycomp(gb.biol, y = gb.site, factor1 = "Habitat", index = "Simpson")
1 - diversitycomp(gb.biol, y = gb.site, factor1 = "Habitat", index = "Simpson")[,2]
diversityresult(gb.biol, y = gb.site, factor = "Habitat", level = "Wood", index = "Simpson", method = "s")
diversityresult(gb.biol, y = gb.site, factor = "Habitat", level = "Wood", index = "Simpson", method = "all")

#4. Use R to explore effective species and evenness 

gb.biol = read.csv(file.choose(), row.names = 1)
gb.site = read.csv(file.choose(), row.names = 1)

library(BiodiversityR)
exp(diversity(gb.biol, index = "shannon"))
2^(diversity(gb.biol, index = "shannon", base = 2))
exp(diversity(gb.biol, index = "shannon")) / specnumber(gb.biol)
diversity(gb.biol, index = "shannon") / log(specnumber(gb.biol))

diversityresult(gb.biol, index = "Jevenness", method = "s")
diversityresult(gb.biol, index = "Eevenness", method = "s")

diversitycomp(gb.biol, y = gb.site, factor1 = "Habitat", index = "Eevenness")
diversityresult(gb.biol, y = gb.site, factor = "Habitat", level = "Edge", index = "Jevenness", method = "s")
gb.H = diversitycomp(gb.biol, y = gb.site, factor1 = "Habitat", index = "Shannon")[2,]
gb.H
gb.H = log(exp(gb.H), base = 2)
gb.H / log2(specnumber(gb.biol, groups = gb.site$Habitat, MARGIN = 1))

#5. Use R to calculate Berger-Parker dominance

gb.biol = read.csv(file.choose(), row.names = 1)
gb.site = read.csv(file.choose(), row.names = 1)
a = c(10,2,1,1,1) #Small community with dominant
b = c(3,3,3,3,3) #Even community
divc = rbind(a,b)
rm(a, b)
divc

prop.table(divc, margin = 1)
apply(prop.table(divc, margin = 1), MARGIN = 1, FUN = max)
apply(prop.table(as.matrix(gb.biol), margin = 1), MARGIN = 1, FUN = max)
library(BiodiversityR)
diversityresult(gb.biol, index = "Berger", method = "s")
diversityresult(gb.biol, index = "Berger", method = "s", y = gb.site,
                factor = "Habitat", level = "Edge")
diversityresult(gb.biol, index = "Berger", method = "all", y = gb.site, factor = "Habitat", level = "Edge")
diversitycomp(gb.biol, y = gb.site, factor1 = "Habitat", index = "Berger")
diversitycomp(gb.site, factor1 = "Habitat", index = "Berger", method = "sd")
gb.bp = diversityresult(gb.biol, index = "Berger", method = "s")
gb.bp$Habitat = gb.site$Habitat #Add habitat as a variable to the result

str(gb.bp)
boxplot(Berger ~ Habitat, data = gb.bp)
title(ylab = "Berger-Parker dominance", xlab = "Habitat (n = 6)")

#6. Use R to conduct a t-test on unreplicated Shannon entropy (two samples)

a = c(1882,1028,19,5,8,273)
b = c(26,535,984,751,621,250)

DeVries = rbind(a,b)

colnames(DeVries) = c("Hisache", "Panprol", "Neshewi", "Morachi", "Taysp.1", "Coldirc")
row.names(DeVries) = c("canopy","under")

x = DeVries[1,]
y = DeVries[2,]

Nx <- sum(x, na.rm = TRUE)
Ny <- sum(y, na.rm = TRUE)
Nx; Ny
Sx <- sum(x > 0, na.rm = TRUE)
Sy <- sum(y > 0, na.rm = TRUE)
Sx; Sy
Px <- x/Nx
Py <- y/Ny

Hx <- -sum(log(Px) * Px, na.rm = TRUE)
Hy <- -sum(log(Py) * Py, na.rm = TRUE)
Hx; Hy

SSx <- (sum(Px*log(Px)^2, na.rm = TRUE))
SQx <- Hx^2
Vx <- ((SSx-SQx)/Nx) + ((Sx-1)/(2*Nx^2))
Vx
SSy <- (sum(Py*log(Py)^2, na.rm = TRUE))
SQy <- Hy^2
Vy <- ((SSy-SQy)/Ny) + ((Sy-1)/(2*Ny^2))
Vy

d.f <- ((Vx + Vy)^2)/((Vx/Nx)+(Vy/Ny))
Ht <- abs(Hx-Hy)/sqrt(Vx+Vy)
Pval <- pt(Ht, df = d.f, lower.tail = FALSE) * 2
Crit <- qt(0.05/2, df = d.f, lower.tail = FALSE)


#################

####BEGINS TO RUN HERE
#Importing the database first

time.biol <- convtable_0_500_sp_nodups
row.names(time.biol) <- (gsub("abundance_", "", rownames(time.biol)))
sites <- gsub("l", "", rownames(time.biol))
time.site <- as.data.frame(cbind(rownames(time.biol), rownames(time.biol)))
colnames(time.site) = c("Sample", "Habitat")
time.biol <- cbind(time.site$Sample, time.biol)
names(time.biol)[1] <- c("Sample")
time.biolws <- time.biol[,-1]

#################MORE, SOME EXPLORING AND CREATIVITY HERE

#species richness
round(specnumber(time.biolws), digits = 2)
max(specnumber(time.biolws))
#Abundances
rowSums(time.biolws, na.rm = TRUE)

#EVENNESS AND DOMINANCE
#J-evenness (0to1)
jevenness <- round(diversity(time.biolws, index = "shannon") / log(specnumber(time.biol)), digits = 2)
png(file = 'plot%d.png')

#Berger-Parker dominance
library(BiodiversityR)
time.bp <- round(diversityresult(time.biolws, index = "Berger", method = "s"), digits = 2)
plot(sites, time.bp$Berger, ylab = "berger", xlab = "latitude", main = "Berger index")

#some creativity comparing the evennness and the berger index
library(dplyr)
dominance <- as.data.frame(cbind(jevenness,time.bp$Berger)) %>% rename(berger = V2) %>% arrange(berger)
plot(dominance$berger ~ dominance$jevenness, data = dominance, xlab = "J-evenness", ylab = "Berger index", main = "dominating species?")
text(dominance$jevenness, dominance$berger, gsub("l", "", rownames(dominance)), cex=0.7, pos=2, adj = c(0,0), offset = 0.2, col="red")
#identify(dominance$jevenness, dominance$berger, gsub("abundance_", "", rownames(dominance)), pos = TRUE)
#plot and correlation test? (rosa paper). Put labels in the dots with the latitudes
cor.test(~dominance$berger + dominance$jevenness, method = "spearman") #TEST!!! Correlation test using the Spearman rho test via the cor.test() command

#plotting actual srichness and abundances
par(mfrow = c(2,2))
sRichness <- round(specnumber(time.biolws), digits = 2)
plot(sites, sRichness, main = "species richness 1900_49, 0_500", xlab = "latitude")
cummulative_S = c(cumsum(rev(sRichness)))
plot(sites, cummulative_S, main = "cummulative species richness", xlab = "latitude", xaxt = 'n')
axis(side=1,at=sites, labels = rev(sites))

abundances <- rowSums(time.biol, na.rm = TRUE)
plot(sites, abundances, main = "abundances 1900_49, 0_500", xlab = "latitude")
cummulative_ab = c(cumsum(rev(abundances)))
plot(sites, cummulative_ab, main = "cummulative abundances", xlab = "latitude", xaxt = 'n')
axis(side=1,at=sites, labels = rev(sites))

par(mfrow = c(1, 1))

#more
#E-evenness
round(exp(diversity(time.biol, index = "shannon"))/specnumber(time.biol), digits = 2)
#inverseSimpson
round(diversity(time.biol, index = "invsimpson"), digits = 2)

### AUTOMATED ANALYSES

#2b. Generate species area curves

names(time.biol)
row.names(time.biol)

library(vegan)

time.sa = specaccum(time.biol, method = "collector")
time.sa

names(time.sa)
time.sa$richness
plot(time.sa)

time.sa = specaccum(time.biol, method = "exact")
plot(time.sa)
time.sa$sd

time.sa = specaccum(time.biol, method = "random")
plot(time.sa)

boxplot(time.sa)

time.sa = specaccum(time.biol, method = "rarefaction")
plot(time.sa, xvar = "individuals")

par(mfrow = c(2, 2))
plot(time.sa, xvar = "individuals", main = "Individual-based accumulator")
plot(time.sa, main = "Site based, no CI", ci = 0)
plot(time.sa, ci.type = "line", main = "Confidence envelope")
plot(time.sa, ci.type = "polygon", main = "Confidence polygon", ci.col = "gray50")
par(mfrow = c(1, 1))

#7. Comparing multiple sites using graphs: calculation for confidence intervals

H_CI = function(x) {

Nx = sum(x)
Px = x/Nx
Hx = -sum(log(Px)*Px, na.rm = TRUE)
Sx = sum(x>0)

SSx = sum(Px*log(Px)^2, na.rm = TRUE)
SQx = Hx^2
Vx = ((SSx-SQx)/Nx) + ((Sx-1)/(2*Nx^2))
DF = ((Vx*2)^2)/((Vx/Nx) + (Vx/Nx))
CV =qt(0.05/2, df = DF, lower.tail = FALSE)
SD = sqrt(Vx)
CI = CV*SD

result = CI
return(result)

}

apply(time.biolws, MARGIN = 1, FUN = H_CI)

#7a. Make a bar chart of differences in Shannon diversity index

library(vegan)
H = apply (time.biolws, MARGIN = 1, FUN = diversity, index = "shannon")
CI = apply(time.biolws, MARGIN = 1, FUN = H_CI)
bp = barplot(H, ylim = c(0, max(H+CI, na.rm = TRUE)), cex.names = 0.8, main = "Shannon index barplot")
arrows(bp, H+CI, bp, H-CI, length = 0.1, angle = 90, code = 3)
title(xlab = "Site name", ylab = "Shannon index, H")

#7b. Make a point plot of Shannon diversity for multiple sites

library(vegan)
H = apply(time.biolws, MARGIN = 1, FUN = diversity, index = "shannon")
CI = apply(time.biolws, MARGIN = 1, FUN = H_CI)
TV = max(H + CI, na.rm = TRUE)
BV = min(H - CI, na.rm = TRUE)
AL = pretty(c(BV, TV))
NS = length(CI)
NS

plot(1:NS, H, ylim = c (min(AL), max(AL)),
     xlim = c(0.5, NS+0.5), axes = FALSE, 
     xlab = "Site name", ylab = "Shannon index, H",
     pch = 18, cex = 2, main = "Shannon index point plot")

axis(1, at = 1:NS, labels = names(H))
axis(2)
box()
arrows(1:NS, H+CI, 1:NS, H-CI, length = 0.1, angle = 90, code = 3)
abline(h = seq(2.8, 3.3, 0.5), lty = 3, col = "gray50")

#8. Bootstrap comparisons for unreplicated samples: use a loop to make a bootstrap sample of Shannon diversity. p229

Happ = function(x){
  cx = rep(1:length(x), x)
  shannon1 = function(data, i){
    pi <- table(data[i]/length(data[i])
    -sum(log(pi)*pi)            )
    }
  bx = boot(cx, shannon1, R = 1000)
  qx = quantile(bx$t, c(0.025, 0.975))
  }

#9. Point plot of Shannon diversity and bootstrap confidence intervals. p241 #Difficult bugs

library(vegan)
library(boot)

H = diversity(time.biolws)

CI = apply(time.biolws,1,Happ) #Bug applying the function, to meta-analysis???

#gb.Hci

TV = max(CI[2,])
BV = min(CI[1,])

AL = pretty(c(BV, TV))

NS = length(H)

plot(1:NS, H,
     ylim = c(min(AL), max(AL)),
     xlim = c(0.5, NS+0.5),
     axes = FALSE,
     xlab = "Site names",
     ylab = "Shannon index, H")

axis(2, at = AL)

axis(1, at = 1:NS, labels = names(H))

arrows(1:NS, CI[2,], 1:NS, CI[1,],
       length = 0.1, angle = 90, code = 3)

abline(h = AL, lty = 3, col = "gray50")
box()

#11. Use R to calculate true beta diversity from presence-absence data. p275

library(vegan)
comm <- time.biolws

alpha <- mean(specnumber(comm))
alpha
gamma <- length(specnumber(comm, MARGIN = 2) > 0)
gamma
beta <- gamma/alpha
beta

#12. Use R to calculate alpha and gamma diversity for pairs of samples in a community dataset

library(vegan)

gbt = rowsum(time.biolws, group = time.site$Habitat)
alpha <- designdist(gbt, method = "(A+B)/2", terms = "binary")
alpha

gamma <- designdist(gbt, method = "A+B-J", terms = "binary")
gamma

gamma/alpha #if you keep these values, you can use them to determine various measures of species turnover

#13. Use R to make a cluster dendogram to visualise beta diversity

library(vegan)
time.beta <- betadiver(time.biol, method = "w")
time.clus <- hclust(time.beta)
plot(time.clus, hang = -1)
plot(time.clus, hang = -1, main = "Beta diversity",
     ylab = "Beta values", xlab = "Beetle community sample",
     sub = "Complete joining cluster")

#14. Use R to make a ternary plot to visualise beta diversity

#install.packages("vcd")
library(vegan)
library(vcd)
time.dis <- betadiver(time.biol)
plot(time.dis)

#More, interesting one! https://www.researchgate.net/figure/Ternary-plots-shows-the-relative-contributions-of-species-similarity-S-species_fig5_319089616

#15. Determine beta diversity using Shannon entropy

library(BiodiversityR)
time.n = decostand(time.biolws, method = "total")
alpha <- tapply(diversity(time.n), time.site$Habitat, FUN = mean)
alpha

gamma <- diversitycomp(time.n, y = time.site, factor1 = "Habitat")[,2]
gamma
gamma - alpha

alpha <- mean(diversity(time.biolws))
alpha
gamma <- diversity(colSums(time.n))
gamma
gamma - alpha

alpha <- mean(diversitycomp(time.n, y = time.site,
                            factor1 = "Habitat", index = "Shannon",
                            method = "all")[,2])
alpha
gamma <- diversity(colSums(time.n))
gamma
gamma - alpha

#16. Examine overlap between multiple communities 
#Equivalent to alpha/gamma, alpha, beta and gamma simple diagram: https://eco-intelligent.com/2016/10/14/alpha-beta-gamma-diversity/#:~:text=Alpha%20diversity%20is%20the%20species,landscape%2C%20taken%20as%20one%20unit.

library(vegan)

alpha = mean(specnumber(time.biolws))
alpha
gamma = specnumber(colSums(time.biolws))
gamma
length(rownames(time.biolws))
o = (alpha/gamma-1/10)/(1-(1/10))
o
alpha = mean(diversity(time.biolws))
alpha
gamma = diversity(colSums(time.biolws))
gamma

m = exp(alpha)
t = exp(gamma)
m ; t
o = (m/t-1/10)/(1-(1/10))
o

#16b. Explore differences in species composition (beta diversity) using various dissimilarity measures

library(vegan)
time.b <- vegdist(time.biolws, method = "bray")
mod <- betadisper(time.b, group = time.site$Habitat)
anova(mod)
time.go <- vegdist(time.biol, method = "gower")
mod <- betadisper(time.go, group = time.site$Habitat)
permutest(mod)

mrpp(time.biolws, grouping = time.site$Habitat, distance = "euclidean")
anosim(time.biolws, grouping = time.site$Habitat, distance = "raup")
adonis(time.biolws~time.site$Habitat, method = "chao")

#17. Create multiple RAD (rank-abundance dominance models or dominance/diversity plots) models for a community dataset
#bug???

library(vegan)
time.rad <- radfit(time.biolws)
time.rad
summary(time.rad)
names(time.rad)
time.rad$l56
sapply(time.rad, function(x) unlist(lapply(x$models, AIC)))
names(time.rad$l84$models)
time.rad$l84$models$Lognormal

#18. Use R to calculate dissimilarity indices for presence-absence data

library(vegan)
class(time.biolws)
time.jac <- vegdist(time.biolws, method = "jaccard")
time.jac

time.bin <- 1 -dist(time.biolws, method = "binary")
time.bin

#19. Make a hierarchical clustering result and a dendogram 
#A bug here, empty rows check that one

library(vegan)
time.eu = vegdist(time.biolws, method = "bray")
time.cl =hclust(time.eu, method = "complete")
str(time.cl)
plot(time.cl)

#19b. Split and label a complicated dendogram

rownames(time.biolws)
time.eu <- dist(time.biolws, method = "euclidean")
time.clus <- hclust(time.eu)
plot(time.clus)

#20. Examine significance of hierarchical clustering
#A bug here

library(pvclust)
rownames(time.biolws)
time.clp <- pvclust(t(time.biolws))
time.clp
names(time.clp)
names(time.clp$edges)
time.clp$edges$au
plot(time.clp)
lines(time.clp, alpha = 0.95, pv = "au", type = "geq")
pvrect(time.clp, pv = "au", border = "blue")
pvpick(time.clp)

msplot(time.clp, edges = c(1:4, 12, 16))
seplot(time.clp, type = "au", identity = TRUE)


#21. Use R for area-based association analysis #A bug here: NA/NaN/Inf in foreign function call (arg 10)

time.biolt <- t(time.biolws[2:length(time.biolws)])
library(vegan)
time.co = designdist(time.biolt, method = "J", terms = "binary")
time.exp = designdist(time.biolt, method = "A*B/P", terms = "binary")
time.csq = (time.co - time.exp)^2 / time.exp
sum(time.csq)
pchisq(sum(time.csq), df = nrow(time.biolt), lower.tail = FALSE)

time.resid = (time.co - time.exp) / sqrt(time.exp)
time.pr = time.resid + abs(min(time.resid))
time.pr = max(time.pr) - time.pr
time.hc = hclust(time.pr)
plot(time.hc, labels = time.nam$Common)

#22. Use R for indicator species analysis #A bug: check vector of only NA values

rownames(time.biolws)
time.site$Habitat
time.grp = rowsum(time.biolws, group = time.site$Habitat)
time.grp[,1:6]
time.cs = chisq.test(time.grp)
names(time.cs)
PR = time.cs$residuals
PR[, which(abs(PR[1,])>2)]
PRS = PR[, which(abs(PR[1,])>2 & abs(PR[2,])>2 & abs(PR[3,])>2 & abs(PR[4,])>2 & abs(PR[5,])>2 & 
             abs(PR[6,])>2  & abs(PR[7,])>2  & abs(PR[8,])>2  & abs(PR[9,])>2  & abs(PR[10,])>2 & 
             abs(PR[11,])>2 & abs(PR[12,])>2 & abs(PR[13,])>2 & abs(PR[14,])>2 & abs(PR[15,])>2 & 
             abs(PR[16,])>2 & abs(PR[17,])>2 & abs(PR[18,])>2 & abs(PR[19,])>2 & abs(PR[20,])>2 &
             abs(PR[21,])>2 & abs(PR[22,])>2 & abs(PR[23,])>2 & abs(PR[24,])>2 & abs(PR[25,])>2 & 
             abs(PR[26,])>2 & abs(PR[27,])>2 & abs(PR[28,])>2 & abs(PR[29,])>2)]
barplot(PRS, beside = TRUE, las = 2, legend = TRUE,
        args.legend = list(bty = "n", x = "topleft"))
abline(h = c(-2,2), lty = 3)
abline(h = 0)
title(ylab = "Pearson residuals")

op <- par(mfrow = c(2,2)) #split the window 2x2
barplot(PRS[1,], las = 2, cex.names = 0.8) #plot first row
title(main = "l56", ylab = "Pearson residuals")
abline(h = c(-2,2), lty = 3) #add horizontal lines at the significance 
abline(h = 0) #add a line across the middle
title(ylab = "Pearson residuals") #add a title

barplot(PRS[2,], las = 2, cex.names = 0.8)
title(main = "l57", ylab = "Pearson residuals")
abline(h = c(-2,2), lty = 3)
abline(h = 0)

barplot(PRS[3,], las = 2, cex.names = 0.8)
title(main = "l58", ylab = "Pearson residuals")
abline(h = c(-2,2), lty = 3)
abline(h = 0)

#Rotate the data to group by habitat
barplot(t(PRS), beside = TRUE, las = 1, legend = TRUE,
        args.legend = list(bty = "n", x = "topleft", cex = 0.6))
title(main = "All species by habitat", ylab = "Pearson residuals")
abline(h = c(-2,2), lty = 3)
abline(h = 0)

par(op) #reset the window to single (the current plot is unaffected)

#23. Carry out classical(metric) MDS (Multi-dimensional (metric) scaling (principal co-ordinates analysis))

moss.eu = dist(time.biolws, method = "euclidean")
moss.pco = cmdscale(moss.eu, k = 2)
moss.pco

plot(moss.pco, type = "n", asp = 1, xlab = "PCoA1", ylab = "PCoA2")
text(moss.pco, labels = rownames(moss.pco))

#24. Use principal components analysis

time.biolws <- time.biol[,-1]
moss.pca = prcomp(time.biolws)
names(moss.pca)
moss.pca$sdev^2
screeplot(moss.pca)
summary(moss.pca)
barplot(summary(moss.pca)$importance[2,], las = 2)
title(ylab = "Proportion of variance", xlab = "PCA axis")
biplot(moss.pca, col = c("blue","darkgreen"), cex = c(1, 0.8))
scores(moss.pca, choices = 1:2, display = "sites")
scores(moss.pca, choices = 1:2, display = "species")

#25. Use the vegan package for correspondence analysis #A bug: all row sums must be >0 in the community data matrix
  #Seems like an evident correction to this point: remove latitudes with all NA (0) values

psa.ca = cca(time.biolws)
print(psa.ca, digits = 3)
names(summary(psa.ca))
summary(psa.ca)$cont
eigenvals(psa.ca)
summary(psa.ca)$unconst.chi
scores(psa.ca, display = "sites")
plot(psa.ca)

psa.nam = abbreviate(colnames(time.biolws), minlength = 5)
plot(psa.ca, type = "n")
text(psa.ca, display = "sites")
points(psa.ca, display = "species", col = "red", cex = 0.7, pch = "+")
plot(psa.ca, type = "n")
text(psa.ca, display = "sites")
text(psa.ca, display = "species", col = "blue", cex = 0.7, labels = psa.nam)
op = plot(psa.ca, type = "n")
text(psa.ca, display = "sites")
points(psa.ca, display = "species", col = "red", cex = 0.7, pch = "+")
identify(op, what = "species", labels = psa.nam, col = "blue")

plot(psa.ca, type = "n")
text(psa.ca, display = "sites", cex = 0.8)
text(psa.ca, display = "species", col = "blue", cex = 0.7,
     labels = psa.nam, select = c("Urtcd"))
text(psa.ca, display = "species", col = "blue", cex = 0.7, labels = psa.nam, select = psa.nam[1:5])

#26. Use grouping information on ordination plots. #A bug: all row sums must be >0 in the community matrix: remove empty sites
   #Same observation as before

library(vegan)
gb.dca = decorana(time.biolws)
plot(gb.dca, display = "sites", type = "t")
plot(gb.dca, display = "sites", type = "p")
ordispider(gb.dca, groups = time.site$Habitat, label = TRUE,
           cex = 0.7, lty = 2)
plot(gb.dca, display = "sites", type = "p")
ordiellipse(gb.dca, groups = time.site$Habitat, label = TRUE, lwd = 1.5)
plot(gb.dca, display = "sites", type = "p")
ordihull(gb.dca, groups = time.site$Habitat, label = TRUE,
         lty = 3, lwd = 0.5, cex = 0.7)
plot(gb.nmds)
ordiellipse(gb.nmds, groups = time.site$Habitat, label = TRUE, show.groups = "Edge")
ordiellipse(gb.nmds, groups = time.site$Habotat, label = TRUE, show.groups = "Edge", kind = "sd", conf = 0.95, lty = 2)
oh = ordihull(gb.dca, groups = time.site$Habitat, draw = "none")
oh
oe = ordiellipse(gb.dca, groups = time.site$Habotat, draw = "none")
summary(oe)

###########

