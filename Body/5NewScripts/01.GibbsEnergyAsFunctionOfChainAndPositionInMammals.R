###################################

rm(list=ls(all=TRUE))

data = read.table("../../Body/2Derived/harvest.txt", header = TRUE)
names(data)

### 1 keep only 37
table(data$temp)
nrow(data) # 243476
data = data[data$temp == 37,]
nrow(data) # 81157

### 2 keep only normal tRNA
length(table(data$trna)) # 23 becaus there is 'local' ???
data[data$trna == 'local',]
nrow(data)
data = data[data$trna != 'local',]
nrow(data)

### 3 keep only tRNA with estimated Gibbs Energy (not NA)
str(data)
nrow(data)
data = data[!is.na(data$gibbs),]
nrow(data)
summary(data$gibbs)

### 4 keep only mammals (!!!!!! not ideal!!!!)
Mammals = read.table("GenerationLenghtforMammals.xlsx.txt", sep = '\t', header = TRUE)
Mammals$Scientific_name = gsub(' ','_',Mammals$Scientific_name)
VecOfMammalianSpecies = unique(Mammals$Scientific_name)
length(VecOfMammalianSpecies)
nrow(data)
data = data[data$species %in% VecOfMammalianSpecies,]
nrow(data)
nrow(data)/22 # ~ 430 species

### 5 add column with Chain # Check once more with MitoWheel!
VecOfTrnaFromLightChain =c('Pro','Glu','SerUCN','Tyr','Cys','Asn','Ala','Gln')   
data$TrnaChain = 'light'
for (i in 1:nrow(data))
{
  if (data$trna[i] %in% VecOfTrnaFromLightChain) {data$TrnaChain[i] = 'light'}
}
table(data$TrnaChain)

### 6 average Gibbs Energy for each tRNA
agg = aggregate(data$gibbs, by = list(data$trna,data$TrnaChain), FUN = median)
names(agg) = c('trna','TrnaChain','MedianGibbs')

agg = agg[order(agg$MedianGibbs),]
### 7 continue till all 22 (color by chain)
boxplot(data[data$trna == 'Pro',]$gibbs, data[data$trna == 'Tyr',]$gibbs, notch = TRUE, outline = FALSE, names = c('Pro','Tyr'))




