require(bipartite)
## nullmodel

indice="interaction evenness"
obs_index=networklevel(adj_matrix,index=indice);obs_index
null <-nullmodel(adj_matrix, N=1000, method="r2dtable") 
null1 <-sapply (X=null, FUN=networklevel, index=indice) 
means_null1 <- apply (X=cbind(null1),MARGIN=2, FUN=mean, na.rm=T)  
sd.means_null1 <- apply(X=cbind(null1), MARGIN=2, FUN=sd, na.rm=T)
z_score<-(obs_index-means_null1)/sd.means_null1
z_score
lower_bound <- quantile(null1, 0.025, na.rm=TRUE);lower_bound
upper_bound <- quantile(null1, 0.975, na.rm=TRUE);upper_bound
median= quantile(null1, 0.5, na.rm=TRUE);median
p_value<-sum(null1>= obs_index)/1000 # valor de p
p_value





# 5. Community detection
communities <- walktrap.community(graph)
cat("\nCommunities:\n")
print(communities$membership)

###set the paramters
par(mar=c(4,10,1,1))

### Baseline
x=c(1.2,2.2,3.2,4.2,5.2)
avg=c(0.01169886,3.121255,30.41418,0.6368347,7.848196)
lower=c(0.01158646,3.09595,29.49946,0.6354129,7.833198)
upper=c(0.01177785,3.152715,31.15735,0.637957,7.861985)

## W_Corallina
x1=c(0.8,1.8,2.8,3.8,4.8)
avg1=c(0.01373086,3.092778,26.49863,0.6424205,7.84925)
lower1=c(0.01360988,3.068306,25.83967,0.6435787,7.837756)
upper1=c(0.01383469,3.12,27.24265,0.6424205,7.864097)

plot(1, type="n", ylab="", yaxt="n", xlab="Nullmodels (IC95%)", xlim=c(0, 37), ylim=c(0, 6), main="")
points(avg,x, pch=16,cex=1)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
abline(v=0,lty=2, lwd=3,col="red")
points(avg1,x1, pch=16,cex=1, col="blue")
arrows(lower1, x1, upper1,  x1, length=0.05, angle=90, code=3,lwd=2.5, col="blue",lty=2)
axis(side=2,at=x,label=c("Connectance","Link per species", "Linkage density", "Interaction Eveness", "Diversity of interactions"),cex.lab=14, las=2,xlim=c(0.67,0.83))
x=locator(1)
legend(x, legend=c("Baseline", "Without Corallina"),
       col=c("black", "blue"), lty=1:2, box.lty=0,cex=1)
#text(locator(1),"*", cex=1.5)

###Lolypop
require(ggplot2)
# Define the values and their associated names
values <- c(0.12, 0.43, -6.35, -3.05,1.83 )
names <- c("Connectance", "Link per species", "Linkage density", "Interaction evenness", "Diversity of interactions")

# Create a data frame
data <- data.frame(names, values)
# Create the lollipop plot
ggplot(data, aes(x = names, y = values)) +
  geom_point(size = 3) +         # Add points
  geom_segment(aes(x = names, xend = names, y = 0, yend = values), 
               linetype = "dashed", color = "blue") +  # Add segments
  theme_minimal() +              # Minimal theme
  labs(title = "Delta Z-score",
       x = "Names",
       y = "Values") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

