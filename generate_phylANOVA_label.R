`generate_phylANOVA_label` <- 
function(phyNOVA)
{
  # All that is needed is the output from phytools::phylANOVA
  # Extract p-values and assign groups from phyloANOVA post-hoc table
  df.prep<-phyNOVA$Pt
  df.prep[lower.tri(df.prep,diag = T)] <- NA
  df.prep<-na.omit(melt(df.prep))
  df.prep[,1]<-paste(df.prep[,1],"-",df.prep[,2],sep="")
  phynova.levels <- df.prep[,3]
  names(phynova.levels)=df.prep[,1]
  phynova.labels <- data.frame(multcompLetters(phynova.levels)['Letters'])
 
  #put the labels in the same order as input (alphabetical or otheriwise):
  phynova.labels$treatment=rownames(phynova.labels)
  phynova.labels=phynova.labels[order(factor(phynova.labels$treatment, levels=names(x$Pt[,1]))),]
  return(phynova.labels)
}