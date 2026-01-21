# ----------- clustSensitivity ---------
#' clustSensitivity
#'
#' @param clusters predicted clusters from classifier
#' @param truth ground truth named vector (cell bar code and real cluster, assuming integers for cluster numbers)
#' @param celltypes celltypes vector with cluster names
#' @return results of sensitivity per cluster
#' @export
#'
clustSensitivity<-function(clusters, truth, celltypes)
{
  print(tab<-table(clusters, truth))
  index_max<-NULL
  for (i in 1:length(tab[,1]))
    index_max<- rbind(index_max,c(which.max(as.vector(tab[i,])),max(tab[i,])))
  index_max<-as.data.frame(index_max)
  print(index_max)
  
  TP_perclust_sum<-aggregate((index_max), by=(index_max[1]), FUN = sum) # the original aggregation!!
  print("Aggregation:")
  print(TP_perclust_sum)
  print("Overall Accuracy: ")
  print(overAcc<-sum(TP_perclust_sum[,3])/sum(tab))
  
  # TP_perclust<-aggregate((index_max), by=(index_max[1]), FUN = max)  #### a temporary solution for cluster mapping in case of multiple clusters!!!!!!
  # print("Aggregation and mapping by max:")
  # print(TP_perclust)
  
  TP_perclust<-TP_perclust_sum # test!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  results<-NULL
  
  results<-c(0,overAcc) # actually, add a line for the overall accuracy...
  
  types<-sort(as.numeric(unique(truth)))
  #types<-unique(truth[,2])
  #print(types)
  
  for (n in 1:length(types))
  { #print(celltypes[n])
    if(length(which(TP_perclust[,1]==n)) >0)
      acc<-TP_perclust[which(TP_perclust[,1]==n),3]/sum(tab[,n])
    else
    {#acc <-0
      acc<-(sort(tab[,n], decreasing = TRUE)[2])/sum(tab[,n])
      # if not found in aggregated results, it means the max for the row was claimed already, second max is whats left...WRONG and should be zero as before..
      # but we agreed on assigning second max for simulation reasons.... :((((()))))
    }
    #print(acc)
    results<-rbind(results,c(celltypes[types[n]], acc))
  }
  
  #print(results)
  return(results)
}