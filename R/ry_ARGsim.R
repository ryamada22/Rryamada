#' Ancestral Recombination Graph simulation
#'
#' This function makes a data set under the model of ARG
#' @param n is a No. of initial members
#' @param g is a No. generations.
#' @param l is a parameter
#' @param r is another paramter.
#' @keywords population_genetics
#' @export
#' @examples
#' n<-8
#' g<-10
#' l<-200
#' r<-0.05
#' d<-ry_ARGsim(n=n,g=g,l=l,r=r)
#' mmm<-matrix(d$parents[1,],ncol=g)


ry_ARGsim<-function(n=8,g=5,l=6,r=r){
 ids<-1:n
 now<-1:n
 parents<-rep(0,n*g*l)
 data<-rep(0,n*g*l)
 for(i in 1:n){
  data[(1+(i-1)*l):(i*l)]<-i
 }
 count<-1+n*l
 for(i in 2:g){
  tmp<-sample(ids,replace=FALSE)
  for(j in 1:(n/2)){
   countinside<-1
   for(x in 1:2){
    first<-1
    if(runif(1)>0.5){
     first<-2
    }
    data[count]<-now[tmp[(j-1)*2+first]]
    parents[count]<-tmp[(j-1)*2+first]
    now[countinside]<-data[count]
    count<-count+1
    countinside<-countinside+1
    for(k in 1:(l-1)){
     if(runif(1)<r){
      first<-first+1
      if(first==3){
       first<-1
      }
     }
     data[count]<-now[tmp[(j-1)*2+first]]
     parents[count]<-tmp[(j-1)*2+first]
     now[countinside]<-data[count]
     count<-count+1
     countinside<-countinside+1
    }
   }
  }
 }

 return(list(allele=matrix(data,nrow=l),parents=matrix(parents,nrow=l)))
}
