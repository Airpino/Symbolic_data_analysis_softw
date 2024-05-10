Math_to_DF<-function(M, dig=4){
  namer<-get.MatH.rownames(M)
  namev<-get.MatH.varnames(M)
  
  ll<-list()
  
  for(i in 1:get.MatH.ncols(M)){
    ll[[i]]<-character()
    for(j in 1:get.MatH.nrows(M)){
      tmp<-M@M[j,i][[1]]
      ll[[i]][j]<-paste0("[m= ",format(tmp@m,digits=dig)," , s= ",format(tmp@s,digits=dig)," ]")
    }
  }
  df<-as.data.frame(ll)
  row.names(df)<-namer
  colnames(df)<-namev
  return(df)
}

compare_proto_to_gen<-function(Pro,Gen){
  rr<-get.MatH.nrows(Pro)
  cc<-get.MatH.ncols(Pro)
  n_o_pro<-get.MatH.rownames(Pro)
  n_v_pro<-get.MatH.varnames(Pro)
  #create DF
  name_pro<-name_var<-character()
  Vpro<-Vgen<-numeric()
  for(i in 1:rr){
    for(j in 1:cc){
      name_pro<-c(name_pro,rep(n_o_pro[i],length(Pro@M[i,j][[1]]@x)))
      name_var<-c(name_var, rep(n_v_pro[j],length(Pro@M[i,j][[1]]@x)))
      Vpro<-c(Vpro,Pro@M[i,j][[1]]@x)
      Vgen<-c(Vgen,Gen@M[1,j][[1]]@x)
    }
  }
  df<-data.frame(Cluster=name_pro,name_var,pro=Vpro,gen=Vgen)
  return(df)
}