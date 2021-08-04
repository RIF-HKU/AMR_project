human_eco=ast_05%>%
  filter(grepl("^C.*",i_pid))%>%
  select(1:5,contains("result"))
swine_eco=ast_05%>%
  filter(grepl("^S.*",i_pid))%>%
  select(1:5,contains("result"))
poultry_eco=ast_05%>%
  filter(grepl("^P.*",i_pid))%>%
  select(1:5,contains("result"))
dim(human_eco)  

find_number=function(data,bacteria){
  bac_data=data%>%
    filter(a_bacteria==bacteria)
  num=nrow(bac_data)
  return(num)
}
sum1=find_number(human_eco,bacteria = "KPNEU")

sum2=find_number(poultry_eco,bacteria = "KPNEU")
sum3=find_number(swine_eco,bacteria = "KPNEU")
ecolist<-list(human_eco,poultry_eco,swine_eco)
count_number2=function(x){
  c=sum(x=="RES",na.rm = T)
  #b=sum(x=="SUS",na.rm = T)
  result_table=cbind(RES_n=c)#SUS=b)
  return(result_table)
}
make_tab4e<-function(data,bacteria,sum){
  datalist<-list()
  for (i in c(1:length(data))){
    table=subset(data[[i]],a_bacteria==bacteria)
    tmp=t(t(apply(table[,-c(1:5)],2,function(x)count_number2(x))))
    datalist[[i]]<-paste(tmp," (",round(tmp/sum[i],3)*100,")",sep = "")
    
  }
  datalist=do.call(cbind,datalist)
  colname=c("Human","Poultry","Swine")#,"Local","Unknown")
  colnames(datalist)=colname
  datalist
}

sumeco=c(sum1,sum2,sum3)
ecolisummary=as.data.frame(make_tab4e(ecolist,"KPNEU",sum = sumeco))

rownames(ecolisummary)=sapply(strsplit(rownames(tmp),"_"), function(x)x[2])

write.csv(ecolisummary,"~/Desktop/KPNEU520.csv")
