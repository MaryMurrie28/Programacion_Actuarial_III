rankhospital<-function(estado,resultado,num="mejor"){
    
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
    estados<-unique(outcome[,7])
    
    if (!estado %in% estados){
        stop("Estado inválido")
    }
    
    if (resultado== "ataque"){
        estados<-outcome[11]
    }else if(resultado== "falla"){
        estados<-outcome[17]
    }else if(resultado=="neumonia"){
        estados<-outcome[23]
    }else{
        stop("Resultado inválido")
    }
    
    nestado<-outcome[outcome[,7]==estado,c(2,7,11,17,23)]  #estado que deseas
    names(nestado)[3:5]<-c("ataque","falla","neumonia")
    nestado[,resultado]<-suppressWarnings(as.numeric(nestado[,resultado]))
    vresultado<-na.omit(nestado[,c("Hospital.Name",resultado)])
    hos<-vresultado[order(vresultado$Hospital.Name),]
    pro<-hos[order(hos[2]),]
    
    if (num=="mejor"){
        pro[1,1]
    } else if(num=="peor"){
        pro[nrow(pro),1]
    }else{
        pro[num,1]
    }
}
