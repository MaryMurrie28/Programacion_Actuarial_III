mejor<-function(estado,resultado){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")   
    estados<-outcome[,7]
    
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
    
    nestado<-outcome[outcome[,7]==estado,c(2,7,11,17,23)]  
    
    names(nestado)[3:5]<-c("ataque","falla","neumonia")
    
    
    nestado[,resultado]<-suppressWarnings(as.numeric(nestado[,resultado]))
    vresultado<-na.omit(nestado[,c("Hospital.Name",resultado)])
    m<-min(vresultado[,2])
    y<-vresultado[vresultado[,2]==m, 1]
    y1<-sort(y)
    y1
}
