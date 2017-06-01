mejor<-function(estado,resultado){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")   
    estados<-outcome[,7]
    
    
    if(length(estados[estados==estado])==0){stop("estado invalido")}
    if(length(c("ataque","falla","neumonia")[c("ataque","falla","neumonia")==resultado])==0){stop("resultado invalido")}
    nestado<-outcome[outcome[,7]==estado,c(2,7,11,17,23)]  #estado que deseas
    
    names(nestado)[3:5]<-c("ataque","falla","neumonia")
    
    
    nestado[,resultado]<-suppressWarnings(as.numeric(nestado[,resultado]))
    vresultado<-na.omit(nestado[,c("Hospital.Name",resultado)])
    m<-min(vresultado[,2])
    y<-vresultado[vresultado[,2]==m, 1]
    y1<-sort(y)
    #sort(y)[1]
    y1
}
