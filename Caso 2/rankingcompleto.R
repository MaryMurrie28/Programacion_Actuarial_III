
rankingcompleto<-function(resultado,num="mejor"){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
   
     estados<-sort(unique(outcome[,7]))
    listap<-as.character()
    estado2<-as.character()
    
    
    if (resultado== "ataque"){
        outcome2<-outcome[11]
    }else if(resultado== "falla"){
        outcome2<-outcome[17]
    }else if(resultado=="neumonia"){
        outcome2<-outcome[23]
    }else{
        stop("Resultado inválido")
    }
    
    for (estado in estados){ 
        
        nestado<-outcome[outcome[,7]==estado,c(2,7,11,17,23)]  #estado que deseas
        names(nestado)[3:5]<-c("ataque","falla","neumonia")
        
        nestado[,resultado]<-suppressWarnings(as.numeric(nestado[,resultado]))
        vresultado<-na.omit(nestado[,c("Hospital.Name",resultado)])
        
        hos<-vresultado[order(vresultado$Hospital.Name),]
        pro<-hos[order(hos[2]),]
        
        if (num=="mejor"){
            me<-pro[1,1]
            
        } else if(num=="peor"){
            me<-pro[nrow(pro),1]
            
        }else{
            me<-pro[num,1]
        }
        listap<-c(listap,me)
        estado2<-c(estado2,estado)
    }
    tabla<-data.frame("Hospital"=listap,"Estado"=estado2)
    tabla
}
