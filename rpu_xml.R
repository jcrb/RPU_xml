#--------------------------------------------------------------
#
#       fonction read.rpu
#
#--------------------------------------------------------------
#
#'@name
#'@author jcb
#'@description lit un ensemble de fichiers csv contenus dans le dossier (créés par XML Parser) 
#'              et retoune un dataframe RPU
#'@param path chemin d'accès au dossier
#'@example source("rpu_xml.R"); path <- "~/html/XML Parser/FICHIERS_CSV/RPU_CSV"; read.rpu(path)
#'
read.rpu <- function(path){
        f <- list.files(path) # vecteur des noms de fichiers
        for(i in 1:length(f)){
                file <- paste(path, f[i], sep="/")
                # ecoding latin1 notemment pour les motifs de Colmar
                d<-read.table(file, header=TRUE, sep="\t", quote = "", encoding="latin1")
                # on ne retient que les jours consolidés cad correspondant à min(as.Date(d$entree, format="%d/%m/%Y"))
                d <- d[as.Date(d$ENTREE, format="%d/%m/%Y") == min(as.Date(d$ENTREE, format="%d/%m/%Y")),]
                if(i==1)
                        data<-d
                else
                        data<-rbind(data,d)
        }
        
        # colnames(data)<-c("date","finess","service","inf1an","entre1_75ans","sup75ans","total","hospitalises","UHCD","tranferts")
        # data$date<-as.Date(data$date,format="%d/%m/%Y")
        
        return(data)
}

#--------------------------------------------------------------
#
#       fonction read_fichier_rpu
#
#--------------------------------------------------------------
#
#
#'@name
#'@author jcb
#'@description lit un fichier csv et retourne un dataframe. Normalement le fichier contient 7 jours consécutifs
#'@param file_name nom du fichier à lire
#'@example file <- "RPU_670780055_141002_Hus.csv"; path <- "~/html/XML Parser/FICHIERS_CSV/RPU_CSV"; f <- paste0(path,"/",file); read_fichier_rpu(f)
#'
read_fichier_rpu <- function(file_name){
        d<-read.table(file_name, header=TRUE, sep="\t", quote = "", encoding="latin1")
        return (d)
}

#--------------------------------------------------------------
#
#       fonction formate_fichier_rpu
#
#--------------------------------------------------------------
#
#
#'@name
#'@author jcb
#'@description formate le dataframe créé par read_fichier_rpu et read.rpu
#'
formate_fichier_rpu <- function(d){
        library("lubridate")
        # finess to hopital
        
        # date de naissance JJ/MM/AAAA -> AAAA-MM-JJ
        d$NAISSANCE <- as.Date(d$NAISSANCE, format="%d/%m/%Y")
        # age: création d'une nouvelle colonne
        d$AGE <- 2014 - year(d$NAISSANCE)
        # date d'entrée et de sortie
        d$ENTREE <- dmy_hm(d$ENTREE)
        d$SORTIE <- dmy_hm(d$SORTIE)
        d$EXTRACT <- dmy_hms(d$EXTRACT)
        # Mode d'entrée
        d$MODE_ENTREE<-factor(d$MODE_ENTREE,levels=c(6,7,8),labels=c('Mutation','Transfert','Domicile'))
        # Provenance
        d$PROVENANCE<-factor(d$PROVENANCE,levels=c(1,2,3,4,5,8),labels=c('MCO','SSR','SLD','PSY','PEA','PEO'))
        # Transport
        d$TRANSPORT<-as.factor(d$TRANSPORT)
        # Prise en charge pendant le transport
        d$TRANSPORT_PEC<-as.factor(d$TRANSPORT_PEC)
        # motif
        
        # Gravité
        d$GRAVITE<-as.factor(d$GRAVITE)
        # DP (diagnostic principal)
        
        # mode de sortie
        d$MODE_SORTIE<-factor(d$MODE_SORTIE,levels=c(6,7,8,4),labels=c('Mutation','Transfert','Domicile','Décès'))
        # Orientation
        d$ORIENTATION<-as.factor(d$ORIENTATION)
        # Destination
        d$DESTINATION<-factor(d$DESTINATION,levels=c(1,2,3,4,6,7),labels=c('MCO','SSR','SLD','PSY','HAD','HMS'))
        # Durée de séjour en heures
        d$SEJOUR <- as.numeric((d$SORTIE - d$ENTREE)/3600)

        return(d)
}