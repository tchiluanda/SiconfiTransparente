library(rsiconfi)
library(stringr)

df_dcl<-rsiconfi::get_rgf(2018,"Q",3,1,"02","1","E")

timestamp()
df_dcl<- rsiconfi::get_rgf_mun_state(2018,"Q",3,1,"02","11","E")
timestamp()

df_pessoal <- rsiconfi::get_rgf(2018,"Q",3,1,"01",c(21:29),"E")


df_rec <- rsiconfi::get_dca(2018,"I-C","1")

library(networkD3)



df_rec %>% 
  filter(stringr::str_starts(cod_conta,"R")) %>%
  mutate(nivel_1= (stringr::str_ends(cod_conta,".0.0.0.00.0.0"))) %>%
  mutate(nivel_2= ((stringr::str_sub(cod_conta,5,5)!= "0") & (stringr::str_ends(cod_conta,"0.0.00.0.0")))) %>%
  mutate(nivel_3= ((stringr::str_sub(cod_conta,7,7)!= "0") & (stringr::str_ends(cod_conta,"0.00.0.0")))) %>%
  filter(nivel_1 | nivel_2 | nivel_3) %>%
  mutate(pai = case_when(
    nivel_1 == TRUE ~ NA,
    nivel_2 == TRUE ~ paste0(stringr::str_sub(cod_conta,1,3), ".0.0.0.00.0.0"),
    nivel_3 == TRUE ~ paste0(stringr::str_sub(cod_conta,1,5), "0.0.00.0.0")
  ))
  




# Load energy projection data
# Load energy projection data
URL <- paste0(
  "https://cdn.rawgit.com/christophergandrud/networkD3/",
  "master/JSONdata/energy.json")
Energy <- jsonlite::fromJSON(URL)
# Plot
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)


devtools::install_github("tomasbarcellos/datasus")

library(datasus)

sistemas_disponiveis()

df_datasus <- datasus('SIASUS', 'dados', 'CNES', 18, 'CE', '01')

hack_datasus <- function(sistema, modalidade, tipo_arquivo, ano, UF, mes){
  
  #Função gera dataframe a partir de ftp feita na página do datasus
  
  #sistema ex:'SIHSUS' Verificar os sistemas disponíveis em http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1
  #modalidade  'dados'
  #tipo_arquivo ex: 'RD'#Varia conforme o sistema
  #ano ex: 17 Dois últimos dígitos do ano 
  #UF ex:'AL' Sigla de UF Brasileira
  #mes ex:'12' strings entre 01 e 12
  
  
  dest_file<- paste0(tipo_arquivo,UF,ano,mes,".dbc")
  str_download <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/",sistema,"/","200508","_/",modalidade,"/",tipo_arquivo,"/", tipo_arquivo,UF,ano,mes,".dbc")
  download.file(str_download,destfile = dest_file, mode='wb')
  library(read.dbc)
  datasus<- read.dbc(dest_file)
  
  
}


#Leitos do Ceará
sistema <- 'CNES' #Verificar os sistemas disponíveis em http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1
modalidade <- 'dados'
tipo_arquivo<- 'LT'#Varia conforme o sistema
ano <- 19 #Dois últimos dígitos do ano 
UF <- 'CE' #Siglas das UFs Brasileiras
mes<- '12' #strings entre 01 e 12


#Carrega em df_datasus os dados relativos à base de dados montada a partir dos parãmetros
df_datasus<- hack_datasus(sistema, modalidade, tipo_arquivo, ano, UF, mes)

df_leitos_ce<- df_datasus


#Equipamentos do Ceará
sistema <- 'CNES' #Verificar os sistemas disponíveis em http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1
modalidade <- 'dados'
tipo_arquivo<- 'EQ'#Varia conforme o sistema
ano <- 19 #Dois últimos dígitos do ano 
UF <- 'CE' #Siglas das UFs Brasileiras
mes<- '12' #strings entre 01 e 12


#Carrega em df_datasus os dados relativos à base de dados montada a partir dos parãmetros
df_equipamentos_ce<- hack_datasus(sistema, modalidade, tipo_arquivo, ano, UF, mes)


#Profissionais do Ceará
sistema <- 'CNES' #Verificar os sistemas disponíveis em http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1
modalidade <- 'dados'
tipo_arquivo<- 'PF'#Varia conforme o sistema
ano <- 19 #Dois últimos dígitos do ano 
UF <- 'CE' #Siglas das UFs Brasileiras
mes<- '12' #strings entre 01 e 12


#Carrega em df_datasus os dados relativos à base de dados montada a partir dos parãmetros
df_profissionais_ce<- hack_datasus(sistema, modalidade, tipo_arquivo, ano, UF, mes)

df_equip_52<-
df_equipamentos_ce %>%
  filter(CODEQUIP==52) %>%
  group_by(CODUFMUN) %>%
  summarise(
    n()
  )
  
