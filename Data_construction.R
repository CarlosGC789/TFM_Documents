### LINES OF CODE FROM 4 TO 92 HAVE BEEN MODIFIED CONTINUOUSLY SINCE UNTIL I RECEIVED
### THE TOKEN AND COULD WORK WITH THE PYTHON FUNCTIONS I WAS EXTRACTING THE DATA FROM BOTH APIDATA AND ESIOS BY HAND. 

library(jsonlite)
library(curl)
url_elec="https://apidatos.ree.es/en/datos/demanda/ire-general?start_date=2019-01-01T00:00&end_date=2020-12-31T23:59&time_trunc=month"
elec=fromJSON(url_elec)
View(elec)
met=elec$included
r=met$attributes
electric=as.data.frame(r$values[1])
View(electric)

View(pp)
library(dplyr)
library(tidyr)

ppt=data.frame(t(pp))
View(ppt)
names(ppt[1,])
str(ppt)
levels(factor(ppt[1,]))
names(ppt)=c("Fecha","Hidráulica","Turbinación bombeo",       
              "Nuclear","Carbón" ,               
              "Fuel + Gas"  ,               "Motores diésel" ,           
              "Turbina de gas"  ,    "Turbina de vapor" , "Ciclo combinado",           
              "Hidroeólica","Eólica"   ,         "Solar fotovoltaica",        
              "Solar térmica"         , "Otras renovables",       "Cogeneración",      
              "Residuos no renovables", "Residuos renovables",   
              "Generación total", "Meh" )
ppt=ppt[-1,-20]
row.names(ppt)=c(1:24)
library(readxl)
pre <- read_excel("C:/Users/carlo/Downloads/anual07-11.xlsx", 
                     col_names = FALSE)
View(pre)

pret=data.frame(t(pre))
View(pret)
names(pret[1,])
str(pret)
levels(factor(pret[1,]))
names(pret)=c("Fecha","Hidráulica","Turbinación bombeo",       
              "Nuclear","Carbón" ,               
              "Fuel + Gas"  ,               "Motores diésel" ,           
              "Turbina de gas"  ,    "Turbina de vapor" , "Ciclo combinado",           
              "Eólica"   ,         "Solar fotovoltaica",        
              "Solar térmica"         , "Otras renovables",       "Cogeneración",      
              "Residuos no renovables", "Residuos renovables",   
              "Generación total")
pret=pret[-1,]
row.names(pret)=c(1:16)
View(pret)

price_año=c(NA,NA,NA,NA,NA,NA,NA,price_year)
ppo=cbind(price_año,pret)  
View(ppo)

price_year=c(mean(tfm[1:12,20]),mean(tfm[13:24,20]),mean(tfm[25:36,20]),mean(tfm[37:48,20]),
             mean(tfm[49:60,20]),mean(tfm[61:72,20]),mean(tfm[73:84,20]),mean(tfm[85:96,20]),
             mean(tfm[97:108,20]))
price_year

#### PRECIO LUZ
url_elec="https://apidatos.ree.es/es/datos/balance/balance-electrico?start_date=2014-01-01T00:00&end_date=2014-12-31T23:59&time_trunc=day"
elec=fromJSON(url_elec)
View(elec)
met=elec$included
r=met$attributes
bu=r$content[2]
tt=bu[[1]]
pe=tt$attributes
eol=pe$values[11][[1]]
eol=eol[,3]
daily2=cbind(eol,daily2)
View(daily2)
daily2=as.data.frame(daily2)
names(daily2)=c("Fecha","Hidraulica","Eolica", "Solar_fotovoltaica","Solar_termica",
               "Hidroeolica","Otras_renovables", "Residuos_renovables", "Generacion_renovables",
               "Turbinacion_bombeo","Nuclear","Ciclo_combinado","Carbon",
               "Motores_diesel", "Turbina_gas","Turbina_vapor","Cogeneracion",
               "Residuos_no_renovables","Generacion_no_renovable")
str(daily2)

daily=rbind(daily2,daily)
price=electric[,1]
prices=c(prices,price)
prices

tfm[,20]=prices
dududu=cbind(ppo,prices)
View(dududu)

##LINE CODE TO SAVE THE DATASET WE HAVE AT THAT MOMENT
write.table(tfm_hour, file="tfm_hour.txt")
## LINE CODE TO READ THE DATASET WE HAD AT THAT MOMENT (TO CONTINUE MODIFYING IT)
tfm_year <- read.csv("~/tfm_year.txt", sep="")

##### DAILY GAS PRICE

library(readxl)
#Readf the excel file we obtained from MIBGAS
gas <- read_excel("C:/Users/carlo/Downloads/gas_dias.xlsx")
View(gas)
#As the excel is from 2023 to 2015, we want the variable to go from 2015 to 2023
rev(as.vector(gas[,2]))
names(gas)=c("Day", "Gas_price")
rev(as.vector(gas$Gas_price))
#Include the price of gas in the dates we have
tfm_daily[c(715:3318),24]=rev(as.vector(gas$Gas_price))
rev(v)

## LINE CODE TO READ THE DATASET WE HAD AT THAT MOMENT (TO CONTINUE MODIFYING IT)
tfm_year <- read.csv("~/tfm_year.txt", sep="")
View(tfm_monthly)
View(tfm_year)

colnames(tfm_daily)[24]=c("Precio_gas")

### INCLUDE SOME ANNUAL ELECTRICITY VALUES
tfm_year[c(1:7),1]=c(39.35,64.43,36.96,37.01,49.93,47.23,44.26)




####### HOURLY ENERGY VALUES

library(readxl)
#Read the hydraulic excel file obtained thanks to the Python function
hidraulica <- read_excel("C:/Users/carlo/Downloads/hidraulica.xlsx")
#Stay with the date and the UGH and not UGH values
hidraulica=hidraulica[,c(1,2,9)]
#Total hydraulic values is the sum of the UGH and not UGH
hidraulica$hidraulica=hidraulica[,1]+hidraulica[,3]
#Stay with the total value and the date
hidraulica=hidraulica[,c(2,4)]
#Change the format of the value to have a numeric variable
hidraulica[,2]=hidraulica$hidraulica$value...1
str(hidraulica)

names(hidraulica)=c("datetime","Hidraulica")
#Change the date format to have the same as the tfm_hour
hidraulica$datetime <- gsub("\\.000", "", hidraulica$datetime)
#Add the hydraulic energy variable to the dataset
tfm_hour=merge(tfm_hour,hidraulica, by = "datetime")

#Read the wind power excel file obtained thanks to Python function
eolica <- read_excel("C:/Users/carlo/Downloads/eolica.xlsx")
#Only stay with the value and the date
eolica=eolica[,c(1,2)]
names(eolica)=c("Eolica","datetime")
#Change the date format to have the same as the tfm_hour
eolica$datetime <- gsub("\\.000", "", eolica$datetime)
#Add the wind power variable to the dataset
tfm_hour=merge(tfm_hour,eolica, by = "datetime")

#Read the solar photovoltaic excel file obtained thanks to Python function
fotov <- read_excel("C:/Users/carlo/Downloads/fotov.xlsx")
#Only stay with the value and the dates
fotov=fotov[,c(1,2)]
names(fotov)=c("Solar_fotovoltaica","datetime")
#Change the date format to have the same as the tfm_hour
fotov$datetime <- gsub("\\.000", "", fotov$datetime)
#Add the solar photovoltaic variable to the dataset
tfm_hour=merge(tfm_hour,fotov, by = "datetime", all=TRUE)
#Since we know that there are no NAs in wind energy, we check for NAs
#in that column to check for repeated dates.
which(is.na(tfm_hour$Eolica))
#Remove repeated dates
tfm_hour=tfm_hour[-c(7154,24793,42264,59735,77374),]

#Read the solar thermal excel file obtained thanks to Python function
termica <- read_excel("C:/Users/carlo/Downloads/termica.xlsx")
#Only stay with the value and the dates
termica=termica[,c(1,2)]
names(termica)=c("Solar_termica","datetime")
#Change the date format to have the same as the tfm_hour
termica$datetime <- gsub("\\.000", "", termica$datetime)
#Add the solar thermal variable to the dataset
tfm_hour=merge(tfm_hour,termica, by = "datetime", all=TRUE)
#Since we know that there are no NAs in wind energy, we check for NAs
#in that column to check for repeated dates.
which(is.na(tfm_hour$Eolica))
#Remove repeated dates
tfm_hour=tfm_hour[-c(7154,24793,42264,59735,77374),]

#Read the Other renewable energies excel file obtained thanks to Python function
otrasrenov <- read_excel("C:/Users/carlo/Downloads/otrasrenov.xlsx")
#Only stay with the value and the dates
otrasrenov=otrasrenov[,c(1,2)]
names(otrasrenov)=c("Otras_renovables","datetime")
#Change the date format to have the same as the tfm_hour
otrasrenov$datetime <- gsub("\\.000", "", otrasrenov$datetime)
#Add the other renewable energies variable to the dataset
tfm_hour=merge(tfm_hour,otrasrenov, by = "datetime", all=TRUE)
#Since we know that there are no NAs in wind energy, we check for NAs
#in that column to check for repeated dates.
which(is.na(tfm_hour$Eolica))
#Remove repeated dates
tfm_hour=tfm_hour[-c(7154,24793,42264,59735,77374),]

#Read the pumping turbine excel file obtained thanks to Python function
turbombeo <- read_excel("C:/Users/carlo/Downloads/turbombeo.xlsx")
#Only stay with the value and the dates
turbombeo=turbombeo[,c(1,2)]
names(turbombeo)=c("Turbinacion_bombeo","datetime")
#Change the date format to have the same as the tfm_hour
turbombeo$datetime <- gsub("\\.000", "", turbombeo$datetime)
#Add the pumping turbine variable to the dataset
tfm_hour=merge(tfm_hour,turbombeo, by = "datetime", all=TRUE)
#Since we know that there are no NAs in wind energy, we check for NAs
#in that column to check for repeated dates.
which(is.na(tfm_hour$Eolica))
#Remove repeated dates
tfm_hour=tfm_hour[-c(74226),]

#Read the nuclear energy excel file obtained thanks to Python function
nuclear <- read_excel("C:/Users/carlo/Downloads/nuclear.xlsx")
#Only stay with the value and the dates
nuclear=nuclear[,c(1,2)]
names(nuclear)=c("Nuclear","datetime")
#Change the date format to have the same as the tfm_hour
nuclear$datetime <- gsub("\\.000", "", nuclear$datetime)
#Add the nuclear energy variable to the dataset
tfm_hour=merge(tfm_hour,nuclear, by = "datetime")

#Read the combined cycle excel file obtained thanks to Python function
ccomb <- read_excel("C:/Users/carlo/Downloads/ccomb.xlsx")
#Only stay with the value and the dates
ccomb=ccomb[,c(1,2)]
names(ccomb)=c("Ciclo_combinado","datetime")
#Change the date format to have the same as the tfm_hour
ccomb$datetime <- gsub("\\.000", "", ccomb$datetime)
#Add the combined cycle variable to the dataset
tfm_hour=merge(tfm_hour,ccomb, by = "datetime")

#Read the coal excel file obtained thanks to Python function
carbon <- read_excel("C:/Users/carlo/Downloads/carbon.xlsx")
#Only stay with the value and the dates
carbon=carbon[,c(1,2)]
names(carbon)=c("Carbon","datetime")
#Change the date format to have the same as the tfm_hour
carbon$datetime <- gsub("\\.000", "", carbon$datetime)
#Add the combined cycle variable to the dataset
tfm_hour=merge(tfm_hour,carbon, by = "datetime", all=TRUE)
#Since we know that there are no NAs in wind energy, we check for NAs
#in that column to check for repeated dates.
which(is.na(tfm_hour$Eolica))
#Remove the repeated dates
tfm_hour=tfm_hour[-c(7154,24793,42264,59735),]

#Read the cogeneration excel file obtained thanks to Python function
cogeneracion <- read_excel("C:/Users/carlo/Downloads/cogeneracion.xlsx")
#Only stay with the value and the dates
cogeneracion=cogeneracion[,c(1,2)]
names(cogeneracion)=c("Cogeneracion","datetime")
#Change the date format to have the same as the tfm_hour
cogeneracion$datetime <- gsub("\\.000", "", cogeneracion$datetime)
#Add the cogeneration variable to the dataset
tfm_hour=merge(tfm_hour,cogeneracion, by = "datetime")

### SHIFT FROM GAS PRICES TO OTHER TIME VARIABLES

#From the daily gas price, I move to the hourly, monthly and
#yearly database as counted in the report.

## In this case, the last step is shown, which is the change from
#monthly to annual in the last step.
tfm_year$Precio_gas=rep(NA,length=23)
v=0
d=0
#Weighted average
for (i in 1:12){
  v=v+sum(tfm_monthly[i+96,21]*tfm_monthly[i+96,24])
  d=d+sum(tfm_monthly[i+96,21])
}
v/d
tfm_year[23,23]=v/d

####### Monthly values from 2000 to 2013

## To add the months from 2000 to 2013 I build a database that
#I will then join to the one we already have.
prueba=as.data.frame(matrix(NA,nrow=168,ncol=24))
names(prueba)=names(tfm_monthly)
#The new dates
prueba[,1]=c("01/2000", "02/2000", "03/2000", "04/2000", "05/2000", "06/2000", "07/2000",
           "08/2000", "09/2000", "10/2000", "11/2000", "12/2000", "01/2001", "02/2001",
            "03/2001", "04/2001", "05/2001", "06/2001", "07/2001", "08/2001", "09/2001",
            "10/2001", "11/2001", "12/2001", "01/2002", "02/2002", "03/2002", "04/2002",
            "05/2002", "06/2002", "07/2002", "08/2002", "09/2002", "10/2002", "11/2002",
            "12/2002", "01/2003", "02/2003", "03/2003", "04/2003", "05/2003", "06/2003",
            "07/2003", "08/2003", "09/2003", "10/2003", "11/2003", "12/2003", "01/2004",
            "02/2004", "03/2004", "04/2004", "05/2004", "06/2004", "07/2004", "08/2004",
            "09/2004", "10/2004", "11/2004", "12/2004", "01/2005", "02/2005", "03/2005",
            "04/2005", "05/2005", "06/2005", "07/2005", "08/2005", "09/2005", "10/2005",
            "11/2005", "12/2005", "01/2006", "02/2006", "03/2006", "04/2006", "05/2006",
            "06/2006", "07/2006", "08/2006", "09/2006", "10/2006", "11/2006", "12/2006",
            "01/2007", "02/2007", "03/2007", "04/2007", "05/2007", "06/2007", "07/2007",
            "08/2007", "09/2007", "10/2007", "11/2007", "12/2007", "01/2008", "02/2008",
            "03/2008", "04/2008", "05/2008", "06/2008", "07/2008", "08/2008", "09/2008",
            "10/2008", "11/2008", "12/2008", "01/2009", "02/2009", "03/2009", "04/2009", "05/2009", "06/2009", "07/2009",
           "08/2009", "09/2009", "10/2009", "11/2009", "12/2009", "01/2010", "02/2010",
           "03/2010", "04/2010", "05/2010", "06/2010", "07/2010", "08/2010", "09/2010",
           "10/2010", "11/2010", "12/2010", "01/2011", "02/2011", "03/2011", "04/2011",
           "05/2011", "06/2011", "07/2011", "08/2011", "09/2011", "10/2011", "11/2011",
           "12/2011", "01/2012", "02/2012", "03/2012", "04/2012", "05/2012", "06/2012",
           "07/2012", "08/2012", "09/2012", "10/2012", "11/2012", "12/2012", "01/2013",
           "02/2013", "03/2013", "04/2013", "05/2013", "06/2013", "07/2013", "08/2013",
           "09/2013", "10/2013", "11/2013", "12/2013")
View(prueba)
# Read the excel file with monthly values
#from 2000 to 2013 for the different energies.
meses<- read_excel("C:/Users/carlo/Downloads/meses_energias.xlsx")

# Add the new rows corresponding to the months from 2000 to 2013 
tfm_monthly=rbind(prueba,tfm_monthly)
View(meses)
## Add the values to the tfm_monthy database. Here you can see how to add an energy. It is done one by one
#because they are not put in the same order in the two databases.
tfm_monthly[c(1:168),19]=as.numeric(meses[14,-1])

#Same as above but with annual energy values from 2000 to 2013.
años<- read_excel("C:/Users/carlo/Downloads/años_energias.xlsx")
View(años)
tfm_year[c(1:14),19]=as.numeric(años[14,-1])


### CO2 PRICES

# Read the excel file with the daily CO2 prices.
co2 <- read.csv("C:/Users/carlo/Downloads/precios_co2.csv", sep=";")



library(lubridate)

#Change the date format of the hourly database
fechas_converted <- with_tz(as.POSIXct(tfm_hour$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
tfm_hour[,1]=fechas_final

#Change the date of the CO2 price to be the same as the daily database.
fecha_convertida <- as.POSIXct(tfm_hour$datetime, tz = "GMT")
fecha_convertida <- format(fecha_convertida, format = "%Y-%m-%dT%H:%M:%S+01:00")
co2$Fecha=fecha_convertida
names(co2)=c("Fecha", "CO2_price")

# Join CO2 price variable to daily database
tfm_daily=merge(tfm_daily,co2, by = "Fecha", all=TRUE)
which(is.na(prueba$Hidraulica))
View(prueba)

summary((factor(prueba$Fecha)))

setdiff(prueba$Fecha,tfm_daily$Fecha)

# Check if the column "Fecha" of the dataframe "tfm_daily" is repeated in more than one row
if (any(duplicated(tfm_daily$Fecha) | duplicated(tfm_daily$Fecha, fromLast = TRUE))) {
  print("La columna 'fecha' del dataframe 'df' está repetida en más de una fila.")
} else {
  print("La columna 'fecha' del dataframe 'df' no está repetida en más de una fila.")
}

# Find the rows that have duplicate dates in the "Fecha" column of dataframe "tfm_daily".
duplicated_rows <- which(duplicated(prueba$Fecha) | duplicated(prueba$Fecha, fromLast = TRUE))

#Obtain a list of duplicate dates
duplicated_dates <- unique(prueba$Fecha[duplicated_rows])

# Print the list of duplicate dates
print(duplicated_dates)
#Remove the duplicated dates
tfm_daily=tfm_daily[-c(997,1230),]

#We remove rows where there are no monthly CO2 price values to add these values.
tfm_monthly=tfm_monthly[-c(1:120),]

tfm_monthly$C02_precio=rep(NA,length=157)
#Add values by hand
tfm_monthly[157,25]=80.29

#Add the CO2 annual price to the annual dataset
tfm_year[c(9:23),24]=c(22.02,13.06,14.32,12.89,7.33,4.45,5.96,7.68,
                       5.35,5.83,15.88,24.84,24.75,53.55,80.87)

###### INSTALLED POWER

library(readxl)
# Read the installed power excel we made
potencia <- read_excel("C:/Users/carlo/Downloads/potencia_instalada.xlsx")

#Create a data frame to put the values in the format we want
prueba=as.data.frame(matrix(NA,nrow=13,ncol=8))

#Write by hand the columns we want
prueba[,8]=as.numeric(potencia[8,-1])
prueba$V9=rep(NA,length=13)
prueba[,9]=as.numeric(potencia[9,-1])
#Change the names of the variables
names(prueba)=c("Potencia_hidraulica","Potencia_nuclear","Potencia_carbon","Potencia_ciclo_comb",
              "Potencia_eolica","Potencia_solar_fotov","Potencia_solar_termica",
              "Potencia_otras_renovables","Potencia_cogeneracion")

library(dplyr)
#Change the units of measurement
prueba <- prueba %>% mutate_all(~ (. / 1000) * 8760)
#Create a new data frame to do cbind later with tfm_year
datt=as.data.frame(matrix(NA, nrow=10,ncol=9))
names(datt)=names(prueba)
prueba=rbind(datt,prueba)
tfm_year=cbind(tfm_year,prueba)

tfm_year[10,29]=(18719/1000)*8760

#def installed power= Maximum power that a production unit can achieve,
#over a given period of time, measured at the output of the alternator terminals.

### change from daily to hourly gas and co2 prices

#Create a new variable in the hour dataset
tfm_hour$CO2_precio=rep(NA, length=79623)

prueba=tfm_hour
datt=tfm_daily


# Convert date column in daily database to POSIXct format
datt$datetime <- as.POSIXct(datt$datetime)

# Convert date column in hourly database to POSIXct format
prueba$datetime <- as.POSIXct(prueba$datetime)

# Join the two databases by date column
prueba_actualizada <- merge(prueba, datt, by = "datetime")
View(prueba_actualizada)

#I keep the columns that interest me
tfm_hour$CO2_precio=prueba_actualizada$CO2_price
tfm_hour$Precio_gas=prueba_actualizada$Precio_gas.y


# I get rid of unwanted variables from the merge and others
tfm_year=tfm_year[,-c(7,8,9,10,19,21,22)]
tfm_monthly=tfm_monthly[,-c(6,7,8,9,11,19,22,23)]
tfm_daily=tfm_daily[,-c(6,9,19,21,22)]
tfm_hour=tfm_hour[,-c(4,5,11)]

# Create vector with missing values for Saturdays and Sundays
valores <- tfm_daily$CO2_price

#  Use the lag function to assign the Friday value to Saturdays and Sundays.
valores[is.na(valores)] <- lag(valores)[is.na(valores)]

# Show resultant vector
tfm_daily$CO2_price=valores
sum(is.na(valores))


#### DAILY INTERNATIONAL BALANCE WITH PORTUGAL, FRANCE AND MOROCCO

library(readxl)
#Read the diary internation trade balance with the three countries we have thanks to Python function
intern_dia <- read_excel("C:/Users/carlo/Downloads/intern_diario.xlsx")
View(intern_dia)
#Divide in France, Portugal and Morocco
lista_datos <- split(intern_dia, intern_dia$short_name)

#We have exports to France in first
francia_exp=as.data.frame(lista_datos[1])
#We stay with the values and date
francia_exp=francia_exp[,c(1,2)]
names(francia_exp)=c("Francia_exp", "datetime")
#Change the date format to have the same as the daily dataset
fechas_converted <- with_tz(as.POSIXct(francia_exp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
francia_exp$datetime=fechas_final

#We have imports from France in second
francia_imp=as.data.frame(lista_datos[2])
#We stay with the values and date
francia_imp=francia_imp[,c(1,2)]
names(francia_imp)=c("Francia_imp", "datetime")
#Change the date format to have the same as the daily dataset
fechas_converted <- with_tz(as.POSIXct(francia_imp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
francia_imp$datetime=fechas_final

#We create a dataset that includes imports and exports with France
balance_francia=merge(francia_imp,francia_exp, by="datetime")

#We have exports to Portugal in third
portugal_exp=as.data.frame(lista_datos[3])
#We stay with the values and date
portugal_exp=portugal_exp[,c(1,2)]
names(portugal_exp)=c("portugal_exp", "datetime")
#Change the date format to have the same as the daily dataset
fechas_converted <- with_tz(as.POSIXct(portugal_exp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
portugal_exp$datetime=fechas_final

#We have imports from Portugal in fourth
portugal_imp=as.data.frame(lista_datos[4])
#We stay with the values and date
portugal_imp=portugal_imp[,c(1,2)]
names(portugal_imp)=c("portugal_imp", "datetime")
#Change the date format to have the same as the daily dataset
fechas_converted <- with_tz(as.POSIXct(portugal_imp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
portugal_imp$datetime=fechas_final

#We create a dataset that includes imports and exports with Portugal
balance_portugal=merge(portugal_imp,portugal_exp, by="datetime")

#We have exports to Morocco in fifth
marruecos_exp=as.data.frame(lista_datos[5])
#We stay with the values and date
marruecos_exp=marruecos_exp[,c(1,2)]
names(marruecos_exp)=c("marruecos_exp", "datetime")
#Change the date format to have the same as the daily dataset
fechas_converted <- with_tz(as.POSIXct(marruecos_exp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
marruecos_exp$datetime=fechas_final

#We have imports from Morocco in sixth
marruecos_imp=as.data.frame(lista_datos[6])
#We stay with the values and date
marruecos_imp=marruecos_imp[,c(1,2)]
names(marruecos_imp)=c("marruecos_imp", "datetime")
#Change the date format to have the same as the daily dataset
fechas_converted <- with_tz(as.POSIXct(marruecos_imp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
marruecos_imp$datetime=fechas_final

#We create a dataset that includes imports and exports with Morocco
balance_marruecos=merge(marruecos_imp,marruecos_exp, by="datetime")

#We calculate the balance of trade (exports minus imports).
balance_francia$Balanza_Francia=balance_francia$Francia_exp-balance_francia$Francia_imp
balance_portugal$Balanza_Portugal=balance_portugal$portugal_exp-balance_portugal$portugal_imp
balance_marruecos$Balanza_Marruecos=balance_marruecos$marruecos_exp-balance_marruecos$marruecos_imp

#We stay with the dates and the trade
balance_francia=balance_francia[,c(1,4)]
balance_portugal=balance_portugal[,c(1,4)]
balance_marruecos=balance_marruecos[,c(1,4)]
names(balance_francia)[1]="Fecha"
names(balance_portugal)[1]="Fecha"
names(balance_marruecos)[1]="Fecha"

#We include these values in the daily dataset
tfm_daily=merge(tfm_daily,balance_francia, by="Fecha", all=TRUE)
tfm_daily=merge(tfm_daily,balance_portugal,by="Fecha", all=TRUE)
tfm_daily=merge(tfm_daily,balance_marruecos,by="Fecha", all=TRUE)

#### MONTHLY INTERNATIONAL BALANCE WITH PORTUGAL, FRANCE AND MOROCCO

#We read the excel of monthly international balance thanks to the Python function 
intern_mes <- read_excel("C:/Users/carlo/Downloads/intern_mes.xlsx")
#We divide the dataset into imports and exports of the three countries
lista_datos <- split(intern_mes, intern_mes$short_name)

#We have exports to France in first
francia_exp=as.data.frame(lista_datos[1])
#We stay with the values and the date
francia_exp=francia_exp[,c(1,2)]
names(francia_exp)=c("Francia_exp", "datetime")
#Change the date format to have the same as the monthly dataset
fechas_converted <- with_tz(as.POSIXct(francia_exp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
francia_exp$datetime=fechas_final

#We have imports from France in second
francia_imp=as.data.frame(lista_datos[2])
#We stay with the values and the date
francia_imp=francia_imp[,c(1,2)]
names(francia_imp)=c("Francia_imp", "datetime")
#Change the date format to have the same as the monthly dataset
fechas_converted <- with_tz(as.POSIXct(francia_imp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
francia_imp$datetime=fechas_final

#We create a dataset that includes imports and exports with France
balance_francia=merge(francia_imp,francia_exp, by="datetime")

#We have exports to Portugal in third
portugal_exp=as.data.frame(lista_datos[3])
#We stay with the values and the date
portugal_exp=portugal_exp[,c(1,2)]
names(portugal_exp)=c("portugal_exp", "datetime")
#Change the date format to have the same as the monthly dataset
fechas_converted <- with_tz(as.POSIXct(portugal_exp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
portugal_exp$datetime=fechas_final

#We have imports from Portugal in fourth
portugal_imp=as.data.frame(lista_datos[4])
#We stay with the values and the date
portugal_imp=portugal_imp[,c(1,2)]
names(portugal_imp)=c("portugal_imp", "datetime")
#Change the date format to have the same as the monthly dataset
fechas_converted <- with_tz(as.POSIXct(portugal_imp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
portugal_imp$datetime=fechas_final

#We create a dataset that includes imports and exports with Portugal
balance_portugal=merge(portugal_imp,portugal_exp, by="datetime")

#We have exports to Morocco in fifth
marruecos_exp=as.data.frame(lista_datos[5])
#We stay with the values and the date
marruecos_exp=marruecos_exp[,c(1,2)]
names(marruecos_exp)=c("marruecos_exp", "datetime")
#Change the date format to have the same as the monthly dataset
fechas_converted <- with_tz(as.POSIXct(marruecos_exp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
marruecos_exp$datetime=fechas_final

#We have imports from Morocco in sixth
marruecos_imp=as.data.frame(lista_datos[6])
#We stay with the values and the date
marruecos_imp=marruecos_imp[,c(1,2)]
names(marruecos_imp)=c("marruecos_imp", "datetime")
#Change the date format to have the same as the monthly dataset
fechas_converted <- with_tz(as.POSIXct(marruecos_imp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
marruecos_imp$datetime=fechas_final

#We create a dataset that includes imports and exports with Morocco
balance_marruecos=merge(marruecos_imp,marruecos_exp, by="datetime")

#We calculate the balance of trade (exports minus imports).
balance_francia$Balanza_Francia=balance_francia$Francia_exp-balance_francia$Francia_imp
balance_portugal$Balanza_Portugal=balance_portugal$portugal_exp-balance_portugal$portugal_imp
balance_marruecos$Balanza_Marruecos=balance_marruecos$marruecos_exp-balance_marruecos$marruecos_imp

#We stay with the date and the trade values
balance_francia=balance_francia[,c(1,4)]
balance_portugal=balance_portugal[,c(1,4)]
balance_marruecos=balance_marruecos[,c(1,4)]
names(balance_francia)[1]="Fecha"
names(balance_portugal)[1]="Fecha"
names(balance_marruecos)[1]="Fecha"

#We change the date format of the balances to have the same as the monthly dataset
fechas <- as.POSIXct(balance_francia$Fecha, format="%Y-%m-%d")
fechas_formateadas <- format(fechas, "%m/%Y")
balance_francia$Fecha=fechas_formateadas

fechas <- as.POSIXct(balance_portugal$Fecha, format="%Y-%m-%d")
fechas_formateadas <- format(fechas, "%m/%Y")
balance_portugal$Fecha=fechas_formateadas

fechas <- as.POSIXct(balance_marruecos$Fecha, format="%Y-%m-%d")
fechas_formateadas <- format(fechas, "%m/%Y")
balance_marruecos$Fecha=fechas_formateadas

#Merge the trade with France with the monthly dataset
prueba=merge(tfm_monthly,balance_francia, by="Fecha", all = TRUE)
View(prueba)

#Change the date format of the monthly dataset
prueba$Fecha <- as.Date(paste("01", prueba$Fecha, sep = "/"), format = "%d/%m/%Y")
prueba <- prueba[order(prueba$Fecha),]
prueba$Fecha <- format(prueba$Fecha, "%m/%Y")
rownames(prueba)=c(1:157)
tfm_monthly=prueba

#Merge the trade with Portugal with the monthly dataset
prueba=merge(tfm_monthly,balance_portugal, by="Fecha", all = TRUE)
View(prueba)

#Change the date format of the monthly dataset
prueba$Fecha <- as.Date(paste("01", prueba$Fecha, sep = "/"), format = "%d/%m/%Y")
prueba <- prueba[order(prueba$Fecha),]
prueba$Fecha <- format(prueba$Fecha, "%m/%Y")
rownames(prueba)=c(1:157)
tfm_monthly=prueba

#Merge the trade with Morocco with the monthly dataset
prueba=merge(tfm_monthly,balance_marruecos, by="Fecha", all = TRUE)
View(prueba)

#Change the date format of the monthly dataset
prueba$Fecha <- as.Date(paste("01", prueba$Fecha, sep = "/"), format = "%d/%m/%Y")
prueba <- prueba[order(prueba$Fecha),]
prueba$Fecha <- format(prueba$Fecha, "%m/%Y")
rownames(prueba)=c(1:157)
tfm_monthly=prueba

#### ANNUAL INTERNATIONAL BALANCE WITH PORTUGAL, FRANCE AND MOROCCO

#Read the excel we have with the annual international balance thanks to the Python function
intern_año <- read_excel("C:/Users/carlo/Downloads/intern_año.xlsx")
#Divide the dataset into the exports and imports with the three countries
lista_datos <- split(intern_año, intern_año$short_name)

#We have exports to France in first
francia_exp=as.data.frame(lista_datos[1])
#We stay with the values and the date
francia_exp=francia_exp[,c(1,2)]
names(francia_exp)=c("Francia_exp", "datetime")
#Change the date format to have the same as the annual dataset
fechas_converted <- with_tz(as.POSIXct(francia_exp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
francia_exp$datetime=fechas_final

#We have imports from France in second
francia_imp=as.data.frame(lista_datos[2])
#We stay with the values and the date
francia_imp=francia_imp[,c(1,2)]
names(francia_imp)=c("Francia_imp", "datetime")
#Change the date format to have the same as the annual dataset
fechas_converted <- with_tz(as.POSIXct(francia_imp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
francia_imp$datetime=fechas_final

#We create a dataset that includes imports and exports with France
balance_francia=merge(francia_imp,francia_exp, by="datetime")

#We have exports to Portugal in third
portugal_exp=as.data.frame(lista_datos[3])
#We stay with the values and the date
portugal_exp=portugal_exp[,c(1,2)]
names(portugal_exp)=c("portugal_exp", "datetime")
#Change the date format to have the same as the annual dataset
fechas_converted <- with_tz(as.POSIXct(portugal_exp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
portugal_exp$datetime=fechas_final

#We have imports from Portugal in fourth
portugal_imp=as.data.frame(lista_datos[4])
#We stay with the values and the date
portugal_imp=portugal_imp[,c(1,2)]
names(portugal_imp)=c("portugal_imp", "datetime")
#Change the date format to have the same as the annual dataset
fechas_converted <- with_tz(as.POSIXct(portugal_imp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
portugal_imp$datetime=fechas_final

#We create a dataset that includes imports and exports with Portugal
balance_portugal=merge(portugal_imp,portugal_exp, by="datetime")

#We have exports to Morocco in fifth
marruecos_exp=as.data.frame(lista_datos[5])
#We stay with the values and the date
marruecos_exp=marruecos_exp[,c(1,2)]
names(marruecos_exp)=c("marruecos_exp", "datetime")
#Change the date format to have the same as the annual dataset
fechas_converted <- with_tz(as.POSIXct(marruecos_exp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
marruecos_exp$datetime=fechas_final

#We have imports from Morocco in sixth
marruecos_imp=as.data.frame(lista_datos[6])
#We stay with the values and the date
marruecos_imp=marruecos_imp[,c(1,2)]
names(marruecos_imp)=c("marruecos_imp", "datetime")
#Change the date format to have the same as the annual dataset
fechas_converted <- with_tz(as.POSIXct(marruecos_imp$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
marruecos_imp$datetime=fechas_final

#We create a dataset that includes imports and exports with Portugal
balance_marruecos=merge(marruecos_imp,marruecos_exp, by="datetime")

#We calculate the balance of trade (exports minus imports).
balance_francia$Balanza_Francia=balance_francia$Francia_exp-balance_francia$Francia_imp
balance_portugal$Balanza_Portugal=balance_portugal$portugal_exp-balance_portugal$portugal_imp
balance_marruecos$Balanza_Marruecos=balance_marruecos$marruecos_exp-balance_marruecos$marruecos_imp

#We stay with the date and trade values
balance_francia=balance_francia[,c(1,4)]
balance_portugal=balance_portugal[,c(1,4)]
balance_marruecos=balance_marruecos[,c(1,4)]
names(balance_francia)[1]="Fecha"
names(balance_portugal)[1]="Fecha"
names(balance_marruecos)[1]="Fecha"

#We change the date format of the balances to have the same as the annual dataset
fechas <- as.POSIXct(balance_francia$Fecha, format="%Y-%m-%d")
fechas_formateadas <- format(fechas, "%Y")
balance_francia$Fecha=fechas_formateadas

fechas <- as.POSIXct(balance_portugal$Fecha, format="%Y-%m-%d")
fechas_formateadas <- format(fechas, "%Y")
balance_portugal$Fecha=fechas_formateadas

fechas <- as.POSIXct(balance_marruecos$Fecha, format="%Y-%m-%d")
fechas_formateadas <- format(fechas, "%Y")
balance_marruecos$Fecha=fechas_formateadas

#Merge the trades with the annual dataset
tfm_year=merge(tfm_year,balance_francia, by="Fecha", all=TRUE)
tfm_year=merge(tfm_year,balance_portugal, by="Fecha", all = TRUE)
tfm_year=merge(tfm_year,balance_marruecos, by="Fecha", all=TRUE)

sum(is.na(tfm_daily$Balanza_Marruecos))
sum(is.na(tfm_daily$Balanza_Francia))
sum(is.na(tfm_daily$Balanza_Portugal))

#### BALANCE OF THE DAILY TRADE BALANCE BUT NOT CALCULATED BY HAND 
library(readxl)
#We read the excel of the trade calculated thanks to the Python function
balances=read_excel("C:/Users/carlo/Downloads/balances_dia.xlsx")
#Divide the dataset into the three countries
lista_datos <- split(balances, balances$short_name)

#We have France in first
francia=as.data.frame(lista_datos[1])
#We stay with the values and the date
francia_exp=francia_exp[,c(1,2)]
names(francia_exp)=c("Balance", "datetime")
#We include it into the daily dataset (in the column where we had the results before)
tfm_daily[,21]=francia[,1]

library(lubridate)
#We have Morocco in second
marruecos=as.data.frame(lista_datos[2])
#We stay with the values and the date
marruecos=marruecos[,c(1,2)]
names(marruecos)=c("Saldo_Marruecos", "Fecha")
#We change the date format to have the same as the daily dataset
fechas_converted <- with_tz(as.POSIXct(marruecos$Fecha, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fechas_final
marruecos$Fecha=fechas_final

#We include it into the daily dataset
tfm_daily=merge(tfm_daily,marruecos,by="Fecha", all = TRUE)
tfm_daily=tfm_daily[,-23]
names(tfm_daily)[23]="Balanza_Marruecos"

#We have Portugal in third
portugal=as.data.frame(lista_datos[3])
#We include it into the daily dataset (in the column where we had the results before)
tfm_daily[,22]=portugal[,1]

#As in the excel we had imports-exports, we multiply by -1 to have the trade (exports-imports)
tfm_daily[,21]=(-1)*tfm_daily[,21]
tfm_daily[,22]=(-1)*tfm_daily[,22]
tfm_daily[,23]=(-1)*tfm_daily[,23]


#### HOURLY INTERNATIONAL BALANCE WITH PORTUGAL, FRANCE AND MOROCCO

#We read the excels of the trade values obtained thanks to the Python function
francia=read_excel("C:/Users/carlo/Downloads/francia.xlsx")
portugal=read_excel("C:/Users/carlo/Downloads/portugal.xlsx")
marruecos=read_excel("C:/Users/carlo/Downloads/marruecos.xlsx")

#We stay with the values and the date
francia=francia[,c(1,2)]
portugal=portugal[,c(1,2)]
marruecos=marruecos[,c(1,2)]

#We modify the date format to have the same as the hourly dataset
fechas_converted <- with_tz(as.POSIXct(francia$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
francia$datetime=fechas_final

fechas_converted <- with_tz(as.POSIXct(portugal$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
portugal$datetime=fechas_final

fechas_converted <- with_tz(as.POSIXct(marruecos$datetime, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
marruecos$datetime=fechas_final

#We change the names of the variables
names(francia)[1]="Balanza_Francia"
names(portugal)[1]="Balanza_Portugal"
names(marruecos)[1]="Balanza_Marruecos"

#We include the three variables in the hourly dataset (here you can see the last incorporation that is Morocco)
prueba=merge(tfm_hour,marruecos,by="datetime", all = TRUE)

# Check if the "datetime" column of the "prueba" dataframe is repeated in more than one row
if (any(duplicated(prueba$datetime) | duplicated(prueba$datetime, fromLast = TRUE))) {
  print("La columna 'fecha' del dataframe 'df' está repetida en más de una fila.")
} else {
  print("La columna 'fecha' del dataframe 'df' no está repetida en más de una fila.")
}

# Find the rows that have duplicate dates in the "datetime" column of dataframe "prueba".
duplicated_rows <- which(duplicated(prueba$datetime) | duplicated(prueba$datetime, fromLast = TRUE))

# Obtain a list of duplicate dates
duplicated_dates <- unique(prueba$datetime[duplicated_rows])

# Print the list of duplicate dates
print(duplicated_dates)
duplicated_rows
#Eliminate the the duplicated dates
prueba=prueba[-c(7154,15890,24794,33530),]
tfm_hour=prueba

#As in the excel we had imports-exports, we multiply by -1 to have the trade (exports-imports)
tfm_hour[,15]=(-1)*tfm_hour[,15]
tfm_hour[,16]=(-1)*tfm_hour[,16]
tfm_hour[,17]=(-1)*tfm_hour[,17]


#### WE CHANGE THE NAMES OF THE VARIABLES
names(tfm_year)[8]="Wind"
names(tfm_hour)=c("Date", "Electricity", "Consumption", "Eolic", "Solar_Photovoltaic", "Solar_Thermal", "Other_Renewables", "Hydraulic",
                  "Nuclear", "Combined_cycle", "Coal", "Cogeneration", "Gas_price", "CO2_price", "Balance_France", "Balance_Portugal", "Balance_Morocco")
names(tfm_daily)
names(tfm_daily)=c("Date", "Hydraulic", "Eolic", "Solar_Photovoltaic", "Solar_Thermal", "Other_Renewables", "Renewable_Wastes", "Pumping_Turbine",
                   "Nuclear", "Combined_cycle", "Coal", "Diesel_Engine", "Gas_Turbine", "Steam_Turbine", "Cogeneration", "Non_Renewable_Wastes",
                   "Consumption", "Electricity", "Gas_Price", "CO2_Price", "Balance_France", "Balance_Portugal", "Balance_Morocco")
names(tfm_monthly)=c("Date", "Hydraulic", "Pumping_Turbine", "Nuclear", "Coal","Combined_cycle", "Eolic", "Solar_Photovoltaic", "Solar_Thermal", "Other_Renewables", "Cogeneration", "Non_Renewable_Wastes",
                     "Renewable_Wastes", "Electricity", "Consumption", "Gas_Price", "CO2_Price", "Balance_France", "Balance_Portugal", "Balance_Morocco")
names(tfm_year)
names(tfm_year)=c("Date", "Electricity", "Hydraulic", "Pumping_Turbine", "Nuclear", "Coal","Combined_cycle", "Eolic", "Solar_Photovoltaic", "Solar_Thermal", "Other_Renewables", "Cogeneration", "Non_Renewable_Wastes",
                     "Renewable_Wastes", "Consumption", "Gas_Price", "CO2_Price", "Installed_Power_Hydraulic","Installed_Power_Nuclear", "Installed_Power_Coal",
                  "Installed_Power_Combined_Cycle", "Installed_Power_Eolic", "Installed_Power_Solar_Photov", "Installed_Power_Solar_Thermal", "Installed_Power_Other_Renewables",
                  "Installed_Power_Cogeneration","Balance_France", "Balance_Portugal", "Balance_Morocco")


### ELIMINATE VARIABLES WE DO NOT FINF THEM USEFUL FOR THE STUDY
View(tfm_year)
tfm_year=tfm_year[,-c(13,14)]
View(tfm_monthly)
tfm_monthly=tfm_monthly[,-c(12,13)]
View(tfm_daily)
tfm_daily=tfm_daily[,-c(7,16)]
View(tfm_hour)


#### CHANGES IN SOLAR PHOTOVOLTAIC


library(readxl)
library(lubridate)
#We read the new hourly values of solar photovoltaic thanks to the Python function
fotov <- read_excel("C:/Users/carlo/Downloads/fotov (1).xlsx")
#We stay with the values and the date
fotov=fotov[,c(1,2)]
names(fotov)=c("Solar_Photovoltaic","Date")
#Change the date format to have the same in both datasets
fotov$Date <- gsub("\\.000", "", fotov$Date)
fechas_converted <- with_tz(as.POSIXct(fotov$Date, format = "%Y-%m-%dT%H:%M:%S"), "Europe/Paris")
fechas_final <- format(fechas_converted, tz = "Europe/Paris", usetz = TRUE, "%Y-%m-%dT%H:%M:%S+01:00")
fechas_final<- gsub(" CET", "", fechas_final)
fechas_final<- gsub(" CEST", "", fechas_final)
fotov$Date=fechas_final
#We include the variable into the hourly dataset
prueba=merge(tfm_hour,fotov, by = "Date", all = TRUE)


# Check if the "Date" column of the "prueba" dataframe is repeated in more than one row
if (any(duplicated(prueba$Date) | duplicated(prueba$Date, fromLast = TRUE))) {
  print("La columna 'fecha' del dataframe 'df' está repetida en más de una fila.")
} else {
  print("La columna 'fecha' del dataframe 'df' no está repetida en más de una fila.")
}

# Find the rows that have duplicate dates in the "Date" column of dataframe "prueba".
duplicated_rows <- which(duplicated(prueba$Date) | duplicated(prueba$Date, fromLast = TRUE))

# Obtain a list of duplicate dates
duplicated_dates <- unique(prueba$Date[duplicated_rows])

# Print the list of duplicate dates
print(duplicated_dates)
duplicated_rows

#Remove the duplicated dates
prueba=prueba[-c(7154,15890,24794,33530,42266,51002,59738,77377),]
sum(is.na(prueba$Solar_Photovoltaic.y))

#Replace the old Solar photovoltaic variable with the new one.
tfm_hour$Solar_Photovoltaic=prueba$Solar_Photovoltaic.y
