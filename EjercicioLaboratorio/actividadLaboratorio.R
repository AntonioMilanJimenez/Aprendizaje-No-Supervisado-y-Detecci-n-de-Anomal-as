####################################################################3
####################### Ejercicio
###################################################################

library(arules)
library(mlbench)
data("Zoo")

Zoo$legs <- Zoo$legs>0
nombres <- colnames(Zoo)

Zoo[nombres] <- lapply(Zoo[nombres], factor)

Zoo <- as(Zoo,"transactions")
itemFrequencyPlot(Zoo, support = 0.1, cex.names=0.8)

#Con apriori extraemos los itemsets frecuentes

iZoo <- apriori(Zoo, parameter = list(support = 0.1, target="frequent"))
iZoo <- sort(iZoo, by="support") 
inspect(head(iZoo, n=10)) 

length(iZoo)
#Al tener 238445 itemsets, se hace necesario extraer los itemsets maximaes y cerrados

imaxZoo <- iZoo[is.maximal(iZoo)]
inspect(head(sort(imaxZoo, by="support")))

icloZoo <- iZoo[is.closed(iZoo)]
inspect(head(sort(icloZoo, by="support")))


#Ahora si extraemos las reglas.
rules <- apriori(Zoo, parameter = list(support = 0.1, confidence = 0.8, minlen = 2))
summary(rules)
length(rules)

#Tenemos más de un millon y medio de reglas por lo que es necesario aumentar el umbral del soporte y de la confianza
rules <- apriori(Zoo, parameter = list(support = 0.37, confidence = 0.9, minlen = 2))
summary(rules)
length(rules)
#Ya se tienen 5689 reglas

#Ahora ordenamos las reglas por la confianza
rulesSorted = sort(rules, by = "confidence")

#Y eliminamos las reglas redundantes:

subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <- FALSE
redundant <- colSums(subsetMatrix, na.rm=TRUE) >= 1
rulesPruned <- rulesSorted[!redundant]

length(rulesPruned)
#Una vez eliminadas las reglas redundantes se tienen 169 reglas.
inspect(rulesPruned)

#Dado que en este problema buscamos algún caso especial que nos ayude a determinar la región de este dataset.
#Para encontrar esos casos especiales buscamos aquellas reglas que tengan una confianza muy cercana a 1 sin ser 1, de forma que
#exista alguna excepción que no haya cumplido la regla.

casosEspeciales <- subset(rulesPruned,confidence<1)
inspect(casosEspeciales)

#OBservando ya las primeras reglas descubrimos casos especiales, concretamente las reglas 1 y 8 son muy interesantes:

inspect(casosEspeciales[c(1,8)])

#Al no tener una confianza de 1 podemos saber que hay algun animal en los datos que tenga patas y aletas, y algun otro animal
#que tenga patas, cola y sea venenoso. Buscando ya en Internet sobre estos casos descubrimos que el pinguino tiene patas
# y aletas, y el ornitorrinco cumple que tenga patas, cola y sea venenoso.

#Si nos informamos sobre las regiones en las que viven estos dos animales encontramos una región común, el sur de Australia, 
#por lo que esta será posiblemente la región que se ha estudiado para realizar este dataset.
