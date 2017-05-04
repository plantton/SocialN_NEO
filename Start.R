##
setwd('/media/plantton/Windows/Users/tangc/Documents/Analysis_of_Large_Scale_Social_Networks/project/codes')
copurchased = read.table('/media/plantton/Windows/Users/tangc/Documents/Analysis_of_Large_Scale_Social_Networks/project/Amazon0505.txt')

colnames(copurchased) = c(':START_ID', ':END_ID')
#
write.csv(unique(c(unique(copurchased[,1]),unique(copurchased[,2]))), row.names=F,file = "Nodes.csv")
write.csv(copurchased, row.names=F, file= 'CoPurchased.csv')


# path: /var/lib/neo4j/data/databases/graph.db

# neo4j-import --into /var/lib/neo4j/data/databases/graph.db --nodes:Product Nodes.csv --relationships:CO_PURCHASED CoPurchased.csv --skip-duplicate-nodes


##
library(igraph)
library(RNeo4j)
library(visNetwork)
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="neo4j")
# clear(graph) # clear all database

# Return Node with property:'0'
query1 = "MATCH (n1:Product {NodeId:"0"}) RETURN n1"
# Return all co-purchased nodes of "0" - From "0" to "n2"
query2 = "MATCH (n1:Product {NodeId:"0"})-[:CO_PURCHASED]->(n2:Product)
RETURN n1, n2"

## Collaborative Filtering

query3 = "MATCH (n1:Product)-[:CO_PURCHASED]->(n2:Product), (n2)-[:CO_PURCHASED]->(n3:Product) WHERE n1.NodeId = "0" AND NOT (n1)-[:CO_PURCHASED]->(n3) AND NOT n3.NodeId = n1.NodeId RETURN n3.NodeId, count(distinct n3) as frequency ORDER BY frequency DESC LIMIT 5"



edges = cypher(graph, query)
head(edges)



