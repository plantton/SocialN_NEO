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
query1 = "MATCH (n1:Product) WHERE n1.NodeId='0' RETURN n1"
# Return all co-purchased nodes of "0" - From "0" to "n2"
query2 = "MATCH (n1:Product)-[r:CO_PURCHASED]->(n2:Product) WHERE n1.NodeId='3' RETURN n1.NodeId AS source, n2.NodeId AS target"

## Collaborative Filtering

query3 = "MATCH (n1:Product)-[:CO_PURCHASED]->(n2:Product), (n2)-[:CO_PURCHASED]->(n3:Product) WHERE n1.NodeId = "0" AND NOT (n1)-[:CO_PURCHASED]->(n3) AND NOT n3.NodeId = n1.NodeId RETURN n3.NodeId, count(distinct n3) as frequency ORDER BY frequency DESC LIMIT 5"

nodes_query = "MATCH (n:Product) RETURN DISTINCT n.NodeId AS id"

edges_query = "MATCH (p1:Product)-[r:CO_PURCHASED]->(p2:Product) RETURN p1.NodeId AS source, p2.NodeId AS target"

nodes = cypher(graph, nodes_query)
edges = cypher(graph, edges_query)
head(nodes)
head(edges)

# Experiments
library(dplyr)
nodes_tbl=copy_to(sc, nodes)
edges_tbl=copy_to(sc, edges)
src_tbls(sc)


getIndex(graph, "Product")

# Create igraph graph object.
# ig = graph.data.frame(edges, directed = T, nodes)
# Too slow

# Alternative test
# https://www.r-bloggers.com/loading-huge-graphs-with-igraph-and-r/
df.edges=data.frame(edges)
cat("— Creating graph… ")

   start <- proc.time()

   vertex.attrs <- list(name = unique(c(df.edges$source, df.edges$target)))
   edges.new <- rbind(match(df.edges$source, vertex.attrs$name),
                  match(df.edges$target, vertex.attrs$name))
   
   G <- graph.empty(n = 0, directed = T)
   G <- add.vertices(G, length(vertex.attrs$name), attr = vertex.attrs)
   G <- add.edges(G, edges.new)

   remove(edges.new)
   remove(vertex.attrs)

   cat(sprintf("— elapsed user-time: %fs", 
(proc.time() - start)[1]))

# Run Girvan-Newman clustering algorithm.
# Abandoned - not efficient.
# communities = edge.betweenness.community(ig)

# Community Detection
# I didn't use it since this method 
# can be extrmely hard to converge on
# directed networks
LargeScaleCommunityFast <- function(g, mode="all") {
    cat("Assigning initial communities…\n")
    vgroups <- V(g)$name
    # random order in which vertices will be processed
    cat("Generating random order…\n")
    order <- sample(vcount(g), vcount(g))
    t <- 0
    done <- FALSE

    while (!done) {
      t <- t + 1
      cat("round: ", t, "\n")

      ## change to FALSE whenever a node changes groups
      done <- TRUE

      for(i in order) {
        ## get the neighbor group frequencies:
        group.freq <- table(vgroups[neighbors(g, i, mode = mode)])
        ## pick one of the most frequent:
        new.group <- sample(names(group.freq)[group.freq == max(group.freq)], 1)
        if (done) {
          ## we are only done if new group is the same as old group
          done <- (new.group == vgroups[i])
        }
        vgroups[i] <- new.group
      }
    }

    cat("Creating community-object…\n")
    comms <- list(membership = as.numeric(vgroups),
                  vcount = vcount(g),
                  algorithm = "LPA",
                  names = V(g)$name)
    class(comms) <- "communities"
    return(comms)
}

# Test: Community Detection
cat("— Creating graph…\n")
start <- proc.time()

cat("— Detecting communities…\n")
start <- proc.time()
C <- LargeScaleCommunityFast(G)
cat(sprintf("— elapsed time: %f\n\n", (proc.time() - start)[1]))

# Export Pajek file
write.graph(graph = G, file = "/media/plantton/Windows/Users/tangc/Documents/Analysis_of_Large_Scale_Social_Networks/project/codes/campnet.paj", format = "pajek")


# Breadth-First Search
# set 'rank' as the number of bfs returned results
# set root vertex as the start point (purchased)

# test
f <- function(graph, data, extra) {
 data['rank'] == 30
}
G.bfs=bfs(G, root=3, "out",  callback=f)
G.bfs$order[2:11]

#
f <- function(graph, data, extra) {
 data['dist'] == 3
}
G.bfs=bfs(G, root=3, "out", restricted=as.numeric(nodes[-4,1]),, dist=T,callback=f)
length(G.bfs$order)

## Breadth-first on Neo4j: Cypher
# Final version of BFS
bfs.query = '
MATCH (p0:Product)
WHERE p0.NodeId="3"
CALL apoc.path.expandConfig(p0,{relationshipFilter:"CO_PURCHASED>",maxLevel:3,uniqueness:"NODE_GLOBAL"}) YIELD path
WITH p0, RELATIONSHIPS(path) as r, LAST(NODES(path)) as es
WHERE es:Product
RETURN p0.NodeId AS source,es.NodeId AS destination, LENGTH(r) AS length
'

#### Case 1: Single node as input:
## Insert variable, 'x' as root NodeId.
x=3

bfs.query=paste('
MATCH (p0:Product)
WHERE p0.NodeId=','\"',x,'\"','
CALL apoc.path.expandConfig(p0,{relationshipFilter:"CO_PURCHASED>",maxLevel:3,uniqueness:"NODE_GLOBAL"}) YIELD path
WITH p0, RELATIONSHIPS(path) as r, LAST(NODES(path)) as es
WHERE es:Product
RETURN p0.NodeId AS source,es.NodeId AS destination, LENGTH(r) AS length
',sep='')

bfs.one = cypher(graph, bfs.query)

# Return first 10 nearest nodes as recommendations.
recommendations=bfs.one[2:11,2]
recommendations


#### Case 2: Two nodes as input:
## Algorithm described in the report
x=67
y=241

bfs.query1=paste('
MATCH (p0:Product)
WHERE p0.NodeId=','\"',x,'\"','
CALL apoc.path.expandConfig(p0,{relationshipFilter:"CO_PURCHASED>",maxLevel:3,uniqueness:"NODE_GLOBAL"}) YIELD path
WITH p0, RELATIONSHIPS(path) as r, LAST(NODES(path)) as es
WHERE es:Product
RETURN p0.NodeId AS source,es.NodeId AS destination, LENGTH(r) AS length
',sep='')

bfs.query2=paste('
MATCH (p0:Product)
WHERE p0.NodeId=','\"',y,'\"','
CALL apoc.path.expandConfig(p0,{relationshipFilter:"CO_PURCHASED>",maxLevel:3,uniqueness:"NODE_GLOBAL"}) YIELD path
WITH p0, RELATIONSHIPS(path) as r, LAST(NODES(path)) as es
WHERE es:Product
RETURN p0.NodeId AS source,es.NodeId AS destination, LENGTH(r) AS length
',sep='')

bfs.1st = cypher(graph, bfs.query1)
bfs.2nd = cypher(graph, bfs.query2)
bfs.1st=bfs.1st[-1,]
bfs.2nd=bfs.2nd[-1,]

# intersect(bfs.1st[,2],bfs.2nd[,2])

merged.bfs=merge(bfs.1st, bfs.2nd, by="destination")
merged.bfs=merged.bfs[ ,c(1,3,5)]
merged.bfs$sum=merged.bfs$length.x + merged.bfs$length.y
merged.bfs=merged.bfs[order(merged.bfs$sum),]
# result: Adjusting the length
merged.bfs[,1]


##### Test of Spark
./bin/spark-shell \
--conf spark.neo4j.bolt.password=neo4j,\
 spark.neo4j.bolt.user=neo4j\
--jars /media/plantton/Windows/Users/tangc/Documents/Softwares/neo4j-spark-connector-master/target/neo4j-spark-connector-full-2.0.0-M2.jar\
--packages neo4j-contrib:neo4j-spark-connector:2.0.0-M2,graphframes:graphframes:0.2.0-spark2.0-s_2.11



