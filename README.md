# bioyicgmlst
cgMLST analysis of bacteria

## Analysis
### Draw a minimum spanning tree graph
```
Rscript scripts/mst_igraph.R example/distances.tab cgMLST 
Rscript scripts/mst_igraph.R example/distances.tab cgMLST example/group.list
```
distances.tab: Distance matr; cgMLST:Output file prefix; group.list:Group table by distance 15.
<p align="center">
<img src="https://github.com/zxgsy520/bioyicgmlst/blob/main/example/cgMLST.mst.png" width=400px"> <br>
</p>
