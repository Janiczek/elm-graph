# `Janiczek/elm-graph`

A graph data structure library trying for a nicer API (no node IDs exposed to the user).

I haven't measured performance but it will most likely be **slower than `elm-community/graph`** as we don't use the optimized `IntDict`, and use `AssocList` instead of the normal `Dict` to get a nicer API.

If you'd have liked to use this package but some crucial functionality is missing, please tell me [in GitHub issues!](https://github.com/Janiczek/elm-graph/issues)

```elm
import Graph exposing (Graph)

myGraph : Graph
myGraph =
    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addEdge "foo" "bar"
```
