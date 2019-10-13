# `Janiczek/elm-graph`

A graph data structure library trying for a nicer API (no node IDs exposed to the user).

If you'd have liked to use this package but some crucial functionality is missing, please tell me [in GitHub issues!](https://github.com/Janiczek/elm-graph/issues)

```elm
import Graph

foo =
    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addEdge "foo" "bar"
```
