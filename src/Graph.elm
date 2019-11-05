module Graph exposing
    ( Graph, Edge, empty, fromVerticesAndEdges
    , addVertex, removeVertex, updateVertex, addEdge, removeEdge, updateEdge, mapVertices, mapEdges, reverseEdges
    , isEmpty, hasVertex, hasEdge, areAdjacent
    , fold, size, vertices, edges, verticesAndEdges, getEdge
    , outdegree, indegree, outgoingEdges, outgoingEdgesWithData, incomingEdges, incomingEdgesWithData, edgeToComparable
    )

{-|


# Construction

@docs Graph, Edge, empty, fromVerticesAndEdges


# Modification

@docs addVertex, removeVertex, updateVertex, addEdge, removeEdge, updateEdge, mapVertices, mapEdges, reverseEdges


# Predicates

@docs isEmpty, hasVertex, hasEdge, areAdjacent


# Querying

@docs fold, size, vertices, edges, verticesAndEdges, getEdge

@docs outdegree, indegree, outgoingEdges, outgoingEdgesWithData, incomingEdges, incomingEdgesWithData, edgeToComparable

-}

import AssocList as Dict exposing (Dict)
import AssocList.Extra as DictExtra
import AssocSet as Set exposing (Set)
import Maybe.Extra
import MultiBiDict.Assoc as MultiBiDict exposing (MultiBiDict)



{-
   - TODO symmetricClosure: Graph vertex edge -> UndirectedGraph vertex edge?
   - TODO check acyclic
   - TODO DFS, BFS
   - TODO topological sort
   - TODO strongly connected components
   - TODO export to DOT
   - TODO export to elm-community/graph?
   - TODO measure performance
   - TODO neighbours function (outgoing AND incoming edges)
   - TODO union, intersect, difference? maybe?
   - TODO some helper function for getting ID of vertex, and getting vertex of ID
   - TODO undirected variant, simpler API
   - TODO currently user can't have two vertices with same data... that's probably OK, right?
   - TODO currently user can't have two same edges with different data... probably *not* OK?
-}


{-| A directed graph data structure.

**The vertices** can hold any data, but you can't have two vertices holding the same data. (Think of it as of `Set`.)

**The edges** can also hold any data, but you can't have two edges going from/to the same place.

-}
type Graph vertex edge
    = Graph
        -- TODO maybe use elm-community/intdict too
        { edges : MultiBiDict Int Int
        , edgeData : Dict ( Int, Int ) edge
        , vertices : Dict vertex Int
        , verticesById : Dict Int vertex
        , unusedId : Int
        }


{-| A representation of an edge between two vertices.
-}
type alias Edge vertex edge =
    { from : vertex
    , to : vertex
    , data : edge
    }


{-| An empty graph. No vertices, no edges.
-}
empty : Graph vertex edge
empty =
    Graph
        { edges = MultiBiDict.empty
        , edgeData = Dict.empty
        , vertices = Dict.empty
        , verticesById = Dict.empty
        , unusedId = 0
        }


{-| Construct a graph from a list of vertices and a list of edges.

Implicitly adds vertices that are present in the edges list, as if they were mentioned in the vertices list too.

    Graph.fromVerticesAndEdges
        [ "x", "y" ]
        [ { from = "x", to = "y", data = 100 }
        , { from = "y", to = "z", data = 200 }
        , { from = "z", to = "x", data = 300 }
        ]
        |> Graph.vertices
        |> List.length
    --> 3

-}
fromVerticesAndEdges : List vertex -> List (Edge vertex edge) -> Graph vertex edge
fromVerticesAndEdges vertices_ edges_ =
    empty
        |> (\graph -> List.foldl (\{ from, to, data } -> addEdge from to data) graph edges_)
        |> (\graph -> List.foldl addVertex graph vertices_)


{-| Is the graph empty?
-}
isEmpty : Graph vertex edge -> Bool
isEmpty (Graph g) =
    Dict.isEmpty g.vertices


{-| Is this vertex present in the graph?

    Graph.empty
        |> Graph.hasVertex "x"
    --> False

    Graph.empty
        |> Graph.addVertex "x"
        |> Graph.hasVertex "x"
    --> True

-}
hasVertex : vertex -> Graph vertex edge -> Bool
hasVertex vertex (Graph g) =
    Dict.member vertex g.vertices


{-| Is this edge present in the graph?

    Graph.empty
        |> Graph.hasEdge "x" "y"
    --> False

    Graph.empty
        |> Graph.addEdge "x" "y" ()
        |> Graph.hasEdge "x" "y"
    --> True

    Graph.empty
        |> Graph.addEdge "y" "x" ()
        |> Graph.hasEdge "x" "y"
    --> False

-}
hasEdge : vertex -> vertex -> Graph vertex edge -> Bool
hasEdge from to (Graph g) =
    let
        edgesFrom =
            Dict.get from g.vertices
                |> Maybe.map (\fromId -> MultiBiDict.get fromId g.edges)
                |> Maybe.withDefault Set.empty
    in
    Dict.get to g.vertices
        |> Maybe.map (\toId -> Set.member toId edgesFrom)
        |> Maybe.withDefault False


{-| Does the graph contain an edge between these two vertices (in any direction)?

    Graph.empty
        |> Graph.areAdjacent "x" "y"
    --> False

    Graph.empty
        |> Graph.addEdge "x" "y" ()
        |> Graph.areAdjacent "x" "y"
    --> True

    Graph.empty
        |> Graph.addEdge "y" "x" ()
        |> Graph.areAdjacent "x" "y"
    --> True

-}
areAdjacent : vertex -> vertex -> Graph vertex edge -> Bool
areAdjacent v1 v2 graph =
    hasEdge v1 v2 graph
        || hasEdge v2 v1 graph


{-| Get the value associated with the edge.

    Graph.empty
        |> Graph.addEdge "foo" "bar" 100
        |> Graph.getEdge "foo" "bar"
    --> Just 100

    Graph.empty
        |> Graph.addEdge "foo" "bar" 100
        |> Graph.getEdge "bar" "quux"
    --> Nothing

-}
getEdge : vertex -> vertex -> Graph vertex edge -> Maybe edge
getEdge from to (Graph g) =
    Maybe.map2 Tuple.pair
        (Dict.get from g.vertices)
        (Dict.get to g.vertices)
        |> Maybe.andThen (\ids -> Dict.get ids g.edgeData)


{-| Determine the number of outgoing edges of the vertex.

    Graph.empty
        |> Graph.outdegree "foo"
    --> 0

    Graph.empty
        |> Graph.addEdge "foo" "bar" ()
        |> Graph.addEdge "foo" "baz" ()
        |> Graph.addEdge "quux" "foo" ()
        |> Graph.outdegree "foo"
    --> 2

-}
outdegree : vertex -> Graph vertex edge -> Int
outdegree vertex (Graph g) =
    Dict.get vertex g.vertices
        |> Maybe.map (\vertexId -> MultiBiDict.get vertexId g.edges |> Set.size)
        |> Maybe.withDefault 0


{-| Determine the number of incoming edges of the vertex.

    Graph.empty
        |> Graph.indegree "foo"
    --> 0

    Graph.empty
        |> Graph.addEdge "foo" "bar" ()
        |> Graph.addEdge "foo" "baz" ()
        |> Graph.addEdge "quux" "foo" ()
        |> Graph.indegree "foo"
    --> 1

-}
indegree : vertex -> Graph vertex edge -> Int
indegree vertex (Graph g) =
    Dict.get vertex g.vertices
        |> Maybe.map (\vertexId -> MultiBiDict.getReverse vertexId g.edges |> Set.size)
        |> Maybe.withDefault 0


{-| Lists all vertices this vertex has edges to.

Directed - only lists the edges that go _from_ the given vertex.

     Graph.empty
         |> Graph.outgoingEdges "foo"
     --> []

     Graph.empty
         |> Graph.addEdge "foo" "bar" ()
         |> Graph.addEdge "foo" "baz" ()
         |> Graph.addEdge "quux" "foo" ()
         |> Graph.outgoingEdges "foo"
     --> [ "bar", "baz" ]

-}
outgoingEdges : vertex -> Graph vertex edge -> List vertex
outgoingEdges vertex (Graph g) =
    Dict.get vertex g.vertices
        |> Maybe.andThen
            (\fromId ->
                MultiBiDict.get fromId g.edges
                    |> Set.toList
                    |> List.map (\toId -> Dict.get toId g.verticesById)
                    |> Maybe.Extra.combine
            )
        |> Maybe.withDefault []


{-| Lists all vertices this vertex has edges to, along with the edge data.

Directed - only lists the edges that go _from_ the given vertex.

     Graph.empty
         |> Graph.outgoingEdgesWithData "foo"
     --> []

     Graph.empty
         |> Graph.addEdge "foo" "bar" 100
         |> Graph.addEdge "foo" "baz" 200
         |> Graph.addEdge "quux" "foo" 300
         |> Graph.outgoingEdgesWithData "foo"
     --> [ ("bar", 100)
     --  , ("baz", 200)
     --  ]

-}
outgoingEdgesWithData : vertex -> Graph vertex edge -> List ( vertex, edge )
outgoingEdgesWithData vertex (Graph g) =
    Dict.get vertex g.vertices
        |> Maybe.andThen
            (\fromId ->
                MultiBiDict.get fromId g.edges
                    |> Set.toList
                    |> List.map
                        (\toId ->
                            Maybe.map2 Tuple.pair
                                (Dict.get toId g.verticesById)
                                (Dict.get ( fromId, toId ) g.edgeData)
                        )
                    |> Maybe.Extra.combine
            )
        |> Maybe.withDefault []


{-| Lists all vertices that have an edge to this vertex.

Directed - only lists the edges that go _into_ the given vertex.

     Graph.empty
         |> Graph.incomingEdges "foo"
     --> []

     Graph.empty
         |> Graph.addEdge "foo" "bar" ()
         |> Graph.addEdge "foo" "baz" ()
         |> Graph.addEdge "quux" "foo" ()
         |> Graph.incomingEdges "foo"
     --> [ "quux" ]

-}
incomingEdges : vertex -> Graph vertex edge -> List vertex
incomingEdges vertex (Graph g) =
    Dict.get vertex g.vertices
        |> Maybe.andThen
            (\fromId ->
                MultiBiDict.get fromId g.edges
                    |> Set.toList
                    |> List.map (\toId -> Dict.get toId g.verticesById)
                    |> Maybe.Extra.combine
            )
        |> Maybe.withDefault []


{-| Lists all vertices that have an edge to this vertex, along with the edge data.

Directed - only lists the edges that go _into_ the given vertex.

     Graph.empty
         |> Graph.incomingEdges "foo"
     --> []

     Graph.empty
         |> Graph.addEdge "foo" "bar" 100
         |> Graph.addEdge "foo" "baz" 200
         |> Graph.addEdge "quux" "foo" 300
         |> Graph.incomingEdges "foo"
     --> [ ("quux", 300) ]

-}
incomingEdgesWithData : vertex -> Graph vertex edge -> List ( vertex, edge )
incomingEdgesWithData vertex (Graph g) =
    Dict.get vertex g.vertices
        |> Maybe.andThen
            (\fromId ->
                MultiBiDict.get fromId g.edges
                    |> Set.toList
                    |> List.map
                        (\toId ->
                            Maybe.map2 Tuple.pair
                                (Dict.get toId g.verticesById)
                                (Dict.get ( fromId, toId ) g.edgeData)
                        )
                    |> Maybe.Extra.combine
            )
        |> Maybe.withDefault []


{-| Add the vertex to the graph. (Nothing happens if it's already present.)

By default it's not connected to any other vertex.

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.vertices
    --> [ "foo" ]

-}
addVertex : vertex -> Graph vertex edge -> Graph vertex edge
addVertex vertex ((Graph g) as graph) =
    if hasVertex vertex graph then
        graph

    else
        Graph
            { g
                | vertices = Dict.insert vertex g.unusedId g.vertices
                , verticesById = Dict.insert g.unusedId vertex g.verticesById
                , unusedId = g.unusedId + 1
            }


{-| Remove the vertex from the graph. (Nothing happens if it's not present.)

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.removeVertex "foo"
        |> Graph.isEmpty
    --> True

-}
removeVertex : vertex -> Graph vertex edge -> Graph vertex edge
removeVertex vertex ((Graph g) as graph) =
    Dict.get vertex g.vertices
        |> Maybe.map
            (\id ->
                Graph
                    { g
                        | edges = MultiBiDict.filter (\fromId _ -> fromId /= id) g.edges
                        , vertices = Dict.remove vertex g.vertices
                        , verticesById = Dict.remove id g.verticesById
                    }
            )
        |> Maybe.withDefault graph


{-| Updates the vertex data. Does nothing if the vertex is not present.

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.updateVertex "foo" String.toUpper
        |> Graph.vertices
    --> [ "FOO" ]

-}
updateVertex : vertex -> (vertex -> vertex) -> Graph vertex edge -> Graph vertex edge
updateVertex vertex fn ((Graph g) as graph) =
    Dict.get vertex g.vertices
        |> Maybe.map
            (\id ->
                let
                    new =
                        fn vertex
                in
                graph
                    |> removeVertex new
                    |> (\(Graph g_) ->
                            Graph
                                { g_
                                    | vertices =
                                        g_.vertices
                                            |> Dict.remove vertex
                                            |> Dict.insert new id
                                    , verticesById = Dict.update id (Maybe.map (always new)) g_.verticesById
                                }
                       )
            )
        |> Maybe.withDefault graph


{-| Add an edge to the graph, along with the associated data

If any of the vertices aren't present yet, this function **will add them** before
adding the edge.

Directed: the order of the vertices does matter.

    Graph.empty
        |> Graph.addEdge "foo" "bar" ()
        |> Graph.vertices
    --> [ "foo", "bar" ]

    Graph.empty
        |> Graph.addEdge "foo" "bar" 100
        |> Graph.edges
    --> [ { from = "foo", to = "bar", data = 100 } ]

-}
addEdge : vertex -> vertex -> edge -> Graph vertex edge -> Graph vertex edge
addEdge from to data graph =
    graph
        |> addVertex from
        |> addVertex to
        |> addEdge_ from to data


{-| A helper for `addEdge` that doesn't check if the vertices are present.
-}
addEdge_ : vertex -> vertex -> edge -> Graph vertex edge -> Graph vertex edge
addEdge_ from to data ((Graph g) as graph) =
    let
        maybeFromId =
            Dict.get from g.vertices

        maybeToId =
            Dict.get to g.vertices
    in
    Maybe.map2
        (\fromId toId ->
            Graph
                { g
                    | edges = MultiBiDict.insert fromId toId g.edges
                    , edgeData = Dict.insert ( fromId, toId ) data g.edgeData
                }
        )
        maybeFromId
        maybeToId
        |> Maybe.withDefault graph


{-| Remove an edge from the first vertex to the second one. Does nothing if the
edge is not present.

Directed: the order of the vertices does matter.

    Graph.empty
        |> Graph.addEdge "foo" "bar" 100
        |> Graph.removeEdge "foo" "bar"
        |> Graph.edges
    --> []

-}
removeEdge : vertex -> vertex -> Graph vertex edge -> Graph vertex edge
removeEdge from to ((Graph g) as graph) =
    Maybe.map2
        (\fromId toId ->
            Graph { g | edges = MultiBiDict.update fromId (Set.remove toId) g.edges }
        )
        (Dict.get from g.vertices)
        (Dict.get to g.vertices)
        |> Maybe.withDefault graph


{-| Updates the edge data. Does nothing if the edge is not present.

    Graph.empty
        |> Graph.addEdge "foo" "bar" 100
        |> Graph.updateEdge "foo" "bar" (\v -> v + 1000)
        |> Graph.edges
    --> [ { from = "foo", bar = "bar", data = 1100 } ]

-}
updateEdge : vertex -> vertex -> (edge -> edge) -> Graph vertex edge -> Graph vertex edge
updateEdge from to fn ((Graph g) as graph) =
    Maybe.map2
        (\fromId toId ->
            Graph
                { g | edgeData = Dict.update ( fromId, toId ) (Maybe.map fn) g.edgeData }
        )
        (Dict.get from g.vertices)
        (Dict.get to g.vertices)
        |> Maybe.withDefault graph


{-| Applies a function to all the vertices.

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addVertex "bar"
        |> Graph.mapVertices String.toUpper
        |> Graph.vertices
    --> [ "FOO", "BAR" ]

-}
mapVertices : (vertex1 -> vertex2) -> Graph vertex1 edge -> Graph vertex2 edge
mapVertices fn (Graph g) =
    Graph
        { edges = g.edges
        , edgeData = g.edgeData

        -- TODO better transition from vertices to verticesById or vice versa? (via some "swap" function?)
        , verticesById = Dict.map (always fn) g.verticesById
        , vertices = DictExtra.mapKeys fn g.vertices
        , unusedId = g.unusedId
        }


{-| Applies a function to all the edges.

    Graph.empty
        |> Graph.addEdge "foo" "bar" 100
        |> Graph.addEdge "bar" "baz" 200
        |> Graph.mapEdges (\x -> x + 1000)
        |> Graph.edges
    --> [ { from = "foo", to = "bar", data = 1100 }
    --  , { from = "bar", to = "baz", data = 1200 }
    --  ]
    --
    --  (sans the ordering on the edges list, see the note in `edges`)

-}
mapEdges : (edge1 -> edge2) -> Graph vertex edge1 -> Graph vertex edge2
mapEdges fn (Graph g) =
    Graph
        { edges = g.edges
        , edgeData = Dict.map (always fn) g.edgeData
        , verticesById = g.verticesById
        , vertices = g.vertices
        , unusedId = g.unusedId
        }


{-| Reverse the direction of all the edges in the graph.

    Graph.empty
        |> Graph.addEdge "foo" "bar" 100
        |> Graph.reverseEdges
        |> Graph.edges
    --> [ { from = "bar", to = "foo", data = 100 } ]

-}
reverseEdges : Graph vertex edge -> Graph vertex edge
reverseEdges (Graph g) =
    Graph
        { g
            | edges =
                g.edges
                    |> MultiBiDict.toReverseList
                    |> MultiBiDict.fromList
        }


{-| Fold a function over all the vertices, starting with the "oldest" vertices.

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addVertex "bar"
        |> Graph.fold (\v acc -> acc ++ v) ""
    --> "foobar"

-}
fold : (vertex -> acc -> acc) -> acc -> Graph vertex edge -> acc
fold fn acc (Graph g) =
    Dict.foldr (always fn) acc g.verticesById


{-| Determine the number of vertices in the graph.

    Graph.size Graph.empty
    --> 0

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addVertex "bar"
        |> Graph.size
    --> 2

-}
size : Graph vertex edge -> Int
size (Graph g) =
    Dict.size g.vertices


{-| Return a list of the vertices, starting with the "oldest".

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addVertex "bar"
        |> Graph.vertices
    --> [ "foo", "bar" ]

-}
vertices : Graph vertex edge -> List vertex
vertices (Graph g) =
    Dict.keys g.vertices


{-| Return a list of the edges.

_Don't expect any sensible order - it's implementation defined._

    Graph.empty
        |> Graph.addEdge "foo" "bar" 100
        |> Graph.addEdge "bar" "quux" 200
        |> Graph.edges
    --> [ { from = "bar", to = "quux", data = 200 }
    --  , { from = "foo", to = "bar", data = 100 }
    --  ]

-}
edges : Graph vertex edge -> List (Edge vertex edge)
edges (Graph g) =
    g.edges
        |> MultiBiDict.toList
        |> List.filterMap
            (\( fromId, tos ) ->
                tos
                    |> Set.toList
                    |> List.map
                        (\toId ->
                            Dict.get ( fromId, toId ) g.edgeData
                                |> Maybe.map (\data -> ( fromId, toId, data ))
                        )
                    |> Maybe.Extra.combine
            )
        |> List.concat
        |> List.map
            (\( fromId, toId, data ) ->
                Maybe.map2
                    (\from to ->
                        { from = from
                        , to = to
                        , data = data
                        }
                    )
                    (Dict.get fromId g.verticesById)
                    (Dict.get toId g.verticesById)
            )
        |> Maybe.Extra.combine
        |> Maybe.withDefault []


{-| Return a record with a list of vertices and list of edges in the graph.

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addEdge "foo" "bar" 100
        |> Graph.verticesAndEdges
    --> { vertices = [ "foo", "bar" ]
    --  , edges = [ { from = "foo", to = "bar", data = 100 } ]
    --  }

-}
verticesAndEdges : Graph vertex edge -> { vertices : List vertex, edges : List (Edge vertex edge) }
verticesAndEdges graph =
    { vertices = vertices graph
    , edges = edges graph
    }


{-| Transform the record into a `(from, to, data)` tuple.

    Graph.edgeToComparable { from = "foo", to = "bar", data = 123 }
    --> ("foo", "bar", 123)

-}
edgeToComparable : Edge comparableVertex comparableEdge -> ( comparableVertex, comparableVertex, comparableEdge )
edgeToComparable { from, to, data } =
    ( from, to, data )
