module Graph exposing
    ( Graph, Edge, empty, fromVerticesAndEdges
    , addVertex, removeVertex, updateVertex, addEdge, removeEdge, map, reverseEdges
    , isEmpty, hasVertex, hasEdge, areAdjacent
    , fold, size, vertices, edges, verticesAndEdges, outgoingEdges, edgeToComparable
    )

{-|


# Construction

@docs Graph, Edge, empty, fromVerticesAndEdges


# Modification

@docs addVertex, removeVertex, updateVertex, addEdge, removeEdge, map, reverseEdges


# Predicates

@docs isEmpty, hasVertex, hasEdge, areAdjacent


# Querying

@docs fold, size, vertices, edges, verticesAndEdges, outgoingEdges, edgeToComparable

-}

import AssocList as Dict exposing (Dict)
import AssocList.Extra as DictExtra
import Maybe.Extra
import Set exposing (Set)



{-
   - TODO symmetricClosure: Graph vertex -> UndirectedGraph vertex?
   - TODO check acyclic
   - TODO DFS, BFS
   - TODO topological sort
   - TODO strongly connected components
   - TODO export to DOT
   - TODO export to elm-community/graph?
   - TODO `{from:v,to:v}` vs `v v` in arguments... make consistent?
   - TODO maybe naming - member instead of hasVertex? what about hasEdge then?
   - TODO measure performance
   - TODO neighbours function (outgoing AND incoming edges)
   - TODO union, intersect, difference? maybe?
   - TODO some helper function for getting ID of vertex, and getting vertex of ID
   - TODO undirected variant, simpler API
   - TODO module documentation
   - TODO think about metadata for vertices. Is the type parameter enough? Is it
     OK that the user would have to add their own IDs or whatever?
   - TODO for now we don't have any metadata for edges, but think about adding that!
       - getEdgeValue : vertex -> vertex -> Graph vertex edge -> Maybe edge
       - setEdgeValue : vertex -> vertex -> edge -> Graph vertex edge -> Maybe edge
       - would it have to be mandatory? or optional and wrapped in Maybe?
         -- TODO fromList? That's the kind of API I don't like much about the elm-community lib...
   - TODO currently user can't have two vertices with same data... that's probably OK, right?
-}


{-| A directed graph data structure.

**The vertices** can hold any data, but you can't have two vertices holding the same data. (Think of it as of `Set`.)

**The edges** don't hold any data, they just connect two vertices. Again, they are **directed:** the order does matter.

-}
type Graph vertex
    = Graph
        { -- TODO maybe use elm-community/intdict too
          -- TODO reverse - incoming edges?
          edges : Dict Int (Set Int)
        , -- TODO can we do without the reverse index here?
          vertices : Dict vertex Int
        , verticesById : Dict Int vertex
        , unusedId : Int
        }


{-| A representation of an edge between two vertices.
-}
type alias Edge vertex =
    { from : vertex
    , to : vertex
    }


{-| An empty graph. No vertices, no edges.
-}
empty : Graph vertex
empty =
    Graph
        { edges = Dict.empty
        , vertices = Dict.empty
        , verticesById = Dict.empty
        , unusedId = 0
        }


{-| Construct a graph from a list of vertices and a list of edges.

Implicitly adds vertices that are present in the edges list, as if they were mentioned in the vertices list too.

    Graph.fromVerticesAndEdges
        [ "x", "y" ]
        [ { from = "x", to = "y" }
        , { from = "y", to = "z" }
        , { from = "z", to = "x" }
        ]
        |> Graph.vertices
        |> List.length
    --> 3

-}
fromVerticesAndEdges : List vertex -> List (Edge vertex) -> Graph vertex
fromVerticesAndEdges vertices_ edges_ =
    empty
        |> (\graph -> List.foldl (\{ from, to } -> addEdge from to) graph edges_)
        |> (\graph -> List.foldl addVertex graph vertices_)


{-| Is the graph empty?
-}
isEmpty : Graph vertex -> Bool
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
hasVertex : vertex -> Graph vertex -> Bool
hasVertex vertex (Graph g) =
    Dict.member vertex g.vertices


{-| Is this edge present in the graph?

    Graph.empty
        |> Graph.hasEdge {from = "x", to = "y"}
    --> False

    Graph.empty
        |> Graph.addEdge "x" "y"
        |> Graph.hasEdge {from = "x", to = "y"}
    --> True

    Graph.empty
        |> Graph.addEdge "y" "x"
        |> Graph.hasEdge {from = "x", to = "y"}
    --> False

-}
hasEdge : Edge vertex -> Graph vertex -> Bool
hasEdge { from, to } (Graph g) =
    let
        maybeEdgesFrom =
            Dict.get from g.vertices
                |> Maybe.andThen (\fromId -> Dict.get fromId g.edges)

        maybeToId =
            Dict.get to g.vertices
    in
    Maybe.map2 Set.member
        maybeToId
        maybeEdgesFrom
        |> Maybe.withDefault False


{-| Does the graph contain an edge between these two vertices (in any direction)?

    Graph.empty
        |> Graph.areAdjacent "x" "y"
    --> False

    Graph.empty
        |> Graph.addEdge "x" "y"
        |> Graph.areAdjacent "x" "y"
    --> True

    Graph.empty
        |> Graph.addEdge "y" "x"
        |> Graph.areAdjacent "x" "y"
    --> True

-}
areAdjacent : vertex -> vertex -> Graph vertex -> Bool
areAdjacent v1 v2 graph =
    hasEdge { from = v1, to = v2 } graph
        || hasEdge { from = v2, to = v1 } graph


{-| Lists all vertices this vertex has edges to.

Directed - only lists the edges that go _from_ the given vertex.

     Graph.empty
         |> Graph.outgoingEdges "foo"
     --> []

     Graph.empty
         |> Graph.addEdge "foo" "bar"
         |> Graph.addEdge "foo" "baz"
         |> Graph.addEdge "quux" "foo"
         |> Graph.outgoingEdges "foo"
     --> [ "bar", "baz" ]

-}
outgoingEdges : vertex -> Graph vertex -> List vertex
outgoingEdges vertex (Graph g) =
    Dict.get vertex g.vertices
        |> Maybe.andThen (\vertexId -> Dict.get vertexId g.edges)
        |> Maybe.map Set.toList
        |> Maybe.andThen
            (List.map (\toId -> Dict.get toId g.verticesById)
                >> Maybe.Extra.combine
            )
        |> Maybe.withDefault []


{-| Add the vertex to the graph. (Nothing happens if it's already present.)

By default it's not connected to any other vertex.

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.vertices
    --> [ "foo" ]

-}
addVertex : vertex -> Graph vertex -> Graph vertex
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
removeVertex : vertex -> Graph vertex -> Graph vertex
removeVertex vertex ((Graph g) as graph) =
    Dict.get vertex g.vertices
        |> Maybe.map
            (\id ->
                Graph
                    { g
                        | edges =
                            g.edges
                                -- outgoing
                                |> Dict.filter (\fromId _ -> fromId /= id)
                                -- incoming
                                |> Dict.map (\_ toIds -> Set.remove id toIds)
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
updateVertex : vertex -> (vertex -> vertex) -> Graph vertex -> Graph vertex
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


{-| Add an edge to the graph.

If any of the vertices aren't present yet, this function **will add them** before
adding the edge.

Directed: the order of the vertices does matter.

    Graph.empty
        |> Graph.addEdge "foo" "bar"
        |> Graph.vertices
    --> [ "foo", "bar" ]

    Graph.empty
        |> Graph.addEdge "foo" "bar"
        |> Graph.edges
    --> [ { from = "foo", to = "bar" } ]

-}
addEdge : vertex -> vertex -> Graph vertex -> Graph vertex
addEdge from to graph =
    graph
        |> addVertex from
        |> addVertex to
        |> addEdge_ from to


{-| A helper for `addEdge` that doesn't check if the vertices are present.
-}
addEdge_ : vertex -> vertex -> Graph vertex -> Graph vertex
addEdge_ from to ((Graph g) as graph) =
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
                    | edges =
                        Dict.update fromId
                            (\maybeEdgesFrom ->
                                case maybeEdgesFrom of
                                    Nothing ->
                                        Just (Set.singleton toId)

                                    Just edgesFrom ->
                                        Just (Set.insert toId edgesFrom)
                            )
                            g.edges
                }
        )
        maybeFromId
        maybeToId
        |> Maybe.withDefault graph


{-| Remove an edge from the first vertex to the second one. Does nothing if the
edge is not present.

Directed: the order of the vertices does matter.

    Graph.empty
        |> Graph.addEdge "foo" "bar"
        |> Graph.removeEdge "foo" "bar"
        |> Graph.edges
    --> []

-}
removeEdge : vertex -> vertex -> Graph vertex -> Graph vertex
removeEdge from to ((Graph g) as graph) =
    Maybe.map2
        (\fromId toId ->
            Graph
                { g
                    | edges =
                        Dict.update fromId
                            (\maybeTos ->
                                case maybeTos of
                                    Nothing ->
                                        Nothing

                                    Just tos ->
                                        Just (Set.remove toId tos)
                            )
                            g.edges
                }
        )
        (Dict.get from g.vertices)
        (Dict.get to g.vertices)
        |> Maybe.withDefault graph


{-| Applies a function to all the vertices.

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addVertex "bar"
        |> Graph.map String.toUpper
        |> Graph.vertices
    --> [ "FOO", "BAR" ]

-}
map : (vertex -> vertex2) -> Graph vertex -> Graph vertex2
map fn (Graph g) =
    Graph
        { edges = g.edges

        -- TODO better transition from vertices to verticesById or vice versa? (via some "swap" function?)
        , verticesById = Dict.map (always fn) g.verticesById
        , vertices = DictExtra.mapKeys fn g.vertices
        , unusedId = g.unusedId
        }


{-| Reverse the direction of all the edges in the graph.

    Graph.empty
        |> Graph.addEdge "foo" "bar"
        |> Graph.reverseEdges
        |> Graph.edges
    --> [ { from = "bar", to = "foo" } ]

-}
reverseEdges : Graph vertex -> Graph vertex
reverseEdges (Graph g) =
    Graph
        { g
            | edges =
                g.edges
                    |> Dict.toList
                    |> List.concatMap
                        (\( from, tos ) ->
                            tos
                                |> Set.toList
                                |> List.map (\to -> ( to, from ))
                        )
                    |> List.foldl
                        (\( from, to ) edges_ ->
                            Dict.update from
                                (\maybeEdgesFrom ->
                                    case maybeEdgesFrom of
                                        Nothing ->
                                            Just (Set.singleton to)

                                        Just edgesFrom ->
                                            Just (Set.insert to edgesFrom)
                                )
                                edges_
                        )
                        Dict.empty
        }


{-| Fold a function over all the vertices, starting with the "oldest" vertices.

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addVertex "bar"
        |> Graph.fold (\v acc -> acc ++ v) ""
    --> "foobar"

-}
fold : (vertex -> acc -> acc) -> acc -> Graph vertex -> acc
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
size : Graph vertex -> Int
size (Graph g) =
    Dict.size g.vertices


{-| Return a list of the vertices, starting with the "oldest".

    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addVertex "bar"
        |> Graph.vertices
    --> [ "foo", "bar" ]

-}
vertices : Graph vertex -> List vertex
vertices (Graph g) =
    Dict.keys g.vertices


{-| Return a list of the edges. Don't expect any sensible order - it's
implementation defined.

    Graph.empty
        |> Graph.addEdge "foo" "bar"
        |> Graph.addEdge "bar" "quux"
        |> Graph.edges
    --> [ { from = "bar", to = "quux" }
        , { from = "foo", to = "bar" }
        ]

-}
edges : Graph vertex -> List (Edge vertex)
edges (Graph g) =
    g.edges
        |> Dict.toList
        |> List.concatMap
            (\( from, tos ) ->
                tos
                    |> Set.toList
                    |> List.map (\to -> ( from, to ))
            )
        |> List.map
            (\( fromId, toId ) ->
                Maybe.map2
                    (\from to ->
                        { from = from
                        , to = to
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
        |> Graph.addEdge "foo" "bar"
        |> Graph.verticesAndEdges
    --> { vertices = [ "foo", "bar" ]
        , edges = [ { from = "foo", to = "bar" } ]
        }

-}
verticesAndEdges : Graph vertex -> { vertices : List vertex, edges : List (Edge vertex) }
verticesAndEdges graph =
    { vertices = vertices graph
    , edges = edges graph
    }


{-| Transform the record into a `(from, to)` tuple.

    Graph.edgeToComparable { from = "foo", to = "bar" }
    --> ("foo", "bar")

-}
edgeToComparable : Edge comparable -> ( comparable, comparable )
edgeToComparable { from, to } =
    ( from, to )
