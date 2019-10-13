module Graph exposing
    ( Graph, Edge, empty, fromVerticesAndEdges
    , addVertex, removeVertex, updateVertex, addEdge, removeEdge, map, reverseEdges
    , isEmpty, hasVertex, hasEdge, areAdjacent
    , fold, size, vertices, edges, outgoingEdges, edgeToComparable
    )

{-|


# Construction

@docs Graph, Edge, empty, fromVerticesAndEdges


# Modification

@docs addVertex, removeVertex, updateVertex, addEdge, removeEdge, map, reverseEdges


# Predicates

@docs isEmpty, hasVertex, hasEdge, areAdjacent


# Querying

@docs fold, size, vertices, edges, outgoingEdges, edgeToComparable

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

import AssocList as Dict exposing (Dict)
import AssocList.Extra as DictExtra
import Maybe.Extra
import Set exposing (Set)


{-| TODO
-}
type Graph vertex
    = Graph
        { -- TODO maybe use elm-community/intdict too
          edges : Dict Int (Set Int)
        , -- TODO can we do without the reverse index here?
          vertices : Dict vertex Int
        , verticesById : Dict Int vertex
        , unusedId : Int
        }


{-| TODO
-}
type alias Edge vertex =
    { from : vertex
    , to : vertex
    }


{-| An empty graph.
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

Adds any vertices if they are present in the edges list but not in the vertices list.

-}
fromVerticesAndEdges : List vertex -> List (Edge vertex) -> Graph vertex
fromVerticesAndEdges vertices_ edges_ =
    empty
        |> (\graph -> List.foldl addEdge graph edges_)
        |> (\graph -> List.foldl addVertex graph vertices_)


{-| Is the graph empty?
-}
isEmpty : Graph vertex -> Bool
isEmpty (Graph g) =
    Dict.isEmpty g.vertices


{-| TODO
-}
hasVertex : vertex -> Graph vertex -> Bool
hasVertex vertex (Graph g) =
    Dict.member vertex g.vertices


{-| Directed variant of `areAdjacent`
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


{-| Does the graph contain an edge between these two vertices?

Undirected - the order of the two vertices doesn't matter.

-}
areAdjacent : vertex -> vertex -> Graph vertex -> Bool
areAdjacent v1 v2 graph =
    hasEdge { from = v1, to = v2 } graph
        || hasEdge { from = v2, to = v1 } graph


{-| Lists all vertices which to which the given vertex "points".

Directed - the edge can only go _from_ the given vertex.

     Graph.empty
         |> Graph.outgoingEdges "foo"
     -->
     []

     Graph.empty
         |> Graph.addEdge {from = "foo", to = "bar"}
         |> Graph.outgoingEdges "foo"
     -->
     [ "bar" ]

TODO rethink naming?
TODO or return List (Edge vertex)?

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
                                |> Dict.filter (\k v -> k /= id)
                                -- incoming
                                |> Dict.map
                                    (\k v ->
                                        -- TODO maybe do member check first?
                                        Set.remove id v
                                    )
                        , vertices = Dict.remove vertex g.vertices
                        , verticesById = Dict.remove id g.verticesById
                    }
            )
        |> Maybe.withDefault graph


{-| Updates the vertex data. Does nothing if the vertex is not present.
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

TODO example

-}
addEdge : Edge vertex -> Graph vertex -> Graph vertex
addEdge ({ from, to } as edge) graph =
    graph
        |> addVertex from
        |> addVertex to
        |> addEdge_ edge


{-| A helper for `addEdge` that doesn't check if the vertices are present.
-}
addEdge_ : Edge vertex -> Graph vertex -> Graph vertex
addEdge_ { from, to } ((Graph g) as graph) =
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


{-| Remove an edge from the first vertex to the second one.

If the edge is not present, nothing happens.

TODO example

-}
removeEdge : Edge vertex -> Graph vertex -> Graph vertex
removeEdge { from, to } ((Graph g) as graph) =
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


{-| TODO
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


{-| TODO
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
-}
fold : (vertex -> acc -> acc) -> acc -> Graph vertex -> acc
fold fn acc (Graph g) =
    Dict.foldr (always fn) acc g.verticesById


{-| TODO
-}
size : Graph vertex -> Int
size (Graph g) =
    Dict.size g.vertices


{-| TODO
-}
vertices : Graph vertex -> List vertex
vertices (Graph g) =
    Dict.keys g.vertices


{-| TODO
-}
edges : Graph vertex -> List { from : vertex, to : vertex }
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


{-| TODO
-}
edgeToComparable : Edge comparable -> ( comparable, comparable )
edgeToComparable { from, to } =
    ( from, to )
