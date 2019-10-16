module GraphTest exposing (..)

import ArchitectureTest exposing (invariantTest, msgTest)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Graph exposing (Edge, Graph)
import List.Extra
import Set exposing (Set)
import Test exposing (Test, describe, fuzz, fuzz2, test, todo)



-- EXAMPLES


graphWithFoo : Graph String Int
graphWithFoo =
    Graph.empty
        |> Graph.addVertex "foo"


graphWithFooBar : Graph String Int
graphWithFooBar =
    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addVertex "bar"


graphWithFooBarWithEdge : Graph String Int
graphWithFooBarWithEdge =
    Graph.empty
        |> Graph.addEdge "foo" "bar" 100


graphWithFooBarWithReverseEdge : Graph String Int
graphWithFooBarWithReverseEdge =
    Graph.empty
        |> Graph.addEdge "bar" "foo" 200



-- ARCHITECTURE TEST STUFF


type alias Model =
    Graph String Int


type Msg
    = AddVertex String
    | RemoveVertex String
    | UpdateVertex String String
    | AddEdge (Edge String Int)
    | RemoveEdge (Edge String Int)
    | UpdateEdge (Edge String Int)


app : ArchitectureTest.TestedApp Model Msg
app =
    { model = ArchitectureTest.FuzzedModel modelFuzzer
    , update = ArchitectureTest.UpdateWithoutCmds update
    , msgFuzzer = msgFuzzer
    , msgToString = Debug.toString
    , modelToString = modelToString
    }


update : Msg -> Model -> Model
update msg graph =
    case msg of
        AddVertex vertex ->
            Graph.addVertex vertex graph

        RemoveVertex vertex ->
            Graph.removeVertex vertex graph

        UpdateVertex before after ->
            -- in this case more like `setVertex` but it shouldn't matter to the tests...
            Graph.updateVertex before (always after) graph

        AddEdge { from, to, data } ->
            Graph.addEdge from to data graph

        RemoveEdge { from, to } ->
            Graph.removeEdge from to graph

        UpdateEdge { from, to, data } ->
            -- in this case more like `setEdge` but it shouldn't matter to the tests...
            Graph.updateEdge from to (always data) graph


modelToString : Model -> String
modelToString graph =
    let
        vertices =
            Graph.vertices graph

        edges =
            Graph.edges graph
    in
    "<GRAPH vertices="
        ++ Debug.toString vertices
        ++ " edges="
        ++ ("[" ++ (edges |> List.map edgeToString |> String.join ", ") ++ "]")
        ++ ">"


edgeToString : Edge vertex edge -> String
edgeToString { from, to, data } =
    Debug.toString from
        ++ "->"
        ++ Debug.toString to
        ++ ": "
        ++ Debug.toString data


modelFuzzer : Fuzzer (Graph String Int)
modelFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Graph.empty
        , Fuzz.constant graphWithFoo
        , Fuzz.constant graphWithFooBar
        , Fuzz.constant graphWithFooBarWithEdge
        , Fuzz.constant graphWithFooBarWithReverseEdge
        , Fuzz.map2 Graph.fromVerticesAndEdges (Fuzz.list vertexFuzzer) (Fuzz.list edgeFuzzer)
        ]


vertexFuzzer : Fuzzer String
vertexFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant "foo"
        , Fuzz.constant "bar"
        , Fuzz.constant "baz"
        , Fuzz.constant "quux"
        , Fuzz.string
        ]


edgeFuzzer : Fuzzer (Edge String Int)
edgeFuzzer =
    Fuzz.map3 Edge
        vertexFuzzer
        vertexFuzzer
        Fuzz.int


edgeListFuzzer : Fuzzer (List (Edge String Int))
edgeListFuzzer =
    {- We don't use the simple `Fuzz.list edgeFuzzer` because
       it would create [Edge "a" "b" 100, Edge "a" "b" 200]
       and wreak havoc on our test suite.

       So we make sure the (from,to) part doesn't have duplicates.
    -}
    Fuzz.list edgeFuzzer
        |> Fuzz.map
            (\list ->
                let
                    uniqueWithoutData =
                        List.map (\{ from, to } -> ( from, to )) list
                            |> Set.fromList
                            |> Set.toList
                in
                List.map2 (\( from, to ) { data } -> { from = from, to = to, data = data })
                    uniqueWithoutData
                    list
            )


msgFuzzers =
    { addVertex = Fuzz.map AddVertex vertexFuzzer
    , removeVertex = Fuzz.map RemoveVertex vertexFuzzer
    , updateVertex = Fuzz.map2 UpdateVertex vertexFuzzer vertexFuzzer
    , addEdge = Fuzz.map AddEdge edgeFuzzer
    , removeEdge = Fuzz.map RemoveEdge edgeFuzzer
    , updateEdge = Fuzz.map UpdateEdge edgeFuzzer
    }


msgFuzzer : Fuzzer Msg
msgFuzzer =
    Fuzz.oneOf
        [ msgFuzzers.addVertex
        , msgFuzzers.removeVertex
        , msgFuzzers.updateVertex
        , msgFuzzers.addEdge
        , msgFuzzers.removeEdge
        , msgFuzzers.updateEdge
        ]



-- TESTS


suite : Test
suite =
    describe "Graph"
        [ describe "empty"
            [ test "has zero vertices" <|
                \() ->
                    Graph.empty
                        |> Graph.vertices
                        |> Expect.equalLists []
            , test "has zero edges" <|
                \() ->
                    Graph.empty
                        |> Graph.edges
                        |> Expect.equalLists []
            ]
        , describe "fromVerticesAndEdges"
            [ test "is empty when given empty lists" <|
                \() ->
                    Graph.fromVerticesAndEdges [] []
                        |> Graph.isEmpty
                        |> Expect.true ""
            , fuzz2 (Fuzz.list vertexFuzzer) (Fuzz.list edgeFuzzer) "resulting graph contains all the given vertices" <|
                \vertices edges ->
                    let
                        finalVertices : Set String
                        finalVertices =
                            Graph.fromVerticesAndEdges vertices edges
                                |> Graph.vertices
                                |> Set.fromList

                        inputVertices : Set String
                        inputVertices =
                            Set.fromList vertices
                    in
                    Set.diff inputVertices finalVertices
                        |> Expect.equalSets Set.empty
            , fuzz2 (Fuzz.list vertexFuzzer) edgeListFuzzer "resulting graph contains all the given edges" <|
                \vertices edges ->
                    let
                        finalEdges : Set ( String, String, Int )
                        finalEdges =
                            Graph.fromVerticesAndEdges vertices edges
                                |> Graph.edges
                                |> List.map Graph.edgeToComparable
                                |> Set.fromList

                        inputEdges : Set ( String, String, Int )
                        inputEdges =
                            edges
                                |> List.map Graph.edgeToComparable
                                |> Set.fromList
                    in
                    Set.diff inputEdges finalEdges
                        |> Expect.equalSets Set.empty
            , fuzz2 (Fuzz.list vertexFuzzer) (Fuzz.list edgeFuzzer) "resulting graph contains all the vertices present in the given edges" <|
                \vertices edges ->
                    let
                        finalVertices : Set String
                        finalVertices =
                            Graph.fromVerticesAndEdges vertices edges
                                |> Graph.vertices
                                |> Set.fromList

                        inputVerticesInEdges : Set String
                        inputVerticesInEdges =
                            edges
                                |> List.concatMap (\{ from, to } -> [ from, to ])
                                |> Set.fromList
                    in
                    Set.diff inputVerticesInEdges finalVertices
                        |> Expect.equalSets Set.empty
            ]
        , describe "addVertex"
            [ msgTest "results in hasVertex v == True" app msgFuzzers.addVertex <|
                \_ msg finalGraph ->
                    Graph.hasVertex (vertexInMsg msg) finalGraph
                        |> Expect.true ""
            , msgTest "makes the vertex visible in `vertices`" app msgFuzzers.addVertex <|
                \_ msg finalGraph ->
                    Graph.vertices finalGraph
                        |> List.member (vertexInMsg msg)
                        |> Expect.true ""
            , msgTest "does nothing if the vertex is already present" app msgFuzzers.addVertex <|
                \initGraph msg finalGraph ->
                    let
                        addedVertex =
                            vertexInMsg msg
                    in
                    if Graph.hasVertex addedVertex initGraph then
                        finalGraph
                            |> Expect.equal initGraph

                    else
                        Expect.pass
            ]
        , describe "removeVertex"
            [ msgTest "removes all edges containing this vertex" app msgFuzzers.removeVertex <|
                \_ msg finalGraph ->
                    let
                        removedVertex =
                            vertexInMsg msg
                    in
                    Graph.edges finalGraph
                        |> List.any (\{ from, to } -> from == removedVertex || to == removedVertex)
                        |> Expect.false ""
            , msgTest "results in hasVertex v == False" app msgFuzzers.removeVertex <|
                \_ msg finalGraph ->
                    Graph.hasVertex (vertexInMsg msg) finalGraph
                        |> Expect.false ""
            , msgTest "makes the vertex not visible in `vertices`" app msgFuzzers.removeVertex <|
                \_ msg finalGraph ->
                    Graph.vertices finalGraph
                        |> List.member (vertexInMsg msg)
                        |> Expect.false ""
            , msgTest "does nothing if the vertex is not present" app msgFuzzers.removeVertex <|
                \initGraph msg finalGraph ->
                    let
                        removedVertex =
                            vertexInMsg msg
                    in
                    if Graph.hasVertex removedVertex initGraph then
                        Expect.pass

                    else
                        finalGraph
                            |> Expect.equal initGraph
            ]
        , describe "updateVertex"
            [ msgTest "results in hasVertex updated == True if initial vertex was present" app msgFuzzers.updateVertex <|
                \initGraph msg finalGraph ->
                    let
                        vertex =
                            vertexInMsg msg

                        updatedVertex =
                            updatedVertexInMsg msg
                    in
                    if Graph.hasVertex vertex initGraph then
                        Graph.hasVertex updatedVertex finalGraph
                            |> Expect.true ""

                    else
                        finalGraph
                            |> Expect.equal initGraph
            ]
        , describe "addEdge"
            [ msgTest "results in hasVertex from = True" app msgFuzzers.addEdge <|
                \_ msg finalGraph ->
                    let
                        { from } =
                            edgeInMsg msg
                    in
                    Graph.hasVertex from finalGraph
                        |> Expect.true ""
            , msgTest "results in hasVertex to = True" app msgFuzzers.addEdge <|
                \_ msg finalGraph ->
                    let
                        { to } =
                            edgeInMsg msg
                    in
                    Graph.hasVertex to finalGraph
                        |> Expect.true ""
            , msgTest "results in hasEdge = True" app msgFuzzers.addEdge <|
                \_ msg finalGraph ->
                    let
                        { from, to } =
                            edgeInMsg msg
                    in
                    Graph.hasEdge from to finalGraph
                        |> Expect.true ""
            , msgTest "does nothing if the edge is already present" app msgFuzzers.addEdge <|
                \initGraph msg finalGraph ->
                    let
                        { from, to } =
                            edgeInMsg msg
                    in
                    if Graph.hasEdge from to initGraph then
                        finalGraph
                            |> Expect.equal initGraph

                    else
                        Expect.pass
            ]
        , describe "removeEdge"
            [ msgTest "results in hasEdge == False" app msgFuzzers.removeEdge <|
                \_ msg finalGraph ->
                    let
                        { from, to } =
                            edgeInMsg msg
                    in
                    Graph.hasEdge from to finalGraph
                        |> Expect.false ""
            , msgTest "makes the edge not visible in `edges`" app msgFuzzers.removeEdge <|
                \_ msg finalGraph ->
                    Graph.edges finalGraph
                        |> List.member (edgeInMsg msg)
                        |> Expect.false ""
            , msgTest "does nothing if the edge is not present" app msgFuzzers.removeEdge <|
                \initGraph msg finalGraph ->
                    let
                        { from, to } =
                            edgeInMsg msg
                    in
                    if Graph.hasEdge from to initGraph then
                        Expect.pass

                    else
                        finalGraph
                            |> Expect.equal initGraph
            ]
        , describe "updateEdge"
            [ msgTest "if old graph has the edge, the new graph has the edge" app msgFuzzers.updateEdge <|
                \initGraph msg finalGraph ->
                    let
                        { from, to } =
                            edgeInMsg msg
                    in
                    if Graph.hasEdge from to initGraph then
                        Graph.hasEdge from to finalGraph
                            |> Expect.true ""

                    else
                        Expect.pass

            {- msgTest "[TODO: THIS ERRORS IN THE WEIRDEST (WRONG) WAYS] the new edge has the new data if initial edge was present" app msgFuzzers.updateEdge <|
               \initGraph msg finalGraph ->
                   let
                       { from, to, data } =
                           edgeInMsg msg
                   in
                   if Graph.hasEdge from to initGraph then
                       Graph.getEdge from to finalGraph
                           |> Expect.equal (Just data)

                   else
                       finalGraph
                           |> Expect.equal initGraph
            -}
            ]
        , describe "mapEdges"
            [ test "usecase" <|
                \() ->
                    graphWithFooBarWithEdge
                        |> Graph.mapEdges (\v -> v + 1000)
                        |> Graph.edges
                        |> Expect.equal [ { from = "foo", to = "bar", data = 1100 } ]
            ]
        , describe "reverseEdges"
            [ invariantTest "reverses all edges" app <|
                \_ _ finalGraph ->
                    let
                        initialEdges =
                            finalGraph
                                |> Graph.edges
                                |> List.map Graph.edgeToComparable
                                |> Set.fromList

                        edgesAfterReversing =
                            finalGraph
                                |> Graph.reverseEdges
                                |> Graph.edges
                                |> List.map Graph.edgeToComparable
                                |> Set.fromList
                    in
                    edgesAfterReversing
                        |> Expect.equalSets
                            (initialEdges |> Set.map (\( from, to, data ) -> ( to, from, data )))
            ]
        , describe "isEmpty"
            [ invariantTest "only True if no vertices" app <|
                \_ _ finalGraph ->
                    if List.isEmpty (Graph.vertices finalGraph) then
                        Graph.isEmpty finalGraph
                            |> Expect.true ""

                    else
                        Expect.pass
            ]
        , describe "hasVertex"
            [ test "returns True if the vertex is present" <|
                \() ->
                    Graph.hasVertex "foo" graphWithFoo
                        |> Expect.true ""
            , test "returns False if the vertex is not present" <|
                \() ->
                    Graph.hasVertex "bar" graphWithFoo
                        |> Expect.false ""
            ]
        , describe "hasEdge"
            [ test "returns True if the edge is present" <|
                \() ->
                    Graph.hasEdge "foo" "bar" graphWithFooBarWithEdge
                        |> Expect.true ""
            , test "returns False if the `from` vertex is not present" <|
                \() ->
                    Graph.hasEdge "foo" "bar" Graph.empty
                        |> Expect.false ""
            , test "returns False if the `to` vertex is not present" <|
                \() ->
                    Graph.hasEdge "foo" "baz" graphWithFooBarWithEdge
                        |> Expect.false ""
            , test "returns False if the edge is not present" <|
                \() ->
                    Graph.hasEdge "foo" "bar" graphWithFooBar
                        |> Expect.false ""
            , test "returns False if the edge is in wrong direction" <|
                \() ->
                    Graph.hasEdge "foo" "bar" graphWithFooBarWithReverseEdge
                        |> Expect.false ""
            , invariantTest "hasEdge implies hasVertex" app <|
                \_ _ finalGraph ->
                    let
                        vertices =
                            Graph.vertices finalGraph
                    in
                    List.Extra.cartesianProduct [ vertices, vertices ]
                        |> List.all
                            (\vs ->
                                case vs of
                                    [ v1, v2 ] ->
                                        if Graph.hasEdge v1 v2 finalGraph then
                                            Graph.hasVertex v1 finalGraph
                                                && Graph.hasVertex v2 finalGraph

                                        else
                                            True

                                    _ ->
                                        False
                            )
                        |> Expect.true ""
            ]
        , describe "areAdjacent"
            [ invariantTest "order doesn't matter" app <|
                \_ _ finalGraph ->
                    let
                        vertices =
                            Graph.vertices finalGraph
                    in
                    List.Extra.cartesianProduct [ vertices, vertices ]
                        |> List.all
                            (\vs ->
                                case vs of
                                    [ v1, v2 ] ->
                                        Graph.areAdjacent v1 v2 finalGraph
                                            == Graph.areAdjacent v2 v1 finalGraph

                                    _ ->
                                        False
                            )
                        |> Expect.true ""
            , invariantTest "hasEdge implies areAdjacent" app <|
                \_ _ finalGraph ->
                    let
                        vertices =
                            Graph.vertices finalGraph
                    in
                    List.Extra.cartesianProduct [ vertices, vertices ]
                        |> List.all
                            (\vs ->
                                case vs of
                                    [ v1, v2 ] ->
                                        if Graph.hasEdge v1 v2 finalGraph then
                                            Graph.areAdjacent v1 v2 finalGraph

                                        else
                                            True

                                    _ ->
                                        False
                            )
                        |> Expect.true ""
            , test "returns False when the `from` vertex doesn't have any edges" <|
                \() ->
                    Graph.areAdjacent "foo" "bar" graphWithFoo
                        |> Expect.false ""
            , test "returns False when the `from` vertex isn't present" <|
                \() ->
                    Graph.areAdjacent "bar" "baz" graphWithFoo
                        |> Expect.false ""
            ]
        , describe "fold"
            [ test "starts with the oldest vertices" <|
                \() ->
                    Graph.empty
                        |> Graph.addVertex "foo"
                        |> Graph.addVertex "bar"
                        |> Graph.fold (\vertex acc -> acc ++ vertex) ""
                        |> Expect.equal "foobar"
            ]
        , describe "size"
            [ test "zero for empty graph" <|
                \() ->
                    Graph.size Graph.empty
                        |> Expect.equal 0
            , invariantTest "equals length of `vertices`" app <|
                \_ _ finalGraph ->
                    Graph.size finalGraph
                        |> Expect.equal (List.length (Graph.vertices finalGraph))
            ]
        , describe "vertices"
            [ test "returns empty list for empty graph" <|
                \() ->
                    Graph.vertices Graph.empty
                        |> Expect.equalLists []
            , test "works for graph with one vertex" <|
                \() ->
                    Graph.vertices graphWithFoo
                        |> Expect.equalLists [ "foo" ]
            , test "works for graph with two vertices" <|
                \() ->
                    Graph.vertices graphWithFooBar
                        |> Set.fromList
                        |> Expect.equalSets (Set.fromList [ "foo", "bar" ])
            ]
        , describe "edges"
            [ test "returns empty list for empty graph" <|
                \() ->
                    Graph.edges Graph.empty
                        |> Expect.equalLists []
            , test "works for graph with one edge" <|
                \() ->
                    Graph.edges graphWithFooBarWithEdge
                        |> Expect.equalLists [ { from = "foo", to = "bar", data = 100 } ]
            , test "works for graph with two edges" <|
                \() ->
                    Graph.empty
                        |> Graph.addEdge "foo" "bar" 100
                        |> Graph.addEdge "bar" "baz" 200
                        |> Graph.edges
                        |> List.map Graph.edgeToComparable
                        |> Set.fromList
                        |> Expect.equalSets
                            ([ { from = "foo", to = "bar", data = 100 }
                             , { from = "bar", to = "baz", data = 200 }
                             ]
                                |> List.map Graph.edgeToComparable
                                |> Set.fromList
                            )
            ]
        , describe "verticesAndEdges"
            [ invariantTest "same as `vertices` and `edges` calls" app <|
                \_ _ finalGraph ->
                    let
                        { vertices, edges } =
                            Graph.verticesAndEdges finalGraph
                    in
                    ( vertices, edges )
                        |> Expect.equal
                            ( Graph.vertices finalGraph
                            , Graph.edges finalGraph
                            )
            ]
        , describe "outgoingEdgesWithData"
            [ test "returns empty list if vertex isn't present" <|
                \() ->
                    Graph.outgoingEdgesWithData "foo" Graph.empty
                        |> Expect.equalLists []
            , test "returns empty list if vertex has no edges" <|
                \() ->
                    Graph.outgoingEdgesWithData "foo" graphWithFoo
                        |> Expect.equalLists []
            , test "returns a list of edges if vertex has them" <|
                \() ->
                    Graph.outgoingEdgesWithData "foo" graphWithFooBarWithEdge
                        |> Expect.equalLists [ ( "bar", 100 ) ]
            , test "is directed" <|
                \() ->
                    Graph.outgoingEdgesWithData "foo" graphWithFooBarWithReverseEdge
                        |> Expect.equalLists []
            ]
        , describe "outgoingEdges"
            [ invariantTest "same as outgoingEdgesWithData without the data" app <|
                \_ _ finalGraph ->
                    Graph.outgoingEdges "foo" finalGraph
                        |> Expect.equalLists
                            (Graph.outgoingEdgesWithData "foo" finalGraph
                                |> List.map (\( to, data ) -> to)
                            )
            ]
        , describe "edgeToComparable"
            [ fuzz edgeFuzzer "from is the first" <|
                \({ from } as edge) ->
                    let
                        ( from_, _, _ ) =
                            Graph.edgeToComparable edge
                    in
                    from_
                        |> Expect.equal from
            , fuzz edgeFuzzer "to is the second" <|
                \({ to } as edge) ->
                    let
                        ( _, to_, _ ) =
                            Graph.edgeToComparable edge
                    in
                    to_
                        |> Expect.equal to
            ]
        ]


vertexInMsg : Msg -> String
vertexInMsg msg =
    case msg of
        AddVertex vertex ->
            vertex

        RemoveVertex vertex ->
            vertex

        UpdateVertex vertex _ ->
            vertex

        _ ->
            -- shouldn't happen
            ""


updatedVertexInMsg : Msg -> String
updatedVertexInMsg msg =
    case msg of
        UpdateVertex _ vertex ->
            vertex

        _ ->
            -- shouldn't happen
            ""


edgeInMsg : Msg -> Edge String Int
edgeInMsg msg =
    case msg of
        AddEdge edge ->
            edge

        RemoveEdge edge ->
            edge

        _ ->
            -- shouldn't happen
            { from = "", to = "", data = 0 }
