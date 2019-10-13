module GraphTest exposing (..)

import ArchitectureTest exposing (invariantTest, msgTest)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Graph exposing (Edge, Graph)
import List.Extra
import Set exposing (Set)
import Test exposing (Test, describe, fuzz, fuzz2, test, todo)



-- EXAMPLES


graphWithFoo : Graph String
graphWithFoo =
    Graph.empty
        |> Graph.addVertex "foo"


graphWithFooBar : Graph String
graphWithFooBar =
    Graph.empty
        |> Graph.addVertex "foo"
        |> Graph.addVertex "bar"


graphWithFooBarWithEdge : Graph String
graphWithFooBarWithEdge =
    Graph.empty
        |> Graph.addEdge { from = "foo", to = "bar" }


graphWithFooBarWithReverseEdge : Graph String
graphWithFooBarWithReverseEdge =
    Graph.empty
        |> Graph.addEdge { from = "bar", to = "foo" }



-- ARCHITECTURE TEST STUFF


type alias Model =
    Graph String


type Msg
    = AddVertex String
    | RemoveVertex String
    | UpdateVertex String String
    | AddEdge (Edge String)
    | RemoveEdge (Edge String)


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
            Graph.updateVertex before (always after) graph

        AddEdge edge ->
            Graph.addEdge edge graph

        RemoveEdge edge ->
            Graph.removeEdge edge graph


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
        ++ Debug.toString edges
        ++ ">"


modelFuzzer : Fuzzer (Graph String)
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


edgeFuzzer : Fuzzer (Edge String)
edgeFuzzer =
    Fuzz.map2 Edge
        vertexFuzzer
        vertexFuzzer


msgFuzzers =
    { addVertex = Fuzz.map AddVertex vertexFuzzer
    , removeVertex = Fuzz.map RemoveVertex vertexFuzzer
    , updateVertex = Fuzz.map2 UpdateVertex vertexFuzzer vertexFuzzer
    , addEdge = Fuzz.map AddEdge edgeFuzzer
    , removeEdge = Fuzz.map RemoveEdge edgeFuzzer
    }


msgFuzzer : Fuzzer Msg
msgFuzzer =
    Fuzz.oneOf
        [ msgFuzzers.addVertex
        , msgFuzzers.removeVertex
        , msgFuzzers.updateVertex
        , msgFuzzers.addEdge
        , msgFuzzers.removeEdge
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
            , fuzz2 (Fuzz.list vertexFuzzer) (Fuzz.list edgeFuzzer) "resulting graph contains all the given edges" <|
                \vertices edges ->
                    let
                        finalEdges : Set ( String, String )
                        finalEdges =
                            Graph.fromVerticesAndEdges vertices edges
                                |> Graph.edges
                                |> List.map Graph.edgeToComparable
                                |> Set.fromList

                        inputEdges : Set ( String, String )
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
                    Graph.hasEdge (edgeInMsg msg) finalGraph
                        |> Expect.true ""
            , msgTest "does nothing if the edge is already present" app msgFuzzers.addEdge <|
                \initGraph msg finalGraph ->
                    if Graph.hasEdge (edgeInMsg msg) initGraph then
                        finalGraph
                            |> Expect.equal initGraph

                    else
                        Expect.pass
            ]
        , describe "removeEdge"
            [ msgTest "results in hasEdge == False" app msgFuzzers.removeEdge <|
                \_ msg finalGraph ->
                    Graph.hasEdge (edgeInMsg msg) finalGraph
                        |> Expect.false ""
            , msgTest "makes the edge not visible in `edges`" app msgFuzzers.removeEdge <|
                \_ msg finalGraph ->
                    Graph.edges finalGraph
                        |> List.member (edgeInMsg msg)
                        |> Expect.false ""
            , msgTest "does nothing if the edge is not present" app msgFuzzers.removeEdge <|
                \initGraph msg finalGraph ->
                    if Graph.hasEdge (edgeInMsg msg) initGraph then
                        Expect.pass

                    else
                        finalGraph
                            |> Expect.equal initGraph
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
                    Graph.hasEdge { from = "foo", to = "bar" } graphWithFooBarWithEdge
                        |> Expect.true ""
            , test "returns False if the `from` vertex is not present" <|
                \() ->
                    Graph.hasEdge { from = "foo", to = "bar" } Graph.empty
                        |> Expect.false ""
            , test "returns False if the `to` vertex is not present" <|
                \() ->
                    Graph.hasEdge { from = "foo", to = "baz" } graphWithFooBarWithEdge
                        |> Expect.false ""
            , test "returns False if the edge is not present" <|
                \() ->
                    Graph.hasEdge { from = "foo", to = "bar" } graphWithFooBar
                        |> Expect.false ""
            , test "returns False if the edge is in wrong direction" <|
                \() ->
                    Graph.hasEdge { from = "foo", to = "bar" } graphWithFooBarWithReverseEdge
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
                                        if Graph.hasEdge { from = v1, to = v2 } finalGraph then
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
                                        if Graph.hasEdge { from = v1, to = v2 } finalGraph then
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
                        |> Expect.equalLists [ { from = "foo", to = "bar" } ]
            , test "works for graph with two edges" <|
                \() ->
                    Graph.empty
                        |> Graph.addEdge { from = "foo", to = "bar" }
                        |> Graph.addEdge { from = "bar", to = "baz" }
                        |> Graph.edges
                        |> List.map Graph.edgeToComparable
                        |> Set.fromList
                        |> Expect.equalSets
                            ([ { from = "foo", to = "bar" }
                             , { from = "bar", to = "baz" }
                             ]
                                |> List.map Graph.edgeToComparable
                                |> Set.fromList
                            )
            ]
        , describe "outgoingEdges"
            [ test "returns empty list if vertex isn't present" <|
                \() ->
                    Graph.outgoingEdges "foo" Graph.empty
                        |> Expect.equalLists []
            , test "returns empty list if vertex has no edges" <|
                \() ->
                    Graph.outgoingEdges "foo" graphWithFoo
                        |> Expect.equalLists []
            , test "returns a list of edges if vertex has them" <|
                \() ->
                    Graph.outgoingEdges "foo" graphWithFooBarWithEdge
                        |> Expect.equalLists [ "bar" ]
            , test "is directed" <|
                \() ->
                    Graph.outgoingEdges "foo" graphWithFooBarWithReverseEdge
                        |> Expect.equalLists []
            ]
        , describe "edgeToComparable"
            [ fuzz edgeFuzzer "from is on the left" <|
                \({ from } as edge) ->
                    let
                        ( from_, _ ) =
                            Graph.edgeToComparable edge
                    in
                    from_
                        |> Expect.equal from
            , fuzz edgeFuzzer "to is on the right" <|
                \({ to } as edge) ->
                    let
                        ( _, to_ ) =
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


edgeInMsg : Msg -> Edge String
edgeInMsg msg =
    case msg of
        AddEdge edge ->
            edge

        RemoveEdge edge ->
            edge

        _ ->
            -- shouldn't happen
            { from = "", to = "" }
