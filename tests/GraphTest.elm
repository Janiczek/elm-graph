module GraphTest exposing (..)

import ArchitectureTest exposing (invariantTest, msgTest)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Graph exposing (Graph)
import List.Extra
import Set
import Test exposing (Test, describe, test, todo)



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
        |> Graph.addEdge "foo" "bar"


graphWithFooBarWithReverseEdge : Graph String
graphWithFooBarWithReverseEdge =
    Graph.empty
        |> Graph.addEdge "bar" "foo"



-- ARCHITECTURE TEST STUFF


type alias Model =
    Graph String


type Msg
    = AddVertex String
    | RemoveVertex String
    | UpdateVertex String String
    | AddEdge String String
    | RemoveEdge String String


app : ArchitectureTest.TestedApp Model Msg
app =
    { model =
        ArchitectureTest.OneOfModels
            [ Graph.empty
            , graphWithFoo
            , graphWithFooBar
            , graphWithFooBarWithEdge
            , graphWithFooBarWithReverseEdge
            ]
    , update = ArchitectureTest.UpdateWithoutCmds update
    , msgFuzzer = msgFuzzer
    , msgToString = msgToString
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

        AddEdge from to ->
            Graph.addEdge from to graph

        RemoveEdge from to ->
            Graph.removeEdge from to graph


msgToString : Msg -> String
msgToString msg =
    case msg of
        AddVertex vertex ->
            "AddVertex " ++ Debug.toString vertex

        RemoveVertex vertex ->
            "RemoveVertex " ++ Debug.toString vertex

        UpdateVertex before after ->
            "UpdateVertex " ++ Debug.toString before ++ " " ++ Debug.toString after

        AddEdge from to ->
            "AddEdge " ++ Debug.toString from ++ " " ++ Debug.toString to

        RemoveEdge from to ->
            "RemoveEdge " ++ Debug.toString from ++ " " ++ Debug.toString to


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


vertexFuzzer : Fuzzer String
vertexFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant "foo"
        , Fuzz.constant "bar"
        , Fuzz.constant "baz"
        , Fuzz.constant "quux"
        , Fuzz.string
        ]


addVertexFuzzer : Fuzzer Msg
addVertexFuzzer =
    Fuzz.map AddVertex vertexFuzzer


removeVertexFuzzer : Fuzzer Msg
removeVertexFuzzer =
    Fuzz.map RemoveVertex vertexFuzzer


updateVertexFuzzer : Fuzzer Msg
updateVertexFuzzer =
    Fuzz.map2 UpdateVertex vertexFuzzer vertexFuzzer


addEdgeFuzzer : Fuzzer Msg
addEdgeFuzzer =
    Fuzz.map2 AddEdge vertexFuzzer vertexFuzzer


removeEdgeFuzzer : Fuzzer Msg
removeEdgeFuzzer =
    Fuzz.map2 RemoveEdge vertexFuzzer vertexFuzzer


msgFuzzer : Fuzzer Msg
msgFuzzer =
    Fuzz.oneOf
        [ addVertexFuzzer
        , removeVertexFuzzer
        , updateVertexFuzzer
        , addEdgeFuzzer
        , removeEdgeFuzzer
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
        , describe "addVertex"
            [ msgTest "results in hasVertex v == True" app addVertexFuzzer <|
                \_ msg finalGraph ->
                    Graph.hasVertex (vertexInMsg msg) finalGraph
                        |> Expect.true ""
            , msgTest "makes the vertex visible in `vertices`" app addVertexFuzzer <|
                \_ msg finalGraph ->
                    Graph.vertices finalGraph
                        |> List.member (vertexInMsg msg)
                        |> Expect.true ""
            , msgTest "does nothing if the vertex is already present" app addVertexFuzzer <|
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
            [ msgTest "removes all edges containing this vertex" app removeVertexFuzzer <|
                \_ msg finalGraph ->
                    let
                        removedVertex =
                            vertexInMsg msg
                    in
                    Graph.edges finalGraph
                        |> List.any (\{ from, to } -> from == removedVertex || to == removedVertex)
                        |> Expect.false ""
            , msgTest "results in hasVertex v == False" app removeVertexFuzzer <|
                \_ msg finalGraph ->
                    Graph.hasVertex (vertexInMsg msg) finalGraph
                        |> Expect.false ""
            , msgTest "makes the vertex not visible in `vertices`" app removeVertexFuzzer <|
                \_ msg finalGraph ->
                    Graph.vertices finalGraph
                        |> List.member (vertexInMsg msg)
                        |> Expect.false ""
            , msgTest "does nothing if the vertex is not present" app removeVertexFuzzer <|
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
            [ msgTest "results in hasVertex updated == True if initial vertex was present" app updateVertexFuzzer <|
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
            [ msgTest "results in hasVertex from = True" app addEdgeFuzzer <|
                \_ msg finalGraph ->
                    let
                        { from } =
                            edgeInMsg msg
                    in
                    Graph.hasVertex from finalGraph
                        |> Expect.true ""
            , msgTest "results in hasVertex to = True" app addEdgeFuzzer <|
                \_ msg finalGraph ->
                    let
                        { to } =
                            edgeInMsg msg
                    in
                    Graph.hasVertex to finalGraph
                        |> Expect.true ""
            , msgTest "results in hasEdge = True" app addEdgeFuzzer <|
                \_ msg finalGraph ->
                    let
                        { from, to } =
                            edgeInMsg msg
                    in
                    Graph.hasEdge from to finalGraph
                        |> Expect.true ""
            , msgTest "does nothing if the edge is already present" app addEdgeFuzzer <|
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
            [ msgTest "results in hasEdge == False" app removeEdgeFuzzer <|
                \_ msg finalGraph ->
                    let
                        { from, to } =
                            edgeInMsg msg
                    in
                    Graph.hasEdge from to finalGraph
                        |> Expect.false ""
            , msgTest "makes the edge not visible in `edges`" app removeEdgeFuzzer <|
                \_ msg finalGraph ->
                    Graph.edges finalGraph
                        |> List.member (edgeInMsg msg)
                        |> Expect.false ""
            , msgTest "does nothing if the edge is not present" app removeEdgeFuzzer <|
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
                        |> Graph.addEdge "foo" "bar"
                        |> Graph.addEdge "bar" "baz"
                        |> Graph.edges
                        |> List.map (\{ from, to } -> ( from, to ))
                        |> Set.fromList
                        |> Expect.equalSets
                            ([ { from = "foo", to = "bar" }
                             , { from = "bar", to = "baz" }
                             ]
                                |> List.map (\{ from, to } -> ( from, to ))
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


edgeInMsg : Msg -> { from : String, to : String }
edgeInMsg msg =
    case msg of
        AddEdge from to ->
            { from = from, to = to }

        RemoveEdge from to ->
            { from = from, to = to }

        _ ->
            -- shouldn't happen
            { from = "", to = "" }
