module GraphTest exposing (..)

import ArchitectureTest exposing (msgTest)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Graph exposing (Graph)
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
    = AddEdge String String
    | RemoveEdge String String
    | AddVertex String
    | RemoveVertex String


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
        AddEdge from to ->
            Graph.addEdge from to graph

        RemoveEdge from to ->
            Graph.removeEdge from to graph

        AddVertex vertex ->
            Graph.addVertex vertex graph

        RemoveVertex vertex ->
            Graph.removeVertex vertex graph


msgToString : Msg -> String
msgToString msg =
    case msg of
        AddEdge from to ->
            "AddEdge " ++ Debug.toString from ++ " " ++ Debug.toString to

        RemoveEdge from to ->
            "RemoveEdge " ++ Debug.toString from ++ " " ++ Debug.toString to

        AddVertex vertex ->
            "AddVertex " ++ Debug.toString vertex

        RemoveVertex vertex ->
            "RemoveVertex " ++ Debug.toString vertex


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


addEdgeFuzzer : Fuzzer Msg
addEdgeFuzzer =
    Fuzz.map2 AddEdge vertexFuzzer vertexFuzzer


removeEdgeFuzzer : Fuzzer Msg
removeEdgeFuzzer =
    Fuzz.map2 RemoveEdge vertexFuzzer vertexFuzzer


addVertexFuzzer : Fuzzer Msg
addVertexFuzzer =
    Fuzz.map AddVertex vertexFuzzer


removeVertexFuzzer : Fuzzer Msg
removeVertexFuzzer =
    Fuzz.map RemoveVertex vertexFuzzer


msgFuzzer : Fuzzer Msg
msgFuzzer =
    Fuzz.oneOf
        [ addEdgeFuzzer
        , removeEdgeFuzzer
        , addVertexFuzzer
        , removeVertexFuzzer
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
            [ todo "write tests"
            ]
        , describe "removeVertex"
            [ msgTest "removes all outgoing edges"
                app
                removeVertexFuzzer
              <|
                \_ msg finalGraph ->
                    let
                        removedVertex =
                            vertexInMsg msg
                    in
                    Graph.vertices finalGraph
                        |> List.all (\otherVertex -> not <| Graph.hasEdge removedVertex otherVertex finalGraph)
                        |> Expect.true ""
            , todo "removes all incoming edges"
            , todo "architecture msg test: removeVertex v -> hasVertex v == False"
            ]
        , describe "addEdge"
            [ todo "adds `from` vertex if it isn't present"
            , todo "adds `to` vertex if it isn't present"
            , todo "fuzz: if addEdge, then hasEdge = True"
            , todo "fuzz: is a no-op if the edge already exists"
            ]
        , describe "removeEdge"
            [ todo "write tests"
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
            [ todo "architecture msg test: order doesn't matter"
            , todo "works when the `from` vertex has single edge"
            , todo "works when the `from` vertex has multiple edges"
            , todo "returns False when the `from` vertex doesn't have any edges"
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
            , todo "rest of tests"
            ]
        , describe "edges"
            [ test "returns empty list for empty graph" <|
                \() ->
                    Graph.vertices Graph.empty
                        |> Expect.equalLists []
            , todo "rest of tests"
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
            , todo "is directed"
            ]
        , describe "neighbours"
            [ todo "define tests"
            ]
        ]


vertexInMsg : Msg -> String
vertexInMsg msg =
    case msg of
        AddVertex vertex ->
            vertex

        RemoveVertex vertex ->
            vertex

        _ ->
            -- shouldn't happen
            ""


edgeInMsg : Msg -> ( String, String )
edgeInMsg msg =
    case msg of
        AddEdge from to ->
            ( from, to )

        RemoveEdge from to ->
            ( from, to )

        _ ->
            -- shouldn't happen
            ( "", "" )
