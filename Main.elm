module Main exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Random
import Array
import Random.Array


-- MAIN


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- TYPES


type ValeurCarte
    = V2
    | V3
    | V4
    | V5
    | V6
    | V7
    | V8
    | V9
    | V10
    | Valet
    | Dame
    | Roi
    | As


type CouleurCarte
    = Pique
    | Coeur
    | Carreau
    | Trefle


type Carte
    = Carte ValeurCarte CouleurCarte


couleurs : List CouleurCarte
couleurs =
    [ Pique, Coeur, Carreau, Trefle ]


valeurs : List ValeurCarte
valeurs =
    [ V2, V3, V4, V5, V6, V7, V8, V9, V10, Valet, Dame, Roi, As ]


cartes : List Carte
cartes =
    List.concatMap
        (\couleur ->
            List.map
                (\valeur -> Carte valeur couleur)
                valeurs
        )
        couleurs


type alias Main =
    List Carte


type alias Donne =
    { nord : Main
    , sud : Main
    , est : Main
    , ouest : Main
    }


emptyDonne : Donne
emptyDonne =
    { nord = [], sud = [], est = [], ouest = [] }


showValeurCarte : ValeurCarte -> String
showValeurCarte valeur =
    case valeur of
        V2 ->
            "2"

        V3 ->
            "3"

        V4 ->
            "4"

        V5 ->
            "5"

        V6 ->
            "6"

        V7 ->
            "7"

        V8 ->
            "8"

        V9 ->
            "9"

        V10 ->
            "10"

        Valet ->
            "V"

        Dame ->
            "D"

        Roi ->
            "R"

        As ->
            "A"


showCouleurCarte : CouleurCarte -> String
showCouleurCarte couleur =
    case couleur of
        Pique ->
            "♠"

        Coeur ->
            "♥"

        Carreau ->
            "♦"

        Trefle ->
            "♣"


isDonneValid : Donne -> Bool
isDonneValid { nord, sud, est, ouest } =
    let
        mains =
            [ nord, sud, est, ouest ]

        validLength main =
            List.length main == 13

        inOneOfMains carte =
            List.any (List.member carte) mains
    in
        List.all validLength mains && List.all inOneOfMains cartes


type alias MainParCouleur =
    { pique : List Carte
    , coeur : List Carte
    , carreau : List Carte
    , trefle : List Carte
    }


emptyMainParCouleur : MainParCouleur
emptyMainParCouleur =
    { pique = [], coeur = [], carreau = [], trefle = [] }


groupByCouleur : Main -> MainParCouleur
groupByCouleur main =
    List.foldl
        (\((Carte _ couleur) as carte) mainAccu ->
            case couleur of
                Pique ->
                    { mainAccu | pique = carte :: mainAccu.pique }

                Coeur ->
                    { mainAccu | coeur = carte :: mainAccu.coeur }

                Carreau ->
                    { mainAccu | carreau = carte :: mainAccu.carreau }

                Trefle ->
                    { mainAccu | trefle = carte :: mainAccu.trefle }
        )
        emptyMainParCouleur
        main
        |> \{ pique, coeur, carreau, trefle } ->
            { pique = sortMain pique
            , coeur = sortMain coeur
            , carreau = sortMain carreau
            , trefle = sortMain trefle
            }


sortMain : Main -> Main
sortMain main =
    let
        valeurCarteIndex : Carte -> Int
        valeurCarteIndex (Carte valeur _) =
            case valeur of
                V2 ->
                    0

                V3 ->
                    1

                V4 ->
                    2

                V5 ->
                    3

                V6 ->
                    4

                V7 ->
                    5

                V8 ->
                    6

                V9 ->
                    7

                V10 ->
                    8

                Valet ->
                    9

                Dame ->
                    10

                Roi ->
                    11

                As ->
                    12
    in
        List.sortBy valeurCarteIndex main |> List.reverse



-- GENERATE


cartesGenerator : Random.Generator (List Carte)
cartesGenerator =
    Array.fromList cartes |> Random.Array.shuffle |> Random.map Array.toList


donneGenerator : Random.Generator Donne
donneGenerator =
    Random.map donneFromCartes cartesGenerator


donneFromCartes : List Carte -> Donne
donneFromCartes cartes =
    List.foldl
        (\( index, carte ) donneAccu ->
            let
                modulo =
                    index % 4
            in
                if modulo == 0 then
                    { donneAccu | nord = carte :: donneAccu.nord }
                else if modulo == 1 then
                    { donneAccu | sud = carte :: donneAccu.sud }
                else if modulo == 2 then
                    { donneAccu | est = carte :: donneAccu.est }
                else
                    { donneAccu | ouest = carte :: donneAccu.ouest }
        )
        emptyDonne
        (List.indexedMap (,) cartes)



-- MODEL


type alias Model =
    { donne : Donne }



-- INIT


init : ( Model, Cmd msg )
init =
    ( { donne = donne1 }, Cmd.none )



-- MSG


type Msg
    = GenerateDonne
    | SetDonne Donne



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateDonne ->
            ( model, Random.generate SetDonne donneGenerator )

        SetDonne donne ->
            ( { model | donne = donne }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ viewDonne model.donne
        , p []
            [ text
                ("La donne est "
                    ++ (if isDonneValid model.donne then
                            "valide"
                        else
                            "invalide"
                       )
                    ++ "."
                )
            ]
        , button [ onClick GenerateDonne ] [ text "Générer" ]
        ]


viewDonne : Donne -> Html msg
viewDonne { nord, sud, est, ouest } =
    let
        nineByNineTable styleAttributes nord sud est ouest center =
            table [ style styleAttributes ]
                [ tr []
                    [ td [] []
                    , td [] [ nord ]
                    , td [] []
                    ]
                , tr []
                    [ td [] [ ouest ]
                    , center
                    , td [] [ est ]
                    ]
                , tr []
                    [ td [] []
                    , td [] [ sud ]
                    , td [] []
                    ]
                ]
    in
        nineByNineTable
            []
            (viewMain nord)
            (viewMain sud)
            (viewMain est)
            (viewMain ouest)
            (nineByNineTable
                [ ( "background-color", "lightgray" )
                , ( "text-align", "center" )
                , ( "width", "100%" )
                ]
                (text "N")
                (text "S")
                (text "E")
                (text "O")
                (td [] [])
            )


viewMain : Main -> Html msg
viewMain main =
    let
        mainParCouleur =
            groupByCouleur main

        viewMainCouleur couleur cartes =
            [ span
                [ style
                    (let
                        red =
                            [ ( "color", "red" ) ]
                     in
                        case couleur of
                            Coeur ->
                                red

                            Carreau ->
                                red

                            _ ->
                                []
                    )
                ]
                [ text (showCouleurCarte couleur) ]
            , text " "
            , (if List.isEmpty cartes then
                "–"
               else
                List.map
                    (\(Carte valeur _) -> showValeurCarte valeur)
                    cartes
                    |> String.join " "
              )
                |> text
            ]
    in
        ul
            [ style
                [ ( "list-style-type", "none" )
                , ( "margin", "0" )
                , ( "padding", "0" )
                ]
            ]
            [ li [] (viewMainCouleur Pique mainParCouleur.pique)
            , li [] (viewMainCouleur Coeur mainParCouleur.coeur)
            , li [] (viewMainCouleur Carreau mainParCouleur.carreau)
            , li [] (viewMainCouleur Trefle mainParCouleur.trefle)
            ]



-- SAMPLES


donne1 : Donne
donne1 =
    { nord =
        [ Carte As Pique
        , Carte Roi Pique
        , Carte Dame Pique
        , Carte V8 Pique
        , Carte V5 Pique
        , Carte V7 Coeur
        , Carte As Trefle
        , Carte Roi Trefle
        , Carte V3 Trefle
        , Carte V2 Trefle
        , Carte V4 Carreau
        , Carte V3 Carreau
        , Carte V2 Carreau
        ]
    , sud =
        [ Carte Valet Pique
        , Carte V10 Pique
        , Carte V9 Pique
        , Carte Dame Coeur
        , Carte V6 Coeur
        , Carte V3 Coeur
        , Carte V2 Coeur
        , Carte Dame Trefle
        , Carte Valet Trefle
        , Carte V10 Trefle
        , Carte V9 Trefle
        , Carte As Carreau
        , Carte V5 Carreau
        ]
    , est =
        [ Carte V7 Pique
        , Carte V6 Pique
        , Carte As Coeur
        , Carte V4 Coeur
        , Carte V5 Coeur
        , Carte V9 Coeur
        , Carte V8 Trefle
        , Carte V7 Trefle
        , Carte Roi Carreau
        , Carte Dame Carreau
        , Carte V10 Carreau
        , Carte V9 Carreau
        , Carte V8 Carreau
        ]
    , ouest =
        [ Carte V4 Pique
        , Carte V3 Pique
        , Carte V2 Pique
        , Carte Roi Coeur
        , Carte Valet Coeur
        , Carte V10 Coeur
        , Carte V8 Coeur
        , Carte V6 Trefle
        , Carte V5 Trefle
        , Carte V4 Trefle
        , Carte Valet Carreau
        , Carte V7 Carreau
        , Carte V6 Carreau
        ]
    }
