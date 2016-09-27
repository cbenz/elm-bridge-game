module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import String


-- MAIN


main : Html msg
main =
    viewDonne donne1



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


type alias Main =
    List Carte


type alias Donne =
    { nord : Main
    , sud : Main
    , est : Main
    , ouest : Main
    }


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



-- generate : () -> Donne
-- isDonneValid : Donne -> Bool
-- isDonneValid {nord,sud,est,ouest} =


type alias MainParCouleur =
    { pique : List Carte, coeur : List Carte, carreau : List Carte, trefle : List Carte }


groupByCouleur : Main -> MainParCouleur
groupByCouleur main =
    List.foldl
        (\((Carte _ couleur) as carte) accu ->
            case couleur of
                Pique ->
                    { accu | pique = carte :: accu.pique }

                Coeur ->
                    { accu | coeur = carte :: accu.coeur }

                Carreau ->
                    { accu | carreau = carte :: accu.carreau }

                Trefle ->
                    { accu | trefle = carte :: accu.trefle }
        )
        { pique = [], coeur = [], carreau = [], trefle = [] }
        main
        |> let
            sort couleur =
                List.sortBy valeurCarteIndex couleur |> List.reverse
           in
            \{ pique, coeur, carreau, trefle } ->
                { pique = sort pique, coeur = sort coeur, carreau = sort carreau, trefle = sort trefle }


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



-- VIEW


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

        viewMainCouleur couleur getter =
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
            , List.map
                (\(Carte valeur _) -> showValeurCarte valeur)
                (getter mainParCouleur)
                |> String.join " "
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
            [ li [] (viewMainCouleur Pique .pique)
            , li [] (viewMainCouleur Coeur .coeur)
            , li [] (viewMainCouleur Carreau .carreau)
            , li [] (viewMainCouleur Trefle .trefle)
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
