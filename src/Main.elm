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
    { pique : CouleurDansMain
    , coeur : CouleurDansMain
    , carreau : CouleurDansMain
    , trefle : CouleurDansMain
    }


type alias CouleurDansMain =
    List Carte


type alias Donne =
    { nord : Main
    , sud : Main
    , est : Main
    , ouest : Main
    }


emptyDonne : Donne
emptyDonne =
    { nord = emptyMain
    , sud = emptyMain
    , est = emptyMain
    , ouest = emptyMain
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


concatMain : Main -> List Carte
concatMain main =
    List.concat [ main.pique, main.coeur, main.carreau, main.trefle ]


isDonneValid : Donne -> Bool
isDonneValid { nord, sud, est, ouest } =
    let
        mains =
            [ nord, sud, est, ouest ]

        validLength main =
            List.length (concatMain main) == 13

        inOneOfMains carte =
            List.any (\main -> List.member carte (concatMain main)) mains
    in
        List.all validLength mains && List.all inOneOfMains cartes


emptyMain : Main
emptyMain =
    { pique = [], coeur = [], carreau = [], trefle = [] }


mainFromCartes : List Carte -> Main
mainFromCartes cartes =
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
        emptyMain
        cartes
        |> \{ pique, coeur, carreau, trefle } ->
            { pique = sortCartes pique
            , coeur = sortCartes coeur
            , carreau = sortCartes carreau
            , trefle = sortCartes trefle
            }


repartitionMain : Main -> List Int
repartitionMain { pique, coeur, carreau, trefle } =
    List.map List.length [ pique, coeur, carreau, trefle ]


isMainReguliere : Main -> Bool
isMainReguliere main =
    let
        repartition =
            repartitionMain main
    in
        -- List.all ((>=) 1) repartition &&
        List.any ((==) repartition)
            [ [ 4, 3, 3, 3 ]
            , [ 4, 4, 3, 2 ]
            , [ 5, 3, 3, 2 ]
            ]


sortCartes : List Carte -> List Carte
sortCartes cartes =
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
        List.sortBy valeurCarteIndex cartes |> List.reverse


type alias Fit =
    ( CouleurCarte, Int )


getFits : Main -> Main -> List Fit
getFits main1 main2 =
    List.filterMap
        (\( getter, couleur ) ->
            let
                fitLength =
                    List.length (List.concat [ getter main1, getter main2 ])
            in
                if fitLength >= 8 then
                    Just ( couleur, fitLength )
                else
                    Nothing
        )
        [ ( .pique, Pique )
        , ( .coeur, Coeur )
        , ( .carreau, Carreau )
        , ( .trefle, Trefle )
        ]


isFit : Main -> Main -> Bool
isFit main1 main2 =
    List.length (getFits main1 main2) > 0


type alias Points =
    { honneur : Int
    , longueur : Int
    , distribution : Maybe Int
    }


pointsHonneurCarte : Carte -> Int
pointsHonneurCarte (Carte valeur _) =
    case valeur of
        Valet ->
            1

        Dame ->
            2

        Roi ->
            3

        As ->
            4

        _ ->
            0


pointsLongueurCouleur : CouleurDansMain -> Maybe Int
pointsLongueurCouleur cartes =
    let
        isBelleCouleur cartes =
            let
                valeurs =
                    List.map
                        (\(Carte valeur _) -> valeur)
                        cartes
            in
                List.any
                    (List.all (\honneur -> List.member honneur valeurs))
                    [ [ As ], [ Roi ], [ Dame, Valet ] ]
                    && (List.length cartes >= 5)
    in
        if isBelleCouleur cartes then
            Just ((List.length cartes) - 4)
        else
            Nothing


pointsDistributionCouleur : CouleurDansMain -> Maybe Int
pointsDistributionCouleur cartes =
    if List.length cartes == 0 then
        -- chicane
        Just 3
    else if List.length cartes == 1 then
        -- singleton
        Just 2
    else if List.length cartes == 2 then
        -- doubleton
        Just 1
    else
        Nothing


pointsMain : Main -> Maybe Main -> Points
pointsMain main otherMain =
    { honneur =
        List.map pointsHonneurCarte (concatMain main) |> List.sum
    , longueur =
        [ pointsLongueurCouleur main.pique
        , pointsLongueurCouleur main.coeur
        , pointsLongueurCouleur main.carreau
        , pointsLongueurCouleur main.trefle
        ]
            |> List.map (Maybe.withDefault 0)
            |> List.sum
    , distribution =
        case otherMain of
            Just otherMain ->
                if isFit main otherMain then
                    [ pointsDistributionCouleur main.pique
                    , pointsDistributionCouleur main.coeur
                    , pointsDistributionCouleur main.carreau
                    , pointsDistributionCouleur main.trefle
                    ]
                        |> List.map (Maybe.withDefault 0)
                        |> List.sum
                        |> Just
                else
                    Nothing

            Nothing ->
                Nothing
    }


showPoints : Points -> String
showPoints { honneur, longueur, distribution } =
    -- H et HL
    -- H et HLD si fit
    [ toString honneur ++ "H"
    , case distribution of
        Just distribution ->
            toString (honneur + longueur + distribution) ++ "HLD"

        Nothing ->
            toString (honneur + longueur) ++ "HL"
    ]
        |> String.join " "


showRepartition : Main -> String
showRepartition { carreau, coeur, pique, trefle } =
    [ carreau, coeur, pique, trefle ]
        |> List.map List.length
        |> List.sort
        |> List.reverse
        |> List.map toString
        |> String.join "-"



-- ENCHERES


type CouleurEnchere
    = EncherePique
    | EnchereCoeur
    | EnchereCarreau
    | EnchereTrefle
    | SansAtout


type Enchere
    = Enchere ( Int, CouleurEnchere )
    | Passe
    | EnchereTodo


nextEnchere : Main -> Maybe Main -> Enchere
nextEnchere main otherMain =
    let
        points =
            pointsMain main otherMain
    in
        if points.honneur >= 12 then
            if List.length main.pique >= 5 then
                Enchere ( 1, EncherePique )
            else if List.length main.coeur >= 5 then
                Enchere ( 1, EnchereCoeur )
            else if points.honneur >= 15 && points.honneur <= 17 && isMainReguliere main then
                Enchere ( 1, SansAtout )
            else
                EnchereTodo
        else
            EnchereTodo


showEnchere : Enchere -> String
showEnchere enchere =
    case enchere of
        Enchere ( niveau, couleur ) ->
            toString niveau ++ showCouleurEnchere couleur

        Passe ->
            "Passe"

        EnchereTodo ->
            "TODO (pas encore programmé)"


showCouleurEnchere : CouleurEnchere -> String
showCouleurEnchere couleur =
    case couleur of
        EncherePique ->
            "♠"

        EnchereCoeur ->
            "♥"

        EnchereCarreau ->
            "♦"

        EnchereTrefle ->
            "♣"

        SansAtout ->
            "SA"



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
        { nord = [], sud = [], est = [], ouest = [] }
        (List.indexedMap (,) cartes)
        |> \{ nord, sud, est, ouest } ->
            { nord = mainFromCartes nord
            , sud = mainFromCartes sud
            , est = mainFromCartes est
            , ouest = mainFromCartes ouest
            }



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
            -- if isMainReguliere donne.sud then
            --     ( { model | donne = donne }, Cmd.none )
            -- else
            --     ( model, Random.generate SetDonne donneGenerator )
            ( { model | donne = donne }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { donne } =
    div
        []
        [ h1 [] [ text "🂡 Elm Bridge Game" ]
        , viewDonne donne
        , ul []
            [ li []
                [ text
                    ("La donne est "
                        ++ (if isDonneValid donne then
                                "valide"
                            else
                                "invalide"
                           )
                        ++ "."
                    )
                ]
            , li [] [ viewFits "Nord et Sud" (getFits donne.nord donne.sud) ]
            , li [] [ viewFits "Est et Ouest" (getFits donne.est donne.ouest) ]
            , li [] [ text ("Prochaine enchère pour Sud : " ++ (showEnchere (nextEnchere donne.sud Nothing))) ]
            ]
        , button [ onClick GenerateDonne ] [ text "Générer" ]
        , hr [] []
        , a [ href "https://github.com/cbenz/elm-bridge-game" ] [ text "Source code" ]
        ]


viewDonne : Donne -> Html msg
viewDonne { nord, sud, est, ouest } =
    let
        flexItem children =
            div [ style [ ( "flex", "1 33%" ) ] ] children

        mainChildren main otherMain =
            [ viewMain main
            , ulWithoutBullets
                [ li [] [ text (showPoints (pointsMain main otherMain)) ]
                , li []
                    [ text
                        (showRepartition main
                            ++ " ("
                            ++ (if isMainReguliere main then
                                    "régulière"
                                else
                                    "non régulière"
                               )
                            ++ ")"
                        )
                    ]
                ]
            ]
    in
        div
            [ style
                [ ( "display", "flex" )
                , ( "flex-wrap", "wrap" )
                ]
            ]
            [ flexItem [ text "" ]
            , flexItem (mainChildren nord (Just sud))
            , flexItem [ text "" ]
            , flexItem (mainChildren ouest (Just est))
            , flexItem [ text "" ]
            , flexItem (mainChildren est (Just ouest))
            , flexItem [ text "" ]
            , flexItem (mainChildren sud (Just nord))
            , flexItem [ text "" ]
            ]


viewMain : Main -> Html msg
viewMain main =
    let
        viewMainCouleur couleur cartes =
            [ span
                [ style (couleurCarteStyle couleur) ]
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
        ulWithoutBullets
            [ li [] (viewMainCouleur Pique main.pique)
            , li [] (viewMainCouleur Coeur main.coeur)
            , li [] (viewMainCouleur Carreau main.carreau)
            , li [] (viewMainCouleur Trefle main.trefle)
            ]


viewFits : String -> List Fit -> Html msg
viewFits msg fit =
    span []
        (List.concat
            [ [ text (msg ++ " ") ]
            , case fit of
                [] ->
                    [ text "ne sont pas fittés." ]

                couleurs ->
                    text "sont fittés "
                        :: (viewFrenchEnumeration
                                (List.map
                                    (\( couleur, fitLength ) ->
                                        span []
                                            [ text (toString fitLength)
                                            , sup [] [ text "ème" ]
                                            , text " à "
                                            , span
                                                [ style (couleurCarteStyle couleur) ]
                                                [ text (showCouleurCarte couleur) ]
                                            ]
                                    )
                                    couleurs
                                )
                           )
                        ++ [ text "." ]
            ]
        )


viewFrenchEnumeration : List (Html msg) -> List (Html msg)
viewFrenchEnumeration xs =
    let
        firsts =
            List.take (List.length xs - 1) xs

        lastAsList =
            List.drop (List.length xs - 1) xs
    in
        (if List.length firsts > 0 then
            List.intersperse (text ", ") firsts
                ++ [ text " et " ]
         else
            []
        )
            ++ lastAsList


couleurCarteStyle : CouleurCarte -> List ( String, String )
couleurCarteStyle couleur =
    let
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


ulWithoutBullets : List (Html msg) -> Html msg
ulWithoutBullets children =
    ul
        [ style
            [ ( "list-style-type", "none" )
            , ( "margin", "0" )
            , ( "padding", "0" )
            ]
        ]
        children



-- SAMPLES


donne1 : Donne
donne1 =
    { sud =
        mainFromCartes
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
    , nord =
        mainFromCartes
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
        mainFromCartes
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
        mainFromCartes
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
