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


type alias Hand =
    { pique : HandColor
    , coeur : HandColor
    , carreau : HandColor
    , trefle : HandColor
    }


type alias HandColor =
    List Carte


type alias Donne =
    { nord : Hand
    , sud : Hand
    , est : Hand
    , ouest : Hand
    }


emptyDonne : Donne
emptyDonne =
    { nord = emptyHand
    , sud = emptyHand
    , est = emptyHand
    , ouest = emptyHand
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


concatHand : Hand -> List Carte
concatHand hand =
    List.concat [ hand.pique, hand.coeur, hand.carreau, hand.trefle ]


isDonneValid : Donne -> Bool
isDonneValid { nord, sud, est, ouest } =
    let
        hands =
            [ nord, sud, est, ouest ]

        validLength hand =
            List.length (concatHand hand) == 13

        inOneOfHands carte =
            List.any (\hand -> List.member carte (concatHand hand)) hands
    in
        List.all validLength hands && List.all inOneOfHands cartes


emptyHand : Hand
emptyHand =
    { pique = [], coeur = [], carreau = [], trefle = [] }


handFromCartes : List Carte -> Hand
handFromCartes cartes =
    List.foldl
        (\((Carte _ couleur) as carte) handAccu ->
            case couleur of
                Pique ->
                    { handAccu | pique = carte :: handAccu.pique }

                Coeur ->
                    { handAccu | coeur = carte :: handAccu.coeur }

                Carreau ->
                    { handAccu | carreau = carte :: handAccu.carreau }

                Trefle ->
                    { handAccu | trefle = carte :: handAccu.trefle }
        )
        emptyHand
        cartes
        |> \{ pique, coeur, carreau, trefle } ->
            { pique = sortCartes pique
            , coeur = sortCartes coeur
            , carreau = sortCartes carreau
            , trefle = sortCartes trefle
            }


handRepartition : Hand -> List Int
handRepartition { pique, coeur, carreau, trefle } =
    List.map List.length [ pique, coeur, carreau, trefle ]


type HandType
    = Reguliere
    | Unicolore
    | Bicolore
    | Quelconque


handType : Hand -> HandType
handType hand =
    let
        repartition =
            handRepartition hand
    in
        if
            -- List.all ((>=) 1) repartition &&
            List.any ((==) repartition)
                [ [ 4, 3, 3, 3 ]
                , [ 4, 4, 3, 2 ]
                , [ 5, 3, 3, 2 ]
                ]
        then
            Reguliere
        else if
            repartition
                |> List.filterMap
                    (\x ->
                        if x >= 4 then
                            Just x
                        else
                            Nothing
                    )
                |> \xs -> List.sum xs >= 9
        then
            Bicolore
        else if
            repartition
                |> List.filterMap
                    (\x ->
                        if x >= 6 then
                            Just True
                        else
                            Nothing
                    )
                |> \xs -> List.length xs == 1
        then
            Unicolore
        else
            Quelconque


showHandType : HandType -> String
showHandType handType =
    case handType of
        Reguliere ->
            "régulière"

        Unicolore ->
            "unicolore"

        Bicolore ->
            "bicolore"

        Quelconque ->
            "quelconque"


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


getFits : Hand -> Hand -> List Fit
getFits hand1 hand2 =
    List.filterMap
        (\( getter, couleur ) ->
            let
                fitLength =
                    List.length (List.concat [ getter hand1, getter hand2 ])
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


isFit : Hand -> Hand -> Bool
isFit hand1 hand2 =
    List.length (getFits hand1 hand2) > 0


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


pointsLongueurCouleur : HandColor -> Maybe Int
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


pointsDistributionCouleur : HandColor -> Maybe Int
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


pointsHand : Hand -> Maybe Hand -> Points
pointsHand hand otherHand =
    { honneur =
        List.map pointsHonneurCarte (concatHand hand) |> List.sum
    , longueur =
        [ pointsLongueurCouleur hand.pique
        , pointsLongueurCouleur hand.coeur
        , pointsLongueurCouleur hand.carreau
        , pointsLongueurCouleur hand.trefle
        ]
            |> List.map (Maybe.withDefault 0)
            |> List.sum
    , distribution =
        case otherHand of
            Just otherHand ->
                if isFit hand otherHand then
                    [ pointsDistributionCouleur hand.pique
                    , pointsDistributionCouleur hand.coeur
                    , pointsDistributionCouleur hand.carreau
                    , pointsDistributionCouleur hand.trefle
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
    let
        extended =
            case distribution of
                Just distribution ->
                    honneur + longueur + distribution

                Nothing ->
                    honneur + longueur
    in
        ((toString honneur
            ++ "H"
            ++ if extended == honneur then
                ("("
                    ++ (case distribution of
                            Just _ ->
                                "LD"

                            Nothing ->
                                "L"
                       )
                    ++ ")"
                )
               else
                ""
         )
            :: if extended == honneur then
                []
               else
                [ case distribution of
                    Just distribution ->
                        toString (honneur + longueur + distribution) ++ "HLD"

                    Nothing ->
                        toString (honneur + longueur) ++ "HL"
                ]
        )
            |> String.join " "


showRepartition : Hand -> String
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


nextEnchere : Hand -> Maybe Hand -> Enchere
nextEnchere hand otherHand =
    let
        points =
            pointsHand hand otherHand
    in
        if points.honneur >= 12 then
            if List.length hand.pique >= 5 then
                Enchere ( 1, EncherePique )
            else if List.length hand.coeur >= 5 then
                Enchere ( 1, EnchereCoeur )
            else if points.honneur >= 15 && points.honneur <= 17 && handType hand == Reguliere then
                Enchere ( 1, SansAtout )
            else
                EnchereTodo
        else
            EnchereTodo


viewEnchere : Enchere -> Html msg
viewEnchere enchere =
    case enchere of
        Enchere ( niveau, couleur ) ->
            span []
                [ text (toString niveau)
                , viewCouleurEnchere couleur
                ]

        Passe ->
            text "Passe"

        EnchereTodo ->
            text "TODO (pas encore programmé)"


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


couleurEnchereStyle : CouleurEnchere -> List ( String, String )
couleurEnchereStyle couleur =
    let
        red =
            [ ( "color", "red" ) ]
    in
        case couleur of
            EnchereCoeur ->
                red

            EnchereCarreau ->
                red

            _ ->
                []


viewCouleurEnchere : CouleurEnchere -> Html msg
viewCouleurEnchere couleur =
    span
        [ style (couleurEnchereStyle couleur) ]
        [ text (showCouleurEnchere couleur) ]



-- HAND TYPES
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
            { nord = handFromCartes nord
            , sud = handFromCartes sud
            , est = handFromCartes est
            , ouest = handFromCartes ouest
            }



-- MODEL


type alias Model =
    { donne : Donne
    , hideOtherHands : Bool
    , generateHandType : HandType
    , generateFittedWithNord : Maybe Bool
    }



-- INIT


init : ( Model, Cmd msg )
init =
    ( { donne = donne1
      , hideOtherHands = False
      , generateHandType = Quelconque
      , generateFittedWithNord = Nothing
      }
    , Cmd.none
    )



-- MSG


type Msg
    = GenerateDonne
    | SetDonneFromGenerator Donne
    | HideOtherHands Bool
    | GenerateHandType HandType
    | GenerateFittedWithNord (Maybe Bool)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateDonne ->
            ( model, Random.generate SetDonneFromGenerator donneGenerator )

        SetDonneFromGenerator donne ->
            if
                (case model.generateHandType of
                    Quelconque ->
                        True

                    other ->
                        handType donne.sud == other
                )
                    && (case model.generateFittedWithNord of
                            Just expectedValue ->
                                isFit donne.sud donne.nord == expectedValue

                            Nothing ->
                                True
                       )
            then
                ( { model | donne = donne }, Cmd.none )
            else
                ( model, Random.generate SetDonneFromGenerator donneGenerator )

        HideOtherHands hideOtherHands ->
            ( { model | hideOtherHands = hideOtherHands }, Cmd.none )

        GenerateHandType generateHandType ->
            ( { model | generateHandType = generateHandType }, Cmd.none )

        GenerateFittedWithNord generateFittedWithNord ->
            ( { model | generateFittedWithNord = generateFittedWithNord }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { donne, hideOtherHands } =
    div
        [ style
            [ ( "margin", "1em" )
            ]
        ]
        [ h1 [] [ text "Jeu de Bridge" ]
        , div [ class "row" ]
            [ div [ class "col-xs-12 col-sm-5" ]
                [ viewDonne donne hideOtherHands ]
            , div [ class "col-xs-12 col-sm-7" ]
                [ ul []
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
                    , li []
                        [ text "Prochaine enchère pour Sud : "
                        , viewEnchere (nextEnchere donne.sud Nothing)
                        ]
                    ]
                , div []
                    [ fieldset []
                        [ legend [] [ text "Contraintes pour Sud" ]
                        , choices
                            "Main"
                            "hand"
                            GenerateHandType
                            (List.map
                                (\x -> ( showHandType x, x ))
                                [ Reguliere, Unicolore, Bicolore, Quelconque ]
                            )
                            3
                        , choices
                            "Fitté avec Nord"
                            "fitWithNord"
                            GenerateFittedWithNord
                            [ ( "oui", Just True )
                            , ( "non", Just False )
                            , ( "peu importe", Nothing )
                            ]
                            2
                        , br [] []
                        , button [ onClick GenerateDonne ] [ text "Générer" ]
                        ]
                    , br [] []
                    , label []
                        [ input [ type' "checkbox", onCheck HideOtherHands, checked hideOtherHands ] []
                        , text "Autres mains cachées"
                        ]
                    ]
                ]
            ]
        , footer [ style [ ( "margin-top", "5em" ) ] ]
            [ hr [] []
            , p []
                [ text "Logiciel libre programmé en langage Elm. "
                , a [ href "https://github.com/cbenz/elm-bridge-game" ] [ text "Code source" ]
                ]
            ]
        ]


viewDonne : Donne -> Bool -> Html msg
viewDonne { nord, sud, est, ouest } hideOtherHands =
    let
        handChildren hand otherHand =
            [ div
                [ style [ ( "white-space", "nowrap" ) ] ]
                [ viewHand hand ]
            , ulWithoutBullets [ style [ ( "margin-top", "0.5em" ) ] ]
                [ li [] [ text (showPoints (pointsHand hand otherHand)) ]
                , li []
                    [ text
                        (let
                            handType' =
                                handType hand
                         in
                            showRepartition hand
                                ++ if handType' == Quelconque then
                                    ""
                                   else
                                    (" ("
                                        ++ (showHandType handType')
                                        ++ ")"
                                    )
                        )
                    ]
                ]
            ]

        hiddenHand =
            text "Main cachée"
    in
        squareDiv
            [ class "row"
            , style [ ( "background-color", "lightgray" ), ( "padding", "1em" ) ]
            ]
            [ div
                [ class "col-xs-4 row"
                , style [ ( "margin", "auto 0" ) ]
                ]
                (if hideOtherHands then
                    [ hiddenHand ]
                 else
                    [ div [ class "col-xs" ] []
                    , div [] (handChildren ouest (Just est))
                    , div [ class "col-xs" ] []
                    ]
                )
            , div
                [ class "col-xs-4"
                , style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    , ( "justify-content", "space-between" )
                    ]
                ]
                [ div []
                    (if hideOtherHands then
                        [ hiddenHand ]
                     else
                        handChildren nord (Just sud)
                    )
                , div []
                    (handChildren sud
                        (if hideOtherHands then
                            Nothing
                         else
                            Just nord
                        )
                    )
                ]
            , div
                [ class "col-xs-4 row"
                , style [ ( "margin", "auto 0" ) ]
                ]
                (if hideOtherHands then
                    [ hiddenHand ]
                 else
                    [ div [ class "col-xs" ] []
                    , div [] (handChildren est (Just ouest))
                    , div [ class "col-xs" ] []
                    ]
                )
            ]


viewHand : Hand -> Html msg
viewHand hand =
    let
        viewHandCouleur couleur cartes =
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
        ulWithoutBullets []
            [ li [] (viewHandCouleur Pique hand.pique)
            , li [] (viewHandCouleur Coeur hand.coeur)
            , li [] (viewHandCouleur Carreau hand.carreau)
            , li [] (viewHandCouleur Trefle hand.trefle)
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
                                            , viewCouleurCarte couleur
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


viewCouleurCarte : CouleurCarte -> Html msg
viewCouleurCarte couleur =
    span
        [ style (couleurCarteStyle couleur) ]
        [ text (showCouleurCarte couleur) ]


ulWithoutBullets : List (Html.Attribute msg) -> List (Html msg) -> Html msg
ulWithoutBullets attributes children =
    ul
        ((style
            [ ( "list-style-type", "none" )
            , ( "margin", "0" )
            , ( "padding", "0" )
            ]
         )
            :: attributes
        )
        children


choices : String -> String -> (a -> msg) -> List ( String, a ) -> Int -> Html msg
choices title nameAttribute tagger labelAndValueList initialCheckedIndex =
    let
        labelWithSpaceStyle =
            [ ( "margin-right", "1em" ) ]
    in
        div []
            ((text (title ++ " : "))
                :: (List.indexedMap
                        (\index ( labelText, value ) ->
                            label [ style labelWithSpaceStyle ]
                                [ input
                                    [ type' "radio"
                                    , name nameAttribute
                                    , onCheck (always (tagger value))
                                    , checked (index == initialCheckedIndex)
                                    ]
                                    []
                                , text labelText
                                ]
                        )
                        labelAndValueList
                   )
            )


squareDiv : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
squareDiv attributes children =
    div
        [ style
            [ ( "padding-bottom", "100%" )
            , ( "position", "relative" )
            ]
        ]
        [ div
            ([ style
                [ ( "position", "absolute" )
                , ( "top", "0" )
                , ( "right", "0" )
                , ( "bottom", "0" )
                , ( "left", "0" )
                ]
             ]
                ++ attributes
            )
            children
        ]



-- SAMPLES


donne1 : Donne
donne1 =
    { sud =
        handFromCartes
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
        handFromCartes
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
        handFromCartes
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
        handFromCartes
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
