module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Task
import Http


type alias Model =
    { lazyScriptCode : String
    , compiledCode : String
    }


type Msg
    = NewLazyScriptCode String
    | NewCompiledCode (Result Http.Error String)
    | Compile
    | NoOp


init : ( Model, Cmd Msg )
init =
    { lazyScriptCode =
        """const a = [1, ...a];

const constantlyGreet = a.map(_ => IO.putStrLn("Hi!"));

const main = constantlyGreet;
"""
    , compiledCode = ""
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewLazyScriptCode code ->
            { model | lazyScriptCode = code } ! []

        NewCompiledCode response ->
            case response of
                Err e ->
                    -- TODO: Change later.
                    { model | compiledCode = "Something wrong happened while compiling :(." } ! []

                Ok code ->
                    { model | compiledCode = code } ! []

        Compile ->
            model ! [ compileCodeCmd model.lazyScriptCode ]

        NoOp ->
            model ! []



-- compileCodeCmd : Cmd Msg
-- compileCodeCmd =
--     -- TODO: Make the request...
--     NewCompiledCode "const a = () => [1, () => a()];"
--         |> Task.succeed
--         |> Task.perform identity


compileCodeCmd : String -> Cmd Msg
compileCodeCmd lazyScriptCode =
    "http://localhost:8080/compile?code="
        ++ lazyScriptCode
        |> Http.getString
        |> Http.send NewCompiledCode


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


topLevelStyle =
    [ "background-color" => bgColor
    , "width" => "100%"
    , "height" => "100vh"
    , "margin" => "0 auto"
    , "display" => "flex"
    , "flex-direction" => "column"
    ]


bgColor =
    "#f6f4f4"


main =
    program { init = init, view = view, update = update, subscriptions = always Sub.none }


view model =
    div
        [ style topLevelStyle
        ]
        [ header
        , content model
        , footer
        ]


headerStyle =
    [ "height" => "135px"
    , "background" => "#333"
    , "display" => "flex"
    , "justify-content" => "center"
    , "color" => "#dedede"
    , "align-items" => "center"
    , "flex-direction" => "column"
    ]


header : Html msg
header =
    div [ style headerStyle ]
        [ span [ style [ "font-size" => "50px", "font-family" => "'Montserrat'" ] ]
            [ text "LazyScript" ]
        , span [ style [ "margin-top" => "15px" ] ]
            [ text "A to-JS compiled lazy programming language." ]
        ]


contentStyle =
    [ "margin" => "30px auto 0"
    , "max-width" => "1150px"
    , "width" => "100%"
    ]


mainStyle =
    [ "display" => "flex"
    , "align-items" => "center"
    , "justify-content" => "space-between"
    , "margin" => "0 auto"
    ]


labelStyle =
    [ "display" => "flex"
    , "flex-direction" => "column"
    , "width" => "100%"
    , "max-width" => "550px"
    , "border-radius" => "5px"
    ]


textareaStyle =
    [ "height" => "325px"
    , "min-height" => "325px"
    , "max-height" => "325px"
    , "width" => "calc(100% - 26px)"
    , "min-width" => "calc(100% - 26px)"
    , "max-width" => "calc(100% - 26px)"
    , "resize" => "none"
    , "border-radius" => "4px"
    , "font-size" => "14px"
    , "color" => "#333"
    , "margin-bottom" => "10px"
    , "cursor" => "text"
    , "border" => "1px solid #db4d3f"
    , "padding" => "12px"
    , "outline" => "none"
    ]


content : Model -> Html Msg
content model =
    div [ style [ "display" => "flex", "flex-direction" => "column", "flex" => "1" ] ]
        [ div [ style contentStyle ]
            [ div [ style mainStyle ]
                [ label [ style labelStyle ]
                    [ code
                        [ style textareaStyle
                        , onInput NewLazyScriptCode
                        , value model.lazyScriptCode
                        , contenteditable True
                        ]
                        (model.lazyScriptCode
                            |> String.lines
                            |> List.map
                                (\line ->
                                    if String.isEmpty line then
                                        br [] []
                                    else
                                        span [ style [ "display" => "block" ] ] [ text line ]
                                )
                        )
                    ]
                , label [ style labelStyle ]
                    [ textarea
                        [ style (textareaStyle ++ [ "outline" => "none" ])
                        , readonly True
                        , value model.compiledCode
                        ]
                        []
                    ]
                ]
            , div [ style [ "display" => "flex", "justify-content" => "center" ] ]
                ([ compileButton, docsButton ]
                    |> List.map buttonContainer
                )
            ]
        , languageDescription
        ]


buttonContainerStyle =
    [ "margin" => "32px 8px 16px"
    , "width" => "130px"
    , "display" => "flex"
    ]


buttonContainer : Html msg -> Html msg
buttonContainer el =
    div [ style buttonContainerStyle ] [ el ]


compileButtonStyle =
    [ "display" => "block"
    , "cursor" => "pointer"
    , "border" => "1px solid #db4d3f"
    , "border-radius" => "3px"
    , "font-size" => "14px"
    , "background" => "#db4d3f"
    , "color" => "#fff"
    , "outline" => "none"
    , "padding" => "10px 16px"
    , "margin-left" => "auto"
    ]


compileButton : Html Msg
compileButton =
    a
        [ style compileButtonStyle
        , onClick Compile
        ]
        [ text "COMPILE" ]


docsButtonStyle =
    [ "display" => "block"
    , "cursor" => "pointer"
    , "border" => "1px solid #db4d3f"
    , "border-radius" => "3px"
    , "font-size" => "14px"
    , "background" => "transparent"
    , "color" => "#db4d3f"
    , "outline" => "none"
    , "padding" => "10px 16px"
    , "margin-right" => "auto"
    ]


docsButton : Html msg
docsButton =
    a [ style docsButtonStyle ] [ text "QUICK START" ]


languageDescription : Html msg
languageDescription =
    div [ style [ "background" => "white", "flex" => "1", "padding-top" => "36px" ] ]
        [ div [ style [ "display" => "flex", "justify-content" => "space-around" ] ] (List.map (uncurry description) descriptionItems)
        ]


descriptionItems =
    [ ( "Flexible & Fun", "Make websites, animations, games, servers, cli tools, and more! Take a look at these examples to get inspired." )
    , ( "Pure and lazy", "LazyScript is a dynamic, lazily evaluated programming language featuring pure IO." )
    , ( "Easy JavaScript interop", "Use packages from NPM/Yarn with minimum hassle, or even drop in a snippet of raw JavaScript while you're learning!" )
    ]


description : String -> String -> Html msg
description title body =
    div
        [ style
            [ "display" => "flex"
            , "width" => "350px"
            , "flex-direction" => "column"
            , "text-align" => "center"
            ]
        ]
        [ span
            [ style
                [ "color" => "rgb(219, 77, 63)"
                , "font-size" => "22px"
                , "margin-bottom" => "12px"
                ]
            ]
            [ text title ]
        , span
            [ style
                [ "font-size" => "18px"
                , "color" => "rgb(57, 57, 57)"
                , "line-height" => "30px"
                ]
            ]
            [ text body ]
        ]


footerStyles =
    [ "height" => "30px"
    , "background" => "#333"
    , "position" => "absolute"
    , "bottom" => "0"
    , "width" => "100%"
    , "display" => "flex"
    , "justify-content" => "center"
    , "align-items" => "center"
    , "color" => "#dedede"
    ]


footerLinkStyle =
    [ "color" => "#dedede"
    , "text-decoration" => "none"
    ]


footer : Html msg
footer =
    div [ style footerStyles ]
        (footerLinks
            |> List.map (\( url, linkText ) -> a [ href url, style footerLinkStyle ] [ text linkText ])
            |> List.intersperse (span [ style [ "margin" => "0 10px" ] ] [ text "|" ])
        )


footerLinks : List ( String, String )
footerLinks =
    [ ( "https://github.com/jjant/lazyscript/docs", "Docs" )
    , ( "https://github.com/jjant/lazyscript", "Repo" )
    , ( "https://github.com/jjant", "GitHub" )
    ]
