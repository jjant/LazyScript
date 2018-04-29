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
    { lazyScriptCode = "const a = [1, ...a];"
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
                    model ! []

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
    ]


bgColor =
    "#F5F5F5"


textareaColor =
    "#F9F3F5"


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
    , "max-width" => "1050px"
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
    , "max-width" => "450px"
    , "background" => "#B7B6C2"
    , "padding" => "20px"
    , "border-radius" => "5px"
    ]


textareaStyle =
    [ "height" => "325px"
    , "min-height" => "325px"
    , "max-height" => "325px"
    , "width" => "100%"
    , "background" => textareaColor
    , "border-radius" => "4px"
    , "font-size" => "20px"
    , "color" => "#333"
    , "padding" => "10px"
    , "margin-bottom" => "10px"
    ]


content : Model -> Html Msg
content model =
    div [ style contentStyle ]
        [ div [ style mainStyle ]
            [ label [ style labelStyle ]
                [ textarea
                    [ style textareaStyle
                    , onInput NewLazyScriptCode
                    , value model.lazyScriptCode
                    ]
                    []
                , span [] [ text "LazyScript code" ]
                ]
            , label [ style labelStyle ]
                [ textarea
                    [ style (textareaStyle ++ [ "outline" => "none" ])
                    , readonly True
                    , value model.compiledCode
                    ]
                    []
                , span [] [ text "Compiled code" ]
                ]
            ]
        , compileButton
        , languageDescription
        ]


compileButtonStyle =
    [ "width" => "100px"
    , "height" => "40px"
    , "display" => "block"
    , "border-radius" => "2px"
    , "font-size" => "16px"
    , "margin" => "32px auto 16px"
    , "outline" => "none"
    ]


compileButton : Html Msg
compileButton =
    button
        [ style compileButtonStyle
        , onClick Compile
        ]
        [ text "Compile" ]


languageDescription : Html msg
languageDescription =
    div []
        [ text "LazyScript is a dynamic, lazily evaluated programming language featuring pure IO." ]


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
    ]


footer : Html msg
footer =
    div [ style footerStyles ]
        (footerLinks
            |> List.map (\( url, linkText ) -> a [ href url, style footerLinkStyle ] [ text linkText ])
            |> List.intersperse (text " | ")
        )


footerLinks : List ( String, String )
footerLinks =
    [ ( "https://github.com/jjant/lazyscript", "Repo" )
    , ( "https://github.com/jjant", "GitHub" )
    ]
