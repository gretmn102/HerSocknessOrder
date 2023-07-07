module Scenario
open Feliz

open IfEngine.Utils
open IfEngine.Types
open IfEngine.Fable.Utils
open IfEngine.Interpreter
open IfEngine

type LabelName =
    | MainMenu
    | Prelude
    | MainSockdomStreet
    | Еpilogue

type CustomStatementArg = unit
type CustomStatement = unit
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CustomStatement =
    let apply state stack (arg: CustomStatementArg) customStatement =
        failwithf "not implemented yet"

    let handle subIndex customStatement =
        failwithf "not implemented yet"

let beginLoc = MainMenu

let scenario, vars =
    let vars = Map.empty
    let getCounter, updateCounter, vars = createNumVar "counter" 0 vars

    [
        label MainMenu [
            menu [
                Html.h1 [
                    prop.style [
                        style.justifyContent.center
                        style.display.flex
                    ]

                    prop.text "Поручение Ее Носочества"
                ]
                Html.div [
                    Html.text "Однажды Ее Носочество Surprise велит верноподданному Агенту собрать всех жителей Носочного царства в главном зале, чтобы объявить важную государственную весть. Тот принимается всех собирать, однако жители не шибко спешат выполнять указание даже именем Ее Носочества и находят тысячу и одну причину, чтобы не идти. Сможет ли Агент всех собрать? Это и предстоит выяснить в этом коротеньком интерактивном рассказе "
                    Html.img [
                        prop.src "https://cdn.discordapp.com/emojis/927633357737713704.webp?size=22&quality=lossless"
                    ]
                ]
                Html.div [
                    prop.style [
                        style.justifyContent.flexEnd
                        style.display.flex
                    ]
                    prop.text "v0.1.0"
                ]
            ] [
                choice "Начать" [ jump Prelude ]
                choice "Авторов!" [
                    menu [
                        Html.text "Все понемногу. И вообще, заходите на наш "
                        Html.a [
                            prop.href "https://discord.gg/YcN2AET65d"
                            prop.target "_blank"
                            prop.children [
                                Html.text "Discord сервер \"Веселый носок\""
                            ]
                        ]
                        Html.text ", у нас весело!"
                    ] [
                        choice "Назад" [ jump MainMenu ]
                    ]
                ]
            ]
        ]

        label Prelude [
            say "TODO: здесь раскрывается логлайн, в котором Ее Носочество дает задание Агенту."
            jump MainSockdomStreet
        ]

        label MainSockdomStreet [
            say "TODO: Агент выходит из замка на главную площадь Носочного царства со множеством разветлений. Ему предстоит посетить множество мест и встретиться с жителями Носочного царства, которых придется уговорить, чтобы выполнить поручение."
            jump Еpilogue
        ]

        label Еpilogue [
            say "TODO: Агент собрал всех в носочном зале, и Ее Носочество объявляет важную весть."
        ]
    ]
    |> fun scenario ->
        scenario, vars

let gameState, update =
    let scenario =
        scenario
        |> List.map (fun (labelName, body) -> labelName, (labelName, body))
        |> Map.ofList
        : Scenario<_, _, _>

    let init: State<Text, LabelName, CustomStatement> =
        {
            LabelState =
                LabelState.create
                    beginLoc
                    (Stack.createSimpleStatement 0)
            Vars = vars
        }

    let interp gameState =
        gameState
        |> interp (CustomStatement.apply, CustomStatement.handle) scenario
        |> function
            | Ok x -> x
            | Error err ->
                failwithf "%s" err

    let gameState =
        {
            Game.Game = interp init
            Game.GameState = init
            Game.SavedGameState = init
        }

    let update msg state =
        Game.update interp init msg state

    gameState, update
