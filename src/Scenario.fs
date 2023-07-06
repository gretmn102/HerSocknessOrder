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

                    prop.text "Название игры" // TODO
                ]
                Html.div [ Html.text "Описание" ] // TODO
                Html.div [
                    prop.style [
                        style.justifyContent.flexEnd
                        style.display.flex
                    ]
                    prop.text "v1.00"
                ]
            ] [
                choice "Начать" [ jump Prelude ]
                choice "Авторов!" [
                    menu [
                        Html.text "Автор" // TODO
                    ] [
                        choice "Назад" [ jump MainMenu ]
                    ]
                ]
            ]
        ]
        label Prelude [
            // TODO
            jump Еpilogue
        ]

        label Еpilogue [
            // TODO
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
