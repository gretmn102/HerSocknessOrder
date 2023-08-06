module Index
open Elmish
open FsharpMyExtension.ResultExt
open IfEngine.Types
open IfEngine.Engine
open IfEngine.Fable
open IfEngine.Fable.WebEngine

type State =
    {
        IfEngineState: WebEngine<Scenario.LabelName, Scenario.CustomStatement, Scenario.CustomStatementArg, Scenario.CustomStatementOutput>
    }

type Msg =
    | IfEngineMsg of InputMsg<Scenario.CustomStatementOutput>

let init () =
    let st =
        {
            IfEngineState =
                WebEngine.create
                    CustomStatementHandler.empty
                    Scenario.scenario
                    (IfEngine.State.init Scenario.beginLoc VarsContainer.empty)
                |> Result.get
        }
    st, Cmd.none

let update (msg: Msg) (state: State) =
    let updateGame msg =
        let gameState =
            WebEngine.update msg state.IfEngineState
            |> Result.get

        { state with
            IfEngineState = gameState
        }

    match msg with
    | IfEngineMsg msg ->
        updateGame msg, Cmd.none

let view (state: State) (dispatch: Msg -> unit) =
    Index.view
        (fun (customStatement: Scenario.CustomStatement) ->
            failwithf "customStatement not implemented"
        )
        state.IfEngineState
        (IfEngineMsg >> dispatch)
