module Scenario
open Feliz

open IfEngine.Utils
open IfEngine.Types
open IfEngine.Fable.Utils
open IfEngine.Interpreter
open IfEngine

[<RequireQualifiedAccess>]
type LabelName =
    | MainMenu
    | Prelude
    | MainSockdomStreet
    | ThroneRoom
    | Gates
    | NoodleFactory
    | Еpilogue

// TODO: refact: move to IfEngine
module IfEngine =
    module Types =
        type VarsContainer = Map<string, Var>

        type IVar<'Value> =
            abstract Get: VarsContainer -> 'Value
            abstract Set: 'Value -> VarsContainer -> VarsContainer
            abstract Update: ('Value -> 'Value) -> VarsContainer -> VarsContainer
            abstract GetVarName: unit -> string

        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        [<RequireQualifiedAccess>]
        module Var =
            let get (var: #IVar<'Value>) varContainer =
                var.Get varContainer

            let set (var: #IVar<'Value>) newValue varContainer =
                var.Set newValue varContainer

            let update (var: #IVar<'Value>) mapping varContainer =
                var.Update mapping varContainer

            let equals (var: #IVar<'Value>) otherValue varContainer =
                (get var varContainer) = otherValue

            let getVarName (var: #IVar<'Value>) =
                var.GetVarName()

        type NumVar(varName: string) =
            interface IVar<int> with
                member __.GetVarName () =
                    varName

                member __.Get varContainer =
                    match Map.tryFind varName varContainer with
                    | Some(Var.Num x) -> x
                    | _ ->
                        printfn "expected Some(Num x) but %s" varName
                        0

                member __.Set newValue varContainer =
                    Map.add varName (Var.Num newValue) varContainer

                member this.Update mapping varContainer =
                    Var.set
                        this
                        (mapping (Var.get this varContainer))
                        varContainer

        type StringVar(varName: string) =
            interface IVar<string> with
                member __.GetVarName () =
                    varName

                member __.Get varContainer =
                    match Map.tryFind varName varContainer with
                    | Some(Var.String x) -> x
                    | _ ->
                        printfn "expected Some(String x) but %s" varName
                        ""

                member __.Set newValue varContainer =
                    Map.add varName (Var.String newValue) varContainer

                member this.Update mapping varContainer =
                    Var.set
                        this
                        (mapping (Var.get this varContainer))
                        varContainer

        type BoolVar(varName: string) =
            interface IVar<bool> with
                member __.GetVarName () =
                    varName

                member __.Get varContainer =
                    match Map.tryFind varName varContainer with
                    | Some(Var.Bool x) -> x
                    | _ ->
                        printfn "expected Some(Bool x) but %s" varName
                        false

                member __.Set newValue varContainer =
                    Map.add varName (Var.Bool newValue) varContainer

                member this.Update mapping varContainer =
                    Var.set
                        this
                        (mapping (Var.get this varContainer))
                        varContainer

        type EnumVar<'T when 'T: enum<int>>(varName: string)  =
            interface IVar<'T> with
                member __.GetVarName() =
                    varName

                member __.Get varContainer =
                    match Map.tryFind varName varContainer with
                    | Some(Var.Num x) -> (enum x): 'T
                    | _ ->
                        printfn "expected Some(Bool x) but %s" varName
                        enum 0

                member __.Set (newValue) varContainer =
                    let newValue = int (unbox newValue)
                    Map.add varName (Var.Num newValue) varContainer

                member this.Update mapping varContainer =
                    Var.set
                        this
                        (mapping (Var.get this varContainer))
                        varContainer

        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        [<RequireQualifiedAccess>]
        module VarsContainer =
            let createNum varName = new NumVar(varName)

            let createEnum varName = new EnumVar<'T>(varName)

            let createString varName = new StringVar(varName)

    module Utils =
        open Types

        let switch (thenBodies: ((VarsContainer -> bool) * Block<'a,'b,'c>) list) (elseBody: Block<'a,'b,'c>) : Block<'a, 'b, 'c> =
            List.foldBack
                (fun ((pred: VarsContainer -> bool), (thenBody: Block<'a,'b,'c>)) elseBody ->
                    [If(pred, thenBody, elseBody)]
                )
                thenBodies
                elseBody

        let (:=) (var: #IVar<_>) newValue =
            ChangeVars (Var.set var newValue)

        let (==) (var: #IVar<_>) newValue varsContainer =
            Var.equals var newValue varsContainer

// TODO: refact: move to IfEngine.Fable
module IFEngine =
    module Fable =
        module Utils =
            let sayImg (text: string) imgSrc =
                Say [
                    Html.p [
                        prop.text text
                    ]

                    Html.img [
                        prop.src imgSrc
                    ]
                ]

open IfEngine.Types
open IfEngine.Utils
open IFEngine.Fable.Utils

type CustomStatementArg = unit
type CustomStatement = unit
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CustomStatement =
    let apply state stack (arg: CustomStatementArg) customStatement =
        failwithf "not implemented yet"

    let handle subIndex customStatement =
        failwithf "not implemented yet"

let beginLoc = LabelName.MainMenu

module GlobalVars =
    let noodleCount = VarsContainer.createNum "noodleCount"

    type NoodlesEverywhere =
        | HasNotStartedYet = 0
        | Started = 1
        | Finished = 2

    let noodlesEverywhere = VarsContainer.createEnum "noodlesEverywhere"

    type TemporaryGatekeeper =
        | HasNotStartedYet = 0
        | Started = 1
        | Finished = 2

    let temporaryGatekeeper = VarsContainer.createEnum "temporaryGatekeeper"

open GlobalVars

let scenario =
    [
        label LabelName.MainMenu [
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
                choice "Начать" [ jump LabelName.Prelude ]
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
                        choice "Назад" [ jump LabelName.MainMenu ]
                    ]
                ]
            ]
        ]

        label LabelName.Prelude [
            say "TODO: здесь раскрывается логлайн, в котором Ее Носочество дает задание Агенту."
            jump LabelName.MainSockdomStreet
        ]

        label LabelName.MainSockdomStreet [
            menu [
                Html.text "TODO: Агент выходит из замка на главную площадь Носочного царства со множеством разветлений. Ему предстоит посетить множество мест и встретиться с жителями Носочного царства, которых придется уговорить, чтобы выполнить поручение."
            ] [
                "Завод по производству лапшички", [
                    jump LabelName.NoodleFactory
                ]

                "Главные врата", [
                    jump LabelName.Gates
                ]

                "Тронный зал", [
                    jump LabelName.ThroneRoom
                ]
            ]
        ]

        label LabelName.Gates [
            yield! switch
                [
                    (temporaryGatekeeper == TemporaryGatekeeper.HasNotStartedYet), [
                        say "Агент подходит к вратам — к единственному месту, через которое можно попасть внутрь Носочного королевства и выйти из оного."

                        sayImg
                            "На страже у ворот стоит Enurezo."
                            "https://cdn.discordapp.com/emojis/952317602594693171.webp?size=240&quality=lossless"

                        say "— Привет, Enu... — не успевает начать Агент, как во врата уже кто-то ломится."
                        say "— Есть кто дома?! — орут с той стороны."
                        say "— Ты вовремя, Агент. — невозмутимо говорит Enurezo и кричит в ответ: — 18 есть?!"

                        sayImg
                            "С той стороны льется негодование, ругань и отборная адекватность."
                            "https://trello.com/1/cards/62da598f6a88c50b13a91d6b/attachments/62fd2abe66de6e3368657746/download/%D0%B8%D0%B7%D0%BE%D0%B1%D1%80%D0%B0%D0%B6%D0%B5%D0%BD%D0%B8%D0%B5.png"

                        say "— Там это, Ее Носочество... — переходит Агент к делу, но Enurezo уже опрашивает следующего новичка и совершенно не обращает на него внимания."

                        say "\"Вот бы состряпать временного стража для врат, чтобы заменить как-то Enurezo\", — напряженно думает Агент."

                        say "*Начался квест \"Временный страж!\"*"
                        temporaryGatekeeper := TemporaryGatekeeper.Started

                        menu [
                        ] [
                            "Вернуться на главную площадь", [
                                jump LabelName.MainSockdomStreet
                            ]
                        ]
                    ]

                    (temporaryGatekeeper == TemporaryGatekeeper.Started), [
                        if' (fun vars -> Var.get noodleCount vars > 0) [
                            menu [
                                Html.text "Enurezo неустанно стоит на страже врат и, похоже, не обращает никакого внимания на Агента. Вот бы состряпать временного стража для врат."
                            ] [
                                "Гора «Лапшевесников»", [
                                    say "\"А ведь из «Лапшевесников» можно состряпать, скажем, дракона, который будет временно отпугивать посетителей. Чем не страж?\", — подумал Агент."

                                    menu [
                                        Html.text "Сделать дракона?"
                                    ] [
                                        "Ага!", [
                                            say "Агент собирает дракона из «Лапшевесников», а Enuroze придирчиво смотрит на это действо и бубнит."
                                            say "Пару штрихов, и дракон готов."
                                            noodleCount := 0

                                            says [
                                                "— Вот наш временный страж! — довольно произносит Агент."
                                                "— Что-то я не дове... — не успевает договорить Enurezo, как вдруг дракон начинает орать: — Стой, кто идет?!"
                                                "И как раз вовремя, потому что на подходе у врат объявился новенький."
                                                "— А это так надо, что новичок сразу убежал? — спросил Enurezo."
                                                "— Да-да, всё по последнему слову техники. Пошли в тронный зал."
                                                "Enurezo нехотя соглашается."
                                            ]

                                            temporaryGatekeeper := TemporaryGatekeeper.Finished

                                            jump LabelName.Gates
                                        ]

                                        "Ой, не", [
                                            say "Ну, и правильно."
                                            jump LabelName.Gates
                                        ]
                                    ]
                                ]

                                "Втюхать Enurezo «Лапшевесник»", [
                                    say "— Спасибо, я не голоден, — отказался Enurezo."
                                    jump LabelName.Gates
                                ]

                                "Вернуться на главную площадь", [
                                    jump LabelName.MainSockdomStreet
                                ]
                            ]
                        ] [
                            menu [
                                Html.text "Enurezo неустанно стоит на страже врат и, похоже, не обращает никакого внимания на Агента. Вот бы состряпать временного стража для врат."
                            ] [
                                "Вернуться на главную площадь", [
                                    jump LabelName.MainSockdomStreet
                                ]
                            ]
                        ]
                    ]
                ]
                [
                    menu [
                        Html.text "На вратах тихо и пусто."
                    ] [
                        "Вернуться на главную площадь", [
                            jump LabelName.MainSockdomStreet
                        ]
                    ]
                ]
        ]

        label LabelName.NoodleFactory [
            yield! switch
                [
                    (noodlesEverywhere == NoodlesEverywhere.HasNotStartedYet), [
                        say "Агент подходит к лапшичному заводу, к месту, где лапшичка становится явью."

                        // say "Сложно сказать, что именно там изготовляют, но известно точно, что любого, кто имеет неосторожность сюда показать свои ушки, начинает тяготить от лапшички."

                        sayImg
                            "— Опа, Агент! — воскликает Шедоу с особым предвкушением. Он бросает лапшевесную машину и несется к Агенту. Хорошо отлаженная машина без устали штампует свежий выпуск «Лапшевесника», от одного вида которой уже начинает тяготить ушки."
                            "https://magma.com/shared/XhuBNd8uyGMH-EWvEDNoQg"

                        say "— Я только спроси!..."
                        say "— Ну что же ты стоишь у порога?! Заходи, добрый друг, — Шэдоу подхватывает Агента за плечи и заводит внутрь. Дверь сзади закрывается и отрезает пути к отступлению."
                        say "— О-о, кто пришел! — на сей раз это уже восклицает Адалинда со свежим «Лапшевесником» в руках."
                        say "Агент машет ей в ответ."
                        say "— Ты знал, что "

                        say "*Начинается квест \"Лапшичка, лапшичка повсюду!\"*"
                        noodlesEverywhere := NoodlesEverywhere.Started
                        noodleCount := 1000
                        menu [
                        ] [
                            "На главную площадь", [
                                jump LabelName.MainSockdomStreet
                            ]
                        ]
                    ]

                    (noodlesEverywhere == NoodlesEverywhere.Started), [
                        if' (fun vars -> Var.get noodleCount vars > 0) [
                            say "— Ну что, раздал «Лапшевесники»? — спрашивает СПб, будто не видит, как лапшичка отягощает ушки Агента, плечи и всё, что только можно отягощать."
                            say "— Нет, — бурчит Агент."
                            say "— Тогда нас ждет еще много дел! — восклицает СПб и несется к Адалинде, чтобы узнать ее мнение о качестве выпускаемой лапшички."
                            menu [
                            ] [
                                "На главную площадь", [
                                    jump LabelName.MainSockdomStreet
                                ]
                            ]
                        ] [
                            say "— Все «Лапшевесники» розданы! — отчитывается Агент."
                            say "— Прекрасная работа, — говорит СПб. — Что ж, пожалуй, можно отправиться к Surprise. Кошка, бросай лапшичку, мы отправляемся в путь!"
                            noodlesEverywhere := NoodlesEverywhere.Finished
                            menu [
                            ] [
                                "На главную площадь", [
                                    jump LabelName.MainSockdomStreet
                                ]
                            ]
                        ]
                    ]
                ]
                [
                    menu [
                        Html.text "На заводе тихо и пусто, лишь старый «Лапшевесник» пошелестывает на ветру."
                    ] [
                        "На главную площадь", [
                            jump LabelName.MainSockdomStreet
                        ]
                    ]
                ]
        ]

        label LabelName.ThroneRoom [
            if' (fun vars ->
                Var.equals noodlesEverywhere NoodlesEverywhere.Finished vars
                && Var.equals temporaryGatekeeper TemporaryGatekeeper.Finished vars
            ) [
                jump LabelName.Еpilogue
            ] [
                menu [
                    Html.text "Похоже, не все еще собрались. Нужно поднапрячься."
                ] [
                    "На главную площадь", [
                        jump LabelName.MainSockdomStreet
                    ]
                ]
            ]
        ]

        label LabelName.Еpilogue [
            say "TODO: Агент собрал всех в носочном зале, и Ее Носочество объявляет важную весть."
        ]
    ]

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
            Vars = Map.empty
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
