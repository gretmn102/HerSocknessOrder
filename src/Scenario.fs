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

// TODO: refact: move VarsContainer to IfEngine
type VarsContainer = Map<string, Var>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module VarsContainer =
    let createNum varName initValue (vars: VarsContainer) : VarsContainer =
        Map.add varName (Num initValue) vars

    let getNum varName (vars: VarsContainer) =
        match vars.[varName] with
        | Num x -> x
        | _ -> failwithf "expected Num _ but %s" varName

    let setNum varName value (vars: VarsContainer) : VarsContainer =
        Map.add varName (Num value) vars

    let inline createEnum varName (initValue: ^T when ^T : enum<int32>) (vars: VarsContainer) : VarsContainer =
        Map.add varName (Num (int32 initValue)) vars

    let inline getEnum varName (vars: VarsContainer) : ^T when ^T : enum<int32> =
        match vars.[varName] with
        | Num x -> (enum x): ^T
        | _ -> failwithf "expected Num _ but %s" varName

    let inline setEnum varName (value: ^T when ^T : enum<int32>) (vars: VarsContainer) : VarsContainer =
        Map.add varName (Num (int32 value)) vars

    let createString varName initValue (vars: VarsContainer) : VarsContainer =
        Map.add varName (String initValue) vars

    let getString varName (vars: VarsContainer) =
        match vars.[varName] with
        | String x -> x
        | _ -> failwithf "expected String _ but %s" varName

    let setString varName value (vars: VarsContainer) : VarsContainer =
        Map.add varName (String value) vars

    let createBool varName initValue (vars: VarsContainer) : VarsContainer =
        Map.add varName (Bool initValue) vars

    let getBool varName (vars: VarsContainer) =
        match vars.[varName] with
        | Bool x -> x
        | _ -> failwithf "expected Bool _ but %s" varName

    let setBool varName value (vars: VarsContainer) : VarsContainer =
        Map.add varName (Bool value) vars

// TODO: refact: move Helpers to IFEngine
module Helpers =
    let switch (thenBodies: ((VarsContainer -> bool) * Block<'a,'b,'c>) list) (elseBody: Block<'a,'b,'c>) : Block<'a, 'b, 'c> =
        List.foldBack
            (fun ((pred: VarsContainer -> bool), (thenBody: Block<'a,'b,'c>)) elseBody ->
                [If(pred, thenBody, elseBody)]
            )
            thenBodies
            elseBody

    let sayWithImg (text: string) imgSrc =
        Say [
            Html.p [
                prop.text text
            ]

            Html.img [
                prop.src imgSrc
            ]
        ]

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
    [<RequireQualifiedAccess>]
    module NoodlesCount =
        let name = "noodlesCount"
        let get vars = VarsContainer.getNum name vars
        let set count vars = VarsContainer.setNum name count vars

    type NoodlesEverywhere =
        | HasNotStartedYet = 0
        | Started = 1
        | Finished = 2

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module NoodlesEverywhere =
        let name = "noodlesEverywhere"
        let get vars = VarsContainer.getEnum name vars
        let set (value: NoodlesEverywhere) vars = VarsContainer.setEnum name value vars
        let is' (value: NoodlesEverywhere) vars = get vars = value

    type TemporaryGatekeeper =
        | HasNotStartedYet = 0
        | Started = 1
        | Finished = 2

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module TemporaryGatekeeper =
        let name = "temporaryGatekeeper"
        let get vars = VarsContainer.getEnum name vars
        let set (value: TemporaryGatekeeper) vars = VarsContainer.setEnum name value vars
        let is' (value: TemporaryGatekeeper) vars = get vars = value

open GlobalVars

let scenario, vars =
    let vars = Map.empty
    let vars = VarsContainer.createEnum NoodlesEverywhere.name NoodlesEverywhere.HasNotStartedYet vars
    let vars = VarsContainer.createNum NoodlesCount.name 0 vars
    let vars = VarsContainer.createEnum TemporaryGatekeeper.name TemporaryGatekeeper.HasNotStartedYet vars

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
            yield! Helpers.switch
                [
                    (TemporaryGatekeeper.is' TemporaryGatekeeper.HasNotStartedYet), [
                        say "Агент подходит к вратам — к единственному месту, через которое можно попасть внутрь Носочного королевства и выйти из оного."

                        Helpers.sayWithImg
                            "На страже у ворот стоит Enurezo."
                            "https://cdn.discordapp.com/emojis/952317602594693171.webp?size=240&quality=lossless"

                        say "— Привет, Enu... — не успевает начать Агент, как во врата уже кто-то ломится."
                        say "— Есть кто дома?! — орут с той стороны."
                        say "— Ты вовремя, Агент. — невозмутимо говорит Enurezo и кричит в ответ: — 18 есть?!"

                        Helpers.sayWithImg
                            "С той стороны льется негодование, ругань и отборная адекватность."
                            "https://trello.com/1/cards/62da598f6a88c50b13a91d6b/attachments/62fd2abe66de6e3368657746/download/%D0%B8%D0%B7%D0%BE%D0%B1%D1%80%D0%B0%D0%B6%D0%B5%D0%BD%D0%B8%D0%B5.png"

                        say "— Там это, Ее Носочество... — переходит Агент к делу, но Enurezo уже опрашивает следующего новичка и совершенно не обращает на него внимания."

                        say "\"Вот бы состряпать временного стража для врат, чтобы заменить как-то Enurezo\", — напряженно думает Агент."

                        say "*Начался квест \"Временный страж!\"*"
                        ChangeVars (TemporaryGatekeeper.set TemporaryGatekeeper.Started)

                        menu [
                        ] [
                            "Вернуться на главную площадь", [
                                jump LabelName.MainSockdomStreet
                            ]
                        ]
                    ]

                    (TemporaryGatekeeper.is' TemporaryGatekeeper.Started), [
                        if' (fun vars -> NoodlesCount.get vars > 0) [
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
                                            ChangeVars (NoodlesCount.set 0)

                                            says [
                                                "— Вот наш временный страж! — довольно произносит Агент."
                                                "— Что-то я не дове... — не успевает договорить Enurezo, как вдруг дракон начинает орать: — Стой, кто идет?!"
                                                "И как раз вовремя, потому что на подходе у врат объявился новенький."
                                                "— А это так надо, что новичок сразу убежал? — спросил Enurezo."
                                                "— Да-да, всё по последнему слову техники. Пошли в тронный зал."
                                                "Enurezo нехотя соглашается."
                                            ]

                                            ChangeVars (TemporaryGatekeeper.set TemporaryGatekeeper.Finished)

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
            yield! Helpers.switch
                [
                    (NoodlesEverywhere.is' NoodlesEverywhere.HasNotStartedYet), [
                        say "Агент подходит к лапшичному заводу, к месту, где лапшичка становится явью."

                        // say "Сложно сказать, что именно там изготовляют, но известно точно, что любого, кто имеет неосторожность сюда показать свои ушки, начинает тяготить от лапшички."

                        Helpers.sayWithImg
                            "— Опа, Агент! — воскликает Шедоу с особым предвкушением. Он бросает лапшевесную машину и несется к Агенту. Хорошо отлаженная машина без устали штампует свежий выпуск «Лапшевесника», от одного вида которой уже начинает тяготить ушки."
                            "https://magma.com/shared/XhuBNd8uyGMH-EWvEDNoQg"

                        say "— Я только спроси!..."
                        say "— Ну что же ты стоишь у порога?! Заходи, добрый друг, — Шэдоу подхватывает Агента за плечи и заводит внутрь. Дверь сзади закрывается и отрезает пути к отступлению."
                        say "— О-о, кто пришел! — на сей раз это уже восклицает Адалинда со свежим «Лапшевесником» в руках."
                        say "Агент машет ей в ответ."
                        say "— Ты знал, что "

                        say "*Начинается квест \"Лапшичка, лапшичка повсюду!\"*"
                        ChangeVars (NoodlesEverywhere.set NoodlesEverywhere.Started)
                        ChangeVars (NoodlesCount.set 1000)
                        menu [
                        ] [
                            "На главную площадь", [
                                jump LabelName.MainSockdomStreet
                            ]
                        ]
                    ]

                    (NoodlesEverywhere.is' NoodlesEverywhere.Started), [
                        if' (fun vars -> NoodlesCount.get vars > 0) [
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
                            ChangeVars (NoodlesEverywhere.set NoodlesEverywhere.Finished)
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
                NoodlesEverywhere.is' NoodlesEverywhere.Finished vars
                && TemporaryGatekeeper.is' TemporaryGatekeeper.Finished vars
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
