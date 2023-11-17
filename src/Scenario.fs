module Scenario
open IfEngine.SyntaxTree
open IfEngine.SyntaxTree.Helpers
open IfEngine.SyntaxTree.CommonContent.Helpers
open Farkdown.Experimental.Helpers

module Farkdown =
    module Helpers =
        let textf format =
            Printf.ksprintf text format

        let p' str =
            p [[ text str ]]

        let say' str =
            say [ p [[ text str ]] ]

open Farkdown.Helpers

type CustomStatementArg = unit
type CustomStatement = unit
type CustomStatementOutput = unit

[<RequireQualifiedAccess>]
type LabelName =
    | MainMenu
    | Prelude
    | MainSockdomStreet
    | ThroneRoom
    | Gates
    | NoodleFactory
    | Еpilogue

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

let scenario: Scenario<CommonContent.Content, _, CustomStatement> =
    [
        label LabelName.MainMenu [
            noodleCount := 0
            noodlesEverywhere := NoodlesEverywhere.HasNotStartedYet
            temporaryGatekeeper := TemporaryGatekeeper.HasNotStartedYet

            menu [
                h1 [ text "Поручение Ее Носочества" ] [
                    p [
                        [
                            text "Однажды Ее Носочество Surprise велит верноподданному Агенту собрать всех жителей Носочного царства в главном зале, чтобы объявить важную государственную весть. Тот принимается всех собирать, однако жители не шибко спешат выполнять указание даже именем Ее Носочества и находят тысячу и одну причину, чтобы не идти. Сможет ли Агент всех собрать? Это и предстоит выяснить в этом коротеньком интерактивном рассказе "
                            img "https://cdn.discordapp.com/emojis/927633357737713704.webp?size=22&quality=lossless" "pepeSmile" "pepeSmile"
                        ]
                    ]

                    p [
                        [ text "v0.1.0" ]
                    ]
                ]
            ] [
                choice "Начать" [ jump LabelName.Prelude ]
                choice "Авторов!" [
                    menu [
                        p [
                            [
                                text "Все понемногу. И вообще, заходите на наш "
                                url "https://discord.gg/YcN2AET65d" "Веселый носок" [
                                    text "Discord сервер \"Веселый носок\""
                                ]
                                text ", у нас весело!"
                            ]
                        ]
                    ] [
                        choice "Назад" [ jump LabelName.MainMenu ]
                    ]
                ]
            ]
        ]

        label LabelName.Prelude [
            say [ p [[ text "TODO: здесь раскрывается логлайн, в котором Ее Носочество дает задание Агенту." ]] ]
            jump LabelName.MainSockdomStreet
        ]

        label LabelName.MainSockdomStreet [
            menu [
                p' "TODO: Агент выходит из замка на главную площадь Носочного царства со множеством разветлений. Ему предстоит посетить множество мест и встретиться с жителями Носочного царства, которых придется уговорить, чтобы выполнить поручение."
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
                        say' "Агент подходит к вратам — к единственному месту, через которое можно попасть внутрь Носочного королевства и выйти из оного."

                        say [
                            p [[ text "На страже у ворот стоит Enurezo." ]]
                            p [[ img "https://cdn.discordapp.com/emojis/952317602594693171.webp?size=240&quality=lossless" "" "" ]]
                        ]

                        say' "— Привет, Enu... — не успевает начать Агент, как во врата уже кто-то ломится."
                        say' "— Есть кто дома?! — орут с той стороны."
                        say' "— Ты вовремя, Агент. — невозмутимо говорит Enurezo и кричит в ответ: — 18 есть?!"

                        say [
                            p [[ text "С той стороны льется негодование, ругань и отборная адекватность." ]]
                            p [[ img "https://trello.com/1/cards/62da598f6a88c50b13a91d6b/attachments/62fd2abe66de6e3368657746/download/%D0%B8%D0%B7%D0%BE%D0%B1%D1%80%D0%B0%D0%B6%D0%B5%D0%BD%D0%B8%D0%B5.png" "" "" ]]
                        ]

                        say' "— Там это, Ее Носочество... — переходит Агент к делу, но Enurezo уже опрашивает следующего новичка и совершенно не обращает на него внимания."

                        say' "\"Вот бы состряпать временного стража для врат, чтобы заменить как-то Enurezo\", — напряженно думает Агент."

                        say [
                            p [[ italic (text "Начался квест \"Временный страж!\"") ]]
                        ]
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
                                p' "Enurezo неустанно стоит на страже врат и, похоже, не обращает никакого внимания на Агента. Вот бы состряпать временного стража для врат."
                            ] [
                                "Гора «Лапшевесников»", [
                                    say' "\"А ведь из «Лапшевесников» можно состряпать, скажем, дракона, который будет временно отпугивать посетителей. Чем не страж?\", — подумал Агент."

                                    menu [
                                        p' "Сделать дракона?"
                                    ] [
                                        "Ага!", [
                                            say' "Агент собирает дракона из «Лапшевесников», а Enuroze придирчиво смотрит на это действо и бубнит."
                                            say' "Пару штрихов, и дракон готов."
                                            noodleCount := 0

                                            say [
                                                p [
                                                    [ text "— Вот наш временный страж! — довольно произносит Агент." ]
                                                    [ text "— Что-то я не дове... — не успевает договорить Enurezo, как вдруг дракон начинает орать: — Стой, кто идет?!" ]
                                                    [ text "И как раз вовремя, потому что на подходе у врат объявился новенький." ]
                                                    [ text "— А это так надо, что новичок сразу убежал? — спросил Enurezo." ]
                                                    [ text "— Да-да, всё по последнему слову техники. Пошли в тронный зал." ]
                                                    [ text "Enurezo нехотя соглашается." ]
                                                ]
                                            ]

                                            temporaryGatekeeper := TemporaryGatekeeper.Finished

                                            jump LabelName.Gates
                                        ]

                                        "Ой, не", [
                                            say' "Ну, и правильно."
                                            jump LabelName.Gates
                                        ]
                                    ]
                                ]

                                "Втюхать Enurezo «Лапшевесник»", [
                                    say' "— Спасибо, я не голоден, — отказался Enurezo."
                                    jump LabelName.Gates
                                ]

                                "Вернуться на главную площадь", [
                                    jump LabelName.MainSockdomStreet
                                ]
                            ]
                        ] [
                            menu [
                                p' "Enurezo неустанно стоит на страже врат и, похоже, не обращает никакого внимания на Агента. Вот бы состряпать временного стража для врат."
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
                        p' "На вратах тихо и пусто."
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
                        say' "Агент подходит к лапшичному заводу, к месту, где лапшичка становится явью."

                        // say "Сложно сказать, что именно там изготовляют, но известно точно, что любого, кто имеет неосторожность сюда показать свои ушки, начинает тяготить от лапшички."

                        say [
                            p [[ text "— Опа, Агент! — воскликает Шедоу с особым предвкушением. Он бросает лапшевесную машину и несется к Агенту. Хорошо отлаженная машина без устали штампует свежий выпуск «Лапшевесника», от одного вида которой уже начинает тяготить ушки." ]]
                            p [[ img "https://magma.com/shared/XhuBNd8uyGMH-EWvEDNoQg" "" "" ]]
                        ]

                        say' "— Я только спроси!..."
                        say' "— Ну что же ты стоишь у порога?! Заходи, добрый друг, — Шэдоу подхватывает Агента за плечи и заводит внутрь. Дверь сзади закрывается и отрезает пути к отступлению."
                        say' "— О-о, кто пришел! — на сей раз это уже восклицает Адалинда со свежим «Лапшевесником» в руках."
                        say' "Агент машет ей в ответ."
                        say' "— Ты знал, что "

                        say [
                            p [[ italic (text "Начинается квест \"Лапшичка, лапшичка повсюду!\"") ]]
                        ]
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
                            say' "— Ну что, раздал «Лапшевесники»? — спрашивает СПб, будто не видит, как лапшичка отягощает ушки Агента, плечи и всё, что только можно отягощать."
                            say' "— Нет, — бурчит Агент."
                            say' "— Тогда нас ждет еще много дел! — восклицает СПб и несется к Адалинде, чтобы узнать ее мнение о качестве выпускаемой лапшички."
                            menu [
                            ] [
                                "На главную площадь", [
                                    jump LabelName.MainSockdomStreet
                                ]
                            ]
                        ] [
                            say' "— Все «Лапшевесники» розданы! — отчитывается Агент."
                            say' "— Прекрасная работа, — говорит СПб. — Что ж, пожалуй, можно отправиться к Surprise. Кошка, бросай лапшичку, мы отправляемся в путь!"
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
                        p' "На заводе тихо и пусто, лишь старый «Лапшевесник» пошелестывает на ветру."
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
                    p' "Похоже, не все еще собрались. Нужно поднапрячься."
                ] [
                    "На главную площадь", [
                        jump LabelName.MainSockdomStreet
                    ]
                ]
            ]
        ]

        label LabelName.Еpilogue [
            say' "TODO: Агент собрал всех в носочном зале, и Ее Носочество объявляет важную весть."
        ]
    ]
    |> List.map (fun (labelName, body) -> labelName, (labelName, body))
    |> Map.ofList
