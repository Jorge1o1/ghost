namespace Ghost

module Model =
    type Move =
        | Letter of string
        | Challenge

    type IPlayer =
        abstract member Name: string
        abstract member Prompt: string -> Move
