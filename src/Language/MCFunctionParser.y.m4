include(`m4/defs.m4')

command :: {[Either String Expr]}
command : commandPart         { [$1] }
        | commandPart command { $1 : $2 }

commandPart :: {Either String Expr}
commandPart : "$" "{" expr "}" { Right $3 }
            | COMMAND_PART     { Left (commandPartVal $1) }
