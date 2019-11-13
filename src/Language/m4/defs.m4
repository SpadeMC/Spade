ifdef(`included_defs.m4',,`dnl
define(`included_defs.m4',1)dnl

define(`list', `$1 :: { [$2] }
$1 : {- empty -} { [] }
	| list1_$1 { `$'1 }

list1(list1_$1, `$2', `$3', `$4')
dnl')

define(`list1', `$1 :: { [$2] }
$1 : $3 { [`$'1] }
	| $3 $4 $1 { `$'1 : `$'3 }
dnl')

define(`listNoSep', `$1 :: { [$2] }
$1 : {- empty -} { [] }
	| $3 $1 { `$'1 : `$'2 }
dnl')

define(`listNoSep1', `$1 :: { [$2] }
$1 : $3 { [`$'1] }
	| $3 $1 { `$'1 : `$'2 }
dnl')

define(`maybe', `$1 :: { Maybe $2 }
$1 : {- empty -} { Nothing }
	| $3 { Just `$'1 }
dnl')

')dnl