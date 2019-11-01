ifdef(`included_defs.m4',,`dnl
define(`included_defs.m4',1)dnl

define(`list', `$1 :: { [$2] }
$1 : {- empty -} { [] }
	| $3 $4 $1 { `$'1 : `$'3 }
dnl')

define(`list1', `$1 :: { [$2] }
$1 : $3 { [`$'1] }
	| $3 $4 $1 { `$'1 : `$'3 }
dnl')

define(`maybe', `$1 :: { Maybe $2 }
$1 : {- empty -} { Nothing }
	| $3 { Just `$'1 }
dnl')

')dnl