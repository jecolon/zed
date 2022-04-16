" Vim syntax file
" Language:	zed
" Maintainer:	José Colón <jec.rod@gmail.com>
" Last Change:	2022 Mar 19
" Remark:	The zed Programming Language

if exists("b:current_syntax")
  finish
else
  let b:current_syntax = "zed"
endif

let s:cpo_save = &cpo
set cpo&vim

syntax clear
syntax case match

syn iskeyword @,@-@

" A bunch of zed keywords
syn keyword zedStatement	and break continue do
syn keyword zedStatement	else if or return select
syn keyword zedStatement	while
:highlight link zedStatement Statement
" Math Functions
syn keyword zedFunction contains
syn keyword zedFunction	atan2 cos exp int log rand sin sqrt
" String Manipulation Methods
syn keyword zedFunction	chars endsWith indexOf lastIndexOf
syn keyword zedFunction	len, replace, startsWith, toLower, toUpper
" Input Output Functions
syn keyword zedFunction	print
" List Manipulation Mthods
syn keyword zedFunction	each filter map reduce
syn keyword zedFunction	mean median mode stdev
syn keyword zedFunction	pop push reverse unique
syn keyword zedFunction	sortAsc sortDec
" Map Manipulation Methods
syn keyword zedFunction	keys values keysByValueAsc keysByValueDesc
" Other builtins
syn keyword zedFunction	col capture next reset
highlight link zedFunction Function

syn keyword zedConditional	if else
highlight link zedConditional Conditional
syn keyword zedRepeat	while do
highlight link zedRepeat Repeat

syn keyword zedTodo	contained TODO
highlight link zedTodo Todo

syn keyword zedEvent	onInit onFile onRec onExit
highlight link zedEvent Special

syn keyword zedConstant true false nil
highlight link zedConstant Constant

" Built-in Variables
syn keyword zedVariable         @file @rnum @frnum @rec @cols
syn keyword zedVariable         @ics @irs @ocs @ors @head @headers
highlight link zedVariable Special

" Arithmetic operators: +, and * take care of ++, and **
syn match   zedOperator		"+\|-\|\*\|/\|%\|=\|\~"
syn match   zedOperator		"+=\|-=\|\*=\|/=\|%=\|:=\|?:\|?=\|\~@"
highlight link zedOperator Operator

syn match   zedAutoParam	"\W@\d\+"
highlight link zedAutoParam Special

":syntax match zedOps /\(:[=]\?\|=[=]\?\|+[+=]\?\|*[*=]\?\|\/[=]\?\|%[=]\?\)/
":highlight link zedOps Operator

syntax match zedIdentifier /\<\I\i*\>/
highlight link zedIdentifier Identifier

syntax region zedComment start=/#/ end=/\(#\|$\)/
highlight link zedComment Comment

syntax match zedIpol /[{][^}]*}/ contained contains=ALL
highlight link zedIpol PreProc

syntax region zedString start=/"/ skip=/\\"/ end=/"/ contains=zedIpol
highlight link zedString String

let &cpo = s:cpo_save
unlet s:cpo_save
