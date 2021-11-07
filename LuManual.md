Lu Language Reference Manual
============================

The Lu language is a minimal, dynamic, imperative programming language inspired by [Lua](https://www.lua.org/).

This manual describes the syntax and semantics of Lu and, in many places is a direct excerpt from the [Lua Manual](https://www.lua.org/manual/5.4/).
Some Lu programs are valid Lua programs. You can experiment with them using the 
[online interpreter](https://www.lua.org/cgi-bin/demo).  However, there are several differences between the languages, as this manual describes below.

Basic Concepts
==============

Values and Types
----------------

Lu is a dynamically typed language. This means that variables do not have types; only values do. There are no type definitions in the language. All values carry their own type.

All values in Lu are first-class values. This means that all values can be stored in variables.

There are five basic types in Lu: `nil`, `boolean`, `number`, `string`, and `table`. The type `nil` has one single value, `nil`, whose main property is to be different from any other value; it often represents the absence of a useful value. The type boolean has two values, false and true. Both nil and false make a condition false; they are collectively called false values. Any other value makes a condition true. Despite its name, false is frequently used as an alternative to nil, with the key difference that false behaves like a regular value in a table, while a nil in a table represents an absent key.

The type `number` represents integer numbers. Lu does not include floating point numbers (unlike Lua).

Unless stated otherwise, any overflow when manipulating integer values wrap around, according to the usual rules of two-complement arithmetic. (In other words, the actual result is the unique representable integer that is equal modulo 2n to the mathematical result, where n is the number of bits of the integer type.)

The type `string` represents immutable sequences of characters. 

The type `table` implements associative arrays, that is, arrays that can have as indices not only numbers, but any Lu value except `nil`. Tables can be heterogeneous; that is, they can contain values of all types (except `nil`). Any key associated to the value `nil` is not considered part of the table. Conversely, any key that is not part of a table has an associated value `nil`.

Tables are the sole data-structuring mechanism in Lu; they can be used to represent ordinary arrays, lists, symbol tables, sets, records, graphs, trees, etc. To represent records, Lu uses the field name as an index. The language supports this representation by providing `a.name` as syntactic sugar for `a["name"]`.

Table values are objects: variables do not actually contain these values, only references to them. Assignment always manipulate references to such values; these operations do not imply any kind of copy.

Environments 
------------

As we will discuss further, any reference to a free name (that is, a name not bound to any declaration) var is syntactically translated to `_G.var`.

Error Handling
--------------

No operation in Lu can raise an error. If an error would occur, the value
'nil' results instead.

The Language
============

This section describes the lexical structure, the syntax, and the semantics of Lu. In
other words, this section describes which tokens are valid, how they can be
combined, and what their combinations mean.

Language constructs will be explained using the usual extended BNF notation,
in which `{a}` means 0 or more a's, and `[a]` means an optional a.

Lexical conventions
-------------------

Lu is a free-form language. It ignores spaces between lexical elements (tokens), except as delimiters between two tokens. In source code, Lu recognizes as spaces the standard ASCII whitespace characters space, form feed, newline, carriage return, horizontal tab, and vertical tab.

*Names* (also called identifiers) in Lu can be any string of Latin letters, Arabic-Indic digits, and underscores, not beginning with a digit and not being a reserved word. Identifiers are used to name variables and table fields.

The following keywords are reserved and cannot be used as names:

     and       break     do        else      elseif    end
     false     for       function  goto      if        in
     local     nil       not       or        repeat    return
     then      true      until     while

Literal strings can be delimited by matching double quotes, and can contain the following C-like escape sequences: '\b' (backspace), '\f' (form feed), '\n' (newline), '\r' (carriage return), '\t' (horizontal tab), '\v' (vertical tab), '\\' (backslash), and '\'' (apostrophe [single quote]). 

Lu strings may not contain the double quote character (i.e. '\"'), not even
when preceded with a backslash.  This is unlike Lua (and most other
programming languages).

Variables
---------

Variables are places that store values. There are two kinds of variables in
Lu: global variables and table fields.

A single name can denotes a global variable:

	var ::= Name
    
Name denotes identifiers.

Before the first assignment to a variable, its value is `nil`.

Square brackets are used to index a table:

	var ::= prefixexp ‘[’ exp ‘]’

The syntax `var.Name` is just syntactic sugar for `var["Name"]`:

	var ::= prefixexp ‘.’ Name
    
An access to a global variable `x` is equivalent to `_G.x`. 

Blocks and Statements
---------------------

A block is a list of statements, which are executed sequentially:

	block ::= {stat}
    
Lu has empty statements that allow you to separate statements with
semicolons, start a block with a semicolon or write two semicolons in
sequence:

	stat ::= ‘;’

Lu allows assignment.

    stat ::= var ‘=’ exp

An assignment to a global name `x = val` is equivalent to the assignment `_G.x = val`.

The control structures if, while, and repeat have the usual meaning and familiar syntax:

	stat ::= while exp do block end
	stat ::= repeat block until exp
	stat ::= if exp then block else block end

The condition expression of a control structure can return any value. Both
`false` and `nil` test false. All values different from `nil` and `false` test
true. In particular, the number `0` and the empty string also test true.

Expressions
-----------

The basic expressions in Lu are the following:

	exp ::= prefixexp
	exp ::= nil | false | true
	exp ::= Numeral
	exp ::= LiteralString
	exp ::= tableconstructor
	exp ::= exp binop exp
	exp ::= unop exp
	prefixexp ::= var | ‘(’ exp ‘)’
    

Binary operators comprise arithmetic operators, relational operators, logical operators, and the concatenation operator. Unary operators comprise the unary minus, the unary logical not, and the unary length operator.

Lu supports the following arithmetic operators:

    +: addition
    -: subtraction
    *: multiplication
    //: floor division
    %: modulo
    -: unary minus

Floor division (//) is a division that rounds the quotient towards minus infinity, resulting in the floor of the division of its operands.

Modulo is defined as the remainder of a division that rounds the quotient towards minus infinity (floor division).

In case of overflows in integer arithmetic, all operations wrap around.

Unlike Lua, Lu does not implicitly convert strings to numbers for arithmetic operations. If a non-number is provided as any argument to an arithmetic operation, the result is nil.

Lu supports the following relational operators:

    ==: equality
    <: less than
    >: greater than
    <=: less or equal
    >=: greater or equal
    
These operators *always* result in false or true and never return nil.

Equality `(==)` first compares the type of its operands. If the types are different, then the result is false. Otherwise, the values of the operands are compared. Strings are equal if they have the same content. Numbers are equal if they denote the same mathematical value.

Tables are compared by reference: two objects are considered equal only if they are the same object. Every time you create a new table, this new object is different from any previously existing object. 

Equality comparisons do not convert strings to numbers or vice versa. Thus, `"0"==0` evaluates to false, and `t[0]` and `t["0"]` denote different entries in a table.

Unlike Lua, the order operations (e.g. '<') are heterogeneous and apply to *all*
values. The nil value is less than all numbers, which are less than all
booleans (false < true), which are less than all strings (ordered
lexicographically), which are less than all table objects (unspecified
ordered).

The string concatenation operator in Lu is denoted by two dots ('..'). If either argument is not a string, then the result is nil.

The length operator is denoted by the unary prefix operator #. The length of a string is its number of characters. The length of a table is the number of entries that it contains. (SCW: need to remove entries that map to nil.)

Operator precedence in Lu follows the table below, from lower to higher priority:

     <     >     <=    >=   ==
     ..
     +     -
     *     //    %
     unary operators (not   #     -)

As usual, you can use parentheses to change the precedences of an
expression. All binary operators are left associative, including concatenation.

Table constructors are expressions that create tables. Every time a constructor is evaluated, a new table is created. A constructor can be used to create an empty table or to create a table and initialize some of its fields. The general syntax for constructors is

	tableconstructor ::= ‘{’ [fieldlist] ‘}’
	fieldlist ::= field {fieldsep field} [fieldsep]
	field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp 
	fieldsep ::= ‘,’ 
    
Each field of the form `[exp1] = exp2` adds to the new table an entry with key `exp1` and value `exp2`. A field of the form `name = exp` is equivalent to `["name"] = exp`.  For example,

     t = { [true] = g, x = 1, [30] = 23 }
     
is equivalent to

       t = {}
       t[true] = g
       t.x = 1            -- t["x"] = 1
       t[30] = 23

The order of the assignments in a constructor is undefined. 

Complete syntax
===============

Here is the complete syntax of Lu in extended BNF. As usual in extended BNF, `{A}` means 0 or more As, and `[A]` means an optional A. 


	block ::= {stat} 

	stat ::=  ‘;’ | 
		 var ‘=’ exp | 
		 while exp do block end | 
		 repeat block until exp | 
		 if exp then block else block end 

	var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 

	exp ::=  nil | false | true | Numeral | LiteralString | 
		 prefixexp | tableconstructor | exp binop exp | unop exp 

	prefixexp ::= var | ‘(’ exp ‘)’

	tableconstructor ::= ‘{’ [fieldlist] ‘}’

	fieldlist ::= field {fieldsep field} 

	field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp 

	fieldsep ::= ‘,’ 

	binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘//’ | ‘%’ | ‘..’ | 
		 ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ |  

	unop ::= ‘-’ | not | ‘#’ 


