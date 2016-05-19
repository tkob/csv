# Setup

    - use "csv.sml";
    ...
    - fun scan s = case CSV.scanTSV Substring.getc (Substring.full s) of SOME (record, strm) => (record, Substring.string strm);
    ...
    val scan = fn : string -> string list * string

# Successfull scan

## Empty

    - CSV.scanTSV Substring.getc (Substring.full "");
    val it = NONE : (string list * substring) option

## Empty line

    - val (record, rest) = scan "\n";
    val record = [""] : string list
    val rest = "" : string

## One field record

    - val (record, rest) = scan "one";
    val record = ["one"] : string list
    val rest = "" : string

## One field record with new line

    - val (record, rest) = scan "one\n";
    val record = ["one"] : string list
    val rest = "" : string

## Escaped field with comma

    - val (record, rest) = scan "\"one\ttwo\"";
    val record = ["one\ttwo"] : string list
    val rest = "" : string

## Escaped field with new line

    - val (record, rest) = scan "\"one\ntwo\"";
    val record = ["one\ntwo"] : string list
    val rest = "" : string

## Multiple fields

    - val (record, rest) = scan "\"one\ttwo\"\tthree\tfour";
    val record = ["one\ttwo","three","four"] : string list
    val rest = "" : string

## Two empty fields

    - val (record, rest) = scan "\t";
    val record = ["",""] : string list
    val rest = "" : string

## Multiple empty fields 

    - val (record, rest) = scan "\t\"\"\t\t\n";
    val record = ["","","",""] : string list
    val rest = "" : string

## Multiple records

    - val (record, rest) = scan "one\ttwo\nthree\tfour";
    val record = ["one","two"] : string list
    val rest = "three\tfour" : string
    - val (record, rest) = scan rest;
    val record = ["three","four"] : string list
    val rest = "" : string

## Multiple records with blank line

    - val (record, rest) = scan "one\ttwo\n\nthree\tfour";
    val record = ["one","two"] : string list
    val rest = "\nthree\tfour" : string
    - val (record, rest) = scan rest;
    val record = [""] : string list
    val rest = "three\tfour" : string
    - val (record, rest) = scan rest;
    val record = ["three","four"] : string list
    val rest = "" : string

## New line maybe CR, LF or CRLF

    - val (record, rest) = scan "one\ttwo\rthree\tfour\nfive\tsix\r\nseven\teight";
    val record = ["one","two"] : string list
    val rest = "three\tfour\nfive\tsix\r\nseven\teight" : string
    - val (record, rest) = scan rest;
    val record = ["three","four"] : string list
    val rest = "five\tsix\r\nseven\teight" : string
    - val (record, rest) = scan rest;
    val record = ["five","six"] : string list
    val rest = "seven\teight" : string
    - val (record, rest) = scan rest;
    val record = ["seven","eight"] : string list
    val rest = "" : string

# Unsuccessful scan

## Unquoted double quotes

```
- val (record, rest) = scan "one\ttwo\"three";

uncaught exception Match [nonexhaustive match failure]
...
```
