# Setup

    - use "csv.sml";
    ...
    - fun scan s = case CSV.scanAwk Substring.getc (Substring.full s) of SOME (record, strm) => (record, Substring.string strm);
    ...
    val scan = fn : string -> string list * string

# Successfull scan

## Empty

    - CSV.scanAwk Substring.getc (Substring.full "");
    val it = NONE : (string list * substring) option

## Empty (stripped)

    - CSV.scanAwk Substring.getc (Substring.full " \t");
    val it = NONE : (string list * substring) option

## Empty line

    - val (record, rest) = scan "\n";
    val record = [""] : string list
    val rest = "" : string

## Empty line (stripped)

    - val (record, rest) = scan " \t\n";
    val record = [""] : string list
    val rest = "" : string

## One field record

    - val (record, rest) = scan "one";
    val record = ["one"] : string list
    val rest = "" : string

## One field record (stripped)

    - val (record, rest) = scan " \tone \t";
    val record = ["one"] : string list
    val rest = "" : string

## One field record with new line

    - val (record, rest) = scan "one\n";
    val record = ["one"] : string list
    val rest = "" : string

## One field record with new line (stripped)

    - val (record, rest) = scan " \tone \t\n";
    val record = ["one"] : string list
    val rest = "" : string

## Multiple fields

    - val (record, rest) = scan "one,two three four";
    val record = ["one,two","three","four"] : string list
    val rest = "" : string

## Multiple records

    - val (record, rest) = scan "one two\nthree four";
    val record = ["one","two"] : string list
    val rest = "three four" : string
    - val (record, rest) = scan rest;
    val record = ["three","four"] : string list
    val rest = "" : string

## Multiple records with blank line

    - val (record, rest) = scan "one two\n\nthree four";
    val record = ["one","two"] : string list
    val rest = "\nthree four" : string
    - val (record, rest) = scan rest;
    val record = [""] : string list
    val rest = "three four" : string
    - val (record, rest) = scan rest;
    val record = ["three","four"] : string list
    val rest = "" : string

## New line maybe CR, LF or CRLF

    - val (record, rest) = scan "one two\rthree four\nfive six\r\nseven eight";
    val record = ["one","two"] : string list
    val rest = "three four\nfive six\r\nseven eight" : string
    - val (record, rest) = scan rest;
    val record = ["three","four"] : string list
    val rest = "five six\r\nseven eight" : string
    - val (record, rest) = scan rest;
    val record = ["five","six"] : string list
    val rest = "seven eight" : string
    - val (record, rest) = scan rest;
    val record = ["seven","eight"] : string list
    val rest = "" : string
