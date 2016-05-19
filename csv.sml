structure CSV :> sig
  type 'strm input1 = (char, 'strm) StringCvt.reader
  type 'strm config = {
    delim : 'strm input1 -> (string, 'strm) StringCvt.reader,
    quote : ('strm input1 -> 'strm -> 'strm option) option,
    textData : 'strm input1 -> (string, 'strm) StringCvt.reader,
    escape : 'strm input1 -> (string, 'strm) StringCvt.reader
  }

  val scan : 'strm config -> 'strm input1 -> (string list, 'strm) StringCvt.reader
  val scanCSV : 'strm input1 -> (string list, 'strm) StringCvt.reader
  val scanTSV : 'strm input1 -> (string list, 'strm) StringCvt.reader
end = struct
  type 'strm input1 = (char, 'strm) StringCvt.reader
  type 'strm config = {
    delim : 'strm input1 -> (string, 'strm) StringCvt.reader,
    quote : ('strm input1 -> 'strm -> 'strm option) option,
    textData : 'strm input1 -> (string, 'strm) StringCvt.reader,
    escape : 'strm input1 -> (string, 'strm) StringCvt.reader
  }

  infix >>=
  fun (SOME x) >>= k = k x
    | NONE     >>= k = NONE

  infix ||
  fun (a || b) input1 strm =
        case a input1 strm of
             SOME x => SOME x
           | NONE => b input1 strm

  fun repeat class input1 strm =
        let
          fun loop cs strm =
                case class input1 strm of
                     SOME (c, strm') => loop (c::cs) strm'
                   | NONE => SOME (rev cs, strm)
        in
          loop [] strm
        end

  fun discard class input1 strm =
        case class input1 strm of
             SOME (_, strm') => SOME strm'
           | NONE => NONE

  fun mapFst f (SOME (fst, snd)) = SOME (f fst, snd)
    | mapFst f NONE = NONE

  fun char c input1 strm =
        case input1 strm of
             NONE => NONE
           | SOME (c', strm') =>
               if c' = c then SOME (String.str c', strm') else NONE

  (* new line and eof *)
  fun cr input1 strm = char (Char.chr 0x0d) input1 strm
  fun lf input1 strm = char (Char.chr 0x0a) input1 strm
  fun crlf input1 strm =
        cr input1 strm  >>= (fn (cr', strm')  =>
        lf input1 strm' >>= (fn (lf', strm'') =>
        SOME (cr' ^ lf', strm'')))
  fun newline input1 strm =
        (crlf || cr || lf) input1 strm
  fun eof input1 strm =
        case input1 strm of
             NONE => SOME ("", strm)
           | SOME _ => NONE
  fun newlineOrEof input1 strm =
        (newline || eof) input1 strm

  fun nonEscaped (config :'strm config) input1 strm = repeat (#textData config) input1 strm

  fun escaped' (config : 'strm config) input1 strm =
        (#textData config || #delim config || cr || lf || #escape config) input1 strm

  fun escaped quote (config : 'strm config) input1 strm =
        quote input1 strm                     >>= (fn strm' =>
        repeat (escaped' config) input1 strm' >>= (fn (s, strm'') =>
        quote input1 strm''                   >>= (fn strm''' =>
        SOME (s, strm'''))))

  fun field config input1 strm =
        case #quote config of
             NONE =>
               mapFst concat (nonEscaped config input1 strm)
           | SOME quote =>
               mapFst concat ((escaped quote config || nonEscaped config) input1 strm)

  fun field' (config : 'strm config) input1 strm =
        (#delim config) input1 strm >>= (fn (_, strm') =>
        field config input1 strm'   >>= (fn (field, strm'') =>
        SOME (field, strm'')))

  fun record config input1 strm =
        field config input1 strm            >>= (fn (first, strm') =>
        repeat (field' config) input1 strm' >>= (fn (rest, strm'') =>
        SOME (first::rest, strm'')))

  fun scan config input1 strm =
        case input1 strm of
             NONE => NONE
           | SOME _ =>
               record config input1 strm >>= (fn (record, strm') =>
               newlineOrEof input1 strm' >>= (fn (_, strm'') =>
               SOME (record, strm'')))

  (* utility for textData config *)
  fun textData' pred input1 strm =
        case input1 strm of
             NONE => NONE
           | SOME (c, strm') =>
               if pred c then SOME (c, strm') else NONE
  fun textData pred input1 strm =
        textData' pred input1 strm           >>= (fn (c, strm') =>
        repeat (textData' pred) input1 strm' >>= (fn (cs, strm'') =>
        SOME (implode (c::cs), strm'')))

  (* for CSV *)
  fun comma input1 strm = char #"," input1 strm
  fun dquote input1 strm = char #"\"" input1 strm
  fun dquotes input1 strm =
        discard dquote input1 strm  >>= (fn strm'  =>
        discard dquote input1 strm' >>= (fn strm'' =>
        SOME ("\"", strm'')))

  fun scanCSV input1 strm =
        let
          val csvConfig =
            { delim = comma,
              quote = SOME (discard dquote),
              textData = textData (fn c => not (
                c < Char.chr 0x20 orelse
                c = #"\""         orelse
                c = #","          orelse
                c = Char.chr 0x7f)),
              escape = dquotes }
        in
          scan csvConfig input1 strm
        end

  (* for TSV *)
  fun tab input1 strm = char (Char.chr 0x09) input1 strm

  fun scanTSV input1 strm =
        let
          val csvConfig =
            { delim = tab,
              quote = SOME (discard dquote),
              textData = textData (fn c => not (
                c < Char.chr 0x20 orelse
                c = #"\""         orelse
                c = Char.chr 0x09 orelse
                c = Char.chr 0x7f)),
              escape = dquotes }
        in
          scan csvConfig input1 strm
        end
end
