structure CSV :> sig
  type 'strm config = {
    delim : (char, 'strm) StringCvt.reader -> 'strm -> 'strm option,
    quote : (char, 'strm) StringCvt.reader -> 'strm -> 'strm option,
    textData : char -> bool,
    escape : (char, 'strm) StringCvt.reader -> (char, 'strm) StringCvt.reader
  }

  val scan : 'strm config -> (char, 'strm) StringCvt.reader -> (string list, 'strm) StringCvt.reader
  val scanCSV : (char, 'strm) StringCvt.reader -> (string list, 'strm) StringCvt.reader
end = struct
  type 'strm config = {
    delim : (char, 'strm) StringCvt.reader -> 'strm -> 'strm option,
    quote : (char, 'strm) StringCvt.reader -> 'strm -> 'strm option,
    textData : char -> bool,
    escape : (char, 'strm) StringCvt.reader -> (char, 'strm) StringCvt.reader
  }

  infix >>=
  fun (SOME x) >>= k = k x
    | NONE     >>= k = NONE

  fun char c input1 strm =
        case input1 strm of
             NONE => NONE
           | SOME (c', strm') => if c' = c then SOME (c', strm') else NONE

  fun comma input1 strm = char #"," input1 strm
  fun dquote input1 strm = char #"\"" input1 strm
  fun cr input1 strm = char (Char.chr 0x0d) input1 strm
  fun lf input1 strm = char (Char.chr 0x0a) input1 strm

  fun crlf input1 strm =
        cr input1 strm  >>= (fn (cr', strm')  =>
        lf input1 strm' >>= (fn (lf', strm'') =>
        SOME (implode [cr', lf'], strm'')))

  fun mapFst f (SOME (fst, snd)) = SOME (f fst, snd)
    | mapFst f NONE = NONE

  fun newline input1 strm =
        case crlf input1 strm of
             SOME x => SOME x
           | NONE =>
               case mapFst String.str (cr input1 strm) of
                    SOME x => SOME x
                  | NONE => mapFst String.str (lf input1 strm)

  fun eof input1 strm =
        case input1 strm of
             NONE => SOME ((), strm)
           | SOME _ => NONE

  fun newlineOrEof input1 strm =
        case newline input1 strm of
             SOME x => SOME x
           | NONE => mapFst (fn () => "") (eof input1 strm)

  fun textData (config : 'strm config) input1 strm =
        case input1 strm of
             NONE => NONE
           | SOME (c, strm') =>
               if #textData config c then SOME (c, strm') else NONE

  fun repeat class input1 strm =
        let
          fun loop cs strm =
                case class input1 strm of
                     SOME (c, strm') => loop (c::cs) strm'
                   | NONE => SOME (rev cs, strm)
        in
          loop [] strm
        end

  fun nonEscaped config input1 strm = repeat (textData config) input1 strm

  fun discard class input1 strm =
        case class input1 strm of
             SOME (_, strm') => SOME strm'
           | NONE => NONE

  fun dquotes input1 strm =
        discard dquote input1 strm  >>= (fn strm'  =>
        discard dquote input1 strm' >>= (fn strm'' =>
        SOME (#"\"", strm'')))

  infix ||
  fun (a || b) input1 strm =
        case a input1 strm of
             SOME x => SOME x
           | NONE => b input1 strm

  fun escaped' (config : 'strm config) input1 strm =
        (textData config || comma || cr || lf || #escape config) input1 strm

  fun escaped (config : 'strm config) input1 strm =
        (#quote config) input1 strm           >>= (fn strm' =>
        repeat (escaped' config) input1 strm' >>= (fn (s, strm'') =>
        (#quote config) input1 strm''         >>= (fn strm''' =>
        SOME (s, strm'''))))

  fun field config input1 strm =
          mapFst implode ((escaped config || nonEscaped config) input1 strm)

  fun field' (config : 'strm config) input1 strm =
        (#delim config) input1 strm >>= (fn strm' =>
        field config input1 strm'   >>= (fn (field, strm'') =>
        SOME (field, strm'')))

  fun record config input1 strm =
        field config input1 strm            >>= (fn (first, strm') =>
        repeat (field' config) input1 strm' >>= (fn (rest, strm'') =>
        SOME (first::rest, strm'')))

  fun scan config input1 strm =
        record config input1 strm >>= (fn (record, strm') =>
        newlineOrEof input1 strm' >>= (fn (_, strm'') =>
        SOME (record, strm'')))

  fun scanCSV input1 strm =
        let
          val csvConfig =
            { delim = discard comma,
              quote = discard dquote,
              textData = fn c => not (
                c < Char.chr 0x20 orelse
                c = Char.chr 0x22 orelse
                c = Char.chr 0x2c orelse
                c = Char.chr 0x7f),
              escape = dquotes }
        in
          scan csvConfig input1 strm
        end
end
