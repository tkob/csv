structure CSV :> sig
  exception CSV

  type 'strm config = {
    delim : (char, 'strm) StringCvt.reader -> 'strm -> 'strm
  }

  val scan : ('a) config -> (char, 'a) StringCvt.reader -> (string list, 'a) StringCvt.reader
  val scanCSV : (char, 'a) StringCvt.reader -> (string list, 'a) StringCvt.reader
end = struct
  exception CSV

  type ('strm) config = {
    delim : (char, 'strm) StringCvt.reader -> 'strm -> 'strm
  }

  fun char c input1 strm =
        case input1 strm of
             NONE => raise CSV
           | SOME (c', strm') => if c' = c then (c', strm') else raise CSV

  fun comma input1 strm = char #"," input1 strm
  fun dquote input1 strm = char #"\"" input1 strm
  fun cr input1 strm = char (Char.chr 0x0d) input1 strm
  fun lf input1 strm = char (Char.chr 0x0a) input1 strm

  fun crlf input1 strm =
        let
          val (cr', strm') = cr input1 strm
          val (lf', strm'') = lf input1 strm'
        in
          (implode [cr', lf'], strm'')
        end

  fun mapFst f (fst, snd) = (f fst, snd)

  fun newline input1 strm =
        crlf input1 strm                   handle CSV =>
        mapFst String.str (cr input1 strm) handle CSV =>
        mapFst String.str (lf input1 strm)

  fun eof input1 strm =
        case input1 strm of
             NONE => ((), strm)
           | SOME _ => raise CSV

  fun newlineOrEof input1 strm =
        newline input1 strm handle CSV =>
        mapFst (fn () => "") (eof input1 strm)

  fun textData input1 strm =
        case input1 strm of
             NONE => raise CSV
           | SOME (c, strm') =>
               if c < Char.chr 0x20 orelse
                  c = Char.chr 0x22 orelse
                  c = Char.chr 0x2c orelse
                  c = Char.chr 0x7f then
                 raise CSV
               else
                 (c, strm')

  fun repeat class input1 strm =
        let
          fun loop cs strm =
                let
                  val (c, strm') = class input1 strm
                in
                  loop (c::cs) strm'
                end
                handle CSV => (rev cs, strm)
        in
          loop [] strm
        end

  fun nonEscaped input1 strm = repeat textData input1 strm

  fun dquotes input1 strm =
        let
          val (_, strm') = dquote input1 strm
          val (_, strm'') = dquote input1 strm'
        in
          (#"\"", strm')
        end

  fun escaped' input1 strm =
        textData input1 strm  handle CSV =>
        comma input1 strm handle CSV =>
        cr input1 strm    handle CSV =>
        lf input1 strm    handle CSV =>
        dquotes input1 strm

  fun escaped input1 strm =
        let
          val (_, strm') = dquote input1 strm
          val (s, strm'') = repeat escaped' input1 strm'
          val (_, strm''') = dquote input1 strm''
        in
          (s, strm''')
        end

  fun field input1 strm =
        let
          val (cs, strm') =
            escaped input1 strm handle CSV => nonEscaped input1 strm
        in
          (implode cs, strm')
        end

  fun discard class input1 strm =
        let
          val (_, strm') = class input1 strm
        in
          strm'
        end

  fun field' (config : 'a config) input1 strm =
        let
          val strm' = (#delim config) input1 strm
          val (field, strm'') = field input1 strm'
        in
          (field, strm'')
        end

  fun record config input1 strm =
        let
          val (first, strm') = field input1 strm
          val (rest, strm'') = repeat (field' config) input1 strm'
        in
          (first::rest, strm'')
        end

  fun scan config input1 strm =
        let
          val (record, strm') = record config input1 strm
          val (_, strm'') = newlineOrEof input1 strm'
        in
          SOME (record, strm'')
        end
        handle CSV => NONE

  fun scanCSV input1 strm = scan {delim=discard comma}  input1 strm

end
