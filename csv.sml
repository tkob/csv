structure CSV :> sig
  exception CSV
  val scan : (char, 'a) StringCvt.reader -> (string list, 'a) StringCvt.reader
end = struct
  exception CSV

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

  fun textData input1 strm =
        case input1 strm of
             NONE => raise CSV
           | SOME (c, strm') =>
               if Char.chr 0x20 <= c andalso c <= Char.chr 0x21 orelse
                  Char.chr 0x23 <= c andalso c <= Char.chr 0x2b orelse
                  Char.chr 0x2d <= c andalso c <= Char.chr 0x7e then
                 (c, strm')
               else
                 raise CSV

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

  fun field' input1 strm =
        let
          val (_, strm') = comma input1 strm
          val (field, strm'') = field input1 strm'
        in
          (field, strm'')
        end

  fun record input1 strm =
        let
          val (first, strm') = field input1 strm
          val (rest, strm'') = repeat field' input1 strm'
        in
          (first::rest, strm'')
        end

  fun scan input1 strm =
        let
          val (record, strm') = record input1 strm
        in
          let
            val (_, strm'') = newline input1 strm'
          in
            SOME (record, strm'')
          end
          handle CSV => SOME (record, strm')
        end
        handle CSV => NONE
end
