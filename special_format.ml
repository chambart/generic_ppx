
type nil

type empty = Dummy

type 'a format = Start : ('fun_stack,'b,'c,empty) s -> 'fun_stack format

and ('fun_stack, 'tuple_stack, 'v, 'lvl) s =
  | Percent : ('fs, 'ts, 'v, 'lvl) f ->
              ('fs, 'ts, 'v, 'lvl) s

  | Lparen : ( _, 'ts1, ('fs2, 'ts2, 'v * 'ts1 list, 'lvl) s) s' ->
             ('ts1 list -> 'fs2, 'ts2,
              'v, 'lvl) s

  | Rparen : ( 'fs1, 'ts1, 'v, 'lvl) s ->
             ( nil, 'v1, 'v1,
               ( 'fs1, 'ts1, 'v, 'lvl) s) s

  | End : (unit,'a,'b,empty) s

  | OTHERS : ('fs, 'ts, 'v, 'lvl) s ->
             ('fs, 'ts, 'v, 'lvl) s

and ('fun_stack, 'tuple_stack, 'v, 'lvl) f =
  | I : ('fs, 'ts, 'v * int, 'lvl) s ->
        (int -> 'fs, 'ts, 'v, 'lvl) f
  | C : ('fs, 'ts, 'v * char, 'lvl) s ->
        (char -> 'fs, 'ts, 'v, 'lvl) f
  | F : ('fs, 'ts, 'v * float, 'lvl) s ->
        (float -> 'fs, 'ts, 'v, 'lvl) f
  | Percent : ('fs, 'ts, 'v, 'lvl) s ->
              ('fs, 'ts, 'v, 'lvl) f
  | Lparen :  ('fs, 'ts, 'v, 'lvl) s ->
              ('fs, 'ts, 'v, 'lvl) f
  | Rparen :  ('fs, 'ts, 'v, 'lvl) s ->
              ('fs, 'ts, 'v, 'lvl) f

and ('fun_stack, 'tuple_stack, 'lvl) s' =
  | Percent : ('fs, 'ts, 'lvl) f' ->
              ('fs, 'ts, 'lvl) s'

  | Lparen : ( _, 'ts1, ('fs2, 'ts2, 'ts1 list, 'lvl) s) s' ->
             ('fs2, 'ts2, 'lvl) s'

  | OTHERS : ('fs, 'ts, 'lvl) s' ->
             ('fs, 'ts, 'lvl) s'

and ('fun_stack, 'tuple_stack, 'lvl) f' =
  | I : ('fs, 'ts, int, 'lvl) s ->
        ('fs, 'ts, 'lvl) f'
  | C : ('fs, 'ts, char, 'lvl) s ->
        ('fs, 'ts, 'lvl) f'
  | F : ('fs, 'ts, float, 'lvl) s ->
        ('fs, 'ts, 'lvl) f'
  | Percent : ('fs, 'ts, 'lvl) s' ->
              ('fs, 'ts, 'lvl) f'
  | Lparen :  ('fs, 'ts, 'lvl) s' ->
              ('fs, 'ts, 'lvl) f'
  | Rparen :  ('fs, 'ts, 'lvl) s' ->
              ('fs, 'ts, 'lvl) f'

let kprint_char c k =
  print_char c; k
let kprint_int c k =
  print_int c; k
let kprint_float c k =
  print_float c; k

let char_of_f : type a b c d. (a, b, c, d) f -> char = function
  | Percent s -> '%'
  | Lparen s -> '('
  | Rparen s -> ')'
  | I s -> 'i'
  | C s -> 'c'
  | F s -> 'f'

let char_of_f' : type a b c. (a, b, c) f' -> char = function
  | Percent s -> '%'
  | Lparen s -> '('
  | Rparen s -> ')'
  | I s -> 'i'
  | C s -> 'c'
  | F s -> 'f'

let rec kiter f l k = match l with
  | [] -> k
  | h :: t -> kiter f t (f h k)

let rec toplevel_printer_s : type t a b k. (t, a, b, empty) s -> k -> t = function
  | Percent f ->
    toplevel_printer_f f
  | End -> fun k -> ()
  | Lparen s ->
    fun k l ->
      toplevel_printer_s (drop_s' s)
        (kiter (deep_printer_s'_elt s) l k)

  | Rparen _ -> DROP

  | OTHERS s ->
    fun k -> toplevel_printer_s s (kprint_char CHAR k)

and toplevel_printer_f : type t a b k. (t, a, b, empty) f -> k -> t = function
  | I s ->
    fun k i ->
      toplevel_printer_s s (kprint_int i k)
  | C s ->
    fun k c ->
      toplevel_printer_s s (kprint_char c k)
  | F s ->
    fun k f ->
      toplevel_printer_s s (kprint_float f k)
  | (Percent s | Lparen s | Rparen s) as f ->
    fun k -> toplevel_printer_s s (kprint_char (char_of_f f) k)

and drop_s' : type a b c d e f t k.
  ( a, b, ( c, d, e, f ) s ) s' -> ( c, d, e, f ) s = function
  | Percent (I s) ->
    drop_s s
  | Percent (C s) ->
    drop_s s
  | Percent (F s) ->
    drop_s s
  | Lparen s ->
    drop_s (drop_s' s)
  | Percent (Lparen s | Rparen s | Percent s) ->
    drop_s' s
  | OTHERS s ->
    drop_s' s

and drop_s : type a b c d e f g k.
  ( a, b, c, ( d, e, f, g ) s ) s -> ( d, e, f, g ) s = function
  | Percent (I s) ->
    drop_s s
  | Percent (C s) ->
    drop_s s
  | Percent (F s) ->
    drop_s s
  | Lparen s ->
    drop_s (drop_s' s)
  | Rparen s -> s
  | Percent (Percent s | Lparen s | Rparen s) ->
    drop_s s
  | OTHERS s ->
    drop_s s

and deep_printer_s'_elt : type a b c d e f k t.
  ( a, b, ( c, d, e, f ) s ) s' -> b -> k -> k = function
  | Percent (I s) ->
    deep_printer_s_elt s kprint_int
  | Percent (C s) ->
    deep_printer_s_elt s kprint_char
  | Percent (F s) ->
    deep_printer_s_elt s kprint_float
  | Lparen s ->
    deep_printer_s_elt (drop_s' s)
      (kiter (deep_printer_s'_elt s))
  | Percent((Percent s | Lparen s | Rparen s) as f) ->
    fun b k -> deep_printer_s'_elt s b (kprint_char (char_of_f' f) k)
  | OTHERS s ->
    fun b k -> deep_printer_s'_elt s b (kprint_char CHAR k)

and deep_printer_s_elt : type a b c d e f g k k.
  (a, b, c, (d,e,f,g) s) s -> (c -> k -> k) -> b -> k -> k = function
  | Percent (I s) ->
    fun kont ->
      deep_printer_s_elt s (fun (b,i) k -> kprint_int i (kont b k))
  | Percent (C s) ->
    fun kont ->
      deep_printer_s_elt s (fun (b,i) k -> kprint_char i (kont b k))
  | Percent (F s) ->
    fun kont ->
      deep_printer_s_elt s (fun (b,i) k -> kprint_float i (kont b k))
  | Rparen s ->
    fun kont -> kont
  | Lparen s ->
    fun kont ->
      deep_printer_s_elt (drop_s' s)
        (fun (b,l) k -> kiter (deep_printer_s'_elt s) l (kont b k))
  | Percent((Percent s | Lparen s | Rparen s) as f) ->
    fun kont ->
      deep_printer_s_elt s (fun b k -> kprint_char (char_of_f f) (kont b k))
  | OTHERS s ->
    fun kont ->
      deep_printer_s_elt s (fun b k -> kprint_char CHAR (kont b k))

let printer (type t) (Start f:t format) : t =
  toplevel_printer_s f ()
