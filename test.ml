
type 'a start = Start of 'a a

and 'a a =
  | Percent : 'a f -> 'a a
  | I : 'a a -> 'a a
  | C : 'a a -> 'a a
  | Space : 'a a -> 'a a
  | Newline : 'a a -> 'a a
  | End : unit a

and 'a f =
  | I : 'a a -> (int -> 'a) f
  | C : 'a a -> (char -> 'a) f
  | Percent : 'a a -> 'a f

let rec print (Start cons) =
  main cons

and main : type t. t a -> t = function
  | I r ->
    print_string "i";
    main r
  | C r ->
    print_string "c";
    main r
  | Space r ->
    print_string " ";
    main r
  | Newline r ->
    print_string "\n";
    main r
  | End -> ()
  | Percent f ->
    format f

and format : type t. t f -> t = function
  | I r ->
    fun i ->
      print_int i;
      main r
  | C r ->
    fun c ->
      print_char c;
      main r
  | Percent r ->
    print_string "%";
    main r

let (!!) cons = print cons
let (!*) x = x

let v = !* "%i %c"
let v = !* "ici"

let v = !! "%i %c"

let s = !! "%i %c\n" 1 'c'
