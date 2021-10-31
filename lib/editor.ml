[@@@warning "-unused-open"]
[@@@warning "-unused-var"]
open Core

type error = (unit, string) Result.t
exception Error_ex of string
type chtype = Curses.chtype

(* let sexp_of_chtype (chtype : chtype) : Sexp.t = *)
(*   [%sexp_of: int] chtype *)

(* let chtype_of_sexp (sexp : Sexp.t) : chtype = *)
(*   int_of_sexp sexp *)

let raise_err (flag : Curses.err) = if not flag then raise @@ Error_ex "ncurses error"

(* module Line = struct *)
(*   type t = string *)

(*   let contents (_ : t) : string = failwith "not implemented" *)
(*   let add_char_at (_ : t) (_ : chtype) (_ : int) : t = failwith "not implemented" *)
(*   let make () : t = failwith "not implemented" *)
(* end *)

(* module Cursor = struct *)
(*   type t = int * int *)
(*   type direction = [`Up | `Down | `Left | `Right] [@@deriving sexp] *)

(*   let get_line (_ : t) : int = failwith "not implemented" *)
(*   let get_column (_ : t) : int = failwith "not implemented" *)
(*   let move (_ : t) (_ : direction) : t = failwith "not implemented" *)
(*   let make () : t = failwith "not implemented" *)
(* end *)

(* module Input = struct *)
(*   type key = *)
(*     | ESC *)
(*     | BACKSPACE *)
(*     | DEL *)
(*     | KEY of chtype *)
(*   [@@deriving sexp] *)

(*   type action = *)
(*     | QUIT *)
(*     | ADD_CHAR of chtype *)
(*     | DEL_CHAR_FW *)
(*     | DEL_CHAR_BW *)
(*     | MOVE_CURSOR of Cursor.direction *)
(*   [@@deriving sexp] *)

(*   let key_of_chtype (c : chtype) : key = *)
(*     match Curses.keyname c with *)
(*     | "^[" -> ESC *)
(*     | _ -> KEY c *)

(*   let action_of_key (key : key) : action = *)
(*     Printf.printf "key %s" @@ Sexp.to_string (sexp_of_key key); *)
(*     match key with *)
(*     | ESC -> QUIT *)
(*     | BACKSPACE -> DEL_CHAR_FW *)
(*     | DEL -> DEL_CHAR_BW *)
(*     | KEY c -> ADD_CHAR c *)

(*   let action_of_chtype c = *)
(*     key_of_chtype c |> action_of_key *)
(* end *)

(* module Buffer = struct *)
(*   type bufcontent = Line.t list *)
(*   type filetype *)
(*   type 'a result = ('a, string) Result.t *)

(*   type dirty = Dirty | Clean *)
(*   type t = { *)
(*     file : filetype option; *)
(*     line : int; *)
(*     column : int; *)
(*     contents : bufcontent; *)
(*     dirty : dirty; *)
(*   } *)

(*   let open_ (_ : string) : unit result = failwith "not implemented" *)
(*   let save (_ : t) : t result = failwith "not implemented" *)
(*   let make () : t = { *)
(*     file = None; *)
(*     line = 1; *)
(*     column = 1; *)
(*     contents = []; *)
(*     dirty = Clean; *)
(*   } *)
(* end *)

(* module Window = struct *)
(*   type t = { *)
(*     buffer : Buffer.t; *)
(*     cwindow : Curses.window; *)
(*   } *)

(*   let open_file (_ : string) : t = failwith "not implemented" *)
(*   let redraw (_ : t) : unit = failwith "not implemented" *)

(*   let make mainwin : t = { *)
(*     buffer = Buffer.make (); *)
(*     cwindow = let (maxy, maxx) = Curses.getmaxyx mainwin in *)
(*       (1* last two lines are for status and command areas *1) *)
(*       Curses.derwin mainwin 0 0 (maxy - 2) maxx; *)
(*   } *)

(*   let get_input (w : t) : chtype = *)
(*     let open Curses in *)
(*     let input = wgetch w.cwindow in *)
(*     print_endline (unctrl input); *)
(*     input *)


(*   let handle_input input : (Input.action, string) Result.t = *)
(*     let action = Input.action_of_chtype input in *)
(*     Ok (action) *)
(* end *)

(* module MainArea = struct *)
(*   type t = { *)
(*     windows : Window.t list; *)
(*     activewin : Window.t *)
(*   } *)

(*   let make mainwin : t = { *)
(*     windows = []; *)
(*     activewin = Window.make mainwin; *)
(*   } *)
(* end *)

(* module StatusArea = struct *)
(*   type t = { *)
(*     cwindow : Curses.window *)
(*   } *)

(*   let make mainwin : t = *)
(*     let (maxy, maxx) = Curses.getmaxyx mainwin in *)
(*     let cwindow = Curses.derwin mainwin (maxy - 3) 0 (maxy - 2) maxx in *)
(*     { *)
(*       cwindow *)
(*     } *)

(*   let dispatch_input t _input = *)
(*     let open Curses in *)
(*     waddstr t.cwindow "foo" |> ignore *)
(* end *)

(* module CommandArea = struct *)
(*   type t = { *)
(*     cwindow : Curses.window *)
(*   } *)

(*   let make mainwin : t = *)
(*     let (maxy, maxx) = Curses.getmaxyx mainwin in *)
(*     let cwindow = Curses.derwin mainwin (maxy - 2) 0 (maxy - 1) maxx in *)
(*     { *)
(*       cwindow *)
(*     } *)

(* end *)

(* module CommandLineArgs = struct *)
(*   type t = { *)
(*     debugkeys : bool *)
(*   } *)

(*   let make () : t = *)
(*     let debugkeys = ref false in *)
(*     let speclist = [("-debugkeys", Arg.Set debugkeys, "Print keys in status area");] in *)
(*     let usage = "gkoseditor" in *)
(*     Arg.parse speclist (fun _ -> ()) usage; *)
(*     { *)
(*       debugkeys = !debugkeys; *)
(*     } *)

(* end *)

(* type t = { *)
(*   mainarea : MainArea.t; *)
(*   statusarea : StatusArea.t; *)
(*   commandarea : CommandArea.t; *)
(*   args : CommandLineArgs.t; *)
(*   mainwindow : Curses.window; *)
(*   activewin : Window.t *)
(* } *)

(* let open_file (_ : t) (_path : string) : error = failwith "not implemented" *)
(* let redraw_all (_ : t) : unit = failwith "not implemented" *)
(* let initmainwin () : Curses.window = *)
(*   let open Curses in *)
(*   let win = initscr () in *)
(*   cbreak () |> raise_err; *)
(*   noecho () |> raise_err; *)
(*   win *)

(* let make () : t = *)
(*   let mainwin = initmainwin () in *)
(*   let mainarea = MainArea.make mainwin in { *)
(*     mainarea = mainarea; *)
(*     statusarea = StatusArea.make mainwin; *)
(*     commandarea = CommandArea.make mainwin; *)
(*     args = CommandLineArgs.make () ; *)
(*     mainwindow = mainwin; *)
(*     activewin = mainarea.activewin; *)
(*   } *)


(* let handle_action _editor _action : error = failwith "not implemented" *)
(* let dispatch _editor action : unit = *)
(*   match action with *)
(*   | _ -> () *)

(* let quit _ = *)
(*   let open Curses in *)
(*   endwin () *)

(* let refresh editor = *)
(*   Curses.wrefresh editor.mainarea.activewin.cwindow |> raise_err; *)
(*   Curses.wrefresh editor.commandarea.cwindow |> raise_err; *)
(*   Curses.wrefresh editor.statusarea.cwindow |> raise_err *)

(* let rec loop (editor : t) : unit = *)
(*   let input = Window.get_input editor.activewin in *)
(*   StatusArea.dispatch_input editor.statusarea input; *)
(*   let result = Window.handle_input input in *)
(*   begin match result with *)
(*     | Ok action -> begin match action with *)
(*         | QUIT -> quit editor *)
(*         | _ -> begin *)
(*             dispatch editor action; *)
(*             refresh editor; *)
(*             loop editor *)
(*           end *)
(*       end *)
(*     | Error e -> raise (Error_ex e) *)
(*   end *)


(* let main' () = *)
(*   let editor = make () in *)
(*   try *)
(*     loop editor *)
(*   with *)
(*   | ex -> quit editor; raise ex *)


module type X_int = sig val x : int end

module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end

module Three = struct let x = 3 end
module Four = Increment(Three)

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type Interval_intf = sig
  type t
  type endpoint
  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
  val intersect : t -> t -> t
end

module type Interval_intf_with_sexp = sig
  type t
  include Interval_intf with type t := t
  include Core.Sexpable with type t := t
end

module Make_interval(Endpoint : sig
    type t
    include Comparable with type t := t
    include Core.Sexpable with type t := t
  end) : (Interval_intf with type endpoint := Endpoint.t) = struct

  type t = Interval of Endpoint.t * Endpoint.t | Empty [@@deriving sexp]

  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low, high)

  let is_empty = function
    | Empty -> true
    | Interval _ -> false


  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) ->
      Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0


  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1, t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) ->
      create (max l1 l2) (min h1 h2)
end

module Int_interval = Make_interval(Int)

module Foldable = struct
  module type S = sig
    type 'a t
    val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
  end
  module type Extension = sig
    type 'a t
    val iter : 'a t -> f:('a -> unit) -> unit
    val length : 'a t -> int
    val count : 'a t -> f: ('a -> bool) -> int
    val for_all : 'a t -> f:('a -> bool) -> bool
    val exists : 'a t -> f:('a -> bool) -> bool
  end

  module Extend(Arg : S) : (Extension with type 'a t := 'a Arg.t) = struct
    open Arg
    let iter t ~f =
      fold t ~init:() ~f:(fun () a -> f a)

    let length t =
      fold t ~init:0 ~f:(fun acc _ -> acc + 1)

    let count t ~f =
      fold t ~init:0 ~f:(fun count x -> count + if f x then 1 else 0)

    exception Short_circuit

    let for_all c ~f =
      try iter c ~f:(fun x -> if not (f x) then raise Short_circuit); true
      with Short_circuit -> false

    let exists c ~f =
      try iter c ~f:(fun x -> if f x then raise Short_circuit); false
      with Short_circuit -> true
  end
end

module Queue : sig
  type 'a t

  val empty : 'a t
  val enqueue : 'a t -> 'a -> 'a t
  val dequeue : 'a t -> ('a * 'a t) option
  val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
end = struct
  include Foldable.Extend(Queue)

  type 'a t = 'a list * 'a list
  let empty = ([], [])

  let enqueue (in_list, out_list) x = (x :: in_list, out_list)

  let dequeue (in_list, out_list) =
    match out_list with
    | hd :: tl -> Some (hd, (in_list, tl))
    | [] ->
      match List.rev in_list with
      | [] -> None
      | hd :: tl -> Some(hd, ([], tl))

  let fold (in_list, out_list) ~init ~f =
    let after_out = List.fold ~init ~f out_list in
    List.fold_right ~init:after_out ~f:(fun x acc -> f acc x) in_list
end


module StackM = struct
  module type S = sig
    type element
    type t

    val push : t -> element -> t
    val pop : t -> (element * t) option
    val empty : unit -> t

    exception Empty
    val pop_exn : t -> (element * t)
  end

  module Stack (T : sig type t end) : (S with type element := T.t) = struct
    type element = T.t
    type t = element list

    let push stk e = e :: stk
    let empty () = []
    let pop stk = match stk with
        [] -> None
      | e :: stk -> Some (e, stk)
    exception Empty

    let pop_exn stk = match pop stk with
        Some (e, stk') -> (e, stk')
      | _ -> raise Empty
  end
end

module IntStack = StackM.Stack(struct type t = int end)

module SplitableString : sig
  type t = string

  val insert_at : t -> char -> int -> t
  val replace_at : t -> char -> int -> t
  val delete_at : t -> int -> t
end = struct
  include String
  type t = string

  let insert_at t chr at =
    let len = String.length t in
    if Int.(at < 0) || Int.(at > len) then t
    else
      let t = t |> String.to_list in
      let (beg, end_) = List.split_n t at in
      beg @ [chr] @ end_ |> String.of_char_list

  let replace_at t chr at =
    let len = String.length t in
    let at = (at + 1) in
    if Int.(at <= 0) || Int.(at > len) then t
    else
      let t = t |> String.to_list in
      let (beg, end_) = List.split_n t at in
      let revbeg = List.rev beg in
      let newbeg = match revbeg with
        | [] -> [chr]
        | _ :: tl -> List.rev @@ chr :: tl in
      newbeg @ end_ |> String.of_char_list

  let delete_at t at =
    let len = String.length t in
    let at = (at + 1) in
    if Int.(at <= 0) || Int.(at > len) then t
    else
      let t = t |> String.to_list in
      let (beg, end_) = List.split_n t at in
      let newbeg = match List.rev beg with
        | [] -> []
        | _ :: tl -> List.rev tl in
      newbeg @ end_ |> String.of_char_list
end

module LBuffer = struct
  type t = (int, string, String.comparator_witness) Map.t

  let empty () = Map.empty (module Int)
  let add_line t line str = Map.set t ~key:line ~data:str
  let del_line t line = Map.remove t line
  let get_line t line = Map.find t line

  let insert_char_at t line column chr =
    match get_line t line with
    | None -> add_line t line (Char.to_string chr)
    | Some linestr -> let newline = SplitableString.insert_at linestr chr column in
      add_line t line newline

  let replace_char_at t line column chr =
    match get_line t line with
    | None -> t
    | Some linestr -> let newline = SplitableString.replace_at linestr chr column in
      add_line t line newline

  let delete_char_at t line column =
    match get_line t line with
    | None -> t
    | Some linestr -> let newline = SplitableString.delete_at linestr column in
      add_line t line newline

  let get_lines t start end_ =
    Map.fold_range_inclusive
      t
      ~min:start
      ~max:end_
      ~init:[]
      ~f:(fun ~key:_ ~data acc -> data :: acc)

  let%test_module "LBuffer" = (module struct
    let (=) = Poly.(=)

    let x =
      let x = empty () in
      let x = add_line x 1 "foo" in
      let x = add_line x 2 "bar" in
      let x = add_line x 3 "tar" in
      let x = add_line x 4 "zar" in
      x

    let%test _ = get_lines x 1 4 = ["foo"; "bar"; "tar"; "zar"]
  end)
end

let main () =
  let open Curses in
  let mainwin = initscr () in
  cbreak () |> raise_err;
  noecho () |> raise_err;
  start_color () |> raise_err;
  use_default_colors () |> raise_err;
  (* curs_set 0 |> raise_err; *)
  let (maxy, maxx) = getmaxyx mainwin in

  let w2h = 3 in
  let subwin1 = derwin mainwin (maxy - w2h) maxx 0 0 in
  let subwin2 = derwin mainwin w2h maxx (maxy - w2h) 0 in
  let acs = get_acs_codes () in
  box subwin1 acs.vline acs.hline;
  box subwin2 acs.vline acs.hline;

  init_pair 1 2 (-1)  |> raise_err;

  scrollok subwin1 true;
  keypad subwin1 true |> raise_err;
  (* idlok subwin1 true; *)
  wmove subwin1 1 1 |> raise_err;
  wmove subwin2 1 1 |> raise_err;

  refresh () |> raise_err;

  waddstr subwin2 (Printf.sprintf "%d %d" maxy maxx) |> raise_err;
  while true do
    let ch = wgetch subwin1 in
    wclrtoeol subwin2;
    wmove subwin2 1 1 |> raise_err;
    waddstr subwin2 (Printf.sprintf "%d" ch) |> raise_err;
    (* box subwin2 acs.vline acs.hline; *)
    wrefresh subwin2 |> raise_err;
    let (y, x) = getyx subwin1 in
    let moveup () =
      wmove subwin1 (y - 1) x |> raise_err;
      let str = ref (String.make 1024 ' ') in
      winstr subwin1 !str |> raise_err;
      let lstr = String.to_list !str in
      let len = 10 in
      wmove subwin1 (y - 1) len |> raise_err in
    let moveback () =
      wmove subwin1 y (x - 1) |> raise_err in
    let delchr () = wdelch subwin1 |> raise_err in
    match ch with
    | 127 ->
      begin
        match (y, x) with
        | 1, 1 -> ()
        | _, 1 -> (* first column *) begin
            moveup ();
            delchr ()
          end
        | _, _ -> begin
            moveback ();
            delchr ()
          end;
      end
    | _ ->
      begin
        wattron subwin1 @@ A.color_pair 1;
        waddch subwin1 ch |> raise_err;
        wattroff subwin1 @@ A.color_pair 1;

        let (y, x) = getyx subwin1 in
        if x = 0 then wmove subwin1 y 1 |> raise_err;
      end;
      box subwin1 acs.vline acs.hline;
      wrefresh subwin1 |> raise_err;
  done
