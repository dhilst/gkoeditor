[@@@warning "-unused-open"]
[@@@warning "-unused-var"]
open Core


type error = (unit, string) Result.t
exception Error_ex of string
type chtype = Curses.chtype

let sexp_of_chtype (chtype : chtype) : Sexp.t =
  [%sexp_of: int] chtype

let chtype_of_sexp (sexp : Sexp.t) : chtype =
  int_of_sexp sexp

let raise_err (flag : Curses.err) = if not flag then raise @@ Error_ex "ncurses error"

module Line = struct
  type t = string

  let contents (_ : t) : string = failwith "not implemented"
  let add_char_at (_ : t) (_ : chtype) (_ : int) : t = failwith "not implemented"
  let make () : t = failwith "not implemented"
end

module Cursor = struct
  type t = int * int
  type direction = [`Up | `Down | `Left | `Right] [@@deriving sexp]

  let get_line (_ : t) : int = failwith "not implemented"
  let get_column (_ : t) : int = failwith "not implemented"
  let move (_ : t) (_ : direction) : t = failwith "not implemented"
  let make () : t = failwith "not implemented"
end

module Input = struct
  type key =
    | ESC
    | BACKSPACE
    | DEL
    | KEY of chtype
  [@@deriving sexp]

  type action =
    | QUIT
    | ADD_CHAR of chtype
    | DEL_CHAR_FW
    | DEL_CHAR_BW
    | MOVE_CURSOR of Cursor.direction
  [@@deriving sexp]

  let key_of_chtype (c : chtype) : key =
    match Curses.keyname c with
    | "^[" -> ESC
    | _ -> KEY c

  let action_of_key (key : key) : action =
    Printf.printf "key %s" @@ Sexp.to_string (sexp_of_key key);
    match key with
    | ESC -> QUIT
    | BACKSPACE -> DEL_CHAR_FW
    | DEL -> DEL_CHAR_BW
    | KEY c -> ADD_CHAR c

  let action_of_chtype c =
    key_of_chtype c |> action_of_key
end

module Buffer = struct
  type bufcontent = Line.t list
  type filetype
  type 'a result = ('a, string) Result.t

  type dirty = Dirty | Clean
  type t = {
    file : filetype option;
    line : int;
    column : int;
    contents : bufcontent;
    dirty : dirty;
  }

  let open_ (_ : string) : unit result = failwith "not implemented"
  let save (_ : t) : t result = failwith "not implemented"
  let make () : t = {
    file = None;
    line = 1;
    column = 1;
    contents = [];
    dirty = Clean;
  }
end

module Window = struct
  type t = {
    buffer : Buffer.t;
    cwindow : Curses.window;
  }

  let open_file (_ : string) : t = failwith "not implemented"
  let redraw (_ : t) : unit = failwith "not implemented"

  let make mainwin : t = {
    buffer = Buffer.make ();
    cwindow = let (maxy, maxx) = Curses.getmaxyx mainwin in
      (* last two lines are for status and command areas *)
      Curses.derwin mainwin 0 0 (maxy - 2) maxx;
  }

  let get_input (w : t) : chtype =
    let open Curses in
    let input = wgetch w.cwindow in
    print_endline (unctrl input);
    input


  let handle_input input : (Input.action, string) Result.t =
    let action = Input.action_of_chtype input in
    Ok (action)
end

module MainArea = struct
  type t = {
    windows : Window.t list;
    activewin : Window.t
  }

  let make mainwin : t = {
    windows = [];
    activewin = Window.make mainwin;
  }
end

module StatusArea = struct
  type t = {
    cwindow : Curses.window
  }

  let make mainwin : t =
    let (maxy, maxx) = Curses.getmaxyx mainwin in
    let cwindow = Curses.derwin mainwin (maxy - 3) 0 (maxy - 2) maxx in
    {
      cwindow
    }

  let dispatch_input t _input =
    let open Curses in
    waddstr t.cwindow "foo" |> ignore
end

module CommandArea = struct
  type t = {
    cwindow : Curses.window
  }

  let make mainwin : t =
    let (maxy, maxx) = Curses.getmaxyx mainwin in
    let cwindow = Curses.derwin mainwin (maxy - 2) 0 (maxy - 1) maxx in
    {
      cwindow
    }

end

module CommandLineArgs = struct
  type t = {
    debugkeys : bool
  }

  let make () : t =
    let debugkeys = ref false in
    let speclist = [("-debugkeys", Arg.Set debugkeys, "Print keys in status area");] in
    let usage = "gkoseditor" in
    Arg.parse speclist (fun _ -> ()) usage;
    {
      debugkeys = !debugkeys;
    }

end

type t = {
  mainarea : MainArea.t;
  statusarea : StatusArea.t;
  commandarea : CommandArea.t;
  args : CommandLineArgs.t;
  mainwindow : Curses.window;
  activewin : Window.t
}

let open_file (_ : t) (_path : string) : error = failwith "not implemented"
let redraw_all (_ : t) : unit = failwith "not implemented"
let initmainwin () : Curses.window =
  let open Curses in
  let win = initscr () in
  cbreak () |> raise_err;
  noecho () |> raise_err;
  win

let make () : t =
  let mainwin = initmainwin () in
  let mainarea = MainArea.make mainwin in {
    mainarea = mainarea;
    statusarea = StatusArea.make mainwin;
    commandarea = CommandArea.make mainwin;
    args = CommandLineArgs.make () ;
    mainwindow = mainwin;
    activewin = mainarea.activewin;
  }


let handle_action _editor _action : error = failwith "not implemented"
let dispatch _editor action : unit =
  match action with
  | _ -> ()

let quit _ =
  let open Curses in
  endwin ()

let refresh editor =
  Curses.wrefresh editor.mainarea.activewin.cwindow |> raise_err;
  Curses.wrefresh editor.commandarea.cwindow |> raise_err;
  Curses.wrefresh editor.statusarea.cwindow |> raise_err

let rec loop (editor : t) : unit =
  let input = Window.get_input editor.activewin in
  StatusArea.dispatch_input editor.statusarea input;
  let result = Window.handle_input input in
  begin match result with
    | Ok action -> begin match action with
        | QUIT -> quit editor
        | _ -> begin
            dispatch editor action;
            refresh editor;
            loop editor
          end
      end
    | Error e -> raise (Error_ex e)
  end


let main' () =
  let editor = make () in
  try
    loop editor
  with
  | ex -> quit editor; raise ex


let main () =
  let open Curses in
  let mainwin = initscr () in
  cbreak () |> raise_err;
  noecho () |> raise_err;
  keypad mainwin true |> raise_err;

  let subwin1 = derwin mainwin 3 100 0 0 in
  let subwin2 = derwin mainwin 10 100 3 0 in
  let acs = get_acs_codes () in
  box subwin1 acs.vline acs.hline;
  box subwin2 acs.vline acs.hline;

  wmove subwin1 1 1 |> raise_err;

  refresh () |> raise_err;

  let (y, x) =  getmaxyx mainwin in
  waddstr subwin2 (Printf.sprintf "%d %d" y x) |> raise_err;
  while true do
    let ch = wgetch subwin1 in
    waddch subwin1 ch |> raise_err;
    waddch subwin2 ch |> raise_err;
    refresh () |> raise_err ;
  done
