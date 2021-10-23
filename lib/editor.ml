[@@@warning "-unused-open"]
open Core


type error = (unit, string) Result.t
exception Error_ex of string
type chtype = Curses.chtype

module Line = struct
  type t = string

  let contents (_ : t) : string = failwith "not implemented"
  let add_char_at (_ : t) (_ : chtype) (_ : int) : t = failwith "not implemented"
  let make () : t = failwith "not implemented"
end

module Cursor = struct
  type t = int * int
  type direction = [`Up | `Down | `Left | `Right]

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

  type action =
    | QUIT
    | ADD_CHAR of chtype
    | DEL_CHAR_FW
    | DEL_CHAR_BW
    | MOVE_CURSOR of Cursor.direction

  let key_of_chtype (c : chtype) : key =
    match Curses.keyname c with
    | "^[" -> ESC
    | s -> Printf.printf "Key => %s" s; KEY c

  let action_of_key (key : key) : action =
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

  let getch (w : t) : chtype =
    let open Curses in
    wgetch w.cwindow

  let handle_input (w : t) : (Input.action, string) Result.t =
    let input = getch w in
    let _action = Input.action_of_chtype input in
    failwith "not implemented"
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
  type t = unit

  let make () : t = ()
end

module CommandArea = struct
  type t = unit

  let make () : t = ()
end

module CommandLineArgs = struct
  type t = unit

  let make () : t = ()
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
  cbreak () |> ignore;
  noecho () |> ignore;
  initscr ()

let make () : t =
  let mainwin = initmainwin () in
  let mainarea = MainArea.make mainwin in {
    mainarea = mainarea;
    statusarea = StatusArea.make ();
    commandarea = CommandArea.make ();
    args = CommandLineArgs.make () ;
    mainwindow = mainwin;
    activewin = mainarea.activewin;
  }

let handle_action _editor _action : error = failwith "not implemented"

let loop (editor : t) : unit = while true do
    let result = Window.handle_input editor.activewin in
    begin
      match result with
      | Error e -> raise (Error_ex e)
      | _ -> ()
    end;
    Curses.wrefresh editor.mainwindow |> ignore;
    Unix.sleep 1000
  done

let reset _ = Curses.endwin ()

let main () =
  let editor = make () in
  try
    loop editor
  with _ -> reset editor
