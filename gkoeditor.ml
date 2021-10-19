open Core
open Curses

module Cursor = struct
  let wmove_rel window y x : unit =
    let (y', x') = getyx window in
    move (y + y') (x' + x) |> ignore
end

module Input = struct
  let wdelch window =
    Cursor.wmove_rel window 0 (-1);
    wdelch window |> ignore
end

type options = {
  input_file : string;
}

module Buffer : sig
  type t

  val make : unit -> t
  val write_char : t -> char -> t
  val read_char : t -> char
  val seek : t -> int -> t
  val contents : t -> string
end = struct
  type t = {
    buffer : Buffer.t;
    mutable pos : int;
    mutable line : int;
    mutable column : int;
  }

  let buffer_size = 4096

  let make () =  {
    buffer = Buffer.create buffer_size;
    pos = 0;
    line = 1;
    column = 1;
  }

  let foo =
    if '\n' = '\n' then
      prit_endline "foo"

  let write_char t ch =
    Buffer.add_char t.buffer ch;
    t.pos <- t.pos + 1;
    if Char.to_string ch = "\n" then begin
      t.line <- t.line + 1;
      t.column <- 1;
    end;
    t

  let read_char t = Buffer.nth t.buffer t.pos

  let seek t pos =
    let pos = min pos 0 in
    if pos < Buffer.length t.buffer then
      t.pos <- pos;
    t

  let contents t = Buffer.contents t.buffer
end


let main_loop window options =
  let exception Break in
  let { input_file } = options in
  begin match Sys.file_exists input_file with
    | `Yes -> let fcontent = In_channel.read_all input_file in
      waddstr window fcontent |> ignore
    |  _ -> ();
  end;
  try while true do
      let input = wgetch window in
      match unctrl input with
      | "^[" -> raise Break
      | "^?" -> Input.wdelch window
      | _ -> waddch window input |> ignore;
        wrefresh window |> ignore;
    done
  with Break ->
    let open Unix in
    let flags = [O_WRONLY; O_CREAT] in
    let fd = openfile ~mode:flags input_file in
    putwin window fd |> ignore

let init () =
  let window = initscr () in
  cbreak () |> ignore;
  noecho () |> ignore;
  window


let cmdline () : options =
  let usage = "gkoeditor filename" in
  let input_file = ref "" in
  let fname f =
    input_file := f in
  let speclist = [] in
  Arg.parse speclist fname usage;
  Printf.printf "%s" !input_file;
  { input_file = !input_file }

let () =
  let options = cmdline () in
  let window = init () in
  main_loop window options;
  endwin ()
