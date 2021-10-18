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


module Buffer = struct
  type t = private string

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
