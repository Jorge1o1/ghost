type player =  PlayerOne | PlayerTwo
type state={
  corpus: string list;
  curr_player: player;
  fragment: string;
  turn: int;
  winner : player option
}
type fragstatus = FullWord | ValidFragment | DeadEnd

let invert_player player =
  match player with
  | PlayerOne -> PlayerTwo
  | PlayerTwo -> PlayerOne

let string_of_player player =
  match player with
  | PlayerOne -> "PlayerOne"
  | PlayerTwo -> "PlayerTwo"

let print_state state =
  Printf.printf "Turn %i. Current Player %s. Word: %s\n" state.turn (string_of_player state.curr_player) state.fragment

let read_corpus () =
  (In_channel.with_open_text "/Users/jlm/repos/ghost/data/corpus.txt" In_channel.input_lines)
  |> List.map String.trim

let check_fragment fragment corpus =
  match List.mem fragment corpus with
  | true -> FullWord
  | false ->
    match List.find_opt (String.starts_with ~prefix:fragment) corpus with
    | Some _ -> ValidFragment
    | None -> DeadEnd

let end_game winner =
  Printf.printf "Congratulations %s, You Win!\n" (string_of_player winner)

let apply state inp =
  let newfrag = state.fragment ^ inp in
  match check_fragment newfrag state.corpus with
  | FullWord | DeadEnd -> {state with winner = Some (invert_player state.curr_player); fragment = newfrag}
  | ValidFragment ->
    match state.curr_player with
    | PlayerOne -> {state with curr_player = PlayerTwo; turn = state.turn + 1; fragment = newfrag}
    | PlayerTwo -> {state with curr_player = PlayerOne; turn = state.turn + 1; fragment = newfrag}

let rec prompt_human () =
  print_string "Awaiting Input: ";
  let inp = read_line () in
  let retry () = (Printf.printf "Invalid Input: %S\n" inp) |> prompt_human in
  try
  (match String.length inp with
    | 1 -> inp
    | _ -> raise (Invalid_argument "Bad input"))
  with _ -> retry ()

let prompt_robot state =
  let try_fragment c =
    let newfrag = (state.fragment ^ (String.make 1 c)) in
    match check_fragment newfrag state.corpus with
    | FullWord | DeadEnd -> None
    | ValidFragment -> Some (String.make 1 c)
  in
  let best = String.to_seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Seq.find_map try_fragment in
  match best with
  | Some inp -> inp
  | None -> "?"

let prompt state =
  match state.curr_player with
  | PlayerOne -> prompt_human ()
  | PlayerTwo -> prompt_robot state

let rec loop state =
  print_state state;
  match state.winner with
  | Some w -> end_game w
  | None -> loop (apply state (prompt state))

let main () =
  print_endline "Welcome to Ghost!";
  let initial_state = {
    curr_player = PlayerOne;
    fragment = "";
    turn = 1;
    corpus = read_corpus ();
    winner = None}
  in
  loop initial_state

let _ = main ()
