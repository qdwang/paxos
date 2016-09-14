open Str

type action = 
  | AskTicket of int
  | ReturnTicket of int * string
  | Propose of int * string
  | Answer of bool
  | Execute of string
  | Rest

let action_serialize action = 
  match action with
  | AskTicket ticket_num -> "AskTicket|" ^ string_of_int ticket_num
  | ReturnTicket (ticket_store, command) -> "ReturnTicket|" ^ string_of_int ticket_store ^ "|" ^ command
  | Propose (ticket_num, command) -> "Propose|" ^ string_of_int ticket_num ^ "|" ^ command
  | Answer result -> "Answer|" ^ string_of_bool result
  | Execute instruction -> "Execute|" ^ instruction
  | Rest -> "Rest"

let action_deserialize action_str = 
  let action_str_lst = split (regexp_string "|") action_str in
  match action_str_lst with
  | "AskTicket" :: ticket_num_str :: _ -> 
    AskTicket (int_of_string ticket_num_str)
  | "ReturnTicket" :: ticket_store_str :: command :: _ ->
    ReturnTicket (int_of_string ticket_store_str, command)
  | "Propose" :: ticket_num_str :: command :: _ ->
    Propose (int_of_string ticket_num_str, command)
  | "Answer" :: result :: _ ->
    Answer (bool_of_string result)
  | "Execute" :: instruction :: _ ->
    Execute instruction
  | "Rest" :: _ ->
    Rest
  | _ -> raise (Failure ("action_deserialize with unknown action string: " ^ action_str))

module Client = struct
  type state = {
    mutable server_lst: string list;
    mutable returned_tickets: (int * string) list;
    mutable answers: string list;
    mutable command: string;
    mutable ticket_num: int;
  }

  let pick_largest lst =
    let rec pick lst elem largest_ts = 
      match lst with
      | hd :: tl ->
        let ticket_store, _ = hd in 
        let (e, lt) = 
          if ticket_store > largest_ts then 
            (hd, ticket_store) 
          else
            (elem, largest_ts) 
        in
        pick tl e lt 
      | _ -> elem
  in
  pick lst (0, "") 0

  let init_state server_lst = {
    server_lst = server_lst;
    returned_tickets = [];
    answers = [];
    command = "";
    ticket_num = 0;
  }

  let ask_for_ticket s =
    s.ticket_num <- s.ticket_num + 1;
    AskTicket s.ticket_num
    
  let return_ticket_callback s =
    if (List.length s.server_lst) / (List.length s.returned_tickets) < 2 then
      let (ticket_store, cmd) = pick_largest s.returned_tickets in
      if ticket_store > 0 then s.command <- cmd;
      Propose (s.ticket_num, s.command)
    else
      Rest
        
  let answer_callback s = 
    if (List.length s.server_lst) / (List.length s.answers) < 2 then
      Execute s.command
    else
      Rest

  let reply s action = 
    match action with
    | ReturnTicket (ticket_store, command) ->
      s.returned_tickets <- (ticket_store, command) :: s.returned_tickets;
      return_ticket_callback s
    | Answer true ->
      s.answers <- "" :: s.answers;
      answer_callback s
    | _ -> Rest

  
end



module Server = struct
  type state = {
    name: string;
    mutable ticket_max: int;
    mutable command: string;
    mutable ticket_store: int;
  }

  let detect_name name s =
    s.name = name

  let init_state name = {
    name = name;
    ticket_max = 0;
    command = "";
    ticket_store = 0;  
  }

  let listen_for_ticket ticket_num s =
    if ticket_num > s.ticket_max then
      let _ = s.ticket_max <- ticket_num in
      ReturnTicket (s.ticket_store, s.command)
    else
      Rest
      
  let listen_for_propose s ticket_num command =
    if ticket_num = s.ticket_max then
      let _ = s.command <- command in
      let _ = s.ticket_store <- ticket_num in
      Answer true
    else
      Rest


  let reply s action = 
    match action with
    | AskTicket ticket_num ->
      listen_for_ticket ticket_num s
    | Propose (ticket_num, command) ->
      listen_for_propose s ticket_num command
    | Execute _ ->
      Rest (* TODO: need the interpreter to execute the command *)
    | _ -> Rest

end



type states_of_node = {
  client: Client.state;
  server: Server.state;
}