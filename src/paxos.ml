type action = 
  | AskTicket of string list * int
  | ReturnTicket of int * string
  | Propose of int * string
  | Answer of bool
  | Execute of string
  | DoNothing


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
    AskTicket (s.server_lst, s.ticket_num)
    
  let return_ticket_callback s =
    if (List.length s.server_lst) / (List.length s.returned_tickets) < 2 then
      let (ticket_store, cmd) = pick_largest s.returned_tickets in
      if ticket_store > 0 then s.command <- cmd;
      Propose (s.ticket_num, s.command)
    else
      DoNothing
        
  let answer_callback s = 
    if (List.length s.server_lst) / (List.length s.answers) < 2 then
      Execute s.command
    else
      DoNothing

end



module Server = struct
  type state = {
    mutable ticket_max: int;
    mutable command: string;
    mutable ticket_store: int;
  }

  let init_state () = {
    ticket_max = 0;
    command = "";
    ticket_store = 0;  
  }

  let listen_for_ticket ticket_num s =
    if ticket_num > s.ticket_max then
      let _ = s.ticket_max <- ticket_num in
      ReturnTicket (s.ticket_store, s.command)
    else
      DoNothing
      
  let listen_for_propose s ticket_num command =
    if ticket_num = s.ticket_max then
      let _ = s.command <- command in
      let _ = s.ticket_store <- ticket_num in
      Answer true
    else
      DoNothing

end



type states_of_node = {
  client: Client.state;
  server: Server.state;
}