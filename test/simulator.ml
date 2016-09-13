open Paxos

let lst_remove item lst =
  let rec remove old_lst new_lst =
    match old_lst with
    | hd :: tl ->
      remove tl (if hd = item then new_lst else hd :: new_lst)
    | _ -> new_lst
  in
  (List.rev (remove lst []))


let test () =
  let server_lst = [
    "http://127.0.0.1:1000/";
    "http://127.0.0.1:2000/";
    "http://127.0.0.1:3000/";
    "http://127.0.0.1:4000/";
    "http://127.0.0.1:5000/";
    "http://127.0.0.1:6000/";
    "http://127.0.0.1:7000/";
    "http://127.0.0.1:8000/";
    "http://127.0.0.1:9000/"
    ] 
  in
  let nodes = List.map (
    fun x -> {
      client = Client.init_state (lst_remove x server_lst); 
      server = Server.init_state ();
    }) server_lst 
  in
  List.iter (fun node -> 
      let rec loop action =
        match action with
        | AskTicket _ ->
          loop (Server.reply node.server action)
        | ReturnTicket _ ->
          loop (Client.reply node.client action)
        | Propose _ ->
          loop (Server.reply node.server action)
        | Answer _ ->
          loop (Client.reply node.client action)
        | Execute _ ->
          loop (Server.reply node.server action)
        | Rest ->
          Rest
      in
      loop (Client.ask_for_ticket node.client)
    ) nodes