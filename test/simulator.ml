open Paxos
open Conn
open Str
open Client

let lst_remove item lst =
  let rec remove old_lst new_lst =
    match old_lst with
    | hd :: tl ->
      remove tl (if hd = item then new_lst else hd :: new_lst)
    | _ -> new_lst
  in
  (List.rev (remove lst []))

let create_servers server_lst =
  let server_state_lst = List.map Server.init_state server_lst in
  Lwt.join (List.map (
    fun name -> 
      server (int_of_string name) (
        fun (uri, _, _) _ ->
          match List.rev (split (regexp_string "/") uri) with
          | client_action :: _ -> 
            let client_action = Uri.pct_decode client_action in
            action_serialize (Server.reply (List.find (Server.detect_name name) server_state_lst) (action_deserialize client_action))
          | _ -> ""
      )
  ) server_lst)


let test () =
  let server_lst = [
    "8001";
    "8002";
    "8003";
  ] in
  let c = Client.init_state server_lst "blablabla" in
  let gen_urls (sl: string list option) cmd =
    List.map (fun port -> "http://127.0.0.1:" ^ port ^ "/" ^ cmd)
             (match sl with | None -> server_lst | Some x -> x)
  in
  let rec client_router body = 
    let reply = Client.reply c (action_deserialize body) in
    let msg = action_serialize reply in
    (*print_endline body;    
    print_endline msg;    *)
    match reply with
    | Propose _ ->
      Lwt_main.run (Lwt.join (List.map (fun url -> client url client_router) (gen_urls (Some (List.map (fun (_,_,x) -> x) c.returned_tickets)) msg)))
    | Execute _ ->
      print_endline "====================";    
      Lwt_main.run (Lwt.join (List.map (fun url -> client url client_router) (gen_urls None msg)))
    | _ -> ()
  in
  let first_msg = action_serialize (Client.ask_for_ticket c) in
  Lwt_main.run (Lwt.join [
    (create_servers server_lst); 
    Lwt.join (List.map (fun url -> client url client_router) (gen_urls None first_msg))
  ])

let benchmark_cohttp () =
  Lwt_main.run (simple_server 8000)