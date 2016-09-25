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
          print_endline "";
          print_endline "";
          print_endline (name ^ " Incoming:" ^ uri);
          match List.rev (split (regexp_string "/") uri) with
          | client_action :: _ -> 
            let client_action = Uri.pct_decode client_action in
            action_serialize (Server.reply (List.find (Server.detect_name name) server_state_lst) (action_deserialize client_action))
          | _ -> ""
      )
  ) server_lst)

type urls = string list [@@deriving yojson]

let test () =
  let server_lst = [
    "8001";
    "8002";
    "8003";
  ] in
  let gen_urls (sl: string list option) cmd =
    List.map (fun port -> "http://127.0.0.1:" ^ port ^ "/" ^ cmd)
            (match sl with | None -> server_lst | Some x -> x)
  in
  let create_client name cmd = 
    let c = Client.init_state name server_lst cmd in
    let rec client_router body = 
      let reply = Client.reply c (action_deserialize body) in
      let msg = action_serialize reply in
      (*print_endline "====================";    
      print_endline body;    
      print_endline msg;    *)
      print_endline ("Client: " ^ c.name);
      match reply with
      | Propose _ ->
        print_endline ("propose_sent:" ^ string_of_bool c.propose_sent);
        if not c.propose_sent then
          let urls = gen_urls (Some (List.map (fun (_,_,x) -> x) c.returned_tickets)) msg in
          print_endline ("Propose:" ^ (Yojson.Safe.to_string (urls_to_yojson urls)));
          let _ = c.propose_sent <- true in
          Lwt_main.run (Lwt.join (List.map (fun url -> client url client_router) urls))
        else
          ()
      | Execute _ ->
        print_endline ("execute_sent:" ^ string_of_bool c.execute_sent);
        if not c.execute_sent then
          let urls = gen_urls None msg in
          print_endline ("Execute:" ^ (Yojson.Safe.to_string (urls_to_yojson urls)));
          let _ = c.execute_sent <- true in
          Lwt_main.run (Lwt.join (List.map (fun url -> client url client_router) urls))
        else
          ()
      | _ -> ()
    in
    let first_msg = action_serialize (Client.ask_for_ticket c) in
    (first_msg, client_router)
  in
  let (client_1_first_msg, client_1_router) = create_client "C1" "aaaaa" in
  let (client_2_first_msg, client_2_router) = create_client "C2" "bbbbb" in    
  Lwt_main.run (Lwt.join [
    (create_servers server_lst);
    Lwt.join [ 
      Lwt.join (List.map (fun url -> client url client_1_router) (gen_urls None client_1_first_msg));
      Lwt.join (List.map (fun url -> client url client_2_router) (gen_urls None client_2_first_msg))
    ]
  ])

let benchmark_cohttp () =
  Lwt_main.run (simple_server 8000)