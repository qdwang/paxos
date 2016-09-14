open Paxos
open Conn
open Str

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
    "8004";
    "8005";
    "8006";
    "8007";
    "8008";
    "8009";
    "8010";
    "8011";
  ] in
  Lwt_main.run (create_servers server_lst)

let benchmark_cohttp () =
  Lwt_main.run (simple_server 8000)