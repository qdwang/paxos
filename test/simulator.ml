open Paxos


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
    ] in
  let states = List.map (fun x -> {client = Client.init_state server_lst; server = Server.init_state ();}) server_lst in
  ()