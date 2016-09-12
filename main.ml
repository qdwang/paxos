open Lwt
open Paxos

let () =
  Lwt_main.run Conn.server
    