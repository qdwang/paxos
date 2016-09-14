open Lwt
open Cohttp
open Cohttp_lwt_unix

let simple_server port =
  let cb _ _ _ = Server.respond_string ~status:`OK ~body:"Hello World" () in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback:cb ())
  
let server port handler =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt_body.to_string >|= handler (uri, meth, headers)
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())


let body =
  Client.get (Uri.of_string "http://127.0.0.1:8000/") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body
