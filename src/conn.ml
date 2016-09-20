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


let client url handler =
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt_body.to_string >|= handler
