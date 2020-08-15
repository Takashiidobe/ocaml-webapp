open Opium.Std
open Core

type message = { message : string }

type excerpt = {
  author : string;
  excerpt : string;
  source : string;
  page : string;
}

(** The route handlers for our app *)

(** Defines a handler that replies to GET requests at the root endpoint *)
let root =
  get "/" (fun _ ->
      `Json Ezjsonm.(dict [ ("message", string "Hello World") ]) |> respond')

let default =
  not_found (fun _ ->
      `Json Ezjsonm.(dict [ ("message", string "Route not found") ]) |> respond')

(** Defines a handler that takes a path parameter from the route

let hello_fallback = get "/hello"
    begin fun _req ->
      respond' @@
      Content.basic_page Html.[p [txt "Hiya"]]
    end

let excerpts_by_author =
  get "/excerpts/author/:name" (fun req ->
      let name = (param req "name")
      let open Lwt in
      Db.Get.excerpts_by_author (param req "name") req
      >>= respond_or_err `Json ()`)


let excerpt_of_form_data data =
  let find data key =
    let open Core in
    (* NOTE Should handle error in case of missing fields *)
    List.Assoc.find_exn ~equal:String.equal data key |> String.concat
  in
  let author  = find data "author"
  and excerpt = find data "excerpt"
  and source  = find data "source"
  and page    = match find data "page" with "" -> None | p -> Some p
  in
  Lwt.return Excerpt.{author; excerpt; source; page}

let post_excerpts_add = post "/excerpts/add" begin fun req ->
    let open Lwt in
    (* NOTE Should handle possible error arising from invalid data *)
    App.urlencoded_pairs_of_body req  >>=
    excerpt_of_form_data              >>= fun excerpt ->
    Db.Update.add_excerpt excerpt req >>=
    respond_or_err (fun () -> Content.excerpt_added_page excerpt)
  end


let excerpts = get "/excerpts" begin fun req ->
    let open Lwt in
    Db.Get.authors req >>=
    respond_or_err Content.author_excerpts_page
  end *)

let routes = [ root; default ]

let add_routes app =
  Core.List.fold ~f:(fun app route -> route app) ~init:app routes
