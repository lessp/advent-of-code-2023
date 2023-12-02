open Base

let year = 2023
let day = 2

module Cube_conundrum = struct
  module Cube_count = struct
    type t =
      { red : int
      ; green : int
      ; blue : int
      }

    let create ~red ~green ~blue = { red; green; blue }
    let empty () = create ~red:0 ~green:0 ~blue:0
  end

  module Game = struct
    type t =
      { id : int
      ; reveals : Cube_count.t list
      }

    let create ~id ~reveals = { id; reveals }

    let max_reveal_per_color t =
      t.reveals
      |> List.fold
           ~f:(fun acc (reveal : Cube_count.t) ->
             { red = max acc.red reveal.red
             ; green = max acc.green reveal.green
             ; blue = max acc.blue reveal.blue
             })
           ~init:(Cube_count.empty ())
    ;;

    let is_playable t ~(available_cubes : Cube_count.t) =
      let required_cubes = max_reveal_per_color t in

      required_cubes.red <= available_cubes.red
      && required_cubes.green <= available_cubes.green
      && required_cubes.blue <= available_cubes.blue
    ;;

    (* Parsing *)
    let parse_reveal s =
      String.split s ~on:','
      |> List.fold
           ~f:(fun (cubes : Cube_count.t) reveal ->
             match reveal |> String.strip |> String.split ~on:' ' with
             | [ count; "red" ] -> { cubes with red = Int.of_string count }
             | [ count; "green" ] -> { cubes with green = Int.of_string count }
             | [ count; "blue" ] -> { cubes with blue = Int.of_string count }
             | _ -> cubes)
           ~init:(Cube_count.empty ())
    ;;

    let parse s =
      match String.split s ~on:':' with
      | [ id; reveals ] -> begin
        match String.split id ~on:' ' with
        | [ "Game"; id ] ->
          let id = Int.of_string id in
          let reveals = reveals |> String.split ~on:';' |> List.map ~f:parse_reveal in

          Some (create ~id ~reveals)
        | _ -> None
      end
      | _ -> None
    ;;
  end
end

module Part_1 = struct
  open Cube_conundrum

  let run (input : string) : (string, string) Result.t =
    let bag = Cube_count.create ~red:12 ~green:13 ~blue:14 in

    let ids_summed =
      input
      |> String.split ~on:'\n'
      |> List.filter_map ~f:Game.parse
      |> List.filter ~f:(Game.is_playable ~available_cubes:bag)
      |> List.fold ~f:(fun sum ({ id; _ } : Game.t) -> sum + id) ~init:0
    in

    Ok (Int.to_string ids_summed)
  ;;
end

module Part_2 = struct
  open Cube_conundrum

  let run (input : string) : (string, string) Result.t =
    let sum =
      input
      |> String.split ~on:'\n'
      |> List.filter_map ~f:Game.parse
      |> List.fold
           ~f:(fun sum (game : Game.t) ->
             let cubes = Game.max_reveal_per_color game in

             sum + (cubes.red * cubes.green * cubes.blue))
           ~init:0
    in

    Ok (Int.to_string sum)
  ;;
end
