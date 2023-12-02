let year = 2023
let day = 2

module Cube_conundrum = struct
  open Base

  module Cube_count = struct
    type t =
      { red : int
      ; green : int
      ; blue : int
      }

    let create ~red ~green ~blue = { red; green; blue }
  end

  module Game = struct
    type t =
      { id : int
      ; reveals : Cube_count.t list
      }

    let create id reveals = { id; reveals }

    let max_reveal_per_color t =
      t.reveals
      |> List.fold
           ~f:(fun acc (reveal : Cube_count.t) ->
             { red = max acc.red reveal.red
             ; green = max acc.green reveal.green
             ; blue = max acc.blue reveal.blue
             })
           ~init:(Cube_count.create ~red:0 ~green:0 ~blue:0)
    ;;

    let is_playable t ~(available_cubes : Cube_count.t) =
      let required_cubes = max_reveal_per_color t in

      required_cubes.red <= available_cubes.red
      && required_cubes.green <= available_cubes.green
      && required_cubes.blue <= available_cubes.blue
    ;;

    let parse_reveal s =
      String.split s ~on:','
      |> List.fold
           ~f:(fun (acc : Cube_count.t) reveal ->
             match reveal |> String.strip |> String.split ~on:' ' with
             | [ cubes; "red" ] -> { acc with red = Int.of_string cubes }
             | [ cubes; "green" ] -> { acc with green = Int.of_string cubes }
             | [ cubes; "blue" ] -> { acc with blue = Int.of_string cubes }
             | _ -> acc)
           ~init:(Cube_count.create ~red:0 ~green:0 ~blue:0)
    ;;

    let parse s =
      match String.split s ~on:':' with
      | [ id; reveals ] ->
        let id = String.split id ~on:' ' |> List.last_exn |> Int.of_string in
        let reveals = reveals |> String.split ~on:';' |> List.map ~f:parse_reveal in

        Some (create id reveals)
      | _ -> None
    ;;
  end

  (* Part 1 *)
  let solve input =
    let bag = Cube_count.create ~red:12 ~green:13 ~blue:14 in

    let playable_games =
      input
      |> String.split ~on:'\n'
      |> List.filter_map ~f:Game.parse
      |> List.filter ~f:(Game.is_playable ~available_cubes:bag)
    in

    playable_games |> List.fold ~f:(fun sum ({ id; _ } : Game.t) -> sum + id) ~init:0
  ;;

  (* Part 2 *)
  let solve_2 input =
    let games = input |> String.split ~on:'\n' |> List.filter_map ~f:Game.parse in

    games
    |> List.fold
         ~f:(fun sum (game : Game.t) ->
           let cubes = Game.max_reveal_per_color game in

           sum + (cubes.red * cubes.green * cubes.blue))
         ~init:0
  ;;
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    Ok (Cube_conundrum.solve input |> Int.to_string)
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    Ok (Cube_conundrum.solve_2 input |> Int.to_string)
  ;;
end
