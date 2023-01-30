type metadata = { featured_packages : string list } [@@deriving of_yaml]
type t = metadata

let all () =
  let ( >>= ) = Result.bind in
  Data.read "packages.yml"
  |> Option.to_result ~none:(`Msg "packages.ml: file not found")
  >>= Yaml.of_string >>= metadata_of_yaml
  |> Utils.decode_or_raise Fun.id

let pp ppf t =
  Fmt.pf ppf {|{ featured_packages = %a }|} (Pp.list Pp.string)
    t.featured_packages

let template () =
  Format.asprintf
    {|
type t = { featured_packages : string list }

let all = %a
    |}
    pp (all ())
