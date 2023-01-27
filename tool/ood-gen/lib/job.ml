type metadata = {
  title : string;
  link : string;
  location : string;
  publication_date : string option;
  company : string;
  company_logo : string;
}
[@@deriving of_yaml]

let all () =
  let job_date j = Option.value ~default:"1970-01-01" j.publication_date in
  Utils.yaml_sequence_file metadata_of_yaml "jobs.yml"
  |> List.sort (fun j1 j2 -> compare (job_date j2) (job_date j1))

let pp ppf v =
  Fmt.pf ppf
    {|
  { title = %a
  ; link = %a
  ; location = %a
  ; publication_date = %a
  ; company = %a
  ; company_logo = %a
  }|}
    Pp.string v.title Pp.string v.link Pp.string v.location
    (Pp.option Pp.string) v.publication_date Pp.string v.company Pp.string
    v.company_logo

let pp_list = Pp.list pp

let template () =
  Format.asprintf
    {|
type t =
  { title : string
  ; link : string
  ; location : string
  ; publication_date : string option
  ; company : string
  ; company_logo : string
  }
  
let all = %a
|}
    pp_list (all ())
