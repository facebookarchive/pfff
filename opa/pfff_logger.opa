//****************************************************************************
// Prelude
//****************************************************************************
/*
 * My poor's man version of scribe and ODS ... to log the use of the different
 * pfff tools: codemap, codequery, sgrep, spatch, cmf (--fix, --strict/--bugs,
 * --deadcode, -mv_module), etc
 *
 * alternatives:
 *  - use RPC (thrift), which would be more typed than abusing urls and
 *    json
 *  - use ocamlnet
 */

//****************************************************************************
// The db
//****************************************************************************

type command_name = string
type command_run = {
   string unixname,
   string extra_args,
   Date.date date
}

// command_name -> list(command_run)
database stringmap(list(command_run)) /db1
database /db1[_] = []

database int /counter = 0

//****************************************************************************
// Stringof
//****************************************************************************

function string_of_command_run(command_run x) {
  "{Date.to_debug_string(x.date)}| " ^
  "{x.unixname}|" ^
  "args = {x.extra_args}"
}

//****************************************************************************
// Query
//****************************************************************************

function view_list() {
  strmap = /db1;
  //henry: is this efficient?
  keys = Map.To.key_list(strmap);
  String.concat("\n", keys)
}

function view(string cmd) {
  xs = /db1[cmd];
  ys = List.rev(List.map(string_of_command_run, xs));
  String.concat("\n", ys)
}

//****************************************************************************
// Save
//****************************************************************************
function save_command_run(command_name cmd, option(RPC.Json.json) json) {
  match (json) {
  //henry: why can't write deep pattern? get some "cyclic type error" :(
  case {some: json}:
    match (json) {
    case {Record: [("unixname", x), ("extra_args", y)]}:
      match ((x, y)) {
      case ({String: s1}, {String:s2}):
        entry = { unixname: s1, extra_args: s2, date: Date.now() }
        /counter <- /counter + 1;
        
        /db1[cmd] <- [ entry | /db1[cmd] ];
        {success}
      default:
        {bad_request}
      }
    default:
      {bad_request}
    }
  default:
    {bad_request}
  }
}

//****************************************************************************
// Web visualization
//****************************************************************************

//****************************************************************************
// The router
//****************************************************************************

function start(Uri.relative url) {
  match (url) {
  case {path: [] ... }: 
    Resource.page("Pfff", <h1>Pfff</h1>)

  case {path: ["_rest_", command] ...}:
    match (HttpRequest.get_method()) {
    case {some: { post } } :
     Resource.raw_status(
       save_command_run(command, HttpRequest.get_json_body())
     )
    default:
      Resource.raw_status({bad_request})
    }

  case {path: ["view", "counter"] ...}:
    Resource.raw_text("counter = {/counter}\n")
  case {path: ["view", "list"] ...}:
    Resource.raw_text(view_list())
  case {path: ["view", cmd] ...}:
    Resource.raw_text(view(cmd))

  default:
    Resource.raw_status({bad_request})
   }
}


Server.start(Server.http, [
   { dispatch: start }
 ]
)
