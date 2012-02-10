//****************************************************************************
// Prelude
//****************************************************************************
/*
 * My poor's man version of scribe and ODS ... to log the use of the different
 * pfff tools: codemap, codequery, sgrep, spatch, cmf (--fix, --strict/--bugs,
 * --deadcode, -mv_module), etc
 *
 * history:
 *  - opti: need to use /db[cmd][date] cos reinsert full list is too slow
 *  
 *
 * alternatives:
 *  - use RPC (thrift), which would be more typed than abusing urls and
 *    json
 *  - use ocamlnet
 *  - use couchdb or mongodb, via OPA api, or just curl them?
 *  - use couchdb or mongodb directly without OPA at all ...
 *
 * todo: too slow, 
 *  - how to efficiently get the keys at the first level (all tools)?
 *  - how to allow concurrent access to the db? need run multiple opa?
 *    use opa-cloud with its load balancer? but is db itself concurrent?
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
database stringmap(intmap(command_run)) /db1

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
  //todo: this is really slow
  keys = Map.To.key_list(strmap);
  String.concat("\n", keys)
}

function view(string cmd) {
  intmap = /db1[cmd];
  vals = Map.To.val_list(intmap);
  ys = List.map(string_of_command_run, vals);
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
        now = Date.now();
        entry = { unixname: s1, extra_args: s2, date: now }
        /counter <- /counter + 1;
        //old: slow  /db1[cmd] <- [ entry | /db1[cmd] ];
        /db1[cmd][Date.in_milliseconds(now)] <- entry;
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
