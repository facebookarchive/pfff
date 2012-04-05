//****************************************************************************
// Prelude
//****************************************************************************
/*
 * My poor's man version of scribe and ODS ... to log the use of the different
 * pfff tools: codemap, codequery, sgrep, spatch, cmf (--fix, --strict/--bugs,
 * --deadcode, -mv_module), etc
 *
 * It's a good OPA exercice: need a state (db), some simple
 * visualization (web ui), and it needs to be accessible from anywhere
 * (web service). No need to install any special things on the machine 
 * of the user (does it handle scribe, how to aggregate scribe of
 * all machines, etc).
 *
 * history:
 *  - opti: need to use /db[cmd][date] cos reinsert full list is too slow
 *  
 *
 * alternatives:
 *  - log in a file on a shared NFS dir and then elsewhere do the aggregation
 *  - use RPC (thrift), which would be more typed than abusing urls and json
 *  - use couchdb or mongodb, via OPA api, or just curl them?
 *  - use couchdb or mongodb directly without OPA at all ...
 *  - use ocamlnet
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
   // todo: do a generic key/value list
   string extra_args,
   Date.date date
}

// command_name -> date -> command_run
database stringmap(intmap(command_run)) /db1

database int /counter = 0

//****************************************************************************
// Stringof
//****************************************************************************

function string_of_command_run(command_run x) {
  "{Date.to_debug_string(x.date)}| {x.unixname}| args = {x.extra_args}"
}

//****************************************************************************
// Query
//****************************************************************************

function users_of_cmd(string cmd, (command_run -> bool) pred) {
  intmap = /db1[cmd];
  vals = Map.To.val_list(intmap);
  vals = List.filter(pred, vals);
  users = List.map(function (x) { x.unixname }, vals);
  set = Set.From.list(users);
  set
}

function view_all_cmds() {
  strmap = /db1;
  keys = Map.To.key_list(strmap);
  keys_with_stats = List.map(function (cmd) {
    users = users_of_cmd(cmd, (function(_) { true }));
    (cmd, Set.size(users))
    }, keys);
  keys_with_stats_before = List.map(function (cmd) {
    users = users_of_cmd(cmd, (function(x) {
      Date.in_milliseconds(x.date) < Date.in_milliseconds(Common.last_week())
    }));
    (cmd, Set.size(users))
    }, keys);

  String.concat("\n", 
    List.map(function ((cmd, n)) {
      now = n;
      before = 
        match (List.assoc(cmd, keys_with_stats_before)) {
          // none should never happen actually, cos all past commands
          // in keys_with_stats_before are also in keys_with_stats
          // are also in the most recent
          case {none}: "IMPOSSIBLE"
          case {some: x}: {
            diff = n - x;
            prefix = if (diff > 0) { "+" } else { "" };
            if (diff == n) { "NEW" } else { "{prefix}{diff}" }
          }
        };
    "{cmd}({now}, {before})"
    }, keys_with_stats))
}

function view_cmd(string cmd) {
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
        //todo: apparently you can also do /db1[cmd] <+ entry;
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
    Resource.raw_text(view_all_cmds())
  case {path: ["view", cmd] ...}:
    Resource.raw_text(view_cmd(cmd))

  default:
    Resource.raw_status({bad_request})
   }
}

Server.start(Server.http, [
   { dispatch: start }
 ]
)
