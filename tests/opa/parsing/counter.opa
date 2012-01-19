
database { 
  int /counter = 0
};

function action(_) {
  /counter <- /counter + 1;
  #msg = <>Hello, user number {/counter}!</>;
}
function page() {
  <h1 onclick={action}>Hello, world</h1>
  <div id="msg">Click the header</div>
}

Server.start(
  Server.http,
  { ~page, title: "Hello, world" }
)
