// opa hello.opa --
Server.start(
   Server.http, {
    page: function() { <h1>Hello, world</h1> },
    title: "Hello, world"
   }
)
