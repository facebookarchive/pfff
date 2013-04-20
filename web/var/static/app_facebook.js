
window.fbAsyncInit = function() {
  FB.init({
    appId      : '438273419596119', // App ID
    channelUrl : 'http://dev469.prn1.facebook.com:8080/channel.html',
    status     : true, // check login status
    cookie     : true, // enable cookies to allow the server to access the session
    xfbml      : true  // parse XFBML
  });

  FB.getLoginStatus(function(response) {
      if (response.status === 'connected') {
        //window.alert("connected");
        testAPI();
      } else if (response.status === 'not_authorized') {
        //window.alert("not authorized");
        // not_authorized
        login();
      } else {
        //window.alert("not logged in");
        login();
      }
    });

};

function login() {
  FB.login(function(response) {
      if (response.authResponse) {
        //window.alert("connected");
        testAPI();
      } else {
        window.alert("canceled");
      }
    });
}

function testAPI() {
  console.log('Welcome!  Fetching your information.... ');
  FB.api('/me', function(response) {
      console.log('Good to see you, ' + response.name + '.');
    });
}

function getUserName(f) {
  FB.getLoginStatus(function(response) {
      if (response.status === 'connected') {
        FB.api('/me', function(response) {
            console.log('GetUserName, ' + response.name + '.');
            f(response.name);
          });
      }
    });
}

// Load the SDK Asynchronously
(function(d){
  var js, id = 'facebook-jssdk', ref = d.getElementsByTagName('script')[0];
  if (d.getElementById(id)) {return;}
  js = d.createElement('script'); js.id = id; js.async = true;
  js.src = "//connect.facebook.net/en_US/all.js";
  ref.parentNode.insertBefore(js, ref);
}(document));
