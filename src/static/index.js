// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );

function logLocalStorage() {
  console.warn(localStorage);
}

function resetLocalStorage() {
  localStorage.removeItem('_id');
  localStorage.removeItem('token');
}

function getStoredAuthData() {
  logLocalStorage();

  // var initConfig = {
  //   elmBackendUrl: 'http://localhost:5000',
  //   nodeBackendUrl: 'http://localhost:4040',
  // };

  var initConfig = {
    elmBackendUrl: 'https://ent-dev-frontend.herokuapp.com',
    nodeBackendUrl: 'https://ent-dev-backend.herokuapp.com',
  };

  var storedId = localStorage.getItem('_id');
  var storedToken = localStorage.getItem('token');

  if (storedId && storedToken) {
    initConfig._id = storedId;
    initConfig.token = storedToken;
  }

  return initConfig;
}

function setupElmPorts(elmApp) {
  elmApp.ports.saveCreds.subscribe(function(creds) {
    console.warn("JS got msg from Elm: saveCreds", creds);
    if (!creds.userId || !creds.token) {
      return console.Error("saveCreds port some arguments are undefined");
    }
    localStorage.setItem('_id', creds.userId);
    localStorage.setItem('token', creds.token);
  });

  elmApp.ports.logout.subscribe(function() {
    console.warn("JS got msg from Elm: logout");
    resetLocalStorage();
  });
}

var elmApp = Elm.Main.fullscreen(getStoredAuthData());
setupElmPorts(elmApp);