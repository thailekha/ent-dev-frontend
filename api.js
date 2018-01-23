var express = require('express');
var bodyParser = require('body-parser');
var path = require('path');
var server = express();
var request = require('request');
var proxy = require('express-http-proxy');
var cors = require('cors');

server.set('port', (process.env.PORT || 5000));
server.use(cors());
server.use(bodyParser.json()); // support json encoded bodies
server.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies
server.use('/',express.static(path.join(__dirname, 'dist')));

server.use('/proxy', proxy('https://scraper601.herokuapp.com'));

server.listen(server.get('port'), function() {
  console.log('Node app is running at', server.get('port'));
});