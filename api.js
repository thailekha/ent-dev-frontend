var express = require('express');
var bodyParser = require('body-parser');
var path = require('path');
var server = express();
var proxy = require('express-http-proxy');
var cors = require('cors');

server.set('port', (process.env.PORT || 5000));
server.use(cors());
server.use(bodyParser.json()); // support json encoded bodies
server.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies
server.use('/',express.static(path.join(__dirname, 'dist')));

server.use('/proxy', proxy('https://scraper601.herokuapp.com', {
	userResDecorator: function(proxyRes, proxyResData, userReq, userRes) {
    data = JSON.parse(proxyResData.toString('utf8'));
    if (data && data.coinranking && data.coinranking.data && data.coinranking.data.map) {
      data.coinranking.data = data.coinranking.data.map(function(d) {
        d.change = d.change + "";
        return d;
      });
    }
    return JSON.stringify(data);
  }
}));

server.listen(server.get('port'), function() {
  console.log('Node app is running at', server.get('port'));
});