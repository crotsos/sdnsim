var net = require('net');
var http = require('http');
var fs = require('fs');
var path = require('path');

var WebSocketServer = require('websocket').server;

http.createServer(function (request, response) {

  console.log('request starting...' + request.url);
  if(request.url == "/topo.json") {
    console.log("got a request for tree.json");
  }
  var filePath = '.' + request.url;
  if (filePath == './')
  filePath = './index.html';

var extname = path.extname(filePath);
var contentType = 'text/html';
switch (extname) {
  case '.js':
    contentType = 'text/javascript';
    break;
  case '.css':
    contentType = 'text/css';
    break;
}

path.exists(filePath, function(exists) {
  if (exists) {
    fs.readFile(filePath, function(error, content) {
      if (error) {
        response.writeHead(500);
        response.end();
      }
      else {
        response.writeHead(200, { 'Content-Type': contentType });
        response.end(content, 'utf-8');
      }
    });
  }
  else {
    response.writeHead(404);
    response.end();
  }
});
}).listen(8125);

console.log('Server running at http://127.0.0.1:8125/');

var server = http.createServer(function(request, response) {
  // process HTTP request. Since we're writing just 
  // WebSockets server we don't have to implement anything.
});
server.listen(1337, function() { });

// create the server
wsServer = new WebSocketServer({httpServer: server});

//snapshot of local state to bootstrap 
//applications connected in the middle 
//of the simulation
var topology;
var clock;
var node_to_ix = {};

// WebSocket server
var sockets = new Array(0);
wsServer.on('request', function(request) {
  var connection = request.accept(null, request.origin);
  sockets.push(connection);
  //bootstrap client if the simulation has 
  //already started
  console.log("new client connected");
  if (topology) {
    msg = {'ts':clock,"type":"topology",
    "data":JSON.stringify(topology)};
    connection.send(JSON.stringify(msg));
  }

  connection.on('close', function(connection) {
    if(sockets.indexOf(connection) > 0) {
     sockets.splice(sockets.indexOf(connection), 1); 
    }
  });

});

var server = net.createServer();
server.on('error', function (e) {
  if (e.code == 'EADDRINUSE') {
    console.log('Address in use, retrying...');
    setTimeout(function () {
      server.close();
      server.listen(PORT, HOST);
    }, 1000);
  }
});

server.listen(8124, function(c) { //'listening' listener
  console.log('server bound');
});

function find_link(link, links) {
  for (i = 0; i < links.length; i++) {
    if( ((node_to_ix[link.target] == links[i].target) && 
          (node_to_ix[link.source] == links[i].source)) || 
        ((node_to_ix[link.target] == links[i].source) &&
         (node_to_ix[link.source] == links[i].target))) {
      return i;
    }
  }
  console.log("cannot find link for " + JSON.stringify(link));
  return -1;
}

 process.on('uncaughtException', function(err) {
     console.log(err);
 });

var socket_buffer = [];
var ix = 0;
server.on("connection", function(c) {
  console.log("Client connected...");

  // make socket spit string
  // c.setEncoding();
  c.ix = ix++;

  c.on('close', function (data) {
    if (typeof socket_buffer[c.ix] != "undefined") 
      delete socket_buffer[c.ix];
    console.log ("socket"+c.ix+": closed");

  });
   c.on("data", function(data) {
   if(typeof socket_buffer[c.ix] == "undefined" ) { 
      socket_buffer[c.ix] = data;
    } else {
      socket_buffer[c.ix] = 
        Buffer.concat([socket_buffer[c.ix], data]);
    }
    while((typeof socket_buffer[c.ix] != "undefined") &&
      (socket_buffer[c.ix].length >= 4) &&
      ((socket_buffer[c.ix].readInt32BE(0) + 4) <=
       socket_buffer[c.ix].length)) {
         string_len = socket_buffer[c.ix].readInt32BE(0);
         msg = socket_buffer[c.ix].toString("utf8", 4, string_len + 4);
         for (i = 0; i < sockets.length; i++) {
           sockets[i].send(msg);
         }
         message = JSON.parse(msg);

         //store when I received the last message, 
         //or when the simulation has resetted.
         if( (clock < message.clock) ||
           (message.clock == 0)) 
           clock = message.clock;
         if(message.type == "topology") {
           topo = JSON.parse(message.data);
           console.log("topo:\n"+message.data);
           for (i=0; i < topo.nodes.length; i++) { 
             node_to_ix[topo.nodes[i].name] = i;
           }
          topology = JSON.parse(message.data);
         } else if(message.type == "link_utilization") {
           links = JSON.parse(message.data);
           console.log("parsing links");
           for (a = 0; a < links.length; a++) {
             j = find_link(links[a], topology.links);
             if(j>=0) {
               if( (links[a].ts > topology.links[j].ts) || 
                   ((links[a].ts == topology.links[j].ts) &&
                    (links[i].value > topology.links[j].value) )) {
                      topology.links[j].ts = links[a].ts;
                      topology.links[j].value = links[a].value;
                    }
             } else {
               topology.links.push(links[a]);
             }
           
         } 
       } else if (message.type == "node_dev") {
         dev = JSON.parse(message.data);
         j = node_to_ix[dev.name];
         if(j !== undefined ) {
          topology.nodes[j].dev.push(
              {"dev_id":dev.dev_id,"ip":dev.ip});
         }
       }
         if ((typeof socket_buffer[c.ix] != "undefined" ) && 
             (socket_buffer[c.ix].length > string_len + 4))
           socket_buffer[c.ix] = 
              socket_buffer[c.ix].slice(string_len + 4);
         else 
           socket_buffer[c.ix] = undefined;
     }
  });
});
