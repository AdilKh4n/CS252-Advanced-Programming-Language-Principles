var net = require('net');
var eol = require('os').EOL;

var srvr = net.createServer();
var clientList = [];

srvr.on('connection', function(client) {
  client.name = client.remoteAddress + ':' + client.remotePort;
  client.write('Welcome, ' + client.name + eol);
  clientList.push(client);

  client.on('data', function(data) {
      if(data.trim() === "\list")
        listallusers(clientList);
      else
    broadcast(data, client);
  });

});

function broadcast(data, client) {
  for (var i in clientList) {
    if (client !== clientList[i]) {
      clientList[i].write(client.name + " says " + data);
    }
  }
}

function listallusers(clientList) {
    for (var i in clientList) {
        if (client == clientList[i]) {
            clientList[i].write(client.name);
        }
  }
}

srvr.listen(9000);

