var net = require('net');
var eol = require('os').EOL;

var srvr = net.createServer();
var clientList = [];

srvr.on('connection', function(client) {
  client.name = client.remoteAddress + ':' + client.remotePort;
  client.write('Welcome, ' + client.name + eol);
  clientList.push(client);

  client.on('data', function(data) {
      if(data.toString().trim() == '\list')
        {
        listallusers(client);
        }
      else if(data.includes("\rename") == true)
        {
        var newname = data.slice(9,data.length);
        renameUser(client,newname);
        }
      else
        {
    broadcast(data, client);
        }
  });

});

function broadcast(data, client) {
  for (var i in clientList) {
    if (client !== clientList[i]) {
      clientList[i].write(client.name + " says " + data);
    }
  }
}

function listallusers(client) {
  client.write('List of users present: ');
    for (var i in clientList) {
            client.write(clientList[i].name + '\n');
        }
}

function renameUser(client,newname) {
  client.write('Renaming user to : ');
   for (var i in clientList) {
     clientList[i].name=newname;
  }
    client.name = newname;
}

srvr.listen(9003);

