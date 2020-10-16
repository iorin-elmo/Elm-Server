const port = process.argv[2] || 8080;
const host = "localhost";
const http = require('http');
const fs = require('fs').promises;
const { Elm } = require("./server.js");

const address = host + ":" + port;
console.log(address);
const app = Elm.Server.init({
  flags: address
});

function makeAsyncFun(sndFun, subFun) {
  const map = new Map();
  let id = 0;
  subFun.subscribe(function([result, id]) {
    const callback = map.get(id);
    map.delete(id);
    callback(result);
  });
  return function(arg) {
    return new Promise(function(resolve) {
      map.set(id, function(result) {
        resolve(result);
      });
      sndFun.send([arg, id]);
      id++;
    });
  }
}

app.ports.readFile.subscribe(async function([path, id]) {
  try {
    const string = await fs.readFile(path, 'utf-8');
    app.ports.readResult.send([string, id]);
  } catch(err) {
    app.ports.readResult.send([null, id])
  }
});

function readBody(req) {
  return new Promise(function(resolve, reject) {
    if(req.method != "POST") {// TODO: other method type
      resolve(null);
    }
    var body = '';
    req.on('data', function(data) {
      body += data;
      if (body.length > 1e6) {
        req.connection.destroy();
        reject('LOOOOOOOOOOOOOONG!');
      }
    });
    req.on('end', function() {
      resolve(body);
    })
  })
}

const getResponse = makeAsyncFun(app.ports.request, app.ports.response);
const server = http.createServer(async function(req, res) {
  const req_ = {
    method: req.method,
    headers: req.headers,
    url: req.url,
    body: await readBody(req)
  };
  console.log(req_);
  const {
    statusCode,
    statusMessage,
    headers,
    body
  } = await getResponse(req_);
  res.writeHeader(statusCode, statusMessage, headers);
  res.end(body);
});
server.listen(port);
