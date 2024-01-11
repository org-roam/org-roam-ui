const WebSocket = require("ws");
const fs = require("fs");
const express = require("express");
const cors = require("cors");

const wsPort = 35903;
const httpPort = 35901;
const WebSocketServer = WebSocket.Server;

let wss = null;
let wsarray = [];
let graphdata = null;
let orgRoamConfig = null;
let count = 0;

let timeout;
let delay = 200;
function debounce(fn, delay) {
  return function () {
    clearTimeout(timeout);
    timeout = setTimeout(() => fn.apply(this, arguments), delay);
  };
}
const debouncedCallback = debounce((callback) => {
  callback();
}, delay);

const processGraphData = (_graphdata) => {
  try {
    const data = JSON.parse(_graphdata);
    const nodes = [];
    const links = [];
    const tags = [];
    data.forEach((file) => {
      const { file_id: id, file_path } = file;
      nodes.push({
        id,
        file: file_path,
        // TODO: fix comment single comment
        // TODO: file name not stored in db
        // use last path component for now
        // then fix the dev env and refactor code
        title: file_path
          .split("/")
          [file_path.split("/").length - 1].split(".")[0],
        level: 0,
        pos: 0,
        properties: {},
        tags: [],
        olp: null,
      });
      const backlinks = JSON.parse(file.id_links);
      backlinks.forEach((link) => {
        links.push({
          source: link.file_id,
          target: link.id,
          type: "bad",
        });
      });
    });
    return { nodes, links, tags };
  } catch (err) {
    // TODO: how to properly debug by logging
    // it looks like I can't use console.log
    // because it throw errors
    // https://github.com/neovim/node-client/issues/202
    plugin.nvim.outWrite(`${err.message}! \n`);
    return { nodes: [], links: [], tags: [] };
  }
};

module.exports = (plugin) => {
  const getGraphData = () => {
    // TODO: I only know this way to get the data in other neovim plugin
    // in org-roam plugin which defined global GetLatestGraphData function
    plugin.nvim.lua("return GetLatestGraphData()").then((data) => {
      graphdata = data;
      updateGraphData();
      return Promise(true);
    });
  };

  function init() {
    plugin.nvim.outWrite(`connecting... \n`);
    wss = new WebSocketServer({
      port: wsPort,
    });

    // TODO: remove from array when disconnect
    // or I can find a way to use wss object
    // to get all clients
    wss.on("connection", function (ws) {
      plugin.nvim.outWrite(`connected! \n`);
      wsarray.push(ws);
      getGraphData();
    });

    // TODO: add ping pong mechanism
    // like close watcher
    wss.on("error", function close() {
      plugin.nvim.outWrite(`connection error! \n`);
    });
    wss.on("close", function close() {
      plugin.nvim.outWrite(`connection closed! \n`);
    });

    plugin.nvim.lua("return vim.g.org_roam_config").then((data) => {
      orgRoamConfig = data;
      if (orgRoamConfig) {
        const folder = `${orgRoamConfig.org_roam_directory}`;
        const watcher = fs.watch(folder, (eventType, filename) => {
          debouncedCallback(() => {
            count = count + 1;
            plugin.nvim.outWrite(`${count} \n`);
            getGraphData();
          });
        });
      }
      // I guess put it in ws disconnect callback would be appropriate
      // https://stackoverflow.com/a/53983383
      // TODO: to close watcher use `watcher.close()`
      return Promise(true);
    });

    // http server format
    // http://localhost:35901/node/${id}
    const app = express();
    app.use(cors());

    app.get("/node/:id", (req, res) => {
      // Extract the 'id' from the request parameters
      const id = req.params.id;
      const { nodes } = processGraphData(graphdata);
      const node = nodes.find((node) => node.id === id);
      try {
        // Synchronously read the contents of the file
        // Output the file contents
        const fileContent = fs.readFileSync(`${node.file}`, "utf8");
        res.send(fileContent);
      } catch (err) {}
    });

    app.get("/img/:filePath", (req, res) => {
      // I dont know why it get encode twice
      const filePath = decodeURIComponent(
        decodeURIComponent(req.params.filePath),
      );
      plugin.nvim.outWrite(`${filePath} \n`);

      // Send the file to the client
      res.sendFile(filePath, (err) => {
        if (err) {
          // Handle the error, for example send a 404 if file not found
          res.status(err.status).end();
        }
      });
    });

    app.listen(httpPort, () => {});
    // http server end
  }

  const updateGraphData = () => {
    plugin.nvim.outWrite("updateGraphData! \n");
    wsarray.forEach((ws) => {
      const { nodes, links, tags } = processGraphData(graphdata);
      // TODO: 需要找一个高效率的调试方式
      ws.send(
        JSON.stringify({
          type: "graphdata",
          data: {
            nodes,
            links,
            tags,
          },
        }),
      );
    });
  };

  // lua vim.fn.SetLines
  // you need to call :UpdateRemotePlugins
  // everytime you made change, [[https://github.com/neovim/node-client/issues/204#issuecomment-1575338830][this is the article]]
  plugin.registerFunction(
    "UpdateGraphData",
    (data) => {
      graphdata = data;
      updateGraphData();
      return Promise(true);
    },
    { sync: false },
  );

  plugin.registerCommand("InitWs", [plugin.nvim.buffer, init]);
  plugin.registerCommand("GetLatestGraphData", [
    plugin.nvim.buffer,
    getGraphData,
  ]);

  // it seems like if i add dev:true, it will reload actually
  // from here [[https://github.com/neovim/node-client?tab=readme-ov-file#api]]
  // if not work, some times maybe the function you are passing have
  // error
  // plugin.setOptions({ dev: true });
};
