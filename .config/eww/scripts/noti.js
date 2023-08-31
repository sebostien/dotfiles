#!/home/sn/.nvm/versions/node/v18.16.0/bin/node

const { exec } = require("child_process");

const iconMap = {
  Firefox: "",
  discord: "ﭮ",
};

const mergeObject = (arr) => {
  const data = {
    apps: [],
    data: {},
  };

  for (const row of arr) {
    if (!data.apps.includes(row.appname.data)) {
      data.apps.push(row.appname.data);
      data.data[row.appname.data] = {
        icon: iconMap[row.appname.data] || "",
        data: [],
      };
    }

    data.data[row.appname.data].data.push(`${row.message.data}`);
  }

  return data;
};

exec("dunstctl history", (error, stdout, stderr) => {
  if (error) {
    console.log(JSON.stringify({ error: error.message, type: "error" }));
    return;
  }
  if (stderr) {
    console.log(JSON.stringify({ error: stderr, type: "stderr" }));
    return;
  }

  const data = JSON.parse(stdout);
  console.log(JSON.stringify(mergeObject(data.data[0]), null, 2));
});
