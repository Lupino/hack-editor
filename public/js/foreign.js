function filesToTree(parent, files) {
  var result = [];
  for (var file in files) {
    var content = files[file];
    var entry = {text: file};
    var path = parent + "/" + file;
    entry.serverPath = path;
    if (typeof content === "object") {
      entry.children = filesToTree(path, content);
      entry.state = { "opened": false };
      entry.isDir = true;
    } else {
      entry.icon = false;
      entry.isDir = false;
      entry.size = content;
    }
    result.push(entry);
  }
  return result;
}

function initTree(data, eventCallback) {
  $("#tree").on("activate_node.jstree", function (e, data) {
    data = data.node.original;
    data.instance = 'TreeNode'
    eventCallback(data);
  }).jstree({core: {data: filesToTree("", data)}});
}

function clearTree() {
  $("#tree").jstree(true).destroy();
  $("#tree").empty();
}

var editorInitialized = false;
var currentPath = "";
var currentDirectory = "";

function selectFile(callback) {
  var form = $('<form><input type="file" name="test"></form>', { css: { 'display': 'none' }});
  var input = $(form.children()[0]);

  input.on('change', function(e) {
    if (this.files.length === 1) {
      var file = this.files[0];
      callback(file.name, file);
    }
  });

  // Trigger file selector (only works on newer browsers...)
  input.click();
}

function isTextFile(fileName) {
 return /\.(json|js|html|markdown|md|rst|css|htm|xml|txt|py|csv|log|sh|sql|yaml|yml|gitignore|editorignore)$/i.test(fileName);
}

function isExecutable(fileName) {
  return /\.(js|sh|py)$/i.test(fileName);
}

function getShell(fileName) {
  if (/\.py/.test(fileName)) {
    return 'python3'
  }
  if (/\.js/.test(fileName)) {
    return 'node'
  }
  if (/\.sh/.test(fileName)) {
    return 'bash'
  }
  return '';
}
