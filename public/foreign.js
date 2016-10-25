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
    }
    result.push(entry);
  }
  return result;
}

function initTree(text, eventCallback) {
  var data = JSON.parse(text);
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
var editor;

function initEditor() {
  if (editorInitialized) {
    return editor;
  }

  $("#in").removeClass("uninitialized");
  editor = ace.edit("in");
  editor.setTheme("ace/theme/chrome");
  editorInitialized = true;
  return editor;
}

function setEditorValue(value) {
  editor.setValue(value);
}

var MODE_MAP = {
  "javascript": /\.js$/i,
  "markdown":   /\.(md|markdown|rst)$/i,
  "html":       /\.html?$/i,
  "css":        /\.css$/i,
  "yaml":       /\.(yaml|yml)$/,
  "xml":        /\.(svg|xml)$/,
  "json":       /\.json$/,
  "python":     /\.py/,
  "tex":        /\.(tex|aux)/
};

function getModeFromFileName(fileName) {
  var mode = "text";
  for(m in MODE_MAP) {
    v = MODE_MAP[m];
    if (v.exec(fileName)) {
      mode = m;
      break;
    }
  }
  return mode;
}

function setEditorMode(editor, fileName) {
  editor.getSession().setMode("ace/mode/" + getModeFromFileName(fileName));
  return editor;
}

var currentPath = "";
var currentDirectory = "";

function selectFile(callback) {
  var form = $('<form><input type="file" name="test"></form>', { css: { 'display': 'none' }});
  var input = $(form.children()[0]);

  input.on('change', function(e) {
    if (this.files.length === 1) {
      var file = this.files[0];
      callback(file.name, file)
    }
  });

  // Trigger file selector (only works on newer browsers...)
  input.click();
}
