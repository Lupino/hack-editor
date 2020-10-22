function isTextFile(fileName) {
 return /\.(json|js|html|markdown|md|rst|css|htm|xml|txt|py|csv|log|sh|sql|yaml|yml|gitignore|editorignore)$/i.test(fileName);
}

function isExecutable(fileName) {
  return /\.(js|sh|py)$/i.test(fileName) || fileName.startsWith('/bin/');
}

function getShell(fileName) {
  if (/\.py/.test(fileName)) {
    return 'run python3'
  }
  if (/\.js/.test(fileName)) {
    return 'run node'
  }
  if (/\.sh/.test(fileName)) {
    return 'run bash'
  }
  return 'run';
}
