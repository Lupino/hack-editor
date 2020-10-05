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
