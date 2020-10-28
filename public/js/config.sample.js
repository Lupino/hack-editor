function isTextFile(fileName) {
 return /\.(json|js|html|markdown|md|rst|css|htm|xml|txt|py|csv|log|sh|sql|yaml|yml|gitignore|editorignore)$/i.test(fileName);
}

function isExecutable(fileName) {
  return /\.(js|sh|py)$/i.test(fileName) || fileName.startsWith('/bin/');
}
