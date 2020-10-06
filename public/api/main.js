import Gateway from 'yuntan-gateway';
import qs from 'querystring';


class ProcApi extends Gateway {
  constructor(options) {
    super({...options, secure: true, host: location.origin});

    const protocol = (location.protocol === 'https:') ? 'wss://' : 'ws://';
    self.socketURL = protocol + location.host;
  }
  createTerm(cols, rows) {
    const pathname = '/api/term/create';
    return this.requestJSON({pathname, method: 'POST', form: {cols, rows}});
  }
  resizeTerm(tid, cols, rows) {
    const pathname = `/api/term/${tid}/resize`;
    return this.requestJSON({pathname, method: 'POST', form: {cols, rows}});
  }
  closeTerm(tid) {
    const pathname = `/api/term/${tid}/close`
    return this.requestJSON({pathname, method: 'POST'});
  }
  loadFileTree() {
    const pathname = `/api/file`;
    return this.request({pathname}).then((rsp) => rsp.json());
  }
  writeFile(fileName, raw) {
    const pathname = `/api/file${fileName}`;
    return this.request({method: 'PUT', pathname, raw});
  }
  readFile(fileName) {
    const pathname = `/api/file${fileName}`;
    return this.request({pathname}).then((rsp) => rsp.text());
  }
  removeFile(fileName) {
    const pathname = `/api/file${fileName}`;
    return this.request({method: 'DELETE', pathname});
  }
  async uploadFile(fileName, file) {
    const pathname = `/api/upload${fileName}`;
    const socketURL = await this.signWSPath(pathname);
    const totalSize = file.size;
    const maxPartSize = 10485760;
    let partSize = Math.floor(totalSize / 10)
    if (partSize > maxPartSize) {
      partSize = maxPartSize;
    }
    let startSize = 0;
    let endSize = 0;
    const elem = document.querySelector('#upload-state');
    elem.max = totalSize;
    elem.value = 0;
    elem.style.display = 'block';
    return new Promise(function(resolve, reject) {
      const socket = new WebSocket(socketURL);
      socket.onopen = function() {
        console.log('start upload file:', fileName)
      };
      socket.onmessage = function(e) {
        if (endSize < totalSize) {
          startSize = endSize;
          elem.value = startSize;
          endSize += partSize;
          if (endSize > totalSize) {
            endSize = totalSize;
          }

          const blob = file.slice(startSize, endSize);

          socket.send(blob);
        } else {
          socket.send('EOF');
          elem.style.display = 'none';
          console.log('finished');
        }
      }
      socket.onclose = function(e){
        resolve();
      };
      socket.onerror = function(e){
        console.log(e);
        reject(e);
      };
    });
  }
  async signPathName(method, pathname) {
    const signData = {
      'key': this.key,
      'pathname': pathname
    }
    let secret = this.secret;

    const query = {};
    query.key = this.key;

    if (this.signSecret) {
      const {nonce, secret: secret_, timestamp} = await this.getSecret(method,
        pathname);
      query.type = 'JSAPI';
      query.nonce = nonce;
      signData.timestamp = timestamp;
      secret = secret_;
    } else {
      signData.timestamp = Math.floor(new Date() / 1000);
    }

    query.timestamp = signData.timestamp
    query.sign = this.signParam(secret, signData)
    return pathname + '?' + qs.stringify(query);
  }

  async signWSPath(pathname) {
    const path = await this.signPathName('WSPROXY', pathname);
    return self.socketURL + path;
  }

  async signFilePath(fileName) {
    if (fileName.startsWith('/public')) {
      return this.host + fileName.substr(7) + '?key='+this.key;
    }
    const pathname = `/api/file${fileName}`;
    const path = await this.signPathName('GET', pathname);
    return this.host + path;
  }
}

window.ProcJSApi = ProcApi;
