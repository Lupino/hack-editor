import Gateway from 'yuntan-gateway';
import qs from 'querystring';


class ProcApi extends Gateway {
  constructor(options) {
    super({...options, secure: true, host: location.origin});

    const protocol = (location.protocol === 'https:') ? 'wss://' : 'ws://';
    self.socketURL = protocol + location.host;
  }
  createTerm(cols, rows) {
    const pathname = '/api/term/create'
    return this.requestJSON({pathname, method: 'POST', form: {cols, rows}});
  }
  resizeTerm(cols, rows) {
    const pathname = '/api/term/resize'
    return this.requestJSON({pathname, method: 'POST', form: {cols, rows}});
  }
  closeTerm() {
    const pathname = '/api/term/close'
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
  uploadFile(fileName, raw) {
    const pathname = `/api/upload${fileName}`;
    return this.request({method: 'PUT', pathname, raw});
  }
  uploadArchive(fileName, raw) {
    const pathname = `/api/uploadArchive${fileName}`;
    return this.request({method: 'PUT', pathname, raw});
  }
  runFile(fileName, raw='[]') {
    let cmd = 'bash'
    if (/.py$/.exec(fileName)) {
      cmd = 'python'
    } else if (/.js$/.exec(fileName)) {
      cmd = 'node'
    }
    const pathname = `/api/${cmd}${fileName}`
    return this.request({pathname, method: 'POST', raw}).then((rsp) => rsp.text())
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
    const pathname = `/api/file${fileName}`;
    const path = await this.signPathName('GET', pathname);
    return this.host + path;
  }
}

window.ProcJSApi = ProcApi;
