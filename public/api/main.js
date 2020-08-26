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
    return this.requestJSON({pathname});
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

  async signWSPath(pathname) {
    const signData = {
      'key': this.key,
      'pathname': pathname
    }
    let secret = this.secret;

    const query = {};
    query.key = this.key;

    if (this.signSecret) {
      const {nonce, secret: secret_, timestamp} = await this.getSecret('WSPROXY',
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
    return self.socketURL + pathname + '?' + qs.stringify(query);
  }
}

window.ProcJSApi = ProcApi;
