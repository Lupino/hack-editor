import * as Terminal from './xterm.js/xterm';
import * as attach from './xterm.js/addons/attach/attach';
import * as fit from './xterm.js/addons/fit/fit';
import * as fullscreen from './xterm.js/addons/fullscreen/fullscreen';
import * as search from './xterm.js/addons/search/search';
import * as winptyCompat from './xterm.js/addons/winptyCompat/winptyCompat';


Terminal.applyAddon(attach);
Terminal.applyAddon(fit);
Terminal.applyAddon(fullscreen);
Terminal.applyAddon(search);
Terminal.applyAddon(winptyCompat);

(function(){

    var TermManager = function(elem, api, hide){

      var terminalContainer = document.querySelector(elem);

      var socket;

      var _this = this;
      var term;
      var connected = false;

      this.init = function(){
        if (connected) {
          term.fit()
        } else {
          this.createTerminal();
        }
      }

      this.isFocused = function(){
        return this.term.isFocused;
      }

      this.runRealTerminal = function(){
        connected = true;
        term.attach(socket);
        term._initialized = true;
      }

      this.createTerminal = function(){

        // Clean terminal
        while (terminalContainer.children.length) {
          terminalContainer.removeChild(terminalContainer.children[0]);
        }
        term = new Terminal({
          cursorBlink: true,
          scrollback: 1000,
          tabStopWidth: 8
        });
        this.term = term;

        window.term = term;  // Expose `term` to window for debugging purposes
        term.on('resize', function (size) {
          if (!connected) {
            return;
          }
          var cols = size.cols,
              rows = size.rows;

          api.resizeTerm(cols, rows)
        });

        term.open(terminalContainer);
        term.winptyCompatInit();
        term.fit();
        // term.focus();
        api.createTerm(term.cols, term.rows)
          .then(function() {
            return api.signWSPath('/api/term')
          })
          .then(function(socketURL) {
            socket = new WebSocket(socketURL);
            socket.onopen = _this.runRealTerminal;
            socket.onclose = function(e){
              console.log(e);
              connected = false
              _this.close();
            };
            socket.onerror = function(e){
              console.log(e);
              connected = false
              _this.close();
            };
          });
      }

      this.close = function() {
        if (connected) {
          connected = false;
          socket.close()
        }
        hide();
        api.closeTerm()
      }
   }

   window.JSTermManager = TermManager;

})();


