
/*******************************************************************************
 * Misc.
 */


// Workaround for missing functionality in IE 8 and earlier.
if( Object.create === undefined ) {
  Object.create = function( o ) {
    function F(){}
    F.prototype = o;
    return new F();
  };
}

// Insert properties of b in place into a.
function Fay$$objConcat(a,b){
  for (var p in b) if (b.hasOwnProperty(p)){
    a[p] = b[p];
  }
  return a;
}

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function Fay$$_(thunkish,nocache){
  while (thunkish instanceof Fay$$$) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function Fay$$__(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof Fay$$$? Fay$$_(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function Fay$$$(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
Fay$$$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};


function Fay$$seq(x) {
  return function(y) {
    Fay$$_(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  Fay$$_(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new Fay$$$(function(){
      var monad = Fay$$_(m,true);
      return Fay$$_(f)(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
  return new Fay$$$(function(){
    var monad = Fay$$_(m,true);
    return Fay$$_(f)(monad.value);
  });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new Fay$$$(function(){
      Fay$$_(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  if(base == "action") {
    // A nullary monadic action. Should become a nullary JS function.
    // Fay () -> function(){ return ... }
    return function(){
      return Fay$$fayToJs(args[0],Fay$$_(fayObj,true).value);
    };

  }
  else if(base == "function") {
    // A proper function.
    return function(){
      var fayFunc = fayObj;
      var return_type = args[args.length-1];
      var len = args.length;
      // If some arguments.
      if (len > 1) {
        // Apply to all the arguments.
        fayFunc = Fay$$_(fayFunc,true);
        // TODO: Perhaps we should throw an error when JS
        // passes more arguments than Haskell accepts.

        // Unserialize the JS values to Fay for the Fay callback.
        if (args == "automatic_function")
        {
          for (var i = 0; i < arguments.length; i++) {
            fayFunc = Fay$$_(fayFunc(Fay$$jsToFay(["automatic"],arguments[i])),true);
          }
          return Fay$$fayToJs(["automatic"], fayFunc);
        }

        for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
          fayFunc = Fay$$_(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
        }
        // Finally, serialize the Fay return value back to JS.
        var return_base = return_type[0];
        var return_args = return_type[1];
        // If it's a monadic return value, get the value instead.
        if(return_base == "action") {
          return Fay$$fayToJs(return_args[0],fayFunc.value);
        }
        // Otherwise just serialize the value direct.
        else {
          return Fay$$fayToJs(return_type,fayFunc);
        }
      } else {
        throw new Error("Nullary function?");
      }
    };

  }
  else if(base == "string") {
    return Fay$$fayToJs_string(fayObj);
  }
  else if(base == "list") {
    // Serialize Fay list to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[0],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    return arr;
  }
  else if(base == "tuple") {
    // Serialize Fay tuple to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    var i = 0;
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[i++],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    return arr;
  }
  else if(base == "defined") {
    fayObj = Fay$$_(fayObj);
    return fayObj instanceof Fay.FFI._Undefined
      ? undefined
      : Fay$$fayToJs(args[0],fayObj.slot1);
  }
  else if(base == "nullable") {
    fayObj = Fay$$_(fayObj);
    return fayObj instanceof Fay.FFI._Null
      ? null
      : Fay$$fayToJs(args[0],fayObj.slot1);
  }
  else if(base == "double" || base == "int" || base == "bool") {
    // Bools are unboxed.
    return Fay$$_(fayObj);
  }
  else if(base == "ptr")
    return fayObj;
  else if(base == "unknown")
    return Fay$$fayToJs(["automatic"], fayObj);
  else if(base == "automatic" && fayObj instanceof Function) {
    return Fay$$fayToJs(["function", "automatic_function"], fayObj);
  }
  else if(base == "automatic" || base == "user") {
    fayObj = Fay$$_(fayObj);

    if(fayObj instanceof Fay$$Cons || fayObj === null){
      // Serialize Fay list to JavaScript array.
      var arr = [];
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(["automatic"],fayObj.car));
        fayObj = Fay$$_(fayObj.cdr);
      }
      return arr;
    } else {
      var fayToJsFun = fayObj && fayObj.instance && Fay$$fayToJsHash[fayObj.instance];
      return fayToJsFun ? fayToJsFun(type,type[2],fayObj) : fayObj;
    }
  }

  throw new Error("Unhandled Fay->JS translation type: " + base);
}

// Stores the mappings from fay types to js objects.
// This will be populated by compiled modules.
var Fay$$fayToJsHash = {};

// Specialized serializer for string.
function Fay$$fayToJs_string(fayObj){
  // Serialize Fay string to JavaScript string.
  var str = "";
  fayObj = Fay$$_(fayObj);
  while(fayObj instanceof Fay$$Cons) {
    str += Fay$$_(fayObj.car);
    fayObj = Fay$$_(fayObj.cdr);
  }
  return str;
};
function Fay$$jsToFay_string(x){
  return Fay$$list(x)
};

// Special num/bool serializers.
function Fay$$jsToFay_int(x){return x;}
function Fay$$jsToFay_double(x){return x;}
function Fay$$jsToFay_bool(x){return x;}

function Fay$$fayToJs_int(x){return Fay$$_(x);}
function Fay$$fayToJs_double(x){return Fay$$_(x);}
function Fay$$fayToJs_bool(x){return Fay$$_(x);}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  if(base == "action") {
    // Unserialize a "monadic" JavaScript return value into a monadic value.
    return new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "function") {
    // Unserialize a function from JavaScript to a function that Fay can call.
    // So
    //
    //    var f = function(x,y,z){ … }
    //
    // becomes something like:
    //
    //    function(x){
    //      return function(y){
    //        return function(z){
    //          return new Fay$$$(function(){
    //            return Fay$$jsToFay(f(Fay$$fayTojs(x),
    //                                  Fay$$fayTojs(y),
    //                                  Fay$$fayTojs(z))
    //    }}}}};
    var returnType = args[args.length-1];
    var funArgs = args.slice(0,-1);

    if (jsObj.length > 0) {
      var makePartial = function(args){
        return function(arg){
          var i = args.length;
          var fayArg = Fay$$fayToJs(funArgs[i],arg);
          var newArgs = args.concat([fayArg]);
          if(newArgs.length == funArgs.length) {
            return new Fay$$$(function(){
              return Fay$$jsToFay(returnType,jsObj.apply(this,newArgs));
            });
          } else {
            return makePartial(newArgs);
          }
        };
      };
      return makePartial([]);
    }
    else
      return function (arg) {
        return Fay$$jsToFay(["automatic"], jsObj(Fay$$fayToJs(["automatic"], arg)));
      };
  }
  else if(base == "string") {
    // Unserialize a JS string into Fay list (String).
    // This is a special case, when String is explicit in the type signature,
    // with `Automatic' a string would not be decoded.
    return Fay$$list(jsObj);
  }
  else if(base == "list") {
    // Unserialize a JS array into a Fay list ([a]).
    var serializedList = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
    }
    // Pop it all in a Fay list.
    return Fay$$list(serializedList);
  }
  else if(base == "tuple") {
    // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
    var serializedTuple = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
    }
    // Pop it all in a Fay list.
    return Fay$$list(serializedTuple);
  }
  else if(base == "defined") {
    return jsObj === undefined
      ? new Fay.FFI._Undefined()
      : new Fay.FFI._Defined(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "nullable") {
    return jsObj === null
      ? new Fay.FFI._Null()
      : new Fay.FFI.Nullable(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "int") {
    // Int are unboxed, so there's no forcing to do.
    // But we can do validation that the int has no decimal places.
    // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
    fayObj = Math.round(jsObj);
    if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";
    return fayObj;
  }
  else if (base == "double" ||
           base == "bool" ||
           base ==  "ptr") {
    return jsObj;
  }
  else if(base == "unknown")
    return Fay$$jsToFay(["automatic"], jsObj);
  else if(base == "automatic" && jsObj instanceof Function) {
    var type = [["automatic"]];
    for (var i = 0; i < jsObj.length; i++)
      type.push(["automatic"]);
    return Fay$$jsToFay(["function", type], jsObj);
  }
  else if(base == "automatic" && jsObj instanceof Array) {
    var list = null;
    for (var i = jsObj.length - 1; i >= 0; i--) {
      list = new Fay$$Cons(Fay$$jsToFay([base], jsObj[i]), list);
    }
    return list;
  }
  else if(base == "automatic" || base == "user") {
    if (jsObj && jsObj['instance']) {
      var jsToFayFun = Fay$$jsToFayHash[jsObj["instance"]];
      return jsToFayFun ? jsToFayFun(type,type[2],jsObj) : jsObj;
    }
    else
      return jsObj;
  }

  throw new Error("Unhandled JS->Fay translation type: " + base);
}

// Stores the mappings from js objects to fay types.
// This will be populated by compiled modules.
var Fay$$jsToFayHash = {};

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
// `list' is already forced by the time it's passed to this function.
// `list' cannot be null and `index' cannot be out of bounds.
function Fay$$index(index,list){
  for(var i = 0; i < index; i++) {
    list = Fay$$_(list.cdr);
  }
  return list.car;
}

// List length.
// `list' is already forced by the time it's passed to this function.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = Fay$$_(list.cdr);
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) * Fay$$_(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) * Fay$$_(y);
  });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) + Fay$$_(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) + Fay$$_(y);
  });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) - Fay$$_(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) - Fay$$_(y);
  });

}

// Built-in /.
function Fay$$divi(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) / Fay$$_(y);
    });
  };
}

// Built-in /.
function Fay$$divi$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) / Fay$$_(y);
  });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = Fay$$_(lit1);
  lit2 = Fay$$_(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = Fay$$_(lit1.cdr), lit2 = Fay$$_(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.instance === lit2.instance) {
    for(var x in lit1) {
      if(!Fay$$equal(lit1[x],lit2[x]))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$equal(x,y);
  });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new Fay$$$(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return !(Fay$$equal(x,y));
  });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) > Fay$$_(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) > Fay$$_(y);
  });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) < Fay$$_(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) < Fay$$_(y);
  });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) >= Fay$$_(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) >= Fay$$_(y);
  });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) <= Fay$$_(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) <= Fay$$_(y);
  });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) && Fay$$_(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) && Fay$$_(y);
  });
  ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) || Fay$$_(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) || Fay$$_(y);
  });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return Date.parse(str);
}

/*******************************************************************************
 * Data.Var
 */

function Fay$$Ref2(val){
  this.val = val;
}

function Fay$$Sig(){
  this.handlers = [];
}

function Fay$$Var(val){
  this.val = val;
  this.handlers = [];
}

// Helper used by Fay$$setValue and for merging
function Fay$$broadcastInternal(self, val, force){
  var handlers = self.handlers;
  var exceptions = [];
  for(var len = handlers.length, i = 0; i < len; i++) {
    try {
      force(handlers[i][1](val), true);
    } catch (e) {
      exceptions.push(e);
    }
  }
  // Rethrow the encountered exceptions.
  if (exceptions.length > 0) {
    console.error("Encountered " + exceptions.length + " exception(s) while broadcasing a change to ", self);
    for(var len = exceptions.length, i = 0; i < len; i++) {
      (function(exception) {
        setTimeout(function() { throw exception; }, 0);
      })(exceptions[i]);
    }
  }
}

function Fay$$setValue(self, val, force){
  if (self instanceof Fay$$Ref2) {
    self.val = val;
  } else if (self instanceof Fay$$Var) {
    self.val = val;
    Fay$$broadcastInternal(self, val, force);
  } else if (self instanceof Fay$$Sig) {
    Fay$$broadcastInternal(self, val, force);
  } else {
    throw "Fay$$setValue given something that's not a Ref2, Var, or Sig"
  }
}

function Fay$$subscribe(self, f){
  var key = {};
  self.handlers.push([key,f]);
  var searchStart = self.handlers.length - 1;
  return function(_){
    for(var i = Math.min(searchStart, self.handlers.length - 1); i >= 0; i--) {
      if(self.handlers[i][0] == key) {
        self.handlers = self.handlers.slice(0,i).concat(self.handlers.slice(i+1));
        return;
      }
    }
    return _; // This variable has to be used, otherwise Closure
              // strips it out and Fay serialization breaks.
  };
}

/*******************************************************************************
 * Application code.
 */

var Data = {};
Data.Data = {};
var Fay = {};
Fay.FFI = {};
Fay.FFI._Nullable = function Nullable(slot1){
  this.slot1 = slot1;
};
Fay.FFI._Nullable.prototype.instance = "Nullable";
Fay.FFI.Nullable = function(slot1){
  return new Fay$$$(function(){
    return new Fay.FFI._Nullable(slot1);
  });
};
Fay.FFI._Null = function Null(){
};
Fay.FFI._Null.prototype.instance = "Null";
Fay.FFI.Null = new Fay$$$(function(){
  return new Fay.FFI._Null();
});
Fay.FFI._Defined = function Defined(slot1){
  this.slot1 = slot1;
};
Fay.FFI._Defined.prototype.instance = "Defined";
Fay.FFI.Defined = function(slot1){
  return new Fay$$$(function(){
    return new Fay.FFI._Defined(slot1);
  });
};
Fay.FFI._Undefined = function Undefined(){
};
Fay.FFI._Undefined.prototype.instance = "Undefined";
Fay.FFI.Undefined = new Fay$$$(function(){
  return new Fay.FFI._Undefined();
});
Fay$$objConcat(Fay$$fayToJsHash,{"Nullable": function(type,argTypes,_obj){
  var obj_ = {"instance": "Nullable"};
  var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"Null": function(type,argTypes,_obj){
  var obj_ = {"instance": "Null"};
  return obj_;
},"Defined": function(type,argTypes,_obj){
  var obj_ = {"instance": "Defined"};
  var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"Undefined": function(type,argTypes,_obj){
  var obj_ = {"instance": "Undefined"};
  return obj_;
}});
Fay$$objConcat(Fay$$jsToFayHash,{"Nullable": function(type,argTypes,obj){
  return new Fay.FFI._Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
},"Null": function(type,argTypes,obj){
  return new Fay.FFI._Null();
},"Defined": function(type,argTypes,obj){
  return new Fay.FFI._Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
},"Undefined": function(type,argTypes,obj){
  return new Fay.FFI._Undefined();
}});
var Prelude = {};
Prelude._Just = function Just(slot1){
  this.slot1 = slot1;
};
Prelude._Just.prototype.instance = "Just";
Prelude.Just = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Just(slot1);
  });
};
Prelude._Nothing = function Nothing(){
};
Prelude._Nothing.prototype.instance = "Nothing";
Prelude.Nothing = new Fay$$$(function(){
  return new Prelude._Nothing();
});
Prelude._Left = function Left(slot1){
  this.slot1 = slot1;
};
Prelude._Left.prototype.instance = "Left";
Prelude.Left = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Left(slot1);
  });
};
Prelude._Right = function Right(slot1){
  this.slot1 = slot1;
};
Prelude._Right.prototype.instance = "Right";
Prelude.Right = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Right(slot1);
  });
};
Prelude.maybe = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) instanceof Prelude._Nothing) {
          var m = $p1;
          return m;
        }
        if (Fay$$_($p3) instanceof Prelude._Just) {
          var x = Fay$$_($p3).slot1;
          var f = $p2;
          return Fay$$_(f)(x);
        }
        throw ["unhandled case in maybe",[$p1,$p2,$p3]];
      });
    };
  };
};
Prelude.$62$$62$$61$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$_(Fay$$bind($p1)($p2));
    });
  };
};
Prelude.$62$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$_(Fay$$then($p1)($p2));
    });
  };
};
Prelude.$_return = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$$_return(Fay$$fayToJs(["unknown"],$p1))));
  });
};
Prelude.fail = new Fay$$$(function(){
  return Prelude.error;
});
Prelude.when = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var m = $p2;
      var p = $p1;
      return Fay$$_(p) ? m : Fay$$return$36$uncurried(Fay$$unit);
    });
  };
};
Prelude.unless = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var m = $p2;
      var p = $p1;
      return Fay$$_(p) ? Fay$$return$36$uncurried(Fay$$unit) : m;
    });
  };
};
Prelude.forM = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var fn = $p2;
      var lst = $p1;
      return Prelude.$36$$36$uncurried(Prelude.sequence,Prelude.map$36$uncurried(fn,lst));
    });
  };
};
Prelude.forM_ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var m = $p2;
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return Fay$$then$36$uncurried(Fay$$_(m)(x),Prelude.forM_$36$uncurried(xs,m));
      }
      if (Fay$$_($p1) === null) {
        return Fay$$return$36$uncurried(Fay$$unit);
      }
      throw ["unhandled case in forM_",[$p1,$p2]];
    });
  };
};
Prelude.mapM = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var lst = $p2;
      var fn = $p1;
      return Prelude.$36$$36$uncurried(Prelude.sequence,Prelude.map$36$uncurried(fn,lst));
    });
  };
};
Prelude.mapM_ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var m = $p1;
        return Fay$$then$36$uncurried(Fay$$_(m)(x),Prelude.mapM_$36$uncurried(m,xs));
      }
      if (Fay$$_($p2) === null) {
        return Fay$$return$36$uncurried(Fay$$unit);
      }
      throw ["unhandled case in mapM_",[$p1,$p2]];
    });
  };
};
Prelude.$61$$60$$60$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$bind$36$uncurried(x,f);
    });
  };
};
Prelude.$_void = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return Fay$$then$36$uncurried(f,Fay$$return$36$uncurried(Fay$$unit));
  });
};
Prelude.$62$$61$$62$ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var x = $p3;
        var g = $p2;
        var f = $p1;
        return Fay$$bind$36$uncurried(Fay$$_(f)(x),g);
      });
    };
  };
};
Prelude.$60$$61$$60$ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var x = $p3;
        var f = $p2;
        var g = $p1;
        return Fay$$bind$36$uncurried(Fay$$_(f)(x),g);
      });
    };
  };
};
Prelude.sequence = function($p1){
  return new Fay$$$(function(){
    var ms = $p1;
    return (function(){
      var k = function($p1){
        return function($p2){
          return new Fay$$$(function(){
            var m$39$ = $p2;
            var m = $p1;
            return Fay$$bind$36$uncurried(m,function($p1){
              var x = $p1;
              return Fay$$bind$36$uncurried(m$39$,function($p1){
                var xs = $p1;
                return Fay$$return$36$uncurried(Fay$$_(Fay$$_(Fay$$cons)(x))(xs));
              });
            });
          });
        };
      };
      return Prelude.foldr$36$uncurried(k,Fay$$return$36$uncurried(null),ms);
    })();
  });
};
Prelude.sequence_ = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$return$36$uncurried(Fay$$unit);
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var m = $tmp1.car;
      var ms = $tmp1.cdr;
      return Fay$$then$36$uncurried(m,Prelude.sequence_$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ms));
    }
    throw ["unhandled case in sequence_",[$p1]];
  });
};
Prelude._GT = function GT(){
};
Prelude._GT.prototype.instance = "GT";
Prelude.GT = new Fay$$$(function(){
  return new Prelude._GT();
});
Prelude._LT = function LT(){
};
Prelude._LT.prototype.instance = "LT";
Prelude.LT = new Fay$$$(function(){
  return new Prelude._LT();
});
Prelude._EQ = function EQ(){
};
Prelude._EQ.prototype.instance = "EQ";
Prelude.EQ = new Fay$$$(function(){
  return new Prelude._EQ();
});
Prelude.compare = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$gt$36$uncurried(x,y)) ? Prelude.GT : Fay$$_(Fay$$lt$36$uncurried(x,y)) ? Prelude.LT : Prelude.EQ;
    });
  };
};
Prelude.succ = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$add$36$uncurried(x,1);
  });
};
Prelude.pred = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$sub$36$uncurried(x,1);
  });
};
Prelude.enumFrom = function($p1){
  return new Fay$$$(function(){
    var i = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(i))(Prelude.enumFrom$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$add$36$uncurried(i,1)));
  });
};
Prelude.enumFromTo = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var n = $p2;
      var i = $p1;
      return Fay$$_(Fay$$gt$36$uncurried(i,n)) ? null : Fay$$_(Fay$$_(Fay$$cons)(i))(Prelude.enumFromTo$36$uncurried(Fay$$add$36$uncurried(i,1),n));
    });
  };
};
Prelude.enumFromBy = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var by = $p2;
      var fr = $p1;
      return Fay$$_(Fay$$_(Fay$$cons)(fr))(Prelude.enumFromBy$36$uncurried(Fay$$add$36$uncurried(fr,by),by));
    });
  };
};
Prelude.enumFromThen = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var th = $p2;
      var fr = $p1;
      return Prelude.enumFromBy$36$uncurried(fr,Fay$$sub$36$uncurried(th,fr));
    });
  };
};
Prelude.enumFromByTo = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var to = $p3;
        var by = $p2;
        var fr = $p1;
        return (function(){
          var neg = function($p1){
            return new Fay$$$(function(){
              var x = $p1;
              return Fay$$_(Fay$$lt$36$uncurried(x,to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(neg)(Fay$$add$36$uncurried(x,by)));
            });
          };
          var pos = function($p1){
            return new Fay$$$(function(){
              var x = $p1;
              return Fay$$_(Fay$$gt$36$uncurried(x,to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(pos)(Fay$$add$36$uncurried(x,by)));
            });
          };
          return Fay$$_(Fay$$lt$36$uncurried(by,0)) ? Fay$$_(neg)(fr) : Fay$$_(pos)(fr);
        })();
      });
    };
  };
};
Prelude.enumFromThenTo = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var to = $p3;
        var th = $p2;
        var fr = $p1;
        return Prelude.enumFromByTo$36$uncurried(fr,Fay$$sub$36$uncurried(th,fr),to);
      });
    };
  };
};
Prelude.fromIntegral = function($p1){
  return new Fay$$$(function(){
    return $p1;
  });
};
Prelude.fromInteger = function($p1){
  return new Fay$$$(function(){
    return $p1;
  });
};
Prelude.not = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Prelude.otherwise = true;
Prelude.show = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_string(JSON.stringify(Fay$$fayToJs(["automatic"],$p1)));
  });
};
Prelude.error = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());
  });
};
Prelude.$_undefined = new Fay$$$(function(){
  return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("Prelude.undefined"));
});
Prelude.either = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) instanceof Prelude._Left) {
          var a = Fay$$_($p3).slot1;
          var f = $p1;
          return Fay$$_(f)(a);
        }
        if (Fay$$_($p3) instanceof Prelude._Right) {
          var b = Fay$$_($p3).slot1;
          var g = $p2;
          return Fay$$_(g)(b);
        }
        throw ["unhandled case in either",[$p1,$p2,$p3]];
      });
    };
  };
};
Prelude.until = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var x = $p3;
        var f = $p2;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? x : Prelude.until$36$uncurried(p,f,Fay$$_(f)(x));
      });
    };
  };
};
Prelude.$36$$33$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(Fay$$_(Fay$$seq)(x))(Fay$$_(f)(x));
    });
  };
};
Prelude.$_const = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var a = $p1;
      return a;
    });
  };
};
Prelude.id = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return x;
  });
};
Prelude.$46$ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var x = $p3;
        var g = $p2;
        var f = $p1;
        return Fay$$_(f)(Fay$$_(g)(x));
      });
    };
  };
};
Prelude.$36$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(f)(x);
    });
  };
};
Prelude.flip = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var y = $p3;
        var x = $p2;
        var f = $p1;
        return Fay$$_(Fay$$_(f)(y))(x);
      });
    };
  };
};
Prelude.curry = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var y = $p3;
        var x = $p2;
        var f = $p1;
        return Fay$$_(f)(Fay$$list([x,y]));
      });
    };
  };
};
Prelude.uncurry = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var p = $p2;
      var f = $p1;
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var x = Fay$$index(0,Fay$$_($tmp1));
          var y = Fay$$index(1,Fay$$_($tmp1));
          return Fay$$_(Fay$$_(f)(x))(y);
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(p);
    });
  };
};
Prelude.snd = function($p1){
  return new Fay$$$(function(){
    if (Fay$$listLen(Fay$$_($p1),2)) {
      var x = Fay$$index(1,Fay$$_($p1));
      return x;
    }
    throw ["unhandled case in snd",[$p1]];
  });
};
Prelude.fst = function($p1){
  return new Fay$$$(function(){
    if (Fay$$listLen(Fay$$_($p1),2)) {
      var x = Fay$$index(0,Fay$$_($p1));
      return x;
    }
    throw ["unhandled case in fst",[$p1]];
  });
};
Prelude.div = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      if (Fay$$_(Fay$$and$36$uncurried(Fay$$gt$36$uncurried(x,0),Fay$$lt$36$uncurried(y,0)))) {
        return Fay$$sub$36$uncurried(Prelude.quot$36$uncurried(Fay$$sub$36$uncurried(x,1),y),1);
      } else {
        if (Fay$$_(Fay$$and$36$uncurried(Fay$$lt$36$uncurried(x,0),Fay$$gt$36$uncurried(y,0)))) {
          return Fay$$sub$36$uncurried(Prelude.quot$36$uncurried(Fay$$add$36$uncurried(x,1),y),1);
        }
      }
      var y = $p2;
      var x = $p1;
      return Prelude.quot$36$uncurried(x,y);
    });
  };
};
Prelude.mod = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      if (Fay$$_(Fay$$and$36$uncurried(Fay$$gt$36$uncurried(x,0),Fay$$lt$36$uncurried(y,0)))) {
        return Fay$$add$36$uncurried(Fay$$add$36$uncurried(Prelude.rem$36$uncurried(Fay$$sub$36$uncurried(x,1),y),y),1);
      } else {
        if (Fay$$_(Fay$$and$36$uncurried(Fay$$lt$36$uncurried(x,0),Fay$$gt$36$uncurried(y,0)))) {
          return Fay$$sub$36$uncurried(Fay$$add$36$uncurried(Prelude.rem$36$uncurried(Fay$$add$36$uncurried(x,1),y),y),1);
        }
      }
      var y = $p2;
      var x = $p1;
      return Prelude.rem$36$uncurried(x,y);
    });
  };
};
Prelude.divMod = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      if (Fay$$_(Fay$$and$36$uncurried(Fay$$gt$36$uncurried(x,0),Fay$$lt$36$uncurried(y,0)))) {
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var q = Fay$$index(0,Fay$$_($tmp1));
            var r = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$sub$36$uncurried(q,1),Fay$$add$36$uncurried(Fay$$add$36$uncurried(r,y),1)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.quotRem$36$uncurried(Fay$$sub$36$uncurried(x,1),y));
      } else {
        if (Fay$$_(Fay$$and$36$uncurried(Fay$$lt$36$uncurried(x,0),Fay$$gt$36$uncurried(y,1)))) {
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var q = Fay$$index(0,Fay$$_($tmp1));
              var r = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$list([Fay$$sub$36$uncurried(q,1),Fay$$sub$36$uncurried(Fay$$add$36$uncurried(r,y),1)]);
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Prelude.quotRem$36$uncurried(Fay$$add$36$uncurried(x,1),y));
        }
      }
      var y = $p2;
      var x = $p1;
      return Prelude.quotRem$36$uncurried(x,y);
    });
  };
};
Prelude.min = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["unknown"],Math.min(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));
    });
  };
};
Prelude.max = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["unknown"],Math.max(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));
    });
  };
};
Prelude.recip = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(1))(x);
  });
};
Prelude.negate = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (-(Fay$$_(x)));
  });
};
Prelude.abs = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.negate$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : x;
  });
};
Prelude.signum = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$gt$36$uncurried(x,0)) ? 1 : Fay$$_(Fay$$eq$36$uncurried(x,0)) ? 0 : (-(1));
  });
};
Prelude.pi = new Fay$$$(function(){
  return Fay$$jsToFay_double(Math.PI);
});
Prelude.exp = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));
  });
};
Prelude.sqrt = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));
  });
};
Prelude.log = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));
  });
};
Prelude.$42$$42$ = new Fay$$$(function(){
  return Prelude.unsafePow;
});
Prelude.$94$$94$ = new Fay$$$(function(){
  return Prelude.unsafePow;
});
Prelude.unsafePow = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["unknown"],Math.pow(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));
    });
  };
};
Prelude.$94$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      if (Fay$$_(Fay$$lt$36$uncurried(b,0))) {
        return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("(^): negative exponent"));
      } else {
        if (Fay$$_(Fay$$eq$36$uncurried(b,0))) {
          return 1;
        } else {
          if (Fay$$_(Prelude.even$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b))) {
            return (function(){
              return new Fay$$$(function(){
                var x = new Fay$$$(function(){
                  return Prelude.$94$$36$uncurried(a,Prelude.quot$36$uncurried(b,2));
                });
                return Fay$$mult$36$uncurried(x,x);
              });
            })();
          }
        }
      }
      var b = $p2;
      var a = $p1;
      return Fay$$mult$36$uncurried(a,Prelude.$94$$36$uncurried(a,Fay$$sub$36$uncurried(b,1)));
    });
  };
};
Prelude.logBase = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var b = $p1;
      return Fay$$_(Fay$$_(Fay$$divi)(Prelude.log$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x)))(Prelude.log$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b));
    });
  };
};
Prelude.sin = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sin(Fay$$fayToJs_double($p1)));
  });
};
Prelude.tan = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.tan(Fay$$fayToJs_double($p1)));
  });
};
Prelude.cos = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.cos(Fay$$fayToJs_double($p1)));
  });
};
Prelude.asin = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.asin(Fay$$fayToJs_double($p1)));
  });
};
Prelude.atan = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.atan(Fay$$fayToJs_double($p1)));
  });
};
Prelude.acos = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.acos(Fay$$fayToJs_double($p1)));
  });
};
Prelude.sinh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(Fay$$sub$36$uncurried(Prelude.exp$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x),Prelude.exp$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried((-(Fay$$_(x)))))))(2);
  });
};
Prelude.tanh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (function(){
      return new Fay$$$(function(){
        var a = new Fay$$$(function(){
          return Prelude.exp$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
        });
        var b = new Fay$$$(function(){
          return Prelude.exp$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried((-(Fay$$_(x))));
        });
        return Fay$$_(Fay$$_(Fay$$divi)(Fay$$sub$36$uncurried(a,b)))(Fay$$add$36$uncurried(a,b));
      });
    })();
  });
};
Prelude.cosh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(Fay$$add$36$uncurried(Prelude.exp$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x),Prelude.exp$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried((-(Fay$$_(x)))))))(2);
  });
};
Prelude.asinh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Prelude.log$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$add$36$uncurried(x,Prelude.sqrt$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$add$36$uncurried(Fay$$_(Fay$$_(Prelude.$42$$42$)(x))(2),1))));
  });
};
Prelude.atanh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(Prelude.log$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$_(Fay$$_(Fay$$divi)(Fay$$add$36$uncurried(1,x)))(Fay$$sub$36$uncurried(1,x)))))(2);
  });
};
Prelude.acosh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Prelude.log$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$add$36$uncurried(x,Prelude.sqrt$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$sub$36$uncurried(Fay$$_(Fay$$_(Prelude.$42$$42$)(x))(2),1))));
  });
};
Prelude.properFraction = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (function(){
      return new Fay$$$(function(){
        var a = new Fay$$$(function(){
          return Prelude.truncate$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
        });
        return Fay$$list([a,Fay$$sub$36$uncurried(x,Prelude.fromIntegral$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(a))]);
      });
    })();
  });
};
Prelude.truncate = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.ceiling$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : Prelude.floor$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
  });
};
Prelude.round = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.round(Fay$$fayToJs_double($p1)));
  });
};
Prelude.ceiling = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));
  });
};
Prelude.floor = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));
  });
};
Prelude.subtract = new Fay$$$(function(){
  return Prelude.flip(Fay$$sub);
});
Prelude.even = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$eq$36$uncurried(Prelude.rem$36$uncurried(x,2),0);
  });
};
Prelude.odd = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Prelude.even$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x));
  });
};
Prelude.gcd = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      return (function(){
        var go = function($p1){
          return function($p2){
            return new Fay$$$(function(){
              if (Fay$$_($p2) === 0) {
                var x = $p1;
                return x;
              }
              var y = $p2;
              var x = $p1;
              return Fay$$_(Fay$$_(go)(y))(Prelude.rem$36$uncurried(x,y));
            });
          };
        };
        return Fay$$_(Fay$$_(go)(Prelude.abs$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(a)))(Prelude.abs$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b));
      })();
    });
  };
};
Prelude.quot = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$eq$36$uncurried(y,0)) ? Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("Division by zero")) : Prelude.quot$39$$36$uncurried(x,y);
    });
  };
};
Prelude.quot$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_int(~~(Fay$$fayToJs_int($p1)/Fay$$fayToJs_int($p2)));
    });
  };
};
Prelude.quotRem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$list([Prelude.quot$36$uncurried(x,y),Prelude.rem$36$uncurried(x,y)]);
    });
  };
};
Prelude.rem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$eq$36$uncurried(y,0)) ? Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("Division by zero")) : Prelude.rem$39$$36$uncurried(x,y);
    });
  };
};
Prelude.rem$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_int(Fay$$fayToJs_int($p1) % Fay$$fayToJs_int($p2));
    });
  };
};
Prelude.lcm = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === 0) {
        return 0;
      }
      if (Fay$$_($p1) === 0) {
        return 0;
      }
      var b = $p2;
      var a = $p1;
      return Prelude.abs$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$mult$36$uncurried(Prelude.quot$36$uncurried(a,Prelude.gcd$36$uncurried(a,b)),b));
    });
  };
};
Prelude.find = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Prelude.Just$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : Prelude.find$36$uncurried(p,xs);
      }
      if (Fay$$_($p2) === null) {
        return Prelude.Nothing;
      }
      throw ["unhandled case in find",[$p1,$p2]];
    });
  };
};
Prelude.filter = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.filter$36$uncurried(p,xs)) : Prelude.filter$36$uncurried(p,xs);
      }
      if (Fay$$_($p2) === null) {
        return null;
      }
      throw ["unhandled case in filter",[$p1,$p2]];
    });
  };
};
Prelude.$_null = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    return false;
  });
};
Prelude.map = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(f)(x)))(Prelude.map$36$uncurried(f,xs));
      }
      throw ["unhandled case in map",[$p1,$p2]];
    });
  };
};
Prelude.nub = function($p1){
  return new Fay$$$(function(){
    var ls = $p1;
    return Prelude.nub$39$$36$uncurried(ls,null);
  });
};
Prelude.nub$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p1) === null) {
        return null;
      }
      var ls = $p2;
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return Fay$$_(Prelude.elem$36$uncurried(x,ls)) ? Prelude.nub$39$$36$uncurried(xs,ls) : Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.nub$39$$36$uncurried(xs,Fay$$_(Fay$$_(Fay$$cons)(x))(ls)));
      }
      throw ["unhandled case in nub'",[$p1,$p2]];
    });
  };
};
Prelude.elem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var y = $tmp1.car;
        var ys = $tmp1.cdr;
        var x = $p1;
        return Fay$$or$36$uncurried(Fay$$eq$36$uncurried(x,y),Prelude.elem$36$uncurried(x,ys));
      }
      if (Fay$$_($p2) === null) {
        return false;
      }
      throw ["unhandled case in elem",[$p1,$p2]];
    });
  };
};
Prelude.notElem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var ys = $p2;
      var x = $p1;
      return Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Prelude.elem$36$uncurried(x,ys));
    });
  };
};
Prelude.sort = new Fay$$$(function(){
  return Prelude.sortBy$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Prelude.compare);
});
Prelude.sortBy = function($p1){
  return new Fay$$$(function(){
    var cmp = $p1;
    return Fay$$_(Prelude.foldr(Prelude.insertBy(cmp)))(null);
  });
};
Prelude.insertBy = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var x = $p2;
          return Fay$$list([x]);
        }
        var ys = $p3;
        var x = $p2;
        var cmp = $p1;
        return (function($tmp1){
          if (Fay$$_($tmp1) === null) {
            return Fay$$list([x]);
          }
          var $tmp2 = Fay$$_($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var y = $tmp2.car;
            var ys$39$ = $tmp2.cdr;
            return (function($tmp2){
              if (Fay$$_($tmp2) instanceof Prelude._GT) {
                return Fay$$_(Fay$$_(Fay$$cons)(y))(Prelude.insertBy$36$uncurried(cmp,x,ys$39$));
              }
              return Fay$$_(Fay$$_(Fay$$cons)(x))(ys);
            })(Fay$$_(Fay$$_(cmp)(x))(y));
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(ys);
      });
    };
  };
};
Prelude.conc = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var ys = $p2;
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.conc$36$uncurried(xs,ys));
      }
      var ys = $p2;
      if (Fay$$_($p1) === null) {
        return ys;
      }
      throw ["unhandled case in conc",[$p1,$p2]];
    });
  };
};
Prelude.concat = new Fay$$$(function(){
  return Fay$$_(Prelude.foldr(Prelude.conc))(null);
});
Prelude.concatMap = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return Fay$$_(Prelude.foldr(Fay$$_(Prelude.$46$(Prelude.$43$$43$))(f)))(null);
  });
};
Prelude.foldr = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var z = $p2;
          return z;
        }
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return Fay$$_(Fay$$_(f)(x))(Prelude.foldr$36$uncurried(f,z,xs));
        }
        throw ["unhandled case in foldr",[$p1,$p2,$p3]];
      });
    };
  };
};
Prelude.foldr1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$listLen(Fay$$_($p2),1)) {
        var x = Fay$$index(0,Fay$$_($p2));
        return x;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(f)(x))(Prelude.foldr1$36$uncurried(f,xs));
      }
      if (Fay$$_($p2) === null) {
        return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("foldr1: empty list"));
      }
      throw ["unhandled case in foldr1",[$p1,$p2]];
    });
  };
};
Prelude.foldl = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var z = $p2;
          return z;
        }
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return Prelude.foldl$36$uncurried(f,Fay$$_(Fay$$_(f)(z))(x),xs);
        }
        throw ["unhandled case in foldl",[$p1,$p2,$p3]];
      });
    };
  };
};
Prelude.foldl1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Prelude.foldl$36$uncurried(f,x,xs);
      }
      if (Fay$$_($p2) === null) {
        return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("foldl1: empty list"));
      }
      throw ["unhandled case in foldl1",[$p1,$p2]];
    });
  };
};
Prelude.$43$$43$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Prelude.conc$36$uncurried(x,y);
    });
  };
};
Prelude.$33$$33$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      return (function(){
        var go = function($p1){
          return function($p2){
            return new Fay$$$(function(){
              if (Fay$$_($p1) === null) {
                return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("(!!): index too large"));
              }
              if (Fay$$_($p2) === 0) {
                var $tmp1 = Fay$$_($p1);
                if ($tmp1 instanceof Fay$$Cons) {
                  var h = $tmp1.car;
                  return h;
                }
              }
              var n = $p2;
              var $tmp1 = Fay$$_($p1);
              if ($tmp1 instanceof Fay$$Cons) {
                var t = $tmp1.cdr;
                return Fay$$_(Fay$$_(go)(t))(Fay$$sub$36$uncurried(n,1));
              }
              throw ["unhandled case in go",[$p1,$p2]];
            });
          };
        };
        return Fay$$_(Fay$$lt$36$uncurried(b,0)) ? Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("(!!): negative index")) : Fay$$_(Fay$$_(go)(a))(b);
      })();
    });
  };
};
Prelude.head = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("head: empty list"));
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      return h;
    }
    throw ["unhandled case in head",[$p1]];
  });
};
Prelude.tail = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("tail: empty list"));
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var t = $tmp1.cdr;
      return t;
    }
    throw ["unhandled case in tail",[$p1]];
  });
};
Prelude.init = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("init: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      var t = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$cons)(h))(Prelude.init$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(t));
    }
    throw ["unhandled case in init",[$p1]];
  });
};
Prelude.last = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("last: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      var a = Fay$$index(0,Fay$$_($p1));
      return a;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var t = $tmp1.cdr;
      return Prelude.last$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(t);
    }
    throw ["unhandled case in last",[$p1]];
  });
};
Prelude.iterate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.iterate$36$uncurried(f,Fay$$_(f)(x)));
    });
  };
};
Prelude.repeat = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.repeat$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x));
  });
};
Prelude.replicate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p1) === 0) {
        return null;
      }
      var x = $p2;
      var n = $p1;
      return Fay$$_(Fay$$lt$36$uncurried(n,0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.replicate$36$uncurried(Fay$$sub$36$uncurried(n,1),x));
    });
  };
};
Prelude.cycle = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("cycle: empty list"));
    }
    var xs = $p1;
    return (function(){
      var xs$39$ = new Fay$$$(function(){
        return Prelude.$43$$43$$36$uncurried(xs,xs$39$);
      });
      return xs$39$;
    })();
  });
};
Prelude.take = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p1) === 0) {
        return null;
      }
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var n = $p1;
        return Fay$$_(Fay$$lt$36$uncurried(n,0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.take$36$uncurried(Fay$$sub$36$uncurried(n,1),xs));
      }
      throw ["unhandled case in take",[$p1,$p2]];
    });
  };
};
Prelude.drop = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var xs = $p2;
      if (Fay$$_($p1) === 0) {
        return xs;
      }
      if (Fay$$_($p2) === null) {
        return null;
      }
      var xss = $p2;
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var xs = $tmp1.cdr;
        var n = $p1;
        return Fay$$_(Fay$$lt$36$uncurried(n,0)) ? xss : Prelude.drop$36$uncurried(Fay$$sub$36$uncurried(n,1),xs);
      }
      throw ["unhandled case in drop",[$p1,$p2]];
    });
  };
};
Prelude.splitAt = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var xs = $p2;
      if (Fay$$_($p1) === 0) {
        return Fay$$list([null,xs]);
      }
      if (Fay$$_($p2) === null) {
        return Fay$$list([null,null]);
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var n = $p1;
        return Fay$$_(Fay$$lt$36$uncurried(n,0)) ? Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]) : (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var a = Fay$$index(0,Fay$$_($tmp1));
            var b = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.splitAt$36$uncurried(Fay$$sub$36$uncurried(n,1),xs));
      }
      throw ["unhandled case in splitAt",[$p1,$p2]];
    });
  };
};
Prelude.takeWhile = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.takeWhile$36$uncurried(p,xs)) : null;
      }
      throw ["unhandled case in takeWhile",[$p1,$p2]];
    });
  };
};
Prelude.dropWhile = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Prelude.dropWhile$36$uncurried(p,xs) : Fay$$_(Fay$$_(Fay$$cons)(x))(xs);
      }
      throw ["unhandled case in dropWhile",[$p1,$p2]];
    });
  };
};
Prelude.span = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return Fay$$list([null,null]);
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var a = Fay$$index(0,Fay$$_($tmp1));
            var b = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.span$36$uncurried(p,xs)) : Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]);
      }
      throw ["unhandled case in span",[$p1,$p2]];
    });
  };
};
Prelude.$_break = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Prelude.span(Fay$$_(Prelude.$46$(Prelude.not))(p));
  });
};
Prelude.zipWith = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var b = $tmp1.car;
          var bs = $tmp1.cdr;
          var $tmp1 = Fay$$_($p2);
          if ($tmp1 instanceof Fay$$Cons) {
            var a = $tmp1.car;
            var as = $tmp1.cdr;
            var f = $p1;
            return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(a))(b)))(Prelude.zipWith$36$uncurried(f,as,bs));
          }
        }
        return null;
      });
    };
  };
};
Prelude.zipWith3 = function($p1){
  return function($p2){
    return function($p3){
      return function($p4){
        return new Fay$$$(function(){
          var $tmp1 = Fay$$_($p4);
          if ($tmp1 instanceof Fay$$Cons) {
            var c = $tmp1.car;
            var cs = $tmp1.cdr;
            var $tmp1 = Fay$$_($p3);
            if ($tmp1 instanceof Fay$$Cons) {
              var b = $tmp1.car;
              var bs = $tmp1.cdr;
              var $tmp1 = Fay$$_($p2);
              if ($tmp1 instanceof Fay$$Cons) {
                var a = $tmp1.car;
                var as = $tmp1.cdr;
                var f = $p1;
                return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$_(f)(a))(b))(c)))(Prelude.zipWith3$36$uncurried(f,as,bs,cs));
              }
            }
          }
          return null;
        });
      };
    };
  };
};
Prelude.zip = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var b = $tmp1.car;
        var bs = $tmp1.cdr;
        var $tmp1 = Fay$$_($p1);
        if ($tmp1 instanceof Fay$$Cons) {
          var a = $tmp1.car;
          var as = $tmp1.cdr;
          return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b])))(Prelude.zip$36$uncurried(as,bs));
        }
      }
      return null;
    });
  };
};
Prelude.zip3 = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var c = $tmp1.car;
          var cs = $tmp1.cdr;
          var $tmp1 = Fay$$_($p2);
          if ($tmp1 instanceof Fay$$Cons) {
            var b = $tmp1.car;
            var bs = $tmp1.cdr;
            var $tmp1 = Fay$$_($p1);
            if ($tmp1 instanceof Fay$$Cons) {
              var a = $tmp1.car;
              var as = $tmp1.cdr;
              return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b,c])))(Prelude.zip3$36$uncurried(as,bs,cs));
            }
          }
        }
        return null;
      });
    };
  };
};
Prelude.unzip = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null]);
    }
    throw ["unhandled case in unzip",[$p1]];
  });
};
Prelude.unzip3 = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),3)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var z = Fay$$index(2,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),3)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            var zs = Fay$$index(2,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip3$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null,null]);
    }
    throw ["unhandled case in unzip3",[$p1]];
  });
};
Prelude.lines = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var s = $p1;
    return (function(){
      var isLineBreak = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Fay$$or$36$uncurried(Fay$$eq$36$uncurried(c,"\r"),Fay$$eq$36$uncurried(c,"\n"));
        });
      };
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {
            return Fay$$list([a]);
          }
        }
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));
          if ($tmp2 instanceof Fay$$Cons) {
            var cs = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.lines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(cs));
          }
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isLineBreak))(s));
    })();
  });
};
Prelude.unlines = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var l = $tmp1.car;
      var ls = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(l,Fay$$_(Fay$$_(Fay$$cons)("\n"))(Prelude.unlines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ls)));
    }
    throw ["unhandled case in unlines",[$p1]];
  });
};
Prelude.words = function($p1){
  return new Fay$$$(function(){
    var str = $p1;
    return (function(){
      var words$39$ = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) === null) {
            return null;
          }
          var s = $p1;
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var a = Fay$$index(0,Fay$$_($tmp1));
              var b = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.words$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b));
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isSpace))(s));
        });
      };
      var isSpace = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Prelude.elem$36$uncurried(c,Fay$$list(" \t\r\n\u000c\u000b"));
        });
      };
      return Fay$$_(words$39$)(Prelude.dropWhile$36$uncurried(isSpace,str));
    })();
  });
};
Prelude.unwords = new Fay$$$(function(){
  return Prelude.intercalate(Fay$$list(" "));
});
Prelude.and = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$and$36$uncurried(x,Prelude.and$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in and",[$p1]];
  });
};
Prelude.or = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return false;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$or$36$uncurried(x,Prelude.or$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in or",[$p1]];
  });
};
Prelude.any = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return false;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$or$36$uncurried(Fay$$_(p)(x),Prelude.any$36$uncurried(p,xs));
      }
      throw ["unhandled case in any",[$p1,$p2]];
    });
  };
};
Prelude.all = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return true;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$and$36$uncurried(Fay$$_(p)(x),Prelude.all$36$uncurried(p,xs));
      }
      throw ["unhandled case in all",[$p1,$p2]];
    });
  };
};
Prelude.intersperse = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var sep = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.prependToAll$36$uncurried(sep,xs));
      }
      throw ["unhandled case in intersperse",[$p1,$p2]];
    });
  };
};
Prelude.prependToAll = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var sep = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(sep))(Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.prependToAll$36$uncurried(sep,xs)));
      }
      throw ["unhandled case in prependToAll",[$p1,$p2]];
    });
  };
};
Prelude.intercalate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var xss = $p2;
      var xs = $p1;
      return Fay$$_(Prelude.concat)(Prelude.intersperse$36$uncurried(xs,xss));
    });
  };
};
Prelude.maximum = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("maximum: empty list"));
    }
    var xs = $p1;
    return Prelude.foldl1$36$uncurried(Prelude.max,xs);
  });
};
Prelude.minimum = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("minimum: empty list"));
    }
    var xs = $p1;
    return Prelude.foldl1$36$uncurried(Prelude.min,xs);
  });
};
Prelude.product = function($p1){
  return new Fay$$$(function(){
    var xs = $p1;
    return Prelude.foldl$36$uncurried(Fay$$mult,1,xs);
  });
};
Prelude.sum = function($p1){
  return new Fay$$$(function(){
    var xs = $p1;
    return Prelude.foldl$36$uncurried(Fay$$add,0,xs);
  });
};
Prelude.scanl = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var l = $p3;
        var z = $p2;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(z))((function($tmp1){
          if (Fay$$_($tmp1) === null) {
            return null;
          }
          var $tmp2 = Fay$$_($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var x = $tmp2.car;
            var xs = $tmp2.cdr;
            return Prelude.scanl$36$uncurried(f,Fay$$_(Fay$$_(f)(z))(x),xs);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(l));
      });
    };
  };
};
Prelude.scanl1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Prelude.scanl$36$uncurried(f,x,xs);
      }
      throw ["unhandled case in scanl1",[$p1,$p2]];
    });
  };
};
Prelude.scanr = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var z = $p2;
          return Fay$$list([z]);
        }
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return (function($tmp1){
            var $tmp2 = Fay$$_($tmp1);
            if ($tmp2 instanceof Fay$$Cons) {
              var h = $tmp2.car;
              var t = $tmp2.cdr;
              return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));
            }
            return Prelude.$_undefined;
          })(Prelude.scanr$36$uncurried(f,z,xs));
        }
        throw ["unhandled case in scanr",[$p1,$p2,$p3]];
      });
    };
  };
};
Prelude.scanr1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      if (Fay$$listLen(Fay$$_($p2),1)) {
        var x = Fay$$index(0,Fay$$_($p2));
        return Fay$$list([x]);
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return (function($tmp1){
          var $tmp2 = Fay$$_($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var h = $tmp2.car;
            var t = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));
          }
          return Prelude.$_undefined;
        })(Prelude.scanr1$36$uncurried(f,xs));
      }
      throw ["unhandled case in scanr1",[$p1,$p2]];
    });
  };
};
Prelude.lookup = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        var _key = $p1;
        return Prelude.Nothing;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
          var x = Fay$$index(0,Fay$$_($tmp1.car));
          var y = Fay$$index(1,Fay$$_($tmp1.car));
          var xys = $tmp1.cdr;
          var key = $p1;
          return Fay$$_(Fay$$eq$36$uncurried(key,x)) ? Prelude.Just$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(y) : Prelude.lookup$36$uncurried(key,xys);
        }
      }
      throw ["unhandled case in lookup",[$p1,$p2]];
    });
  };
};
Prelude.length = function($p1){
  return new Fay$$$(function(){
    var xs = $p1;
    return Prelude.length$39$$36$uncurried(0,xs);
  });
};
Prelude.length$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var xs = $tmp1.cdr;
        var acc = $p1;
        return Prelude.length$39$$36$uncurried(Fay$$add$36$uncurried(acc,1),xs);
      }
      var acc = $p1;
      return acc;
    });
  };
};
Prelude.reverse = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(Prelude.reverse$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs),Fay$$list([x]));
    }
    if (Fay$$_($p1) === null) {
      return null;
    }
    throw ["unhandled case in reverse",[$p1]];
  });
};
Prelude.print = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["automatic"],$p1))));
  });
};
Prelude.putStrLn = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs_string($p1))));
  });
};
Prelude.ifThenElse = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var b = $p3;
        var a = $p2;
        var p = $p1;
        return Fay$$_(p) ? a : b;
      });
    };
  };
};
Fay$$objConcat(Fay$$fayToJsHash,{"Just": function(type,argTypes,_obj){
  var obj_ = {"instance": "Just"};
  var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"Nothing": function(type,argTypes,_obj){
  var obj_ = {"instance": "Nothing"};
  return obj_;
},"Left": function(type,argTypes,_obj){
  var obj_ = {"instance": "Left"};
  var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"Right": function(type,argTypes,_obj){
  var obj_ = {"instance": "Right"};
  var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"GT": function(type,argTypes,_obj){
  var obj_ = {"instance": "GT"};
  return obj_;
},"LT": function(type,argTypes,_obj){
  var obj_ = {"instance": "LT"};
  return obj_;
},"EQ": function(type,argTypes,_obj){
  var obj_ = {"instance": "EQ"};
  return obj_;
}});
Fay$$objConcat(Fay$$jsToFayHash,{"Just": function(type,argTypes,obj){
  return new Prelude._Just(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
},"Nothing": function(type,argTypes,obj){
  return new Prelude._Nothing();
},"Left": function(type,argTypes,obj){
  return new Prelude._Left(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
},"Right": function(type,argTypes,obj){
  return new Prelude._Right(Fay$$jsToFay(argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
},"GT": function(type,argTypes,obj){
  return new Prelude._GT();
},"LT": function(type,argTypes,obj){
  return new Prelude._LT();
},"EQ": function(type,argTypes,obj){
  return new Prelude._EQ();
}});
Prelude.reverse$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(Prelude.reverse$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs),Fay$$list([x]));
    }
    if (Fay$$_($p1) === null) {
      return null;
    }
    throw ["unhandled case in reverse",[$p1]];
  });
};
Prelude.$43$$43$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var y = $p2;
    var x = $p1;
    return Prelude.conc$36$uncurried(x,y);
  });
};
Prelude.length$39$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    while (true) {
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var xs = $tmp1.cdr;
        var acc = $p1;
        $p1 = Fay$$add$36$uncurried(acc,1);
        $p2 = xs;
        continue;
      }
      var acc = $p1;
      return acc;
    }
  });
};
Prelude.lookup$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p2) === null) {
      var _key = $p1;
      return Prelude.Nothing;
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var xys = $tmp1.cdr;
        var key = $p1;
        return Fay$$_(Fay$$eq$36$uncurried(key,x)) ? Prelude.Just$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(y) : Prelude.lookup$36$uncurried(key,xys);
      }
    }
    throw ["unhandled case in lookup",[$p1,$p2]];
  });
};
Prelude.Just$36$uncurried = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Just(slot1);
  });
};
Prelude.scanr1$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p2) === null) {
      return null;
    }
    if (Fay$$listLen(Fay$$_($p2),1)) {
      var x = Fay$$index(0,Fay$$_($p2));
      return Fay$$list([x]);
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var f = $p1;
      return (function($tmp1){
        var $tmp2 = Fay$$_($tmp1);
        if ($tmp2 instanceof Fay$$Cons) {
          var h = $tmp2.car;
          var t = $tmp2.cdr;
          return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));
        }
        return Prelude.$_undefined;
      })(Prelude.scanr1$36$uncurried(f,xs));
    }
    throw ["unhandled case in scanr1",[$p1,$p2]];
  });
};
Prelude.scanr$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    if (Fay$$_($p3) === null) {
      var z = $p2;
      return Fay$$list([z]);
    }
    var $tmp1 = Fay$$_($p3);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var z = $p2;
      var f = $p1;
      return (function($tmp1){
        var $tmp2 = Fay$$_($tmp1);
        if ($tmp2 instanceof Fay$$Cons) {
          var h = $tmp2.car;
          var t = $tmp2.cdr;
          return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));
        }
        return Prelude.$_undefined;
      })(Prelude.scanr$36$uncurried(f,z,xs));
    }
    throw ["unhandled case in scanr",[$p1,$p2,$p3]];
  });
};
Prelude.scanl$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    var l = $p3;
    var z = $p2;
    var f = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(z))((function($tmp1){
      if (Fay$$_($tmp1) === null) {
        return null;
      }
      var $tmp2 = Fay$$_($tmp1);
      if ($tmp2 instanceof Fay$$Cons) {
        var x = $tmp2.car;
        var xs = $tmp2.cdr;
        return Prelude.scanl$36$uncurried(f,Fay$$_(Fay$$_(f)(z))(x),xs);
      }
      return (function(){ throw (["unhandled case",$tmp1]); })();
    })(l));
  });
};
Prelude.foldl$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    while (true) {
      if (Fay$$_($p3) === null) {
        var z = $p2;
        return z;
      }
      var $tmp1 = Fay$$_($p3);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var z = $p2;
        var f = $p1;
        $p1 = f;
        $p2 = Fay$$_(Fay$$_(f)(z))(x);
        $p3 = xs;
        continue;
      }
      throw ["unhandled case in foldl",[$p1,$p2,$p3]];
    }
  });
};
Prelude.foldl1$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var f = $p1;
      return Prelude.foldl$36$uncurried(f,x,xs);
    }
    if (Fay$$_($p2) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("foldl1: empty list"));
    }
    throw ["unhandled case in foldl1",[$p1,$p2]];
  });
};
Prelude.error$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());
  });
};
Prelude.intersperse$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p2) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var sep = $p1;
      return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.prependToAll$36$uncurried(sep,xs));
    }
    throw ["unhandled case in intersperse",[$p1,$p2]];
  });
};
Prelude.prependToAll$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p2) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var sep = $p1;
      return Fay$$_(Fay$$_(Fay$$cons)(sep))(Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.prependToAll$36$uncurried(sep,xs)));
    }
    throw ["unhandled case in prependToAll",[$p1,$p2]];
  });
};
Prelude.all$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p2) === null) {
      return true;
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var p = $p1;
      return Fay$$and$36$uncurried(Fay$$_(p)(x),Prelude.all$36$uncurried(p,xs));
    }
    throw ["unhandled case in all",[$p1,$p2]];
  });
};
Prelude.any$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p2) === null) {
      return false;
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var p = $p1;
      return Fay$$or$36$uncurried(Fay$$_(p)(x),Prelude.any$36$uncurried(p,xs));
    }
    throw ["unhandled case in any",[$p1,$p2]];
  });
};
Prelude.or$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return false;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$or$36$uncurried(x,Prelude.or$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in or",[$p1]];
  });
};
Prelude.and$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$and$36$uncurried(x,Prelude.and$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in and",[$p1]];
  });
};
Prelude.dropWhile$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p2) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var p = $p1;
      return Fay$$_(Fay$$_(p)(x)) ? Prelude.dropWhile$36$uncurried(p,xs) : Fay$$_(Fay$$_(Fay$$cons)(x))(xs);
    }
    throw ["unhandled case in dropWhile",[$p1,$p2]];
  });
};
Prelude.elem$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var y = $tmp1.car;
      var ys = $tmp1.cdr;
      var x = $p1;
      return Fay$$or$36$uncurried(Fay$$eq$36$uncurried(x,y),Prelude.elem$36$uncurried(x,ys));
    }
    if (Fay$$_($p2) === null) {
      return false;
    }
    throw ["unhandled case in elem",[$p1,$p2]];
  });
};
Prelude.break$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Prelude.span(Fay$$_(Prelude.$46$(Prelude.not))(p));
  });
};
Prelude.words$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var str = $p1;
    return (function(){
      var words$39$ = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) === null) {
            return null;
          }
          var s = $p1;
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var a = Fay$$index(0,Fay$$_($tmp1));
              var b = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.words$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b));
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isSpace))(s));
        });
      };
      var isSpace = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Prelude.elem$36$uncurried(c,Fay$$list(" \t\r\n\u000c\u000b"));
        });
      };
      return Fay$$_(words$39$)(Prelude.dropWhile$36$uncurried(isSpace,str));
    })();
  });
};
Prelude.unlines$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var l = $tmp1.car;
      var ls = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(l,Fay$$_(Fay$$_(Fay$$cons)("\n"))(Prelude.unlines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ls)));
    }
    throw ["unhandled case in unlines",[$p1]];
  });
};
Prelude.lines$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var s = $p1;
    return (function(){
      var isLineBreak = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Fay$$or$36$uncurried(Fay$$eq$36$uncurried(c,"\r"),Fay$$eq$36$uncurried(c,"\n"));
        });
      };
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {
            return Fay$$list([a]);
          }
        }
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));
          if ($tmp2 instanceof Fay$$Cons) {
            var cs = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.lines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(cs));
          }
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isLineBreak))(s));
    })();
  });
};
Prelude.unzip3$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),3)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var z = Fay$$index(2,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),3)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            var zs = Fay$$index(2,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip3$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null,null]);
    }
    throw ["unhandled case in unzip3",[$p1]];
  });
};
Prelude.unzip$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null]);
    }
    throw ["unhandled case in unzip",[$p1]];
  });
};
Prelude.zip3$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p3);
    if ($tmp1 instanceof Fay$$Cons) {
      var c = $tmp1.car;
      var cs = $tmp1.cdr;
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var b = $tmp1.car;
        var bs = $tmp1.cdr;
        var $tmp1 = Fay$$_($p1);
        if ($tmp1 instanceof Fay$$Cons) {
          var a = $tmp1.car;
          var as = $tmp1.cdr;
          return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b,c])))(Prelude.zip3$36$uncurried(as,bs,cs));
        }
      }
    }
    return null;
  });
};
Prelude.zip$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var b = $tmp1.car;
      var bs = $tmp1.cdr;
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var a = $tmp1.car;
        var as = $tmp1.cdr;
        return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b])))(Prelude.zip$36$uncurried(as,bs));
      }
    }
    return null;
  });
};
Prelude.zipWith3$36$uncurried = function($p1,$p2,$p3,$p4){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p4);
    if ($tmp1 instanceof Fay$$Cons) {
      var c = $tmp1.car;
      var cs = $tmp1.cdr;
      var $tmp1 = Fay$$_($p3);
      if ($tmp1 instanceof Fay$$Cons) {
        var b = $tmp1.car;
        var bs = $tmp1.cdr;
        var $tmp1 = Fay$$_($p2);
        if ($tmp1 instanceof Fay$$Cons) {
          var a = $tmp1.car;
          var as = $tmp1.cdr;
          var f = $p1;
          return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$_(f)(a))(b))(c)))(Prelude.zipWith3$36$uncurried(f,as,bs,cs));
        }
      }
    }
    return null;
  });
};
Prelude.zipWith$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p3);
    if ($tmp1 instanceof Fay$$Cons) {
      var b = $tmp1.car;
      var bs = $tmp1.cdr;
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var a = $tmp1.car;
        var as = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(a))(b)))(Prelude.zipWith$36$uncurried(f,as,bs));
      }
    }
    return null;
  });
};
Prelude.span$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p2) === null) {
      return Fay$$list([null,null]);
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var p = $p1;
      return Fay$$_(Fay$$_(p)(x)) ? (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          var b = Fay$$index(1,Fay$$_($tmp1));
          return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(Prelude.span$36$uncurried(p,xs)) : Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]);
    }
    throw ["unhandled case in span",[$p1,$p2]];
  });
};
Prelude.takeWhile$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p2) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var p = $p1;
      return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.takeWhile$36$uncurried(p,xs)) : null;
    }
    throw ["unhandled case in takeWhile",[$p1,$p2]];
  });
};
Prelude.splitAt$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var xs = $p2;
    if (Fay$$_($p1) === 0) {
      return Fay$$list([null,xs]);
    }
    if (Fay$$_($p2) === null) {
      return Fay$$list([null,null]);
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var n = $p1;
      return Fay$$_(Fay$$lt$36$uncurried(n,0)) ? Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]) : (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          var b = Fay$$index(1,Fay$$_($tmp1));
          return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(Prelude.splitAt$36$uncurried(Fay$$sub$36$uncurried(n,1),xs));
    }
    throw ["unhandled case in splitAt",[$p1,$p2]];
  });
};
Prelude.drop$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var xs = $p2;
    if (Fay$$_($p1) === 0) {
      return xs;
    }
    if (Fay$$_($p2) === null) {
      return null;
    }
    var xss = $p2;
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var xs = $tmp1.cdr;
      var n = $p1;
      return Fay$$_(Fay$$lt$36$uncurried(n,0)) ? xss : Prelude.drop$36$uncurried(Fay$$sub$36$uncurried(n,1),xs);
    }
    throw ["unhandled case in drop",[$p1,$p2]];
  });
};
Prelude.take$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === 0) {
      return null;
    }
    if (Fay$$_($p2) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var n = $p1;
      return Fay$$_(Fay$$lt$36$uncurried(n,0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.take$36$uncurried(Fay$$sub$36$uncurried(n,1),xs));
    }
    throw ["unhandled case in take",[$p1,$p2]];
  });
};
Prelude.replicate$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === 0) {
      return null;
    }
    var x = $p2;
    var n = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(n,0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.replicate$36$uncurried(Fay$$sub$36$uncurried(n,1),x));
  });
};
Prelude.repeat$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.repeat$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x));
  });
};
Prelude.iterate$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var x = $p2;
    var f = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.iterate$36$uncurried(f,Fay$$_(f)(x)));
  });
};
Prelude.last$36$uncurried = function($p1){
  return new Fay$$$(function(){
    while (true) {
      if (Fay$$_($p1) === null) {
        return Prelude.error$36$uncurried(Fay$$list("last: empty list"));
      }
      if (Fay$$listLen(Fay$$_($p1),1)) {
        var a = Fay$$index(0,Fay$$_($p1));
        return a;
      }
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var t = $tmp1.cdr;
        $p1 = t;
        continue;
      }
      throw ["unhandled case in last",[$p1]];
    }
  });
};
Prelude.init$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("init: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      var t = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$cons)(h))(Prelude.init$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(t));
    }
    throw ["unhandled case in init",[$p1]];
  });
};
Prelude.conc$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var ys = $p2;
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.conc$36$uncurried(xs,ys));
    }
    var ys = $p2;
    if (Fay$$_($p1) === null) {
      return ys;
    }
    throw ["unhandled case in conc",[$p1,$p2]];
  });
};
Prelude.foldr1$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$listLen(Fay$$_($p2),1)) {
      var x = Fay$$index(0,Fay$$_($p2));
      return x;
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var f = $p1;
      return Fay$$_(Fay$$_(f)(x))(Prelude.foldr1$36$uncurried(f,xs));
    }
    if (Fay$$_($p2) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("foldr1: empty list"));
    }
    throw ["unhandled case in foldr1",[$p1,$p2]];
  });
};
Prelude.foldr$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    if (Fay$$_($p3) === null) {
      var z = $p2;
      return z;
    }
    var $tmp1 = Fay$$_($p3);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var z = $p2;
      var f = $p1;
      return Fay$$_(Fay$$_(f)(x))(Prelude.foldr$36$uncurried(f,z,xs));
    }
    throw ["unhandled case in foldr",[$p1,$p2,$p3]];
  });
};
Prelude.insertBy$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    if (Fay$$_($p3) === null) {
      var x = $p2;
      return Fay$$list([x]);
    }
    var ys = $p3;
    var x = $p2;
    var cmp = $p1;
    return (function($tmp1){
      if (Fay$$_($tmp1) === null) {
        return Fay$$list([x]);
      }
      var $tmp2 = Fay$$_($tmp1);
      if ($tmp2 instanceof Fay$$Cons) {
        var y = $tmp2.car;
        var ys$39$ = $tmp2.cdr;
        return (function($tmp2){
          if (Fay$$_($tmp2) instanceof Prelude._GT) {
            return Fay$$_(Fay$$_(Fay$$cons)(y))(Prelude.insertBy$36$uncurried(cmp,x,ys$39$));
          }
          return Fay$$_(Fay$$_(Fay$$cons)(x))(ys);
        })(Fay$$_(Fay$$_(cmp)(x))(y));
      }
      return (function(){ throw (["unhandled case",$tmp1]); })();
    })(ys);
  });
};
Prelude.sortBy$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var cmp = $p1;
    return Fay$$_(Prelude.foldr(Prelude.insertBy(cmp)))(null);
  });
};
Prelude.not$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Prelude.nub$39$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var ls = $p2;
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$_(Prelude.elem$36$uncurried(x,ls)) ? Prelude.nub$39$$36$uncurried(xs,ls) : Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.nub$39$$36$uncurried(xs,Fay$$_(Fay$$_(Fay$$cons)(x))(ls)));
    }
    throw ["unhandled case in nub'",[$p1,$p2]];
  });
};
Prelude.map$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    if (Fay$$_($p2) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var f = $p1;
      return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(f)(x)))(Prelude.map$36$uncurried(f,xs));
    }
    throw ["unhandled case in map",[$p1,$p2]];
  });
};
Prelude.filter$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var p = $p1;
      return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.filter$36$uncurried(p,xs)) : Prelude.filter$36$uncurried(p,xs);
    }
    if (Fay$$_($p2) === null) {
      return null;
    }
    throw ["unhandled case in filter",[$p1,$p2]];
  });
};
Prelude.find$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var p = $p1;
      return Fay$$_(Fay$$_(p)(x)) ? Prelude.Just$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : Prelude.find$36$uncurried(p,xs);
    }
    if (Fay$$_($p2) === null) {
      return Prelude.Nothing;
    }
    throw ["unhandled case in find",[$p1,$p2]];
  });
};
Prelude.gcd$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var b = $p2;
    var a = $p1;
    return (function(){
      var go = function($p1){
        return function($p2){
          return new Fay$$$(function(){
            if (Fay$$_($p2) === 0) {
              var x = $p1;
              return x;
            }
            var y = $p2;
            var x = $p1;
            return Fay$$_(Fay$$_(go)(y))(Prelude.rem$36$uncurried(x,y));
          });
        };
      };
      return Fay$$_(Fay$$_(go)(Prelude.abs$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(a)))(Prelude.abs$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b));
    })();
  });
};
Prelude.quot$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var y = $p2;
    var x = $p1;
    return Fay$$_(Fay$$eq$36$uncurried(y,0)) ? Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("Division by zero")) : Prelude.quot$39$$36$uncurried(x,y);
  });
};
Prelude.abs$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.negate$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : x;
  });
};
Prelude.rem$39$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Fay$$fayToJs_int($p1) % Fay$$fayToJs_int($p2));
  });
};
Prelude.rem$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var y = $p2;
    var x = $p1;
    return Fay$$_(Fay$$eq$36$uncurried(y,0)) ? Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("Division by zero")) : Prelude.rem$39$$36$uncurried(x,y);
  });
};
Prelude.quot$39$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(~~(Fay$$fayToJs_int($p1)/Fay$$fayToJs_int($p2)));
  });
};
Prelude.even$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$eq$36$uncurried(Prelude.rem$36$uncurried(x,2),0);
  });
};
Prelude.floor$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));
  });
};
Prelude.ceiling$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));
  });
};
Prelude.fromIntegral$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return $p1;
  });
};
Prelude.truncate$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.ceiling$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : Prelude.floor$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
  });
};
Prelude.sqrt$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));
  });
};
Prelude.log$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));
  });
};
Prelude.exp$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));
  });
};
Prelude.$94$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var b = $p2;
    var a = $p1;
    if (Fay$$_(Fay$$lt$36$uncurried(b,0))) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("(^): negative exponent"));
    } else {
      if (Fay$$_(Fay$$eq$36$uncurried(b,0))) {
        return 1;
      } else {
        if (Fay$$_(Prelude.even$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b))) {
          return (function(){
            return new Fay$$$(function(){
              var x = new Fay$$$(function(){
                return Prelude.$94$$36$uncurried(a,Prelude.quot$36$uncurried(b,2));
              });
              return Fay$$mult$36$uncurried(x,x);
            });
          })();
        }
      }
    }
    var b = $p2;
    var a = $p1;
    return Fay$$mult$36$uncurried(a,Prelude.$94$$36$uncurried(a,Fay$$sub$36$uncurried(b,1)));
  });
};
Prelude.negate$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (-(Fay$$_(x)));
  });
};
Prelude.quotRem$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var y = $p2;
    var x = $p1;
    return Fay$$list([Prelude.quot$36$uncurried(x,y),Prelude.rem$36$uncurried(x,y)]);
  });
};
Prelude.until$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    var x = $p3;
    var f = $p2;
    var p = $p1;
    return Fay$$_(Fay$$_(p)(x)) ? x : Prelude.until$36$uncurried(p,f,Fay$$_(f)(x));
  });
};
Prelude.enumFromByTo$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    var to = $p3;
    var by = $p2;
    var fr = $p1;
    return (function(){
      var neg = function($p1){
        return new Fay$$$(function(){
          var x = $p1;
          return Fay$$_(Fay$$lt$36$uncurried(x,to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(neg)(Fay$$add$36$uncurried(x,by)));
        });
      };
      var pos = function($p1){
        return new Fay$$$(function(){
          var x = $p1;
          return Fay$$_(Fay$$gt$36$uncurried(x,to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(pos)(Fay$$add$36$uncurried(x,by)));
        });
      };
      return Fay$$_(Fay$$lt$36$uncurried(by,0)) ? Fay$$_(neg)(fr) : Fay$$_(pos)(fr);
    })();
  });
};
Prelude.enumFromBy$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var by = $p2;
    var fr = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(fr))(Prelude.enumFromBy$36$uncurried(Fay$$add$36$uncurried(fr,by),by));
  });
};
Prelude.enumFromTo$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var n = $p2;
    var i = $p1;
    return Fay$$_(Fay$$gt$36$uncurried(i,n)) ? null : Fay$$_(Fay$$_(Fay$$cons)(i))(Prelude.enumFromTo$36$uncurried(Fay$$add$36$uncurried(i,1),n));
  });
};
Prelude.enumFrom$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var i = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(i))(Prelude.enumFrom$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$add$36$uncurried(i,1)));
  });
};
Prelude.sequence_$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$return$36$uncurried(Fay$$unit);
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var m = $tmp1.car;
      var ms = $tmp1.cdr;
      return Fay$$then$36$uncurried(m,Prelude.sequence_$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ms));
    }
    throw ["unhandled case in sequence_",[$p1]];
  });
};
Prelude.mapM_$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p2);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      var m = $p1;
      return Fay$$then$36$uncurried(Fay$$_(m)(x),Prelude.mapM_$36$uncurried(m,xs));
    }
    if (Fay$$_($p2) === null) {
      return Fay$$return$36$uncurried(Fay$$unit);
    }
    throw ["unhandled case in mapM_",[$p1,$p2]];
  });
};
Prelude.$36$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var x = $p2;
    var f = $p1;
    return Fay$$_(f)(x);
  });
};
Prelude.forM_$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var m = $p2;
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$then$36$uncurried(Fay$$_(m)(x),Prelude.forM_$36$uncurried(xs,m));
    }
    if (Fay$$_($p1) === null) {
      return Fay$$return$36$uncurried(Fay$$unit);
    }
    throw ["unhandled case in forM_",[$p1,$p2]];
  });
};
var FFI = {};
Prelude.sequence_$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$return$36$uncurried(Fay$$unit);
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var m = $tmp1.car;
      var ms = $tmp1.cdr;
      return Fay$$then$36$uncurried(m,Prelude.sequence_$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ms));
    }
    throw ["unhandled case in sequence_",[$p1]];
  });
};
Prelude.enumFrom$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var i = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(i))(Prelude.enumFrom$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$add$36$uncurried(i,1)));
  });
};
Prelude.even$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$eq$36$uncurried(Prelude.rem$36$uncurried(x,2),0);
  });
};
Prelude.error$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());
  });
};
Prelude.floor$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));
  });
};
Prelude.ceiling$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));
  });
};
Prelude.negate$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (-(Fay$$_(x)));
  });
};
Prelude.abs$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.negate$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : x;
  });
};
Prelude.Just$36$uncurried$36$uncurried = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Just(slot1);
  });
};
Prelude.init$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("init: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      var t = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$cons)(h))(Prelude.init$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(t));
    }
    throw ["unhandled case in init",[$p1]];
  });
};
Prelude.repeat$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.repeat$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x));
  });
};
Prelude.unzip$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null]);
    }
    throw ["unhandled case in unzip",[$p1]];
  });
};
Prelude.unzip3$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),3)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var z = Fay$$index(2,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),3)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            var zs = Fay$$index(2,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip3$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null,null]);
    }
    throw ["unhandled case in unzip3",[$p1]];
  });
};
Prelude.break$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Prelude.span(Fay$$_(Prelude.$46$(Prelude.not))(p));
  });
};
Prelude.lines$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var s = $p1;
    return (function(){
      var isLineBreak = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Fay$$or$36$uncurried(Fay$$eq$36$uncurried(c,"\r"),Fay$$eq$36$uncurried(c,"\n"));
        });
      };
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {
            return Fay$$list([a]);
          }
        }
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));
          if ($tmp2 instanceof Fay$$Cons) {
            var cs = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.lines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(cs));
          }
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isLineBreak))(s));
    })();
  });
};
Prelude.unlines$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var l = $tmp1.car;
      var ls = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(l,Fay$$_(Fay$$_(Fay$$cons)("\n"))(Prelude.unlines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ls)));
    }
    throw ["unhandled case in unlines",[$p1]];
  });
};
Prelude.words$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var str = $p1;
    return (function(){
      var words$39$ = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) === null) {
            return null;
          }
          var s = $p1;
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var a = Fay$$index(0,Fay$$_($tmp1));
              var b = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.words$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b));
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isSpace))(s));
        });
      };
      var isSpace = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Prelude.elem$36$uncurried(c,Fay$$list(" \t\r\n\u000c\u000b"));
        });
      };
      return Fay$$_(words$39$)(Prelude.dropWhile$36$uncurried(isSpace,str));
    })();
  });
};
Prelude.and$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$and$36$uncurried(x,Prelude.and$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in and",[$p1]];
  });
};
Prelude.or$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return false;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$or$36$uncurried(x,Prelude.or$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in or",[$p1]];
  });
};
Prelude.reverse$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(Prelude.reverse$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs),Fay$$list([x]));
    }
    if (Fay$$_($p1) === null) {
      return null;
    }
    throw ["unhandled case in reverse",[$p1]];
  });
};
Prelude.last$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    while (true) {
      if (Fay$$_($p1) === null) {
        return Prelude.error$36$uncurried(Fay$$list("last: empty list"));
      }
      if (Fay$$listLen(Fay$$_($p1),1)) {
        var a = Fay$$index(0,Fay$$_($p1));
        return a;
      }
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var t = $tmp1.cdr;
        $p1 = t;
        continue;
      }
      throw ["unhandled case in last",[$p1]];
    }
  });
};
Prelude.sortBy$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var cmp = $p1;
    return Fay$$_(Prelude.foldr(Prelude.insertBy(cmp)))(null);
  });
};
Prelude.not$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Prelude.fromIntegral$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return $p1;
  });
};
Prelude.truncate$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.ceiling$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : Prelude.floor$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
  });
};
Prelude.sqrt$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));
  });
};
Prelude.log$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));
  });
};
Prelude.exp$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));
  });
};
Data.Nullable = {};
Data.Nullable.fromNullable = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) instanceof Fay.FFI._Nullable) {
      var x = Fay$$_($p1).slot1;
      return Prelude.Just$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
    }
    if (Fay$$_($p1) instanceof Fay.FFI._Null) {
      return Prelude.Nothing;
    }
    throw ["unhandled case in fromNullable",[$p1]];
  });
};
Data.Nullable.toNullable = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) instanceof Prelude._Just) {
      var x = Fay$$_($p1).slot1;
      return Fay.FFI.Nullable$36$uncurried$36$uncurried$36$uncurried(x);
    }
    if (Fay$$_($p1) instanceof Prelude._Nothing) {
      return Fay.FFI.Null;
    }
    throw ["unhandled case in toNullable",[$p1]];
  });
};
Data.Text = {};
Data.Text.intercalate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["list",[["user","Text",[]]]],$p2).join(Fay$$fayToJs(["user","Text",[]],$p1)));
    });
  };
};
Data.Text.fromString = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs_string($p1));
  });
};
Data.Text.snoc = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1) + Fay$$fayToJs(["user","Char",[]],$p2));
    });
  };
};
Data.Text.cons = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Char",[]],$p1) + Fay$$fayToJs(["user","Text",[]],$p2));
    });
  };
};
Data.Text.pack = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs_string($p1));
  });
};
Data.Text.unpack = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_string(Fay$$fayToJs(["user","Text",[]],$p1));
  });
};
Data.Text.append = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1) + Fay$$fayToJs(["user","Text",[]],$p2));
    });
  };
};
Data.Text.$60$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1) + Fay$$fayToJs(["user","Text",[]],$p2));
    });
  };
};
Data.Text.length = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Fay$$fayToJs(["user","Text",[]],$p1).length);
  });
};
Data.Text.$_null = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Text",[]],$p1).length == 0);
  });
};
Data.Text.take = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p2).substring(0,Fay$$fayToJs_int($p1)));
    });
  };
};
Data.Text.drop = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p2).substring(Fay$$fayToJs_int($p1)));
    });
  };
};
Data.Text.empty = new Fay$$$(function(){
  return Fay$$jsToFay(["user","Text",[]],"");
});
Data.Text.lines = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["list",[["user","Text",[]]]],Fay$$fayToJs(["user","Text",[]],$p1).split('\n'));
  });
};
Data.Text.unlines = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["list",[["user","Text",[]]]],$p1).join('\n'));
  });
};
Data.Text.isPrefixOf = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Text",[]],$p2).lastIndexOf(Fay$$fayToJs(["user","Text",[]],$p1), 0) == 0);
    });
  };
};
Data.Text.intersperse = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p2).split('').join(Fay$$fayToJs(["user","Char",[]],$p1)));
    });
  };
};
Data.Text.reverse = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).split('').reverse().join(''));
  });
};
Data.Text.stripSuffix = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var text = $p2;
      var prefix = $p1;
      return (function(){
        var extract = function($p1){
          return function($p2){
            return new Fay$$$(function(){
              return Fay$$jsToFay(["nullable",[["user","Text",[]]]],(function(suffix,text){ return text.substring(text.length - suffix.length) == suffix? text.substring(0,text.length - suffix.length) : null; })(Fay$$fayToJs(["user","Text",[]],$p1),Fay$$fayToJs(["user","Text",[]],$p2)));
            });
          };
        };
        return Data.Nullable.fromNullable$36$uncurried$36$uncurried$36$uncurried(Fay$$_(Fay$$_(extract)(prefix))(text));
      })();
    });
  };
};
Data.Text.splitOn = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["list",[["user","Text",[]]]],Fay$$fayToJs(["user","Text",[]],$p2).split(Fay$$fayToJs(["user","Text",[]],$p1)));
    });
  };
};
Data.Text.putStrLn = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],console.log('%s',Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
Data.Text.toShortest = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs_double($p1).toString());
  });
};
Data.Text.showInt = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs_int($p1).toString());
  });
};
Data.Text.uncons = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Maybe",[["tuple",[["user","Char",[]],["user","Text",[]]]]]],Fay$$fayToJs(["user","Text",[]],$p1)[0] ? { instance: 'Just', slot1 : [Fay$$fayToJs(["user","Text",[]],$p1)[0],Fay$$fayToJs(["user","Text",[]],$p1).slice(1)] } : { instance : 'Nothing' });
  });
};
Data.Text.head = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Char",[]],Fay$$fayToJs(["user","Text",[]],$p1)[0] || (function () {throw new Error('Data.Text.head: empty Text'); }()));
  });
};
Data.Text.last = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Char",[]],Fay$$fayToJs(["user","Text",[]],$p1).length ? Fay$$fayToJs(["user","Text",[]],$p1)[Fay$$fayToJs(["user","Text",[]],$p1).length-1] : (function() { throw new Error('Data.Text.last: empty Text') })());
  });
};
Data.Text.tail = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).length ? Fay$$fayToJs(["user","Text",[]],$p1).slice(1) : (function () { throw new Error('Data.Text.tail: empty Text') })());
  });
};
Data.Text.init = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).length ? Fay$$fayToJs(["user","Text",[]],$p1).slice(0,-1) : (function () { throw new Error('Data.Text.init: empty Text') })());
  });
};
Data.Text.map = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],[].map.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["user","Char",[]]]],$p1)).join(''));
    });
  };
};
Data.Text.toLower = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).toLowerCase());
  });
};
Data.Text.toUpper = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).toUpperCase());
  });
};
Data.Text.concat = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["list",[["user","Text",[]]]],$p1).join(''));
  });
};
Data.Text.concatMap = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],[].map.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["user","Text",[]]]],$p1)).join(''));
    });
  };
};
Data.Text.any = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_bool([].filter.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["bool"]]],$p1)).length > 0);
    });
  };
};
Data.Text.all = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_bool([].filter.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["bool"]]],$p1)).length == Fay$$fayToJs(["function",[["user","Char",[]],["bool"]]],$p1).length);
    });
  };
};
Data.Text.maximum = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Char",[]],(function (s) {    if (s === '') { throw new Error('Data.Text.maximum: empty string'); }    var max = s[0];    for (var i = 1; i < s.length; s++) {      if (s[i] > max) { max = s[i]; }    }    return max;  })(Fay$$fayToJs(["user","Text",[]],$p1)));
  });
};
Data.Text.minimum = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Char",[]],(function (s) {    if (s === '') { throw new Error('Data.Text.maximum: empty string'); }    var min = s[0];    for (var i = 1; i < s.length; s++) {      if (s[i] < min) { min = s[i]; }    }    return min;  })(Fay$$fayToJs(["user","Text",[]],$p1)));
  });
};
Data.Nullable.fromNullable$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) instanceof Fay.FFI._Nullable) {
      var x = Fay$$_($p1).slot1;
      return Prelude.Just$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
    }
    if (Fay$$_($p1) instanceof Fay.FFI._Null) {
      return Prelude.Nothing;
    }
    throw ["unhandled case in fromNullable",[$p1]];
  });
};
Fay.FFI.Nullable$36$uncurried = function(slot1){
  return new Fay$$$(function(){
    return new Fay.FFI._Nullable(slot1);
  });
};
Prelude.Just$36$uncurried$36$uncurried$36$uncurried = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Just(slot1);
  });
};
Prelude.Just$36$uncurried$36$uncurried = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Just(slot1);
  });
};
Prelude.Just$36$uncurried = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Just(slot1);
  });
};
Prelude.floor$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));
  });
};
Prelude.ceiling$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));
  });
};
Prelude.reverse$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(Prelude.reverse$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs),Fay$$list([x]));
    }
    if (Fay$$_($p1) === null) {
      return null;
    }
    throw ["unhandled case in reverse",[$p1]];
  });
};
Prelude.or$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return false;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$or$36$uncurried(x,Prelude.or$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in or",[$p1]];
  });
};
Prelude.and$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$and$36$uncurried(x,Prelude.and$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in and",[$p1]];
  });
};
Prelude.break$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Prelude.span(Fay$$_(Prelude.$46$(Prelude.not))(p));
  });
};
Prelude.words$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var str = $p1;
    return (function(){
      var words$39$ = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) === null) {
            return null;
          }
          var s = $p1;
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var a = Fay$$index(0,Fay$$_($tmp1));
              var b = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.words$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b));
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isSpace))(s));
        });
      };
      var isSpace = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Prelude.elem$36$uncurried(c,Fay$$list(" \t\r\n\u000c\u000b"));
        });
      };
      return Fay$$_(words$39$)(Prelude.dropWhile$36$uncurried(isSpace,str));
    })();
  });
};
Prelude.unlines$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var l = $tmp1.car;
      var ls = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(l,Fay$$_(Fay$$_(Fay$$cons)("\n"))(Prelude.unlines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ls)));
    }
    throw ["unhandled case in unlines",[$p1]];
  });
};
Prelude.lines$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var s = $p1;
    return (function(){
      var isLineBreak = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Fay$$or$36$uncurried(Fay$$eq$36$uncurried(c,"\r"),Fay$$eq$36$uncurried(c,"\n"));
        });
      };
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {
            return Fay$$list([a]);
          }
        }
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));
          if ($tmp2 instanceof Fay$$Cons) {
            var cs = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.lines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(cs));
          }
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isLineBreak))(s));
    })();
  });
};
Prelude.unzip3$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),3)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var z = Fay$$index(2,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),3)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            var zs = Fay$$index(2,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip3$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null,null]);
    }
    throw ["unhandled case in unzip3",[$p1]];
  });
};
Prelude.unzip$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null]);
    }
    throw ["unhandled case in unzip",[$p1]];
  });
};
Prelude.repeat$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.repeat$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x));
  });
};
Prelude.init$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("init: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      var t = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$cons)(h))(Prelude.init$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(t));
    }
    throw ["unhandled case in init",[$p1]];
  });
};
Prelude.error$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());
  });
};
Prelude.negate$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (-(Fay$$_(x)));
  });
};
Prelude.enumFrom$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var i = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(i))(Prelude.enumFrom$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$add$36$uncurried(i,1)));
  });
};
Prelude.sequence_$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$return$36$uncurried(Fay$$unit);
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var m = $tmp1.car;
      var ms = $tmp1.cdr;
      return Fay$$then$36$uncurried(m,Prelude.sequence_$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ms));
    }
    throw ["unhandled case in sequence_",[$p1]];
  });
};
Prelude.even$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$eq$36$uncurried(Prelude.rem$36$uncurried(x,2),0);
  });
};
Prelude.abs$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.negate$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : x;
  });
};
Prelude.last$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    while (true) {
      if (Fay$$_($p1) === null) {
        return Prelude.error$36$uncurried(Fay$$list("last: empty list"));
      }
      if (Fay$$listLen(Fay$$_($p1),1)) {
        var a = Fay$$index(0,Fay$$_($p1));
        return a;
      }
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var t = $tmp1.cdr;
        $p1 = t;
        continue;
      }
      throw ["unhandled case in last",[$p1]];
    }
  });
};
Prelude.sortBy$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var cmp = $p1;
    return Fay$$_(Prelude.foldr(Prelude.insertBy(cmp)))(null);
  });
};
Prelude.not$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Prelude.fromIntegral$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return $p1;
  });
};
Prelude.truncate$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.ceiling$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : Prelude.floor$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
  });
};
Prelude.sqrt$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));
  });
};
Prelude.log$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));
  });
};
Prelude.exp$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));
  });
};
var DOM = {};
DOM.getWindow = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","Global",[]],window));
});
DOM.getDocument = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","Document",[]],window.document));
});
DOM.getBody = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document.body));
});
DOM.getElementById = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document['getElementById'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOM.getElementsByName = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["list",[["user","Element",[]]]],document['getElementsByName'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOM.addEvent = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['addEventListener'](Fay$$fayToJs(["user","Text",[]],$p1),Fay$$fayToJs(["action",[["unknown"]]],$p2))));
    });
  };
};
DOM.removeEvent = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['removeEventListener'](Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p3))));
      });
    };
  };
};
DOM.stopProp = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Event",[]],$p1)['stopPropagation']()));
  });
};
DOM.preventDefault = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Event",[]],$p1)['preventDefault']()));
  });
};
DOM.createElement = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],window['document']['createElement'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOM.appendChild = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['appendChild'](Fay$$fayToJs(["user","Element",[]],$p2))));
    });
  };
};
DOM.appendChildBefore = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['parentNode']['insertBefore'](Fay$$fayToJs(["user","Element",[]],$p2), Fay$$fayToJs(["user","Element",[]],$p1))));
    });
  };
};
DOM.removeChild = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['removeChild'](Fay$$fayToJs(["user","Element",[]],$p2))));
    });
  };
};
DOM.parentNode = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],Fay$$fayToJs(["user","Element",[]],$p1)['parentNode']));
  });
};
DOM.children = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","NodeList",[]],Fay$$fayToJs(["user","Element",[]],$p1)['children']));
  });
};
DOM.childNodes = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","NodeList",[]],Fay$$fayToJs(["user","Element",[]],$p1)['childNodes']));
  });
};
DOM.nodeListToArray = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["list",[["user","Element",[]]]],Array.prototype.slice.call(Fay$$fayToJs(["user","NodeList",[]],$p1)));
  });
};
DOM.nodesBetween = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["list",[["user","Element",[]]]],(function(start, end) {  var i, contents, result = [], parentNode = start.parentNode;  if(parentNode !== end.parentNode) return;  contents = Array.prototype.slice.call(parentNode.childNodes);  for(i=contents.indexOf(start); i<contents.indexOf(end); i++) {    result.push(contents[i]);  }})(Fay$$fayToJs(["user","Element",[]],$p1), Fay$$fayToJs(["user","Element",[]],$p2))));
    });
  };
};
DOM.removeNodesBetween = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(start, end) {  var i, contents, parentNode = start.parentNode;  if(parentNode !== end.parentNode) return;  contents = Array.prototype.slice.call(parentNode.childNodes);  for(i=contents.indexOf(start); i<contents.indexOf(end); i++) {    parentNode.removeChild(contents[i]);  }})(Fay$$fayToJs(["user","Element",[]],$p1), Fay$$fayToJs(["user","Element",[]],$p2))));
    });
  };
};
DOM.createTextNode = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document['createTextNode'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOM.getTextData = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Element",[]],$p1)['data']));
  });
};
DOM.setTextData = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['data'] = Fay$$fayToJs(["user","Text",[]],$p2)));
    });
  };
};
DOM.clearInnerHtml = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['innerHTML'] = ''));
  });
};
DOM.klass = new Fay$$$(function(){
  return DOM.addClass;
});
DOM.addClass = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1).classList['add'](Fay$$fayToJs(["user","Text",[]],$p2))));
    });
  };
};
DOM.removeClass = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['classList']['remove'](Fay$$fayToJs(["user","Text",[]],$p2))));
    });
  };
};
DOM.toggleClass = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['classList']['toggle'](Fay$$fayToJs(["user","Text",[]],$p2))));
    });
  };
};
DOM.hasClass = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay_bool(Fay$$fayToJs(["user","Element",[]],$p1)['classList']['contains'](Fay$$fayToJs(["user","Text",[]],$p2))));
    });
  };
};
DOM.setAttr = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['setAttribute'](Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["user","Text",[]],$p3))));
      });
    };
  };
};
DOM.getAttr = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Element",[]],$p1)['getAttribute'](Fay$$fayToJs(["user","Text",[]],$p2))));
    });
  };
};
DOM.hasAttr = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay_bool(Fay$$fayToJs(["user","Element",[]],$p1)['hasAttribute'](Fay$$fayToJs(["user","Text",[]],$p2))));
    });
  };
};
DOM.getValue = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Element",[]],$p1)['value']));
  });
};
DOM.setValue = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['value'] = Fay$$fayToJs(["user","Text",[]],$p2)));
    });
  };
};
DOM.isChecked = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay_bool(Fay$$fayToJs(["user","Element",[]],$p1)['checked']));
  });
};
DOM.setChecked = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['checked'] = Fay$$fayToJs_bool($p2)));
    });
  };
};
DOM.getRadioValue = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],(function(name) {  var i, rs = document.getElementsByName(name);  if(rs) {    for(var i=0; i<rs.length; i++) {      var radio = rs[i];      if(radio.type === 'radio' && radio.checked)        return radio.value;    }  }})(Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOM.setRadioValue = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(name, val) {  var i, rs = document.getElementsByName(name);  if(rs) {    for(var i=0; i<rs.length; i++) {      var radio = rs[i];      if(radio.type === 'radio' && radio.value === val)        radio.checked = true;    }  }})(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Text",[]],$p2))));
    });
  };
};
DOM.getCurrentUrl = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],window['location']['href']));
});
DOM.logS = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],console['log'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOM.logF = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],console['log'](Fay$$fayToJs(["unknown"],$p1))));
  });
};
DOM.setInterval = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Timer",[]],(function (f,i) { var id = window['setInterval'](function () { f(id); }, i); return id; })(Fay$$fayToJs(["function",[["user","Timer",[]],["action",[["unknown"]]]]],$p2),Fay$$fayToJs_double($p1))));
    });
  };
};
DOM.clearInterval = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['clearInterval'](Fay$$fayToJs(["user","Timer",[]],$p1))));
  });
};
DOM.setTimeout = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Timer",[]],(function (f,i) { var id = window['setTimeout'](function () { f(id); }, i); return id; })(Fay$$fayToJs(["function",[["user","Timer",[]],["action",[["unknown"]]]]],$p2),Fay$$fayToJs_double($p1))));
    });
  };
};
DOM.clearTimeout = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['clearTimeout'](Fay$$fayToJs(["user","Timer",[]],$p1))));
  });
};
DOM._GET = function GET(){
};
DOM._GET.prototype.instance = "GET";
DOM.GET = new Fay$$$(function(){
  return new DOM._GET();
});
DOM._POST = function POST(){
};
DOM._POST.prototype.instance = "POST";
DOM.POST = new Fay$$$(function(){
  return new DOM._POST();
});
DOM._PUT = function PUT(){
};
DOM._PUT.prototype.instance = "PUT";
DOM.PUT = new Fay$$$(function(){
  return new DOM._PUT();
});
DOM._HEAD = function HEAD(){
};
DOM._HEAD.prototype.instance = "HEAD";
DOM.HEAD = new Fay$$$(function(){
  return new DOM._HEAD();
});
DOM._UNSENT = function UNSENT(){
};
DOM._UNSENT.prototype.instance = "UNSENT";
DOM.UNSENT = new Fay$$$(function(){
  return new DOM._UNSENT();
});
DOM._OPENED = function OPENED(){
};
DOM._OPENED.prototype.instance = "OPENED";
DOM.OPENED = new Fay$$$(function(){
  return new DOM._OPENED();
});
DOM._HEADERS_RECEIVED = function HEADERS_RECEIVED(){
};
DOM._HEADERS_RECEIVED.prototype.instance = "HEADERS_RECEIVED";
DOM.HEADERS_RECEIVED = new Fay$$$(function(){
  return new DOM._HEADERS_RECEIVED();
});
DOM._LOADING = function LOADING(){
};
DOM._LOADING.prototype.instance = "LOADING";
DOM.LOADING = new Fay$$$(function(){
  return new DOM._LOADING();
});
DOM._DONE = function DONE(){
};
DOM._DONE.prototype.instance = "DONE";
DOM.DONE = new Fay$$$(function(){
  return new DOM._DONE();
});
DOM.xmlHttpRequest = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","XMLHttpRequest",[]],(function(window) { if(window['XMLHttpRequest']) return new XMLHttpRequest(); else return new ActiveXObject('Microsoft.XMLHTTP'); })(window)));
});
DOM.open = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","XMLHttpRequest",[]],(function(method, url, xhr) { xhr['open'](method['instance'], url, true); return xhr; })(Fay$$fayToJs(["user","RequestMethod",[]],$p1), Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["user","XMLHttpRequest",[]],$p3))));
      });
    };
  };
};
DOM.send = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","XMLHttpRequest",[]],$p1)['send']()));
  });
};
DOM.setReadyStateHandler = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","XMLHttpRequest",[]],(function(handler, xhr) { xhr['onreadystatechange'] = function() { handler(xhr); }; return xhr; })(Fay$$fayToJs(["function",[["user","XMLHttpRequest",[]],["action",[["unknown"]]]]],$p1), Fay$$fayToJs(["user","XMLHttpRequest",[]],$p2))));
    });
  };
};
DOM.readyState = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","ReadyState",[]],{ instance: ['UNSENT', 'OPENED', 'HEADERS_RECEIVED', 'LOADING', 'DONE'][Fay$$fayToJs(["user","XMLHttpRequest",[]],$p1)['readyState']] }));
  });
};
DOM.responseText = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","XMLHttpRequest",[]],$p1)['responseText']));
  });
};
DOM.status = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay_int(Fay$$fayToJs(["user","XMLHttpRequest",[]],$p1)['status']));
  });
};
DOM.parseInt = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay_int(parseInt(Fay$$fayToJs(["user","Text",[]],$p1), 10)));
  });
};
DOM.scrollIntoView = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1).scrollIntoView()));
  });
};
DOM.scrollRelative = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],window.scrollBy(0,Fay$$fayToJs_int($p1))));
  });
};
DOM.scrollAbsolute = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],window.scrollTo(0,Fay$$fayToJs_int($p1))));
  });
};
Fay$$objConcat(Fay$$fayToJsHash,{"GET": function(type,argTypes,_obj){
  var obj_ = {"instance": "GET"};
  return obj_;
},"POST": function(type,argTypes,_obj){
  var obj_ = {"instance": "POST"};
  return obj_;
},"PUT": function(type,argTypes,_obj){
  var obj_ = {"instance": "PUT"};
  return obj_;
},"HEAD": function(type,argTypes,_obj){
  var obj_ = {"instance": "HEAD"};
  return obj_;
},"UNSENT": function(type,argTypes,_obj){
  var obj_ = {"instance": "UNSENT"};
  return obj_;
},"OPENED": function(type,argTypes,_obj){
  var obj_ = {"instance": "OPENED"};
  return obj_;
},"HEADERS_RECEIVED": function(type,argTypes,_obj){
  var obj_ = {"instance": "HEADERS_RECEIVED"};
  return obj_;
},"LOADING": function(type,argTypes,_obj){
  var obj_ = {"instance": "LOADING"};
  return obj_;
},"DONE": function(type,argTypes,_obj){
  var obj_ = {"instance": "DONE"};
  return obj_;
}});
Fay$$objConcat(Fay$$jsToFayHash,{"GET": function(type,argTypes,obj){
  return new DOM._GET();
},"POST": function(type,argTypes,obj){
  return new DOM._POST();
},"PUT": function(type,argTypes,obj){
  return new DOM._PUT();
},"HEAD": function(type,argTypes,obj){
  return new DOM._HEAD();
},"UNSENT": function(type,argTypes,obj){
  return new DOM._UNSENT();
},"OPENED": function(type,argTypes,obj){
  return new DOM._OPENED();
},"HEADERS_RECEIVED": function(type,argTypes,obj){
  return new DOM._HEADERS_RECEIVED();
},"LOADING": function(type,argTypes,obj){
  return new DOM._LOADING();
},"DONE": function(type,argTypes,obj){
  return new DOM._DONE();
}});
var ACEditor = {};
ACEditor.newEditor = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Editor",[]],ace['edit'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
ACEditor.setTheme = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Editor",[]],(function(theme, editor){editor['setTheme']('ace/theme/' + theme); return editor;})(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Editor",[]],$p2))));
    });
  };
};
ACEditor.setMode = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Editor",[]],(function(mode, editor){editor['getSession']()['setMode']('ace/mode/' + mode); return editor;})(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Editor",[]],$p2))));
    });
  };
};
ACEditor.setValue = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Editor",[]],(function(value, editor) { editor['setValue'](value, -1); return editor })(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Editor",[]],$p2))));
    });
  };
};
ACEditor.getValue = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],(function(editor) { return editor['getValue']() })(Fay$$fayToJs(["user","Editor",[]],$p1))));
  });
};
ACEditor.addEvent = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Editor",[]],(function (evt, func, editor) { editor['on'](evt, func); return editor; })(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p2), Fay$$fayToJs(["user","Editor",[]],$p3))));
      });
    };
  };
};
ACEditor.removeAllEvent = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Editor",[]],(function(evt, editor) { editor['removeAllListeners'](evt); return editor; })(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Editor",[]],$p2))));
    });
  };
};
Prelude.floor$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));
  });
};
Prelude.ceiling$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));
  });
};
Prelude.negate$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (-(Fay$$_(x)));
  });
};
Prelude.sequence_$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$return$36$uncurried(Fay$$unit);
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var m = $tmp1.car;
      var ms = $tmp1.cdr;
      return Fay$$then$36$uncurried(m,Prelude.sequence_$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ms));
    }
    throw ["unhandled case in sequence_",[$p1]];
  });
};
Prelude.enumFrom$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var i = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(i))(Prelude.enumFrom$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$add$36$uncurried(i,1)));
  });
};
Prelude.init$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("init: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      var t = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$cons)(h))(Prelude.init$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(t));
    }
    throw ["unhandled case in init",[$p1]];
  });
};
Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());
  });
};
Prelude.repeat$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.repeat$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x));
  });
};
Prelude.unzip$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null]);
    }
    throw ["unhandled case in unzip",[$p1]];
  });
};
Prelude.unzip3$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),3)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var z = Fay$$index(2,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),3)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            var zs = Fay$$index(2,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip3$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null,null]);
    }
    throw ["unhandled case in unzip3",[$p1]];
  });
};
Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Prelude.span(Fay$$_(Prelude.$46$(Prelude.not))(p));
  });
};
Prelude.lines$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var s = $p1;
    return (function(){
      var isLineBreak = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Fay$$or$36$uncurried(Fay$$eq$36$uncurried(c,"\r"),Fay$$eq$36$uncurried(c,"\n"));
        });
      };
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {
            return Fay$$list([a]);
          }
        }
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));
          if ($tmp2 instanceof Fay$$Cons) {
            var cs = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.lines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(cs));
          }
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isLineBreak))(s));
    })();
  });
};
Prelude.unlines$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var l = $tmp1.car;
      var ls = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(l,Fay$$_(Fay$$_(Fay$$cons)("\n"))(Prelude.unlines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ls)));
    }
    throw ["unhandled case in unlines",[$p1]];
  });
};
Prelude.words$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var str = $p1;
    return (function(){
      var words$39$ = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) === null) {
            return null;
          }
          var s = $p1;
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var a = Fay$$index(0,Fay$$_($tmp1));
              var b = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.words$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b));
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isSpace))(s));
        });
      };
      var isSpace = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Prelude.elem$36$uncurried(c,Fay$$list(" \t\r\n\u000c\u000b"));
        });
      };
      return Fay$$_(words$39$)(Prelude.dropWhile$36$uncurried(isSpace,str));
    })();
  });
};
Prelude.and$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$and$36$uncurried(x,Prelude.and$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in and",[$p1]];
  });
};
Prelude.or$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return false;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$or$36$uncurried(x,Prelude.or$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in or",[$p1]];
  });
};
Prelude.reverse$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(Prelude.reverse$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs),Fay$$list([x]));
    }
    if (Fay$$_($p1) === null) {
      return null;
    }
    throw ["unhandled case in reverse",[$p1]];
  });
};
Prelude.Just$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Just(slot1);
  });
};
Data.Nullable.fromNullable$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) instanceof Fay.FFI._Nullable) {
      var x = Fay$$_($p1).slot1;
      return Prelude.Just$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
    }
    if (Fay$$_($p1) instanceof Fay.FFI._Null) {
      return Prelude.Nothing;
    }
    throw ["unhandled case in fromNullable",[$p1]];
  });
};
Fay.FFI.Nullable$36$uncurried$36$uncurried = function(slot1){
  return new Fay$$$(function(){
    return new Fay.FFI._Nullable(slot1);
  });
};
Prelude.even$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$eq$36$uncurried(Prelude.rem$36$uncurried(x,2),0);
  });
};
Prelude.abs$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.negate$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : x;
  });
};
Prelude.last$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    while (true) {
      if (Fay$$_($p1) === null) {
        return Prelude.error$36$uncurried(Fay$$list("last: empty list"));
      }
      if (Fay$$listLen(Fay$$_($p1),1)) {
        var a = Fay$$index(0,Fay$$_($p1));
        return a;
      }
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var t = $tmp1.cdr;
        $p1 = t;
        continue;
      }
      throw ["unhandled case in last",[$p1]];
    }
  });
};
Prelude.sortBy$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var cmp = $p1;
    return Fay$$_(Prelude.foldr(Prelude.insertBy(cmp)))(null);
  });
};
Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Prelude.fromIntegral$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return $p1;
  });
};
Prelude.truncate$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.ceiling$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : Prelude.floor$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
  });
};
Prelude.sqrt$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));
  });
};
Prelude.log$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));
  });
};
Prelude.exp$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));
  });
};
var DOMUtils = {};
DOMUtils.setProp = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],(function(prop, val, elem) { elem[prop] = val; return elem })(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["user","Element",[]],$p3))));
      });
    };
  };
};
DOMUtils.removeProp = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],(function(prop, elem) { elem[prop] = null; return elem })(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Element",[]],$p2))));
    });
  };
};
DOMUtils.getProp = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Element",[]],$p2)[Fay$$fayToJs(["user","Text",[]],$p1)]));
    });
  };
};
DOMUtils.querySelector = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document['querySelector'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOMUtils.querySelectorAll = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["list",[["user","Element",[]]]],document['querySelectorAll'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOMUtils.setHtml = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],(function(text, elem) { elem['innerHTML'] = text; return elem; })(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Element",[]],$p2))));
    });
  };
};
DOMUtils.setDisplay = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],(function (val, elem) { elem['style']['display'] = val; return elem })(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Element",[]],$p2))));
    });
  };
};
DOMUtils.addEventListener = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],(function(evt, func, elem) {elem['addEventListener'](evt, func); return elem;})(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p2), Fay$$fayToJs(["user","Element",[]],$p3))));
      });
    };
  };
};
DOMUtils.windowAddEventListener = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(evt, func) {window.addEventListener(evt, func);})(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p2))));
    });
  };
};
DOMUtils.clearEventListeners = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],(function(elem) {  var elem_ = elem['cloneNode'](true);  elem['parentNode']['replaceChild'](elem_, elem);})(Fay$$fayToJs(["user","Element",[]],$p1))));
  });
};
DOMUtils.getEventTargetAttr = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],(function(txt, evt) {  var elem = evt['target'];  var attr = elem['getAttribute'](txt);  if (attr) {    return attr;  } else {    return '';  }})(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Event",[]],$p2))));
    });
  };
};
DOMUtils.getModal = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Modal",[]],UIkit['modal'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOMUtils.showModal = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Modal",[]],$p1)['show']()));
  });
};
DOMUtils.hideModal = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Modal",[]],$p1)['hide']()));
  });
};
DOMUtils.modalEvent = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function (elem, show, hide) {  $(elem).on({    'show.uk.modal': show,    'hide.uk.modal': hide  })})(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["action",[["unknown"]]],$p2), Fay$$fayToJs(["action",[["unknown"]]],$p3))));
      });
    };
  };
};
DOMUtils.prompt = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var doPrompt = $p3;
        var val = $p2;
        var msg = $p1;
        return (function(){
          var hide = new Fay$$$(function(){
            return Fay$$bind$36$uncurried(DOMUtils.getModal$36$uncurried$36$uncurried("#modal-prompt"),DOMUtils.hideModal);
          });
          var doPrompt$39$ = new Fay$$$(function(){
            return Prelude.$61$$60$$60$$36$uncurried(doPrompt,Prelude.$61$$60$$60$$36$uncurried(DOMUtils.getProp("value"),DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-prompt input")));
          });
          return Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-prompt"),DOMUtils.clearEventListeners)),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-prompt .prompt"),DOMUtils.setHtml(msg))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-prompt input"),Fay$$_(DOMUtils.setProp("value"))(val))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-prompt .js-modal-ok"),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$_const(doPrompt$39$))),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$_const(hide)))),Fay$$bind$36$uncurried(DOMUtils.getModal$36$uncurried$36$uncurried("#modal-prompt"),DOMUtils.showModal)))));
        })();
      });
    };
  };
};
DOMUtils.confirm_ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var doCancel = $p3;
        var doConfirm = $p2;
        var msg = $p1;
        return (function(){
          var hide = new Fay$$$(function(){
            return Fay$$bind$36$uncurried(DOMUtils.getModal$36$uncurried$36$uncurried("#modal-confirm"),DOMUtils.hideModal);
          });
          return Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-confirm"),DOMUtils.clearEventListeners)),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-confirm .uk-modal-content"),DOMUtils.setHtml(msg))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-confirm .modal-confirm-cancel"),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$_const(doCancel))),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$_const(hide)))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-confirm .modal-confirm"),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$_const(doConfirm))),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$_const(hide)))),Fay$$bind$36$uncurried(DOMUtils.getModal$36$uncurried$36$uncurried("#modal-confirm"),DOMUtils.showModal)))));
        })();
      });
    };
  };
};
DOMUtils.confirm = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var doConfirm = $p2;
      var msg = $p1;
      return DOMUtils.confirm_$36$uncurried(msg,doConfirm,Fay$$return$36$uncurried(Fay$$unit));
    });
  };
};
DOMUtils.notify = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],UIkit['notify'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOMUtils.saveAs = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],saveAs(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["user","Text",[]],$p1)['substr'](1))));
    });
  };
};
DOMUtils.confirm_$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    var doCancel = $p3;
    var doConfirm = $p2;
    var msg = $p1;
    return (function(){
      var hide = new Fay$$$(function(){
        return Fay$$bind$36$uncurried(DOMUtils.getModal$36$uncurried$36$uncurried("#modal-confirm"),DOMUtils.hideModal);
      });
      return Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-confirm"),DOMUtils.clearEventListeners)),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-confirm .uk-modal-content"),DOMUtils.setHtml(msg))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-confirm .modal-confirm-cancel"),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$_const(doCancel))),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$_const(hide)))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(DOMUtils.querySelector$36$uncurried$36$uncurried("#modal-confirm .modal-confirm"),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$_const(doConfirm))),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$_const(hide)))),Fay$$bind$36$uncurried(DOMUtils.getModal$36$uncurried$36$uncurried("#modal-confirm"),DOMUtils.showModal)))));
    })();
  });
};
DOMUtils.getModal$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Modal",[]],UIkit['modal'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOMUtils.querySelector$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document['querySelector'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
var FilePath = {};
FilePath.append = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","FilePath",[]],(function(f, g) { p = f + '/' + g; return p['replace'](/\/+/g, '/') })(Fay$$fayToJs(["user","FilePath",[]],$p1), Fay$$fayToJs(["user","FilePath",[]],$p2)));
    });
  };
};
FilePath.$60$$47$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var g = $p2;
      var f = $p1;
      return FilePath.append$36$uncurried(f,g);
    });
  };
};
FilePath.dropFileName = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","FilePath",[]],(function(fn) { return fn['substr'](0, fn['lastIndexOf']('/')) })(Fay$$fayToJs(["user","FilePath",[]],$p1)));
  });
};
FilePath.append$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","FilePath",[]],(function(f, g) { p = f + '/' + g; return p['replace'](/\/+/g, '/') })(Fay$$fayToJs(["user","FilePath",[]],$p1), Fay$$fayToJs(["user","FilePath",[]],$p2)));
  });
};
var FPromise = {};
FPromise.fromReject = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return f;
  });
};
FPromise.fromResolve = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return f;
  });
};
FPromise.toReject = new Fay$$$(function(){
  return function($tmp1){
    return $tmp1;
  };
});
FPromise.toResolve = new Fay$$$(function(){
  return function($tmp1){
    return $tmp1;
  };
});
FPromise.newPromise = function($p1){
  return new Fay$$$(function(){
    var cb = $p1;
    return (function(){
      var f = function($p1){
        return function($p2){
          return function($p3){
            return new Fay$$$(function(){
              var g1 = $p3;
              var f1 = $p2;
              var f0 = $p1;
              return Fay$$_(Fay$$_(f0)(Fay$$_(FPromise.toResolve)(f1)))(Fay$$_(FPromise.toReject)(g1));
            });
          };
        };
      };
      return FPromise.newPromise_$36$uncurried$36$uncurried(Fay$$_(f)(cb));
    })();
  });
};
FPromise.newPromise_ = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],new Promise(Fay$$fayToJs(["function",[["function",[["unknown"],["action",[["unknown"]]]]],["function",[["user","Text",[]],["action",[["unknown"]]]]],["action",[["unknown"]]]]],$p1))));
  });
};
FPromise.then_ = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return Fay$$_(FPromise.then___(FPromise.fromResolve$36$uncurried$36$uncurried(f)))(Prelude.$36$$36$uncurried(Prelude.$_const,Fay$$return$36$uncurried(Fay$$unit)));
  });
};
FPromise.then__ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var g = $p2;
      var f = $p1;
      return Fay$$_(FPromise.then___(FPromise.fromResolve$36$uncurried$36$uncurried(f)))(FPromise.fromReject$36$uncurried$36$uncurried(g));
    });
  };
};
FPromise.then___ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],(function(resolve, reject, promise) {   return promise['then'](resolve, reject);})(Fay$$fayToJs(["function",[["unknown"],["action",[["unknown"]]]]],$p1), Fay$$fayToJs(["function",[["user","Text",[]],["action",[["unknown"]]]]],$p2), Fay$$fayToJs(["user","Promise",[]],$p3))));
      });
    };
  };
};
FPromise.$_catch = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return FPromise.catch_(FPromise.fromReject$36$uncurried$36$uncurried(f));
  });
};
FPromise.catch_ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],(function(reject, promise) {   return promise['catch'](reject);})(Fay$$fayToJs(["function",[["user","Text",[]],["action",[["unknown"]]]]],$p1), Fay$$fayToJs(["user","Promise",[]],$p2))));
    });
  };
};
FPromise.resolve = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Promise['resolve'](Fay$$fayToJs(["unknown"],$p1))));
  });
};
FPromise.reject = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Promise['reject'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
FPromise.fromReject$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return f;
  });
};
FPromise.fromResolve$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return f;
  });
};
FPromise.newPromise_$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],new Promise(Fay$$fayToJs(["function",[["function",[["unknown"],["action",[["unknown"]]]]],["function",[["user","Text",[]],["action",[["unknown"]]]]],["action",[["unknown"]]]]],$p1))));
  });
};
var ProcAPI = {};
ProcAPI.newProcAPI = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","ProcAPI",[]],new ProcJSApi({key: Fay$$fayToJs(["user","Text",[]],$p1), secret: Fay$$fayToJs(["user","Text",[]],$p2)})));
    });
  };
};
ProcAPI.readFile = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['readFile'](Fay$$fayToJs(["user","FilePath",[]],$p2))));
    });
  };
};
ProcAPI.writeFile = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['writeFile'](Fay$$fayToJs(["user","FilePath",[]],$p2), Fay$$fayToJs(["user","Text",[]],$p3))));
      });
    };
  };
};
ProcAPI.removeFile = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['removeFile'](Fay$$fayToJs(["user","FilePath",[]],$p2))));
    });
  };
};
ProcAPI.uploadFile = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['uploadFile'](Fay$$fayToJs(["user","FilePath",[]],$p2), Fay$$fayToJs(["user","Text",[]],$p3))));
      });
    };
  };
};
ProcAPI.uploadArchive = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['uploadArchive'](Fay$$fayToJs(["user","FilePath",[]],$p2), Fay$$fayToJs(["user","Text",[]],$p3))));
      });
    };
  };
};
ProcAPI.runFile_ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['runFile'](Fay$$fayToJs(["user","FilePath",[]],$p2), Fay$$fayToJs(["user","Text",[]],$p3))));
      });
    };
  };
};
ProcAPI.runFile = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var fn = $p2;
      var api = $p1;
      return Fay$$_(Prelude.$46$(Fay$$_(ProcAPI.runFile_(api))(fn)))(Fay$$_(Prelude.$46$(Data.Text.pack))(Prelude.show));
    });
  };
};
ProcAPI.loadFileTree = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['loadFileTree']()));
  });
};
ProcAPI.signWSPath = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['signWSPath'](Fay$$fayToJs(["user","FilePath",[]],$p2))));
    });
  };
};
ProcAPI.signFilePath = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['signFilePath'](Fay$$fayToJs(["user","FilePath",[]],$p2))));
    });
  };
};
var TermManager = {};
TermManager.newTermManager = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","TermManager",[]],new JSTermManager(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","ProcAPI",[]],$p2), Fay$$fayToJs(["action",[["unknown"]]],$p3))));
      });
    };
  };
};
TermManager.openTerm = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","TermManager",[]],$p1)['init']()));
  });
};
TermManager.closeTerm = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","TermManager",[]],$p1)['close']()));
  });
};
var Regex = {};
Regex._G = function G(){
};
Regex._G.prototype.instance = "G";
Regex.G = new Fay$$$(function(){
  return new Regex._G();
});
Regex._I = function I(){
};
Regex._I.prototype.instance = "I";
Regex.I = new Fay$$$(function(){
  return new Regex._I();
});
Regex._M = function M(){
};
Regex._M.prototype.instance = "M";
Regex.M = new Fay$$$(function(){
  return new Regex._M();
});
Regex._U = function U(){
};
Regex._U.prototype.instance = "U";
Regex.U = new Fay$$$(function(){
  return new Regex._U();
});
Regex._Y = function Y(){
};
Regex._Y.prototype.instance = "Y";
Regex.Y = new Fay$$$(function(){
  return new Regex._Y();
});
Regex.newRegex = new Fay$$$(function(){
  return Fay$$_(Prelude.flip(Regex.newRegex_))(null);
});
Regex.newRegex_ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Regex",[]],(function(txt, flags){  flags = flags['map'](function(flag) {    return flag['instance']['toLowerCase']();  });  return new RegExp(txt, flags['join'](''));})(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["list",[["user","Flag",[]]]],$p2)));
    });
  };
};
Regex.compile = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var txt = $p2;
      var reg = $p1;
      return Regex.compile_$36$uncurried(reg,txt,null);
    });
  };
};
Regex.compile_ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return Fay$$jsToFay(["user","Regex",[]],(function(reg, txt, flags){  flags = flags['map'](function(flag) {    return flag['instance']['toLowerCase']();  });  reg['compile'](txt, flags['join'](''));  return reg;})(Fay$$fayToJs(["user","Regex",[]],$p1), Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["list",[["user","Flag",[]]]],$p3)));
      });
    };
  };
};
Regex.exec = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["list",[["user","Text",[]]]],(function(reg, txt) { return reg['exec'](txt) || [];})(Fay$$fayToJs(["user","Regex",[]],$p1), Fay$$fayToJs(["user","Text",[]],$p2)));
    });
  };
};
Regex.test = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Regex",[]],$p1)['test'](Fay$$fayToJs(["user","Text",[]],$p2)));
    });
  };
};
Regex.flags = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["list",[["user","Flag",[]]]],(function(reg){  return reg['flags']['split']('')['map'](function(flag){    return {instance: flag['toUpperCase']()};  });})(Fay$$fayToJs(["user","Regex",[]],$p1)));
  });
};
Regex.$_global = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Regex",[]],$p1)['global']);
  });
};
Regex.ignoreCase = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Regex",[]],$p1)['ignoreCase']);
  });
};
Regex.multiline = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Regex",[]],$p1)['multiline']);
  });
};
Regex.unicode = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Regex",[]],$p1)['unicode']);
  });
};
Regex.sticky = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Regex",[]],$p1)['sticky']);
  });
};
Fay$$objConcat(Fay$$fayToJsHash,{"G": function(type,argTypes,_obj){
  var obj_ = {"instance": "G"};
  return obj_;
},"I": function(type,argTypes,_obj){
  var obj_ = {"instance": "I"};
  return obj_;
},"M": function(type,argTypes,_obj){
  var obj_ = {"instance": "M"};
  return obj_;
},"U": function(type,argTypes,_obj){
  var obj_ = {"instance": "U"};
  return obj_;
},"Y": function(type,argTypes,_obj){
  var obj_ = {"instance": "Y"};
  return obj_;
}});
Fay$$objConcat(Fay$$jsToFayHash,{"G": function(type,argTypes,obj){
  return new Regex._G();
},"I": function(type,argTypes,obj){
  return new Regex._I();
},"M": function(type,argTypes,obj){
  return new Regex._M();
},"U": function(type,argTypes,obj){
  return new Regex._U();
},"Y": function(type,argTypes,obj){
  return new Regex._Y();
}});
Regex.compile_$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Regex",[]],(function(reg, txt, flags){  flags = flags['map'](function(flag) {    return flag['instance']['toLowerCase']();  });  reg['compile'](txt, flags['join'](''));  return reg;})(Fay$$fayToJs(["user","Regex",[]],$p1), Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["list",[["user","Flag",[]]]],$p3)));
  });
};
var Utils = {};
Utils.isTextFile = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(isTextFile(Fay$$fayToJs(["user","FilePath",[]],$p1)));
  });
};
Utils.canProc = new Fay$$$(function(){
  return Regex.test(Regex.newRegex_$36$uncurried("\\.(js|sh|py)$",Fay$$list([Regex.I])));
});
Utils.modeMap = new Fay$$$(function(){
  return Fay$$list([Fay$$list(["javascript",Regex.newRegex_$36$uncurried("\\.js$",Fay$$list([Regex.I]))]),Fay$$list(["markdown",Regex.newRegex_$36$uncurried("\\.(md|markdown|rst)$",Fay$$list([Regex.I]))]),Fay$$list(["html",Regex.newRegex_$36$uncurried("\\.(html|htm)$",Fay$$list([Regex.I]))]),Fay$$list(["css",Regex.newRegex_$36$uncurried("\\.css$",Fay$$list([Regex.I]))]),Fay$$list(["yaml",Regex.newRegex_$36$uncurried("\\.(yaml|yml)$",Fay$$list([Regex.I]))]),Fay$$list(["xml",Regex.newRegex_$36$uncurried("\\.(svg|xml)$",Fay$$list([Regex.I]))]),Fay$$list(["json",Regex.newRegex_$36$uncurried("\\.json$",Fay$$list([Regex.I]))]),Fay$$list(["python",Regex.newRegex_$36$uncurried("\\.py$",Fay$$list([Regex.I]))]),Fay$$list(["tex",Regex.newRegex_$36$uncurried("\\.(tex|aux)$",Fay$$list([Regex.I]))]),Fay$$list(["sh",Regex.newRegex_$36$uncurried("\\.sh$",Fay$$list([Regex.I]))])]);
});
Utils.getMode = new Fay$$$(function(){
  return new Fay$$$(function(){
    var go = function($p1){
      return function($p2){
        return new Fay$$$(function(){
          if (Fay$$_($p1) === null) {
            return "text";
          }
          var f = $p2;
          var $tmp1 = Fay$$_($p1);
          if ($tmp1 instanceof Fay$$Cons) {
            if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
              var mode = Fay$$index(0,Fay$$_($tmp1.car));
              var reg = Fay$$index(1,Fay$$_($tmp1.car));
              var xs = $tmp1.cdr;
              if (Fay$$_(Regex.test$36$uncurried(reg,f))) {
                return mode;
              } else {
                if (Fay$$_(Prelude.otherwise)) {
                  return Fay$$_(Fay$$_(go)(xs))(f);
                }
              }
            }
          }
          throw ["unhandled case in go",[$p1,$p2]];
        });
      };
    };
    return Fay$$_(go)(Utils.modeMap);
  });
});
Utils.isImage = new Fay$$$(function(){
  return Regex.test(Regex.newRegex_$36$uncurried("\\.(jpg|png|gif|jpeg|svg)$",Fay$$list([Regex.I])));
});
Regex.newRegex_$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Regex",[]],(function(txt, flags){  flags = flags['map'](function(flag) {    return flag['instance']['toLowerCase']();  });  return new RegExp(txt, flags['join'](''));})(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["list",[["user","Flag",[]]]],$p2)));
  });
};
Regex.test$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Regex",[]],$p1)['test'](Fay$$fayToJs(["user","Text",[]],$p2)));
  });
};
var Main = {};
Main._Saved = function Saved(){
};
Main._Saved.prototype.instance = "Saved";
Main.Saved = new Fay$$$(function(){
  return new Main._Saved();
});
Main._Saving = function Saving(){
};
Main._Saving.prototype.instance = "Saving";
Main.Saving = new Fay$$$(function(){
  return new Main._Saving();
});
Main._Unsave = function Unsave(){
};
Main._Unsave.prototype.instance = "Unsave";
Main.Unsave = new Fay$$$(function(){
  return new Main._Unsave();
});
Main._EditorMode = function EditorMode(){
};
Main._EditorMode.prototype.instance = "EditorMode";
Main.EditorMode = new Fay$$$(function(){
  return new Main._EditorMode();
});
Main._TermMode = function TermMode(){
};
Main._TermMode.prototype.instance = "TermMode";
Main.TermMode = new Fay$$$(function(){
  return new Main._TermMode();
});
Main.setSaveState = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function (state) { window['saveState'] = state['instance']})(Fay$$fayToJs(["user","SaveState",[]],$p1))));
  });
};
Main.getSaveState = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","SaveState",[]],{ instance: window['saveState'] }));
});
Main.setScreenMode = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function (state) { window['screenMode'] = state['instance']})(Fay$$fayToJs(["user","ScreenMode",[]],$p1))));
  });
};
Main.getScreenMode = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","ScreenMode",[]],{ instance: window['screenMode'] }));
});
Main.setTimer = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function (t) { window['saveTimeout'] = t; })(Fay$$fayToJs(["user","Timer",[]],$p1))));
  });
};
Main.getTimer = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","Timer",[]],window['saveTimeout']));
});
Main.setAutoSave = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(autosave) { window['autosave'] = autosave; }) (Fay$$fayToJs_bool($p1))));
  });
};
Main.getAutoSave = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay_bool(window['autosave']));
});
Main.saveBtn = new Fay$$$(function(){
  return DOM.getElementById$36$uncurried("save");
});
Main.saved = new Fay$$$(function(){
  return Fay$$bind$36$uncurried(Main.getSaveState,function($p1){
    var st = $p1;
    return (function($tmp1){
      if (Fay$$_($tmp1) instanceof Main._Saving) {
        return Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Main.saveBtn,DOMUtils.setHtml("已保存"))),Main.setSaveState$36$uncurried(Main.Saved));
      }
      return Fay$$return$36$uncurried(Fay$$unit);
    })(st);
  });
});
Main.saving = new Fay$$$(function(){
  return Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Main.saveBtn,Fay$$_(DOMUtils.setProp("disabled"))("disabled")),DOMUtils.setHtml("保存..."))),Main.setSaveState$36$uncurried(Main.Saving));
});
Main.unsaved = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return (function(){
      var save = function($p1){
        return new Fay$$$(function(){
          var t = $p1;
          return Fay$$then$36$uncurried(Main.setTimer$36$uncurried(t),Main.saveCurrent$36$uncurried(api));
        });
      };
      return Fay$$then$36$uncurried(Main.setSaveState$36$uncurried(Main.Unsave),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Main.saveBtn,DOMUtils.removeProp("disabled")),DOMUtils.setHtml("保存"))),Fay$$bind$36$uncurried(Main.getAutoSave,function($p1){
        var autosave = $p1;
        return Prelude.$36$$36$uncurried(Prelude.when(autosave),Fay$$bind$36$uncurried(Main.getTimer,function($p1){
          var t = $p1;
          return Fay$$then$36$uncurried(DOM.clearTimeout$36$uncurried(t),Prelude.$36$$36$uncurried(Prelude.$_void,DOM.setTimeout$36$uncurried(1000,save)));
        }));
      })));
    })();
  });
};
Main.getCurrentPath = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","FilePath",[]],window['currentPath']));
});
Main.setCurrentPath_ = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(p){window['currentPath'] = p })(Fay$$fayToJs(["user","FilePath",[]],$p1))));
  });
};
Main.setCurrentPath = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var path = $p2;
      var isdir = $p1;
      return (function(){
        var dir = new Fay$$$(function(){
          return Fay$$_(isdir) ? path : FilePath.dropFileName$36$uncurried(path);
        });
        return Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("currentPath"),DOMUtils.setHtml(path))),Fay$$then$36$uncurried(Main.setCurrentPath_$36$uncurried(path),Main.setCurrentDirectory$36$uncurried(dir)));
      })();
    });
  };
};
Main.getCurrentDirectory = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","FilePath",[]],window['currentDirectory']));
});
Main.setCurrentDirectory = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(p){window['currentDirectory'] = p })(Fay$$fayToJs(["user","FilePath",[]],$p1))));
  });
};
Main.isUnsave = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) instanceof Main._Unsave) {
      return true;
    }
    return false;
  });
};
Main.saveCurrent = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Fay$$bind$36$uncurried(Main.getCurrentPath,function($p1){
      var currentPath = $p1;
      return Fay$$bind$36$uncurried(Main.getSaveState,function($p1){
        var saveState = $p1;
        return Prelude.$36$$36$uncurried(Prelude.when(Fay$$and$36$uncurried(Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Data.Text.null$36$uncurried(currentPath)),Fay$$and$36$uncurried(Main.isUnsave$36$uncurried(saveState),Utils.isTextFile$36$uncurried(currentPath)))),Fay$$then$36$uncurried(Main.saving,Fay$$bind$36$uncurried(Main.getEditor,function($p1){
          var editor = $p1;
          return Fay$$bind$36$uncurried(ACEditor.getValue$36$uncurried(editor),function($p1){
            var dat = $p1;
            return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(ProcAPI.writeFile$36$uncurried(api,currentPath,dat),FPromise.then_$36$uncurried(Prelude.$36$$36$uncurried(FPromise.toResolve,Prelude.$_const(Main.saved)))),FPromise.catch$36$uncurried(Prelude.$36$$36$uncurried(FPromise.toReject,Prelude.$36$$36$uncurried(Prelude.$_const,Main.unsaved$36$uncurried(api))))));
          });
        })));
      });
    });
  });
};
Main.newDoc = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var api = $p1;
      return (function(){
        var fixed = function($p1){
          return function($p2){
            return new Fay$$$(function(){
              if (Fay$$_($p2) === true) {
                var fn = $p1;
                return fn;
              }
              if (Fay$$_($p2) === false) {
                var fn = $p1;
                return Data.Text.$60$$62$$36$uncurried(fn,".md");
              }
              throw ["unhandled case in fixed",[$p1,$p2]];
            });
          };
        };
        return Fay$$then$36$uncurried(Main.saveCurrent$36$uncurried(api),Prelude.$36$$36$uncurried(Fay$$_(DOMUtils.prompt("输入文件名"))(""),function($p1){
          var fn = $p1;
          return Fay$$bind$36$uncurried(Main.getCurrentDirectory,function($p1){
            var currentDirectory = $p1;
            return (function(){
              return new Fay$$$(function(){
                var path = new Fay$$$(function(){
                  return FilePath.$60$$47$$62$$36$uncurried(currentDirectory,fn);
                });
                return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(ProcAPI.writeFile$36$uncurried(api,Fay$$_(Fay$$_(fixed)(path))(Utils.isTextFile$36$uncurried(fn)),"\n"),FPromise.then_$36$uncurried(Prelude.$36$$36$uncurried(FPromise.toResolve,Prelude.$36$$36$uncurried(Prelude.$_const,Main.updateTree$36$uncurried(api))))),FPromise.catch$36$uncurried(Fay$$_(FPromise.toReject)(Data.Text.putStrLn))));
              });
            })();
          });
        }));
      })();
    });
  };
};
Main.deleteDoc = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var api = $p1;
      return Fay$$bind$36$uncurried(Main.getCurrentPath,function($p1){
        var currentPath = $p1;
        return Prelude.$36$$36$uncurried(Prelude.unless(Data.Text.null$36$uncurried(currentPath)),Prelude.$36$$36$uncurried(DOMUtils.confirm(Data.Text.$60$$62$$36$uncurried("删除 ",Data.Text.$60$$62$$36$uncurried(currentPath," ?"))),Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(ProcAPI.removeFile$36$uncurried(api,currentPath),FPromise.then_$36$uncurried(Prelude.$36$$36$uncurried(FPromise.toResolve,Prelude.$_const(Fay$$then$36$uncurried(Fay$$then$36$uncurried(Main.updateTree$36$uncurried(api),Main.showCurrentPath$36$uncurried(false,"")),Main.cleanScreen$36$uncurried("Deleted")))))),FPromise.catch$36$uncurried(Fay$$_(FPromise.toReject)(Data.Text.putStrLn))))));
      });
    });
  };
};
Main.isDir = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(Fay$$fayToJs(["user","TreeNode",[]],$p1)['isDir']);
  });
};
Main.serverPath = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","TreeNode",[]],$p1)['serverPath']);
  });
};
Main.fileSize = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Fay$$fayToJs(["user","TreeNode",[]],$p1)['size']);
  });
};
Main.initTree = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],initTree(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["function",[["user","TreeNode",[]],["action",[["unknown"]]]]],$p2))));
    });
  };
};
Main.clearTree = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["unknown"],clearTree()));
});
Main.updateTree = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Fay$$then$36$uncurried(Main.clearTree,Main.loadTree$36$uncurried(api));
  });
};
Main.loadTree = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(ProcAPI.loadFileTree$36$uncurried(api),FPromise.then_$36$uncurried(Fay$$_(FPromise.toResolve)(function($p1){
      var $gen_0 = $p1;
      return Main.initTree$36$uncurried($gen_0,Main.treeNodeAction(api));
    }))));
  });
};
Main.getEditor = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","Editor",[]],window['editor']));
});
Main.setEditor = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['editor'] = Fay$$fayToJs(["user","Editor",[]],$p1)));
  });
};
Main.isEditorInitialized = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay_bool(window['editorInitialized']));
});
Main.setIsEditorInitialized = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['editorInitialized'] = true));
});
Main.termElem = new Fay$$$(function(){
  return DOM.getElementById$36$uncurried("term");
});
Main.readOnlyElem = new Fay$$$(function(){
  return DOM.getElementById$36$uncurried("read-only");
});
Main.switchScreenBtn = new Fay$$$(function(){
  return DOM.getElementById$36$uncurried("openTerm");
});
Main.editorElem = new Fay$$$(function(){
  return DOM.getElementById$36$uncurried("editor");
});
Main.sidebarElem = new Fay$$$(function(){
  return DOM.getElementById$36$uncurried("sidebar");
});
Main.mainElem = new Fay$$$(function(){
  return DOM.getElementById$36$uncurried("main");
});
Main.menuElem = new Fay$$$(function(){
  return DOM.getElementById$36$uncurried("menu");
});
Main.setShow = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === true) {
      return Fay$$_(Prelude.flip(DOM.removeClass))("hide");
    }
    if (Fay$$_($p1) === false) {
      return Fay$$_(Prelude.flip(DOM.addClass))("hide");
    }
    throw ["unhandled case in setShow",[$p1]];
  });
};
Main.showImage = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Fay$$then$36$uncurried(Main.setEditorData$36$uncurried("text",""),Prelude.$36$$36$uncurried(Main.signCurrentPath(api),function($p1){
      return function($p2){
        var url = $p2;
        return Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Main.readOnlyElem,DOMUtils.setHtml(Data.Text.$60$$62$$36$uncurried("<img src='",Data.Text.$60$$62$$36$uncurried(url,"'>")))),Main.setShow$36$uncurried(true));
      };
    }));
  });
};
Main.showFile = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return (function(){
      var doResolve = function($p1){
        return function($p2){
          return new Fay$$$(function(){
            var txt = $p2;
            var path = $p1;
            return Fay$$then$36$uncurried(Main.setEditorData$36$uncurried(Fay$$_(Utils.getMode)(path),txt),Prelude.$36$$36$uncurried(Prelude.when(Utils.isTextFile$36$uncurried(path)),Main.addChangeEvent$36$uncurried(api)));
          });
        };
      };
      return Fay$$bind$36$uncurried(Main.getCurrentPath,function($p1){
        var currentPath = $p1;
        return Fay$$then$36$uncurried(Fay$$bind$36$uncurried(Main.readOnlyElem,Main.setShow$36$uncurried(false)),Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(ProcAPI.readFile$36$uncurried(api,currentPath),FPromise.then_$36$uncurried(Prelude.$36$$36$uncurried(FPromise.toResolve,Fay$$_(doResolve)(currentPath)))),FPromise.catch$36$uncurried(Fay$$_(FPromise.toReject)(Prelude.print)))));
      });
    })();
  });
};
Main.initEditor = new Fay$$$(function(){
  return Fay$$bind$36$uncurried(Main.isEditorInitialized,function($p1){
    var isInitialized = $p1;
    return Fay$$_(isInitialized) ? Main.getEditor : Fay$$then$36$uncurried(Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(ACEditor.newEditor$36$uncurried("editor"),ACEditor.setTheme("chrome")),Main.setEditor),Fay$$then$36$uncurried(Fay$$bind$36$uncurried(Main.editorElem,Fay$$_(Prelude.flip(DOM.removeClass))("uninitialized")),Fay$$then$36$uncurried(Main.setIsEditorInitialized,Main.getEditor)));
  });
});
Main.enableElem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === true) {
        var el = $p1;
        return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried(el),DOMUtils.removeProp("disabled")));
      }
      if (Fay$$_($p2) === false) {
        var el = $p1;
        return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried(el),Fay$$_(DOMUtils.setProp("disabled"))("disabled")));
      }
      throw ["unhandled case in enableElem",[$p1,$p2]];
    });
  };
};
Main.setEditorData = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var body = $p2;
      var mode = $p1;
      return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Main.initEditor,ACEditor.removeAllEvent("change")),ACEditor.setValue(body)),ACEditor.setMode(mode)));
    });
  };
};
Main.addChangeEvent = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Main.getEditor,Fay$$_(ACEditor.addEvent("change"))(Prelude.$36$$36$uncurried(Prelude.$_const,Main.unsaved$36$uncurried(api)))));
  });
};
Main.showCurrentPath = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var path = $p2;
      var isdir = $p1;
      return Fay$$then$36$uncurried(Main.setCurrentPath$36$uncurried(isdir,path),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Main.enableElem("download"),Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isdir)),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Main.enableElem("downloadLink"),Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isdir)),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Main.enableElem("delete"),Prelude.$36$$36$uncurried(Prelude.not,Data.Text.null$36$uncurried(path))),Prelude.$36$$36$uncurried(Main.enableElem("run"),Fay$$_(Utils.canProc)(path))))));
    });
  };
};
Main.cleanScreen = function($p1){
  return new Fay$$$(function(){
    var e = $p1;
    return Fay$$then$36$uncurried(Main.setEditorData$36$uncurried("text",e),Fay$$bind$36$uncurried(Main.readOnlyElem,Main.setShow$36$uncurried(false)));
  });
};
Main.treeNodeAction = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var tn = $p2;
      var api = $p1;
      return (function(){
        var currentPath = new Fay$$$(function(){
          return Main.serverPath$36$uncurried(tn);
        });
        return Fay$$then$36$uncurried(Main.showCurrentPath$36$uncurried(Main.isDir$36$uncurried(tn),currentPath),Fay$$_(Main.isDir$36$uncurried(tn)) ? Main.cleanScreen$36$uncurried("Directory") : Fay$$_(Fay$$_(Utils.isImage)(currentPath)) ? Main.showImage$36$uncurried(api) : Fay$$_(Fay$$lt$36$uncurried(Main.fileSize$36$uncurried(tn),1048576)) ? Main.showFile$36$uncurried(api) : Main.cleanScreen$36$uncurried("File to large."));
      })();
    });
  };
};
Main.selectFile = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],selectFile(Fay$$fayToJs(["function",[["user","Text",[]],["user","Text",[]],["action",[["unknown"]]]]],$p1))));
  });
};
Main.uploadFile = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var isArc = $p2;
        var api = $p1;
        return (function(){
          var action = function($p1){
            return function($p2){
              return new Fay$$$(function(){
                var dat = $p2;
                var name = $p1;
                return Fay$$bind$36$uncurried(Main.getCurrentDirectory,function($p1){
                  var currentDirectory = $p1;
                  return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Fay$$_(Fay$$_(Fay$$_(doUpload)(api))(FilePath.$60$$47$$62$$36$uncurried(currentDirectory,name)))(dat),FPromise.then_$36$uncurried(Prelude.$36$$36$uncurried(FPromise.toResolve,Prelude.$36$$36$uncurried(Prelude.$_const,Main.updateTree$36$uncurried(api))))),FPromise.catch$36$uncurried(Fay$$_(FPromise.toReject)(Prelude.print))));
                });
              });
            };
          };
          var doUpload = new Fay$$$(function(){
            return Fay$$_(isArc) ? ProcAPI.uploadArchive : ProcAPI.uploadFile;
          });
          return Main.selectFile$36$uncurried(action);
        })();
      });
    };
  };
};
Main.runProcAndShow = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var args = $p3;
        var fn = $p2;
        var api = $p1;
        return (function(){
          var showResult = function($p1){
            return new Fay$$$(function(){
              var txt = $p1;
              return Fay$$then$36$uncurried(Main.updateTree$36$uncurried(api),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("proc-result-message"),DOMUtils.setHtml(txt))),Fay$$bind$36$uncurried(DOMUtils.getModal$36$uncurried$36$uncurried("#proc-result"),DOMUtils.showModal)));
            });
          };
          return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Fay$$_(ProcAPI.runFile$36$uncurried(api,fn))(args),FPromise.then_$36$uncurried(Fay$$_(FPromise.toResolve)(showResult))),FPromise.catch$36$uncurried(Fay$$_(FPromise.toReject)(showResult))));
        })();
      });
    };
  };
};
Main.runCurrentFile = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Fay$$bind$36$uncurried(Main.getCurrentPath,function($p1){
      var currentPath = $p1;
      return Main.runProcAndShow$36$uncurried(api,currentPath,null);
    });
  });
};
Main.showTerm = function($p1){
  return new Fay$$$(function(){
    var tm = $p1;
    return Fay$$then$36$uncurried(Fay$$bind$36$uncurried(Main.termElem,Main.setShow$36$uncurried(true)),Fay$$then$36$uncurried(Main.setScreenMode$36$uncurried(Main.TermMode),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Main.switchScreenBtn,DOMUtils.setHtml("编辑器"))),TermManager.openTerm$36$uncurried(tm))));
  });
};
Main.hideTerm = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Fay$$then$36$uncurried(Fay$$bind$36$uncurried(Main.termElem,Main.setShow$36$uncurried(false)),Fay$$then$36$uncurried(Main.setScreenMode$36$uncurried(Main.EditorMode),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Main.switchScreenBtn,DOMUtils.setHtml("终端"))),Main.updateTree$36$uncurried(api))));
  });
};
Main.switchScreen = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var api = $p2;
        var tm = $p1;
        return Fay$$bind$36$uncurried(Main.getScreenMode,function($p1){
          var mode = $p1;
          return (function($tmp1){
            if (Fay$$_($tmp1) instanceof Main._TermMode) {
              return Main.hideTerm$36$uncurried(api);
            }
            if (Fay$$_($tmp1) instanceof Main._EditorMode) {
              return Main.showTerm$36$uncurried(tm);
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(mode);
        });
      });
    };
  };
};
Main.signCurrentPath = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var next = $p2;
      var api = $p1;
      return Fay$$bind$36$uncurried(Main.getCurrentPath,function($p1){
        var currentPath = $p1;
        return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(ProcAPI.signFilePath$36$uncurried(api,currentPath),FPromise.then_$36$uncurried(Fay$$_(FPromise.toResolve)(Fay$$_(next)(currentPath)))),FPromise.catch$36$uncurried(Fay$$_(FPromise.toReject)(Prelude.print))));
      });
    });
  };
};
Main.download = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var api = $p1;
      return Main.signCurrentPath$36$uncurried(api,DOMUtils.saveAs);
    });
  };
};
Main.downloadLink = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var api = $p1;
      return Prelude.$36$$36$uncurried(Main.signCurrentPath(api),function($p1){
        return function($p2){
          var url = $p2;
          return Prelude.$36$$36$uncurried(Fay$$_(DOMUtils.prompt("下载地址"))(url),Prelude.$36$$36$uncurried(Prelude.$_const,Fay$$return$36$uncurried(Fay$$unit)));
        };
      });
    });
  };
};
Main.getKeyFromLocation = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],/key=([^&]+)/.exec(location.search)[1]));
});
Main.getSecret_ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Maybe",[["user","Text",[]]]],(function(nothing, just, key) { var v = localStorage.getItem(key); if (v) {return just(v)} else {return nothing} })(Fay$$fayToJs(["user","Maybe",[["user","Text",[]]]],$p1), Fay$$fayToJs(["function",[["user","Text",[]],["user","Maybe",[["user","Text",[]]]]]],$p2), Fay$$fayToJs(["user","Text",[]],$p3))));
      });
    };
  };
};
Main.getSecret = new Fay$$$(function(){
  return Fay$$_(Main.getSecret_(Prelude.Nothing))(Prelude.Just);
});
Main.setSecret = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],localStorage.setItem(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Text",[]],$p2))));
    });
  };
};
Main.resetSecret = function($p1){
  return new Fay$$$(function(){
    var sec = $p1;
    return Fay$$_(Prelude.$46$(Fay$$_(DOMUtils.prompt("请输入新密钥"))(sec)))(Main.setSecret);
  });
};
Main.prepareSecrect = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var next = $p2;
      var key = $p1;
      return Fay$$bind$36$uncurried(Fay$$_(Main.getSecret)(key),function($p1){
        var msec = $p1;
        return (function($tmp1){
          if (Fay$$_($tmp1) instanceof Prelude._Nothing) {
            return Prelude.$36$$36$uncurried(Fay$$_(DOMUtils.prompt("请输入密钥"))(""),function($p1){
              var sec = $p1;
              return Fay$$then$36$uncurried(Main.setSecret$36$uncurried(key,sec),Fay$$_(next)(sec));
            });
          }
          if (Fay$$_($tmp1) instanceof Prelude._Just) {
            var sec = Fay$$_($tmp1).slot1;
            return Fay$$_(next)(sec);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(msec);
      });
    });
  };
};
Main.switchSidebar = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var tm = $p1;
      return Fay$$bind$36$uncurried(Main.sidebarElem,function($p1){
        var el = $p1;
        return Fay$$bind$36$uncurried(DOM.hasClass$36$uncurried(el,"hide"),function($p1){
          var has = $p1;
          return Fay$$then$36$uncurried(Fay$$_(Main.setShow$36$uncurried(has))(el),Fay$$bind$36$uncurried(Main.mainElem,function($p1){
            var mel = $p1;
            return Fay$$then$36$uncurried(Fay$$_(has) ? DOM.removeClass$36$uncurried(mel,"fullscreen") : DOM.addClass$36$uncurried(mel,"fullscreen"),Fay$$bind$36$uncurried(Main.getScreenMode,function($p1){
              var mode = $p1;
              return (function($tmp1){
                if (Fay$$_($tmp1) instanceof Main._TermMode) {
                  return TermManager.openTerm$36$uncurried(tm);
                }
                if (Fay$$_($tmp1) instanceof Main._EditorMode) {
                  return Fay$$return$36$uncurried(Fay$$unit);
                }
                return (function(){ throw (["unhandled case",$tmp1]); })();
              })(mode);
            }));
          }));
        });
      });
    });
  };
};
Main.program = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var sec = $p2;
      var key = $p1;
      return Fay$$bind$36$uncurried(ProcAPI.newProcAPI$36$uncurried(key,sec),function($p1){
        var api = $p1;
        return Fay$$bind$36$uncurried(TermManager.newTermManager$36$uncurried("#terminal-container",api,Main.hideTerm$36$uncurried(api)),function($p1){
          var tm = $p1;
          return Fay$$then$36$uncurried(Main.setAutoSave$36$uncurried(true),Fay$$then$36$uncurried(Main.setScreenMode$36$uncurried(Main.EditorMode),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(DOMUtils.windowAddEventListener("beforeunload"),Prelude.$_const(TermManager.closeTerm$36$uncurried(tm))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("new"),Fay$$_(DOMUtils.addEventListener("click"))(Main.newDoc(api)))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("delete"),Fay$$_(DOMUtils.addEventListener("click"))(Main.deleteDoc(api)))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Main.saveBtn,Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$36$$36$uncurried(Prelude.$_const,Main.saveCurrent$36$uncurried(api))))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("upload"),Fay$$_(DOMUtils.addEventListener("click"))(Fay$$_(Main.uploadFile(api))(false)))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("uploadArchive"),Fay$$_(DOMUtils.addEventListener("click"))(Fay$$_(Main.uploadFile(api))(true)))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Main.switchScreenBtn,Fay$$_(DOMUtils.addEventListener("click"))(Fay$$_(Main.switchScreen(tm))(api)))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("run"),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$36$$36$uncurried(Prelude.$_const,Main.runCurrentFile$36$uncurried(api))))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("download"),Fay$$_(DOMUtils.addEventListener("click"))(Main.download(api)))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("downloadLink"),Fay$$_(DOMUtils.addEventListener("click"))(Main.downloadLink(api)))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("resetSecret"),Fay$$_(DOMUtils.addEventListener("click"))(Prelude.$36$$36$uncurried(Prelude.$_const,Fay$$_(Main.resetSecret$36$uncurried(sec))(key))))),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Main.menuElem,Fay$$_(DOMUtils.addEventListener("click"))(Main.switchSidebar(tm)))),Main.loadTree$36$uncurried(api)))))))))))))));
        });
      });
    });
  };
};
Main.main = new Fay$$$(function(){
  return Fay$$bind$36$uncurried(Main.getKeyFromLocation,function($p1){
    var key = $p1;
    return Prelude.$36$$36$uncurried(Main.prepareSecrect(key),Main.program(key));
  });
});
Fay$$objConcat(Fay$$fayToJsHash,{"Saved": function(type,argTypes,_obj){
  var obj_ = {"instance": "Saved"};
  return obj_;
},"Saving": function(type,argTypes,_obj){
  var obj_ = {"instance": "Saving"};
  return obj_;
},"Unsave": function(type,argTypes,_obj){
  var obj_ = {"instance": "Unsave"};
  return obj_;
},"EditorMode": function(type,argTypes,_obj){
  var obj_ = {"instance": "EditorMode"};
  return obj_;
},"TermMode": function(type,argTypes,_obj){
  var obj_ = {"instance": "TermMode"};
  return obj_;
}});
Fay$$objConcat(Fay$$jsToFayHash,{"Saved": function(type,argTypes,obj){
  return new Main._Saved();
},"Saving": function(type,argTypes,obj){
  return new Main._Saving();
},"Unsave": function(type,argTypes,obj){
  return new Main._Unsave();
},"EditorMode": function(type,argTypes,obj){
  return new Main._EditorMode();
},"TermMode": function(type,argTypes,obj){
  return new Main._TermMode();
}});
Prelude.$36$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var x = $p2;
    var f = $p1;
    return Fay$$_(f)(x);
  });
};
Main.loadTree$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(ProcAPI.loadFileTree$36$uncurried(api),FPromise.then_$36$uncurried(Fay$$_(FPromise.toResolve)(function($p1){
      var $gen_0 = $p1;
      return Main.initTree$36$uncurried($gen_0,Main.treeNodeAction(api));
    }))));
  });
};
Main.resetSecret$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var sec = $p1;
    return Fay$$_(Prelude.$46$(Fay$$_(DOMUtils.prompt("请输入新密钥"))(sec)))(Main.setSecret);
  });
};
DOM.getElementById$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document['getElementById'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
Main.runCurrentFile$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Fay$$bind$36$uncurried(Main.getCurrentPath,function($p1){
      var currentPath = $p1;
      return Main.runProcAndShow$36$uncurried(api,currentPath,null);
    });
  });
};
Main.saveCurrent$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Fay$$bind$36$uncurried(Main.getCurrentPath,function($p1){
      var currentPath = $p1;
      return Fay$$bind$36$uncurried(Main.getSaveState,function($p1){
        var saveState = $p1;
        return Prelude.$36$$36$uncurried(Prelude.when(Fay$$and$36$uncurried(Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Data.Text.null$36$uncurried(currentPath)),Fay$$and$36$uncurried(Main.isUnsave$36$uncurried(saveState),Utils.isTextFile$36$uncurried(currentPath)))),Fay$$then$36$uncurried(Main.saving,Fay$$bind$36$uncurried(Main.getEditor,function($p1){
          var editor = $p1;
          return Fay$$bind$36$uncurried(ACEditor.getValue$36$uncurried(editor),function($p1){
            var dat = $p1;
            return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(ProcAPI.writeFile$36$uncurried(api,currentPath,dat),FPromise.then_$36$uncurried(Prelude.$36$$36$uncurried(FPromise.toResolve,Prelude.$_const(Main.saved)))),FPromise.catch$36$uncurried(Prelude.$36$$36$uncurried(FPromise.toReject,Prelude.$36$$36$uncurried(Prelude.$_const,Main.unsaved$36$uncurried(api))))));
          });
        })));
      });
    });
  });
};
TermManager.closeTerm$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","TermManager",[]],$p1)['close']()));
  });
};
Main.setScreenMode$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function (state) { window['screenMode'] = state['instance']})(Fay$$fayToJs(["user","ScreenMode",[]],$p1))));
  });
};
Main.setAutoSave$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(autosave) { window['autosave'] = autosave; }) (Fay$$fayToJs_bool($p1))));
  });
};
Main.hideTerm$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Fay$$then$36$uncurried(Fay$$bind$36$uncurried(Main.termElem,Main.setShow$36$uncurried(false)),Fay$$then$36$uncurried(Main.setScreenMode$36$uncurried(Main.EditorMode),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Main.switchScreenBtn,DOMUtils.setHtml("终端"))),Main.updateTree$36$uncurried(api))));
  });
};
TermManager.newTermManager$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","TermManager",[]],new JSTermManager(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","ProcAPI",[]],$p2), Fay$$fayToJs(["action",[["unknown"]]],$p3))));
  });
};
ProcAPI.newProcAPI$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","ProcAPI",[]],new ProcJSApi({key: Fay$$fayToJs(["user","Text",[]],$p1), secret: Fay$$fayToJs(["user","Text",[]],$p2)})));
  });
};
TermManager.openTerm$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","TermManager",[]],$p1)['init']()));
  });
};
DOM.addClass$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1).classList['add'](Fay$$fayToJs(["user","Text",[]],$p2))));
  });
};
DOM.removeClass$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['classList']['remove'](Fay$$fayToJs(["user","Text",[]],$p2))));
  });
};
Main.setShow$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === true) {
      return Fay$$_(Prelude.flip(DOM.removeClass))("hide");
    }
    if (Fay$$_($p1) === false) {
      return Fay$$_(Prelude.flip(DOM.addClass))("hide");
    }
    throw ["unhandled case in setShow",[$p1]];
  });
};
DOM.hasClass$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay_bool(Fay$$fayToJs(["user","Element",[]],$p1)['classList']['contains'](Fay$$fayToJs(["user","Text",[]],$p2))));
  });
};
Main.setSecret$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],localStorage.setItem(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Text",[]],$p2))));
  });
};
Main.signCurrentPath$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var next = $p2;
    var api = $p1;
    return Fay$$bind$36$uncurried(Main.getCurrentPath,function($p1){
      var currentPath = $p1;
      return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(ProcAPI.signFilePath$36$uncurried(api,currentPath),FPromise.then_$36$uncurried(Fay$$_(FPromise.toResolve)(Fay$$_(next)(currentPath)))),FPromise.catch$36$uncurried(Fay$$_(FPromise.toReject)(Prelude.print))));
    });
  });
};
FPromise.catch$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return FPromise.catch_(FPromise.fromReject$36$uncurried$36$uncurried(f));
  });
};
FPromise.then_$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return Fay$$_(FPromise.then___(FPromise.fromResolve$36$uncurried$36$uncurried(f)))(Prelude.$36$$36$uncurried(Prelude.$_const,Fay$$return$36$uncurried(Fay$$unit)));
  });
};
ProcAPI.signFilePath$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['signFilePath'](Fay$$fayToJs(["user","FilePath",[]],$p2))));
  });
};
Main.showTerm$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var tm = $p1;
    return Fay$$then$36$uncurried(Fay$$bind$36$uncurried(Main.termElem,Main.setShow$36$uncurried(true)),Fay$$then$36$uncurried(Main.setScreenMode$36$uncurried(Main.TermMode),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Main.switchScreenBtn,DOMUtils.setHtml("编辑器"))),TermManager.openTerm$36$uncurried(tm))));
  });
};
Main.updateTree$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Fay$$then$36$uncurried(Main.clearTree,Main.loadTree$36$uncurried(api));
  });
};
Main.runProcAndShow$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    var args = $p3;
    var fn = $p2;
    var api = $p1;
    return (function(){
      var showResult = function($p1){
        return new Fay$$$(function(){
          var txt = $p1;
          return Fay$$then$36$uncurried(Main.updateTree$36$uncurried(api),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("proc-result-message"),DOMUtils.setHtml(txt))),Fay$$bind$36$uncurried(DOMUtils.getModal$36$uncurried$36$uncurried("#proc-result"),DOMUtils.showModal)));
        });
      };
      return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Fay$$_(ProcAPI.runFile$36$uncurried(api,fn))(args),FPromise.then_$36$uncurried(Fay$$_(FPromise.toResolve)(showResult))),FPromise.catch$36$uncurried(Fay$$_(FPromise.toReject)(showResult))));
    })();
  });
};
ProcAPI.runFile$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var fn = $p2;
    var api = $p1;
    return Fay$$_(Prelude.$46$(Fay$$_(ProcAPI.runFile_(api))(fn)))(Fay$$_(Prelude.$46$(Data.Text.pack))(Prelude.show));
  });
};
DOMUtils.getModal$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Modal",[]],UIkit['modal'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
DOMUtils.getModal$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Modal",[]],UIkit['modal'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
Main.selectFile$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],selectFile(Fay$$fayToJs(["function",[["user","Text",[]],["user","Text",[]],["action",[["unknown"]]]]],$p1))));
  });
};
FilePath.$60$$47$$62$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var g = $p2;
    var f = $p1;
    return FilePath.append$36$uncurried(f,g);
  });
};
Main.cleanScreen$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var e = $p1;
    return Fay$$then$36$uncurried(Main.setEditorData$36$uncurried("text",e),Fay$$bind$36$uncurried(Main.readOnlyElem,Main.setShow$36$uncurried(false)));
  });
};
Main.showFile$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return (function(){
      var doResolve = function($p1){
        return function($p2){
          return new Fay$$$(function(){
            var txt = $p2;
            var path = $p1;
            return Fay$$then$36$uncurried(Main.setEditorData$36$uncurried(Fay$$_(Utils.getMode)(path),txt),Prelude.$36$$36$uncurried(Prelude.when(Utils.isTextFile$36$uncurried(path)),Main.addChangeEvent$36$uncurried(api)));
          });
        };
      };
      return Fay$$bind$36$uncurried(Main.getCurrentPath,function($p1){
        var currentPath = $p1;
        return Fay$$then$36$uncurried(Fay$$bind$36$uncurried(Main.readOnlyElem,Main.setShow$36$uncurried(false)),Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(ProcAPI.readFile$36$uncurried(api,currentPath),FPromise.then_$36$uncurried(Prelude.$36$$36$uncurried(FPromise.toResolve,Fay$$_(doResolve)(currentPath)))),FPromise.catch$36$uncurried(Fay$$_(FPromise.toReject)(Prelude.print)))));
      });
    })();
  });
};
Main.fileSize$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Fay$$fayToJs(["user","TreeNode",[]],$p1)['size']);
  });
};
Main.showImage$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Fay$$then$36$uncurried(Main.setEditorData$36$uncurried("text",""),Prelude.$36$$36$uncurried(Main.signCurrentPath(api),function($p1){
      return function($p2){
        var url = $p2;
        return Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Main.readOnlyElem,DOMUtils.setHtml(Data.Text.$60$$62$$36$uncurried("<img src='",Data.Text.$60$$62$$36$uncurried(url,"'>")))),Main.setShow$36$uncurried(true));
      };
    }));
  });
};
Main.isDir$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(Fay$$fayToJs(["user","TreeNode",[]],$p1)['isDir']);
  });
};
Main.showCurrentPath$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var path = $p2;
    var isdir = $p1;
    return Fay$$then$36$uncurried(Main.setCurrentPath$36$uncurried(isdir,path),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Main.enableElem("download"),Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isdir)),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Main.enableElem("downloadLink"),Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isdir)),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Main.enableElem("delete"),Prelude.$36$$36$uncurried(Prelude.not,Data.Text.null$36$uncurried(path))),Prelude.$36$$36$uncurried(Main.enableElem("run"),Fay$$_(Utils.canProc)(path))))));
  });
};
Main.serverPath$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","TreeNode",[]],$p1)['serverPath']);
  });
};
Main.setEditorData$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var body = $p2;
    var mode = $p1;
    return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Main.initEditor,ACEditor.removeAllEvent("change")),ACEditor.setValue(body)),ACEditor.setMode(mode)));
  });
};
Data.Text.null$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Text",[]],$p1).length == 0);
  });
};
Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Prelude.not$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Prelude.not$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Prelude.not$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Prelude.not$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Main.setCurrentPath$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var path = $p2;
    var isdir = $p1;
    return (function(){
      var dir = new Fay$$$(function(){
        return Fay$$_(isdir) ? path : FilePath.dropFileName$36$uncurried(path);
      });
      return Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(DOM.getElementById$36$uncurried("currentPath"),DOMUtils.setHtml(path))),Fay$$then$36$uncurried(Main.setCurrentPath_$36$uncurried(path),Main.setCurrentDirectory$36$uncurried(dir)));
    })();
  });
};
Main.unsaved$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return (function(){
      var save = function($p1){
        return new Fay$$$(function(){
          var t = $p1;
          return Fay$$then$36$uncurried(Main.setTimer$36$uncurried(t),Main.saveCurrent$36$uncurried(api));
        });
      };
      return Fay$$then$36$uncurried(Main.setSaveState$36$uncurried(Main.Unsave),Fay$$then$36$uncurried(Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Fay$$bind$36$uncurried(Main.saveBtn,DOMUtils.removeProp("disabled")),DOMUtils.setHtml("保存"))),Fay$$bind$36$uncurried(Main.getAutoSave,function($p1){
        var autosave = $p1;
        return Prelude.$36$$36$uncurried(Prelude.when(autosave),Fay$$bind$36$uncurried(Main.getTimer,function($p1){
          var t = $p1;
          return Fay$$then$36$uncurried(DOM.clearTimeout$36$uncurried(t),Prelude.$36$$36$uncurried(Prelude.$_void,DOM.setTimeout$36$uncurried(1000,save)));
        }));
      })));
    })();
  });
};
ACEditor.newEditor$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Editor",[]],ace['edit'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
ProcAPI.readFile$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['readFile'](Fay$$fayToJs(["user","FilePath",[]],$p2))));
  });
};
Main.addChangeEvent$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var api = $p1;
    return Prelude.$36$$36$uncurried(Prelude.$_void,Fay$$bind$36$uncurried(Main.getEditor,Fay$$_(ACEditor.addEvent("change"))(Prelude.$36$$36$uncurried(Prelude.$_const,Main.unsaved$36$uncurried(api)))));
  });
};
Utils.isTextFile$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(isTextFile(Fay$$fayToJs(["user","FilePath",[]],$p1)));
  });
};
Data.Text.$60$$62$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1) + Fay$$fayToJs(["user","Text",[]],$p2));
  });
};
Main.initTree$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],initTree(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["function",[["user","TreeNode",[]],["action",[["unknown"]]]]],$p2))));
  });
};
ProcAPI.loadFileTree$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['loadFileTree']()));
  });
};
ProcAPI.removeFile$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['removeFile'](Fay$$fayToJs(["user","FilePath",[]],$p2))));
  });
};
ProcAPI.writeFile$36$uncurried = function($p1,$p2,$p3){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],Fay$$fayToJs(["user","ProcAPI",[]],$p1)['writeFile'](Fay$$fayToJs(["user","FilePath",[]],$p2), Fay$$fayToJs(["user","Text",[]],$p3))));
  });
};
ACEditor.getValue$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],(function(editor) { return editor['getValue']() })(Fay$$fayToJs(["user","Editor",[]],$p1))));
  });
};
Main.isUnsave$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) instanceof Main._Unsave) {
      return true;
    }
    return false;
  });
};
Main.setCurrentDirectory$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(p){window['currentDirectory'] = p })(Fay$$fayToJs(["user","FilePath",[]],$p1))));
  });
};
Main.setCurrentPath_$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(p){window['currentPath'] = p })(Fay$$fayToJs(["user","FilePath",[]],$p1))));
  });
};
FilePath.dropFileName$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","FilePath",[]],(function(fn) { return fn['substr'](0, fn['lastIndexOf']('/')) })(Fay$$fayToJs(["user","FilePath",[]],$p1)));
  });
};
DOM.setTimeout$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Timer",[]],(function (f,i) { var id = window['setTimeout'](function () { f(id); }, i); return id; })(Fay$$fayToJs(["function",[["user","Timer",[]],["action",[["unknown"]]]]],$p2),Fay$$fayToJs_double($p1))));
  });
};
DOM.clearTimeout$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['clearTimeout'](Fay$$fayToJs(["user","Timer",[]],$p1))));
  });
};
Main.setSaveState$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function (state) { window['saveState'] = state['instance']})(Fay$$fayToJs(["user","SaveState",[]],$p1))));
  });
};
Main.setTimer$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function (t) { window['saveTimeout'] = t; })(Fay$$fayToJs(["user","Timer",[]],$p1))));
  });
};
FPromise.fromReject$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return f;
  });
};
FPromise.fromResolve$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return f;
  });
};
FPromise.newPromise_$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Promise",[]],new Promise(Fay$$fayToJs(["function",[["function",[["unknown"],["action",[["unknown"]]]]],["function",[["user","Text",[]],["action",[["unknown"]]]]],["action",[["unknown"]]]]],$p1))));
  });
};
DOMUtils.querySelector$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document['querySelector'](Fay$$fayToJs(["user","Text",[]],$p1))));
  });
};
Prelude.$61$$60$$60$$36$uncurried = function($p1,$p2){
  return new Fay$$$(function(){
    var x = $p2;
    var f = $p1;
    return Fay$$bind$36$uncurried(x,f);
  });
};
Prelude.floor$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));
  });
};
Prelude.ceiling$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));
  });
};
Prelude.negate$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (-(Fay$$_(x)));
  });
};
Prelude.Just$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Just(slot1);
  });
};
Prelude.reverse$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(Prelude.reverse$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs),Fay$$list([x]));
    }
    if (Fay$$_($p1) === null) {
      return null;
    }
    throw ["unhandled case in reverse",[$p1]];
  });
};
Prelude.or$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return false;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$or$36$uncurried(x,Prelude.or$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in or",[$p1]];
  });
};
Prelude.and$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$and$36$uncurried(x,Prelude.and$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(xs));
    }
    throw ["unhandled case in and",[$p1]];
  });
};
Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Prelude.span(Fay$$_(Prelude.$46$(Prelude.not))(p));
  });
};
Prelude.words$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var str = $p1;
    return (function(){
      var words$39$ = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) === null) {
            return null;
          }
          var s = $p1;
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var a = Fay$$index(0,Fay$$_($tmp1));
              var b = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.words$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(b));
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isSpace))(s));
        });
      };
      var isSpace = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Prelude.elem$36$uncurried(c,Fay$$list(" \t\r\n\u000c\u000b"));
        });
      };
      return Fay$$_(words$39$)(Prelude.dropWhile$36$uncurried(isSpace,str));
    })();
  });
};
Prelude.unlines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var l = $tmp1.car;
      var ls = $tmp1.cdr;
      return Prelude.$43$$43$$36$uncurried(l,Fay$$_(Fay$$_(Fay$$cons)("\n"))(Prelude.unlines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ls)));
    }
    throw ["unhandled case in unlines",[$p1]];
  });
};
Prelude.lines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var s = $p1;
    return (function(){
      var isLineBreak = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Fay$$or$36$uncurried(Fay$$eq$36$uncurried(c,"\r"),Fay$$eq$36$uncurried(c,"\n"));
        });
      };
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {
            return Fay$$list([a]);
          }
        }
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));
          if ($tmp2 instanceof Fay$$Cons) {
            var cs = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(a))(Prelude.lines$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(cs));
          }
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(Fay$$_(Prelude.break$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(isLineBreak))(s));
    })();
  });
};
Prelude.unzip3$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),3)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var z = Fay$$index(2,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),3)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            var zs = Fay$$index(2,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip3$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null,null]);
    }
    throw ["unhandled case in unzip3",[$p1]];
  });
};
Prelude.unzip$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Prelude.unzip$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null]);
    }
    throw ["unhandled case in unzip",[$p1]];
  });
};
Prelude.repeat$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(x))(Prelude.repeat$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x));
  });
};
Prelude.init$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$list("init: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      var t = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$cons)(h))(Prelude.init$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(t));
    }
    throw ["unhandled case in init",[$p1]];
  });
};
Prelude.error$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());
  });
};
Prelude.enumFrom$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var i = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(i))(Prelude.enumFrom$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(Fay$$add$36$uncurried(i,1)));
  });
};
Prelude.sequence_$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$return$36$uncurried(Fay$$unit);
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var m = $tmp1.car;
      var ms = $tmp1.cdr;
      return Fay$$then$36$uncurried(m,Prelude.sequence_$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(ms));
    }
    throw ["unhandled case in sequence_",[$p1]];
  });
};
Data.Nullable.fromNullable$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) instanceof Fay.FFI._Nullable) {
      var x = Fay$$_($p1).slot1;
      return Prelude.Just$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
    }
    if (Fay$$_($p1) instanceof Fay.FFI._Null) {
      return Prelude.Nothing;
    }
    throw ["unhandled case in fromNullable",[$p1]];
  });
};
Fay.FFI.Nullable$36$uncurried$36$uncurried$36$uncurried = function(slot1){
  return new Fay$$$(function(){
    return new Fay.FFI._Nullable(slot1);
  });
};
Prelude.even$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$eq$36$uncurried(Prelude.rem$36$uncurried(x,2),0);
  });
};
Prelude.abs$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.negate$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : x;
  });
};
Prelude.last$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    while (true) {
      if (Fay$$_($p1) === null) {
        return Prelude.error$36$uncurried(Fay$$list("last: empty list"));
      }
      if (Fay$$listLen(Fay$$_($p1),1)) {
        var a = Fay$$index(0,Fay$$_($p1));
        return a;
      }
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var t = $tmp1.cdr;
        $p1 = t;
        continue;
      }
      throw ["unhandled case in last",[$p1]];
    }
  });
};
Prelude.sortBy$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var cmp = $p1;
    return Fay$$_(Prelude.foldr(Prelude.insertBy(cmp)))(null);
  });
};
Prelude.fromIntegral$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return $p1;
  });
};
Prelude.truncate$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$lt$36$uncurried(x,0)) ? Prelude.ceiling$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x) : Prelude.floor$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried(x);
  });
};
Prelude.sqrt$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));
  });
};
Prelude.log$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));
  });
};
Prelude.exp$36$uncurried$36$uncurried$36$uncurried$36$uncurried$36$uncurried = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));
  });
};
Fay$$_(Main.main, true);
