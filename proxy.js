var p = new Proxy({}, {
    has: function(target, prop) {
      console.log('Log: ' + prop);
      return true;
    }
  });
  
  console.log('a' in p); 