syntax rotate = function (ctx) {
  var inCtx = ctx.contextify(ctx.next().value);
  var result = #``;
  var stx;
for (stx of inCtx) {
result = result.concat(#`${stx} = ${stx};`);
inCtx.next(); // Eating comma
}
return result.reverse();
}
var a = 1; var b = 2; var c = 3;
rotate (a,b,c)