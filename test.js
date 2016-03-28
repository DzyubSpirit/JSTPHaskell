var fs = require('fs');
var vm = require('vm');

fs.readFile('example.jsrs',function(err, data) {
	if (err) return;
	var sandbox = vm.createContext({module: {}});
	var js = vm.createScript('(' + data + ')');
	var res = js.runInNewContext(sandbox);
	for (var prop in res) {
		sandbox[prop] = res[prop];
		console.log(prop+': '+res[prop]);
	}
	console.log(res);
	// for (var prop in res.age) {
	// 	console.log(prop+': '+res.age[prop])
	// }
	// console.log(res.age());
});