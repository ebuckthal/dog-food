-- A shim to bridge fennel and love2d
fennel = require("fennel")
table[("insert")](package[("loaders")], fennel[("searcher")])
pp = function(x) print(require("fennelview")(x)) end

fennel.dofile("main.fnl")
