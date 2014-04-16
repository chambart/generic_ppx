open Special_format

let (!!) = printer

let v1 = !! "hello world\n"
let v1 = !! "%i%%\n" 1
let v2 = !! "%i(%f)\n" 1 [1.;2.]
let v3 = !! "%i(%f)%c\n" 1 [1.;2.] 'a'
let v4 = !! "%i((%f)(%c)\n)%i\n" 1 [[1.;2.],['a';'b']; [3.;4.],['c';'d']] 6
let v5 = !! "%i(%(%i([%f ]) ([%c ])%))%i\n" 1 [(5,[1.;2.]),['a';'b']; (6,[3.;4.]),['c';'d']] 7
