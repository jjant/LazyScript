const if_ = (pred, then, else_) => (pred ? then : else_);

const id = x => ({
	map: f => id(f(x)),
	chain: f => f(x)
});

const square = x => x * x;

const map = f => xs => xs.map(f);

// Problem: parse query params

const exampleData = 'api=1&query=IngenieroButty';

// String -> String -> [String]
const split = separator => string => string.split(separator);

// String -> [[String]]
const stringToKeyValuePairs = string =>
	id(string)
		.map(split('&'))
		.chain(map(split('=')));

const run = () => {
	if_(true, console.log('True'), console.log('False'));
	// console.log(stringToKeyValuePairs(exampleData));
};

run();
