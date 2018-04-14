const GET = 'IO/GET';
const PUT = 'IO/PUT';
const END = 'IO/END';

const GetConstructor = f => ({
	action: GET,
	next: f
});

const PutConstructor = (c, f) => ({
	action: PUT,
	char: c,
	next: f
});

const EndConstructor = () => ({
	action: END
});

const evalAction = ac => {
	switch (ac.action) {
		case GET: {
			const val = prompt();
			evalAction(ac.next(val));
		}
		case PUT: {
			console.log(ac.char);
			evalAction(ac.next);
		}
		case END:
			return;
		default:
			return;
	}
};

const IO = {
	get: {
		action: GET,
		bind: GetConstructor
	},
	put: (a, b) => PutConstructor(a, b),
	end: { ...EndConstructor() }
};

const main = IO.get.bind(a => IO.get.bind(b => IO.put(a, IO.put(b, IO.end))));

evalAction(main);
