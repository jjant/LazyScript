const GET_LINE = Symbol('GET_LINE');
const PUT_LINE = Symbol('PUT_LINE');
const RETURN = Symbol('RETURN');

const identity = x => x;
const compose = g => f => x => g(f(x));
const unit = undefined;

const IO = {
	map(f) {
		return Object.assign(
			Object.create(IO),
			flatMap(this)(compose(returnIO)(f))
		);
	},
	flatMap(f) {
		return Object.assign(Object.create(IO), flatMap(this)(f));
	},
	then(io2) {
		return Object.assign(Object.create(IO), flatMap(this)(_ => io2));
	}
};

//    returnIO :: a -> IO a
const returnIO = a =>
	Object.assign(Object.create(IO), {
		type: RETURN,
		val: a,
		doMap: identity,
		andThen: returnIO
	});

//   getLine :: IO String
const getLine = Object.assign(Object.create(IO), {
	type: GET_LINE,
	doMap: identity,
	andThen: returnIO
});

//    putLine :: String -> IO ()
const putLine = s =>
	Object.assign(Object.create(IO), {
		type: PUT_LINE,
		val: s,
		doMap: identity,
		andThen: _ => returnIO(unit)
	});

//    map :: IO a -> (a -> b) -> IO b
const map = io => f => ({
	...io,
	doMap: compose(f)(io.doMap)
});

//    flatMap :: IO a -> (a -> IO b) -> IO b
const flatMap = io => f => ({
	...io,
	// andThen: compose(f)(io.andThen)
	andThen: x => {
		const ioAct = io.andThen(x);

		return { ...ioAct, andThen: compose(f)(ioAct.andThen) };
	}
});

//   runIO :: IO a -> a
const runIO = io => {
	if (io.type === RETURN && io.andThen === returnIO) {
		return io.doMap(io.val);
	}

	const val = execIO(io);

	const mapped = io.doMap(val);

	const nextAction = io.andThen(mapped);

	return runIO(nextAction);
};

const execIO = io => {
	switch (io.type) {
		case GET_LINE:
			return prompt();
		case PUT_LINE:
			console.log(io.val);
			return;
		case RETURN:
			return io.val;
	}
};

// const main = flatMap(putLine('Hola'))(_ => putLine('Chau'));

const getLowerCaseLine = map(getLine)(x => x.toLowerCase());

const putFive = flatMap(returnIO(5))(putLine);

// const b = runIO(getLowerCaseLine);

const main = getLine.flatMap(a => getLine.flatMap(b => returnIO([a, b])));

c = returnIO(5)
	.flatMap(putLine)
	.then(putLine(12));
