const GET_LINE = Symbol('GET_LINE');
const PUT_LINE = Symbol('PUT_LINE');
const RETURN = Symbol('RETURN');
const NOTHING = 'NOTHING!!';
const unit = undefined;

// Primitive IO actions, these are pure functions.
const returnIO = a => ({
	type: RETURN,
	flatMap: f => f(a),

	// Derived operations
	map(f) {
		return this.flatMap(x => returnIO(f(x)));
	},
	// Sequencing operator, (>>).
	then(io) {
		return this.flatMap(_ => io);
	}
});

const putLine = s => ({
	type: PUT_LINE,
	flatMap(f) {
		return {
			...this,
			next: this.next === NOTHING ? f(undefined) : this.next.flatMap(f)
		};
	},

	// Derived operations
	map(f) {
		return this.flatMap(x => returnIO(f(x)));
	},
	// Sequencing operator, (>>).
	then(io) {
		return this.flatMap(_ => io);
	},

	// Internal data. NEVER TOUCH.
	next: NOTHING,
	toPut: s
});

const getLine = {
	type: GET_LINE,
	// Bind, chain, (>>=)
	flatMap(f) {
		return this.whatToDoWithLineRead === NOTHING
			? { ...this, whatToDoWithLineRead: f }
			: {
					...this,
					whatToDoWithLineRead: s => this.whatToDoWithLineRead(s).flatMap(f)
				};
	},

	// Derived operations
	map(f) {
		return this.flatMap(x => returnIO(f(x)));
	},
	// Sequencing operator, (>>).
	then(io) {
		return this.flatMap(_ => io);
	},

	// Internal data. NEVER TOUCH.
	whatToDoWithLineRead: NOTHING
};

// Impure functions. You should never have to use this in your code.

const runIO = io => {
	switch (io.type) {
		case RETURN: // Do nothing
			return;

		case GET_LINE:
			const lineRead = execGetLine(io);

			return runIO(io.whatToDoWithLineRead(lineRead));

		case PUT_LINE:
			execPutLine(io);

			return runIO(io.next);
	}
};

const execGetLine = _ => {
	const val = prompt();

	return val;
};

const execPutLine = io => {
	console.log(io.toPut);

	return unit;
};

// Example programs

const main = putLine("What's your name?")
	.then(getLine)
	.map(name => 'Welcome, ' + name + '!')
	.flatMap(putLine);

// Actually executing programs, this will be done automatically for you in the future.

// runIO :: IO () -> IO ()
runIO(main);
