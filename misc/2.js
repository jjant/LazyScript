const makeThunk = func => ({
	evaluated: false,
	result: undefined,
	func
});

const evalThunk = thunk => {
	if (thunk.evaluated) return result;

	thunk.evaluated = true;
	return thunk.func();
};

const if_ = (pred, then, else_) =>
	evalThunk(pred) ? evalThunk(then) : evalThunk(else_);

if_(
	makeThunk(() => 1 == 2),
	makeThunk(() => console.log('Absurd!')),
	makeThunk(() => console.log('1 != 2'))
);
