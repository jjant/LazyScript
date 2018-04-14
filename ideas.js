// const map = f => [] => [];
//       map = f => ([x, ...xs]) => [f(x), ...map(f)(xs)]);

const a = [1, ...a];

const isEmpty = xs => xs.length === 0;
const first = xs => xs[0];

const take = n => xs => {
	const [x, ...rest] = xs;

	return n == 0 || isEmpty(xs) ? [] : [x, ...take(n - 1)(rest)];
};

a.forEach(() => console.log('Hi'));

// Compiled...

const a = [1, ...a];
const a_ = () => [1, ...a_()]; // NO!

const a__ = n => [...(n > 0 ? a__(n - 1) : []), 1]; // NO!

const b_ = {
	list: []
	// generate:
};

const map_$_1 = f => xs_$ => [];
const map_$_2 = f => ([x_$, ...xs_$]) => [f(x), ...map_$_2(f)(xs)];

map_$ = f => xs_$ => {
	if (xs_$ === []) return map_$_1(f)(xs_$);
	else return map_$_2(f)(xs_$);
};
