const a = () => [1 , () => { return [2 , () => { return [3 , () => { return a(); }]; }]; }]
const access = (n, ari) => (n === 0 ? ari()[0] : access(n - 1, ari()[1]));
console.log(access(10, a).toString() + ' === 2');
console.log(access(0, a).toString() + ' === 1');
