const acorn = require('acorn');
const recast = require('recast');
const fs = require('fs');

const source = fs.readFileSync('./index.js', 'utf8');

const ast = recast.parse(source);

const namedTypes = recast.types.namedTypes;
const b = recast.types.builders;

ast.program.body.map(node => {
	// if (node.type === 'FunctionExpression') console.log(node);
	// console.log(node.type);
});

const runNode = ast.program.body[7];

ast.program.body[7] = b.variableDeclaration('var', [
	b.variableDeclarator('Hi', 9)
]);

const output = recast.print(ast).code;

fs.writeFileSync('./out.js', output, 'utf8');
