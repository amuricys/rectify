// D2: Client-side equation parser for custom dynamical systems
//
// Syntax:
//   dx/dt = sigma * (y - x)
//   dy/dt = x * (rho - z) - y
//   dz/dt = x * y - beta * z
//
//   parameters: sigma = 10.0, rho = 28.0, beta = 2.667
//   initial: x = 1.0, y = 1.0, z = 1.0

export interface ParsedSystem {
	name: string;
	stateVars: string[];
	equations: string[]; // RHS expressions
	parameters: { name: string; default: number }[];
	inputs: string[];
	initialState: number[];
}

export interface ParseError {
	error: string;
	line?: number;
}

const BUILTINS = new Set([
	'sin',
	'cos',
	'tan',
	'exp',
	'log',
	'sqrt',
	'abs',
	'tanh',
	'min',
	'max',
	'pi',
	't'
]);

// Valid identifier: starts with letter or underscore, contains alphanumeric or underscore
const IDENT_RE = /^[a-zA-Z_][a-zA-Z0-9_]*$/;

export function parseEquations(text: string, name: string): ParsedSystem | ParseError {
	const lines = text.split('\n').map((l) => l.trim());
	const stateVars: string[] = [];
	const equations: string[] = [];
	const parameters: { name: string; default: number }[] = [];
	const inputs: string[] = [];
	const initialState: number[] = [];

	// Track declared names
	const paramNames = new Set<string>();
	const inputNames = new Set<string>();
	const stateNames = new Set<string>();
	const initialValues = new Map<string, number>();

	for (let i = 0; i < lines.length; i++) {
		const line = lines[i];

		// Skip empty lines and comments
		if (line === '' || line.startsWith('#')) continue;

		// Check for parameter declaration
		if (line.startsWith('parameters:') || line.startsWith('params:')) {
			const paramStr = line.replace(/^(parameters|params):/, '').trim();
			if (!paramStr) continue;

			const paramParts = paramStr.split(',');
			for (const part of paramParts) {
				const match = part.trim().match(/^([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*([+-]?\d+\.?\d*(?:e[+-]?\d+)?)$/i);
				if (!match) {
					return { error: `Invalid parameter declaration: "${part.trim()}"`, line: i + 1 };
				}
				const pName = match[1];
				const pVal = parseFloat(match[2]);
				if (isNaN(pVal)) {
					return { error: `Invalid parameter value for "${pName}"`, line: i + 1 };
				}
				if (paramNames.has(pName)) {
					return { error: `Duplicate parameter: "${pName}"`, line: i + 1 };
				}
				paramNames.add(pName);
				parameters.push({ name: pName, default: pVal });
			}
			continue;
		}

		// Check for input declaration
		if (line.startsWith('input:') || line.startsWith('inputs:')) {
			const inputStr = line.replace(/^inputs?:/, '').trim();
			if (!inputStr) continue;

			const inputParts = inputStr.split(',');
			for (const part of inputParts) {
				const iName = part.trim();
				if (!IDENT_RE.test(iName)) {
					return { error: `Invalid input name: "${iName}"`, line: i + 1 };
				}
				inputNames.add(iName);
				inputs.push(iName);
			}
			continue;
		}

		// Check for initial condition declaration
		if (line.startsWith('initial:')) {
			const initStr = line.replace(/^initial:/, '').trim();
			if (!initStr) continue;

			const initParts = initStr.split(',');
			for (const part of initParts) {
				const match = part.trim().match(/^([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*([+-]?\d+\.?\d*(?:e[+-]?\d+)?)$/i);
				if (!match) {
					return { error: `Invalid initial condition: "${part.trim()}"`, line: i + 1 };
				}
				initialValues.set(match[1], parseFloat(match[2]));
			}
			continue;
		}

		// Check for equation: d<var>/dt = <expr>
		const eqMatch = line.match(/^d([a-zA-Z_][a-zA-Z0-9_]*)\/dt\s*=\s*(.+)$/);
		if (eqMatch) {
			const varName = eqMatch[1];
			const rhs = eqMatch[2].trim();

			if (stateNames.has(varName)) {
				return { error: `Duplicate state equation for "${varName}"`, line: i + 1 };
			}

			// Validate the RHS expression contains only allowed tokens
			const exprError = validateExpression(rhs, i + 1);
			if (exprError) return exprError;

			stateNames.add(varName);
			stateVars.push(varName);
			equations.push(rhs);
			continue;
		}

		return { error: `Unrecognized line: "${line}"`, line: i + 1 };
	}

	if (stateVars.length === 0) {
		return { error: 'No state equations found. Use "d<var>/dt = <expr>" syntax.' };
	}

	// Build initial state from declarations (order matches stateVars)
	for (const v of stateVars) {
		initialState.push(initialValues.get(v) ?? 0.0);
	}

	// Auto-detect parameters: symbols in equations that aren't state vars, builtins, or inputs
	const allUsedSymbols = new Set<string>();
	for (const eq of equations) {
		const tokens = eq.match(/[a-zA-Z_][a-zA-Z0-9_]*/g) || [];
		for (const tok of tokens) {
			allUsedSymbols.add(tok);
		}
	}

	for (const sym of allUsedSymbols) {
		if (stateNames.has(sym) || BUILTINS.has(sym) || inputNames.has(sym) || paramNames.has(sym)) {
			continue;
		}
		// Auto-declare as parameter with default 1.0
		paramNames.add(sym);
		parameters.push({ name: sym, default: 1.0 });
	}

	return {
		name: name || 'custom',
		stateVars,
		equations,
		parameters,
		inputs,
		initialState
	};
}

function validateExpression(expr: string, lineNum: number): ParseError | null {
	// Tokenize and check for disallowed constructs
	// Allow: numbers, identifiers, operators (+, -, *, /, ^, (, ), **, .), comma
	const remaining = expr
		.replace(/[0-9]+\.?[0-9]*(?:e[+-]?[0-9]+)?/gi, '') // numbers
		.replace(/[a-zA-Z_][a-zA-Z0-9_]*/g, '') // identifiers
		.replace(/[+\-*/^().,%\s]/g, '') // operators and whitespace
		.trim();

	if (remaining.length > 0) {
		return { error: `Invalid characters in expression: "${remaining}"`, line: lineNum };
	}

	// Check for balanced parentheses
	let depth = 0;
	for (const ch of expr) {
		if (ch === '(') depth++;
		if (ch === ')') depth--;
		if (depth < 0) return { error: 'Unbalanced parentheses', line: lineNum };
	}
	if (depth !== 0) return { error: 'Unbalanced parentheses', line: lineNum };

	return null;
}
