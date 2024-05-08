import { Primes } from './sieve'

const N: HTMLInputElement = document.querySelector("input#N")!;
const A: HTMLInputElement = document.querySelector("input#A")!;

N.addEventListener('input', () => update());
A.addEventListener('input', () => update());

function inputBigInt(e: HTMLInputElement) {
    return BigInt([...e.value.toString()]
        .filter(c => !isNaN(+c))
        .join(''));
}

function pow(a: bigint, k: bigint) {
    let r = 1n;
    while(k > 0n) {
        if(k&1n) r *= a;
        k >>= 1n, a *= a;
    }
    return r;
}

function pow_mod(a: bigint, k: bigint, m: bigint) {
    let r = 1n;
    while(k > 0n) {
        if(k&1n) r = (r*a) % m;
        k >>= 1n, a = (a*a) % m;
    }
    return r;
}

function random() {
    return BigInt(Math.floor(Math.random() * Number.MAX_SAFE_INTEGER));
}

function trailing_zeros(n: bigint) {
    let r = 0n;
    while(!(n&1n)) n >>= 1n, r += 1n;
    return r;
}

function miller_rabin(n: bigint) {
    if(n < 2n) return 0;
	if(n == 2n || n == 3n) return 1;
    if((n&1n) == 0n) return 0;
	const s = trailing_zeros(n-1n), d = (n-1n) >> s;
	for(let i = 0; i < 50; i += 1) {
		let a = 2n + random() % (n-3n),
			x = pow_mod(a, d, n);
		for(let r = 0; r < s; r += 1) {
			if(x == 1n || x == n - 1n) break;
			x = (x*x) % n;
		}
		if(x != 1n && x != n - 1n) return 0;
	}
	return 1;
}

const primes = new Primes();

export function update() {
    const a = inputBigInt(A);
    const n = inputBigInt(N);
    document
        .querySelector("result")!
        .innerHTML =
    [
        `a^n = ${pow(a, n).toString()}`,
        `a is prime? ${miller_rabin(a)}`,
        `factors of a = ${a < 10000000000n ? primes.factors(Number(a)).join(', ') : 'too large'}`,
    ].join('<br>');
}

update();