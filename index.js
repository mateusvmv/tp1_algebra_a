define("sieve", ["require", "exports"], function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Primes = void 0;
    class Primes {
        constructor() {
            this.list = [2];
            this.low = 2;
            this.lim = 2;
            this.b = new Uint32Array(70000 / 8);
        }
        next_seg() {
            if (this.lim * this.lim < this.low + this.lim)
                this.lim *= 2;
            this.b.fill(~0);
            for (let i = 1; i < this.list.length; i += 1) {
                let p = this.list[i];
                if (p > this.lim)
                    break;
                let n = (this.low + p) - ((this.low + p) % (p * 2));
                let j = n + p - this.low;
                while (j < this.lim)
                    this.b[j / 2] = 0, j += p + p;
            }
            for (let i = 0; i < this.lim; i += 2)
                if (this.b[i / 2])
                    this.list.push(this.low + i + 1);
            this.low += this.lim;
        }
        back() {
            return this.list[this.list.length - 1];
        }
        advance_sqrt(to) {
            while (to > this.back() * this.back())
                this.next_seg();
        }
        advance(to) {
            while (to > this.back())
                this.next_seg();
        }
        has(n) {
            if (n < 2)
                return false;
            this.advance_sqrt(n);
            for (let p of this.list) {
                if (p * p > n)
                    break;
                if (n % p == 0)
                    return 0;
            }
            return 1;
        }
        factors(n) {
            const primes = [];
            if (n < 2)
                return primes;
            this.advance_sqrt(n);
            for (let prime of this.list) {
                if (prime * prime > n) {
                    primes.push(n);
                    break;
                }
                while (n % prime == 0) {
                    primes.push(prime);
                    n /= prime;
                }
                if (n == 1)
                    break;
            }
            return primes;
        }
    }
    exports.Primes = Primes;
    ;
});
define("index", ["require", "exports", "sieve"], function (require, exports, sieve_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.update = void 0;
    const N = document.querySelector("input#N");
    const A = document.querySelector("input#A");
    N.addEventListener('input', () => update());
    A.addEventListener('input', () => update());
    function inputBigInt(e) {
        return BigInt([...e.value.toString()]
            .filter(c => !isNaN(+c))
            .join(''));
    }
    function pow(a, k) {
        let r = 1n;
        while (k > 0n) {
            if (k & 1n)
                r *= a;
            k >>= 1n, a *= a;
        }
        return r;
    }
    function pow_mod(a, k, m) {
        let r = 1n;
        while (k > 0n) {
            if (k & 1n)
                r = (r * a) % m;
            k >>= 1n, a = (a * a) % m;
        }
        return r;
    }
    function random() {
        return BigInt(Math.floor(Math.random() * Number.MAX_SAFE_INTEGER));
    }
    function trailing_zeros(n) {
        let r = 0n;
        while (!(n & 1n))
            n >>= 1n, r += 1n;
        return r;
    }
    function miller_rabin(n) {
        if (n < 2n)
            return 0;
        if (n == 2n || n == 3n)
            return 1;
        if ((n & 1n) == 0n)
            return 0;
        const s = trailing_zeros(n - 1n), d = (n - 1n) >> s;
        for (let i = 0; i < 50; i += 1) {
            let a = 2n + random() % (n - 3n), x = pow_mod(a, d, n);
            for (let r = 0; r < s; r += 1) {
                if (x == 1n || x == n - 1n)
                    break;
                x = (x * x) % n;
            }
            if (x != 1n && x != n - 1n)
                return 0;
        }
        return 1;
    }
    const primes = new sieve_1.Primes();
    function update() {
        const a = inputBigInt(A);
        const n = inputBigInt(N);
        document
            .querySelector("result")
            .innerHTML =
            [
                `a^n = ${pow(a, n).toString()}`,
                `a is prime? ${miller_rabin(a)}`,
                `factors of a = ${a < 10000000000n ? primes.factors(Number(a)).join(', ') : 'too large'}`,
            ].join('<br>');
    }
    exports.update = update;
    update();
});
