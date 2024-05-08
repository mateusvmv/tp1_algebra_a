export class Primes {
    list: number[] = [2];
    low = 2;
    lim = 2;
    b = new Uint32Array(70000/8);
    next_seg() {
        if(this.lim * this.lim < this.low + this.lim) this.lim *= 2;
        this.b.fill(~0);
        for(let i = 1; i < this.list.length; i += 1) {
            let p = this.list[i];
            if(p > this.lim) break;
            let n = (this.low + p) - ((this.low + p) % (p*2));
            let j = n + p - this.low;
            while(j < this.lim) this.b[j/2] = 0, j += p+p;
        }
        for(let i = 0 ; i < this.lim; i += 2)
            if(this.b[i/2]) this.list.push(this.low + i + 1);
        this.low += this.lim;
    }
    back() {
        return this.list[this.list.length - 1];
    }
    advance_sqrt(to: number) {
		while(to > this.back() * this.back()) this.next_seg();
	}
	advance(to: number) {
		while(to > this.back()) this.next_seg();
	}
	has(n: number) {
        if(n < 2) return false;
        this.advance_sqrt(n);
		for(let p of this.list) {
			if(p * p > n) break;
			if(n%p == 0) return 0;
		}
		return 1;
    }
    factors(n: number) {
		const primes: number[] = [];
		if(n < 2) return primes;
		this.advance_sqrt(n);
		for(let prime of this.list) {
			if(prime * prime > n) {
				primes.push(n);
				break;
			}
			while(n % prime == 0) {
				primes.push(prime);
				n /= prime;
			}
			if(n == 1) break;
		}
		return primes;
	}
};