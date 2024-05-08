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

function update() {
    const a = inputBigInt(A);
    const n = inputBigInt(N);
    document
        .querySelector("result")!
        .textContent = pow(a, n).toString();
}

update();