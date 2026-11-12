'use strict';

/* =====================================================================
   BAGIAN 1 - MESIN PERHITUNGAN (murni logika, tanpa eval())
   Tokenizer -> Recursive Descent Parser -> Hasil
   ===================================================================== */

class CalcError extends Error {}

const FUNCS = ['sin', 'cos', 'tan', 'log', 'ln'];

/** Memecah teks ekspresi menjadi token. */
function tokenize(src) {
  const tokens = [];
  let i = 0;
  while (i < src.length) {
    const c = src[i];
    if (c === ' ') { i++; continue; }

    if (/[0-9.]/.test(c)) {
      let j = i, dots = 0;
      while (j < src.length && /[0-9.]/.test(src[j])) {
        if (src[j] === '.') dots++;
        j++;
      }
      const s = src.slice(i, j);
      if (dots > 1 || s === '.') throw new CalcError('Format angka tidak valid');
      tokens.push({ t: 'num', v: parseFloat(s) });
      i = j;
      continue;
    }

    const fn = FUNCS.find(f => src.startsWith(f, i));
    if (fn) { tokens.push({ t: 'fn', v: fn }); i += fn.length; continue; }

    if (c === '√') { tokens.push({ t: 'fn', v: 'sqrt' }); i++; continue; }
    if (c === 'π') { tokens.push({ t: 'num', v: Math.PI }); i++; continue; }
    if (c === 'e') { tokens.push({ t: 'num', v: Math.E }); i++; continue; }
    if ('+-×÷^%'.includes(c)) { tokens.push({ t: 'op', v: c }); i++; continue; }
    if (c === '(' || c === ')') { tokens.push({ t: 'par', v: c }); i++; continue; }
    if (c === '!') { tokens.push({ t: 'fact' }); i++; continue; }

    throw new CalcError('Karakter tidak dikenal: ' + c);
  }
  return tokens;
}

/** Menghilangkan noise floating point kecil (mis. sin(180°) = 1.2e-16). */
function cleanTrig(v) {
  return Math.round(v * 1e12) / 1e12 + 0; // "+ 0" mengubah -0 menjadi 0
}

function factorial(n) {
  if (!Number.isInteger(n) || n < 0) {
    throw new CalcError('Faktorial hanya untuk bilangan bulat ≥ 0');
  }
  if (n > 170) throw new CalcError('Hasil terlalu besar');
  let r = 1;
  for (let i = 2; i <= n; i++) r *= i;
  return r;
}

function applyFunction(name, x, mode) {
  const toRad = a => (mode === 'DEG' ? (a * Math.PI) / 180 : a);
  switch (name) {
    case 'sin': return cleanTrig(Math.sin(toRad(x)));
    case 'cos': return cleanTrig(Math.cos(toRad(x)));
    case 'tan': {
      const undefinedTan = mode === 'DEG'
        ? ((x % 180) + 180) % 180 === 90
        : Math.abs(Math.cos(x)) < 1e-12;
      if (undefinedTan) throw new CalcError('tan tidak terdefinisi untuk sudut ini');
      return cleanTrig(Math.tan(toRad(x)));
    }
    case 'log':
      if (x <= 0) throw new CalcError('log hanya untuk bilangan > 0');
      return Math.log10(x);
    case 'ln':
      if (x <= 0) throw new CalcError('ln hanya untuk bilangan > 0');
      return Math.log(x);
    case 'sqrt':
      if (x < 0) throw new CalcError('Akar dari bilangan negatif tidak valid');
      return Math.sqrt(x);
    default:
      throw new CalcError('Fungsi tidak dikenal');
  }
}

/**
 * Menghitung ekspresi string. Prioritas (tinggi -> rendah):
 * kurung/fungsi/! > ^ > unary(-) > × ÷ % > + -
 */
function evaluate(src, mode = 'DEG') {
  // Tutup otomatis kurung yang belum tertutup
  const open = (src.match(/\(/g) || []).length;
  const close = (src.match(/\)/g) || []).length;
  if (close > open) throw new CalcError('Tanda kurung tidak seimbang');
  src += ')'.repeat(open - close);

  const tokens = tokenize(src);
  let p = 0;
  const peek = () => tokens[p];
  const isOp = (t, list) => t && t.t === 'op' && list.includes(t.v);
  const isPar = (t, ch) => t && t.t === 'par' && t.v === ch;
  const startsOperand = t => t && (t.t === 'num' || t.t === 'fn' || isPar(t, '('));

  function parseExpr() {            // + -
    let left = parseTerm();
    while (isOp(peek(), ['+', '-'])) {
      const op = tokens[p++].v;
      const right = parseTerm();
      left = op === '+' ? left + right : left - right;
    }
    return left;
  }

  function parseTerm() {            // × ÷ %  (+ perkalian implisit)
    let left = parseUnary();
    for (;;) {
      const t = peek();
      if (isOp(t, ['×', '÷', '%'])) {
        p++;
        const right = parseUnary();
        if (t.v === '×') {
          left *= right;
        } else if (t.v === '÷') {
          if (right === 0) throw new CalcError('Tidak bisa membagi dengan nol');
          left /= right;
        } else {
          if (right === 0) throw new CalcError('Modulo dengan nol tidak terdefinisi');
          left %= right;
        }
      } else if (startsOperand(t)) {
        left *= parseUnary();       // contoh: 2π, 3(4+1)
      } else {
        break;
      }
    }
    return left;
  }

  function parseUnary() {           // -x, +x
    if (isOp(peek(), ['-'])) { p++; return -parseUnary(); }
    if (isOp(peek(), ['+'])) { p++; return parseUnary(); }
    return parsePower();
  }

  function parsePower() {           // ^ (asosiatif kanan)
    const base = parsePostfix();
    if (isOp(peek(), ['^'])) {
      p++;
      const exp = parseUnary();
      if (base === 0 && exp < 0) throw new CalcError('Tidak bisa membagi dengan nol');
      const r = Math.pow(base, exp);
      if (Number.isNaN(r)) throw new CalcError('Hasil tidak valid (bukan bilangan real)');
      return r;
    }
    return base;
  }

  function parsePostfix() {         // faktorial
    let v = parsePrimary();
    while (peek() && peek().t === 'fact') { p++; v = factorial(v); }
    return v;
  }

  function parseParen() {
    p++; // lewati "("
    const v = parseExpr();
    if (!isPar(peek(), ')')) throw new CalcError('Tanda kurung tidak seimbang');
    p++;
    return v;
  }

  function parsePrimary() {
    const t = tokens[p];
    if (!t) throw new CalcError('Ekspresi belum lengkap');
    if (t.t === 'num') { p++; return t.v; }
    if (isPar(t, '(')) return parseParen();
    if (t.t === 'fn') {
      p++;
      let arg;
      if (isPar(peek(), '(')) arg = parseParen();
      else if (t.v === 'sqrt') arg = parsePostfix();     // contoh: √25
      else throw new CalcError('Fungsi membutuhkan tanda kurung');
      return applyFunction(t.v, arg, mode);
    }
    throw new CalcError('Ekspresi tidak valid');
  }

  if (tokens.length === 0) throw new CalcError('Ekspresi kosong');
  const value = parseExpr();
  if (p < tokens.length) throw new CalcError('Ekspresi tidak valid');
  if (!Number.isFinite(value)) throw new CalcError('Hasil tidak terdefinisi atau terlalu besar');

  // Bulatkan ke 12 digit signifikan: 0.1 + 0.2 -> 0.3
  const rounded = parseFloat(value.toPrecision(12));
  return rounded === 0 ? 0 : rounded;
}

/** Teks untuk ditampilkan (notasi ilmiah untuk angka sangat besar/kecil). */
function formatResult(v) {
  const a = Math.abs(v);
  if (a !== 0 && (a >= 1e15 || a < 1e-6)) {
    return v.toExponential(11).replace(/\.?0+e/, 'e');
  }
  return String(v);
}

/** Teks angka biasa (tanpa notasi 'e') agar bisa dipakai lagi di ekspresi. */
function plainString(v) {
  const s = String(v);
  if (!/e/i.test(s)) return s;
  return v.toLocaleString('en-US', { useGrouping: false, maximumFractionDigits: 20 });
}

if (typeof module !== 'undefined' && module.exports) {
  module.exports = { evaluate, formatResult, plainString, CalcError };
}

/* =====================================================================
   BAGIAN 2 - ANTARMUKA (DOM, tombol, keyboard)
   ===================================================================== */
if (typeof document !== 'undefined') {
  document.addEventListener('DOMContentLoaded', () => {
    /* ---------- Navigasi ---------- */
    const navToggle = document.getElementById('navToggle');
    const navLinks = document.getElementById('navLinks');
    navToggle.addEventListener('click', () => {
      const open = navLinks.classList.toggle('open');
      navToggle.setAttribute('aria-expanded', String(open));
    });
    navLinks.querySelectorAll('a').forEach(a =>
      a.addEventListener('click', () => {
        navLinks.classList.remove('open');
        navToggle.setAttribute('aria-expanded', 'false');
      })
    );
    document.getElementById('year').textContent = new Date().getFullYear();

    const sections = document.querySelectorAll('main section[id]');
    if ('IntersectionObserver' in window) {
      const io = new IntersectionObserver(entries => {
        entries.forEach(e => {
          if (e.isIntersecting) {
            navLinks.querySelectorAll('a').forEach(a =>
              a.classList.toggle('active', a.getAttribute('href') === '#' + e.target.id)
            );
          }
        });
      }, { rootMargin: '-40% 0px -55% 0px' });
      sections.forEach(s => io.observe(s));
    }

    /* ---------- Kalkulator ---------- */
    const mainEl = document.getElementById('mainLine');
    const exprEl = document.getElementById('exprLine');
    const errorEl = document.getElementById('errorLine');
    const badgeEl = document.getElementById('modeBadge');
    const modeBtn = document.getElementById('modeBtn');

    const OPS = '+-×÷^%';
    let expr = '';
    let angleMode = 'DEG';
    let justEvaluated = false;
    let histText = '';
    let shownResult = '';
    let errorMsg = '';

    const endsWithOperand = () => /[0-9.)!πe]$/.test(expr);
    const isOperator = ch => OPS.includes(ch);

    function resetIfEvaluated() {
      if (justEvaluated) { expr = ''; justEvaluated = false; }
    }

    function inputDigit(d) {
      resetIfEvaluated();
      if (/[)!πe]$/.test(expr)) expr += '×';
      const m = expr.match(/(\d*\.?\d*)$/);
      if (m && m[1] === '0' && d !== '.') expr = expr.slice(0, -1);
      expr += d;
    }

    function inputDot() {
      resetIfEvaluated();
      if (/[)!πe]$/.test(expr)) expr += '×';
      const m = expr.match(/(\d*\.?\d*)$/);
      const cur = m ? m[1] : '';
      if (cur.includes('.')) return;
      if (cur === '') expr += '0';
      expr += '.';
    }

    function inputOperator(o) {
      justEvaluated = false;
      if (expr === '') { if (o === '-') expr = '-'; return; }
      const last = expr.slice(-1);
      if (last === '(') { if (o === '-') expr += '-'; return; }
      if (isOperator(last)) {
        if (o === '-' && last !== '-' && last !== '+') { expr += '-'; return; } // 2×-3
        expr = expr.replace(/[+\-×÷^%]+$/, '');
        if (expr === '') { if (o === '-') expr = '-'; return; }
        expr += o;
        return;
      }
      expr += o;
    }

    function inputFunction(f) {
      resetIfEvaluated();
      if (endsWithOperand()) expr += '×';
      expr += f + '(';
    }

    function inputConstant(c) {
      resetIfEvaluated();
      if (endsWithOperand()) expr += '×';
      expr += c;
    }

    function inputOpen() {
      resetIfEvaluated();
      if (endsWithOperand()) expr += '×';
      expr += '(';
    }

    function inputClose() {
      const open = (expr.match(/\(/g) || []).length;
      const close = (expr.match(/\)/g) || []).length;
      if (open > close && endsWithOperand()) { justEvaluated = false; expr += ')'; }
    }

    function inputSquare() {
      if (endsWithOperand()) { justEvaluated = false; expr += '^2'; }
    }

    function inputFactorial() {
      if (endsWithOperand()) { justEvaluated = false; expr += '!'; }
    }

    function backspace() {
      justEvaluated = false;
      expr = expr.replace(/(sin\(|cos\(|tan\(|log\(|ln\(|√\(|[\s\S])$/, '');
    }

    function calculate() {
      if (expr === '') return;
      try {
        const result = evaluate(expr, angleMode);
        histText = expr;
        shownResult = formatResult(result);
        expr = plainString(result);
        justEvaluated = true;
      } catch (err) {
        errorMsg = err instanceof CalcError ? err.message : 'Terjadi kesalahan perhitungan';
      }
    }

    function toggleMode() {
      angleMode = angleMode === 'DEG' ? 'RAD' : 'DEG';
      badgeEl.textContent = angleMode;
      modeBtn.textContent = angleMode === 'DEG' ? 'Deg' : 'Rad';
    }

    function render() {
      mainEl.textContent = justEvaluated ? shownResult : (expr || '0');
      mainEl.scrollLeft = mainEl.scrollWidth;
      errorEl.textContent = errorMsg;

      let small = '';
      if (justEvaluated) {
        small = histText + ' =';
      } else if (expr && /[^0-9.]/.test(expr.replace(/^-/, ''))) {
        try { small = '= ' + formatResult(evaluate(expr, angleMode)); } catch (e) { small = ''; }
      }
      exprEl.textContent = small;
      exprEl.scrollLeft = exprEl.scrollWidth;
    }

    function handle(action, value) {
      errorMsg = '';
      switch (action) {
        case 'digit':  inputDigit(value); break;
        case 'dot':    inputDot(); break;
        case 'op':     inputOperator(value); break;
        case 'fn':     inputFunction(value); break;
        case 'const':  inputConstant(value); break;
        case 'open':   inputOpen(); break;
        case 'close':  inputClose(); break;
        case 'square': inputSquare(); break;
        case 'fact':   inputFactorial(); break;
        case 'back':   backspace(); break;
        case 'clear':  expr = ''; histText = ''; justEvaluated = false; break;
        case 'mode':   toggleMode(); break;
        case 'equals': calculate(); break;
      }
      render();
    }

    document.querySelectorAll('.calc .key').forEach(btn => {
      btn.addEventListener('click', () => handle(btn.dataset.action, btn.dataset.value));
    });

    /* ---------- Keyboard ---------- */
    const KEY_MAP = {
      '+': ['op', '+'], '-': ['op', '-'], '*': ['op', '×'], 'x': ['op', '×'],
      '/': ['op', '÷'], '^': ['op', '^'], '%': ['op', '%'],
      '(': ['open'], ')': ['close'], '!': ['fact'],
      '.': ['dot'], ',': ['dot'],
      'Enter': ['equals'], '=': ['equals'],
      'Backspace': ['back'], 'Escape': ['clear'], 'Delete': ['clear'],
      's': ['fn', 'sin'], 'c': ['fn', 'cos'], 't': ['fn', 'tan'],
      'l': ['fn', 'log'], 'n': ['fn', 'ln'], 'r': ['fn', '√'],
      'p': ['const', 'π'], 'e': ['const', 'e']
    };

    document.addEventListener('keydown', ev => {
      if (ev.ctrlKey || ev.metaKey || ev.altKey) return;
      const t = ev.target;
      const inCalc = t === document.body || (t.closest && t.closest('.calc'));
      if (!inCalc) return;

      if (/^[0-9]$/.test(ev.key)) {
        ev.preventDefault();
        handle('digit', ev.key);
        return;
      }
      const m = KEY_MAP[ev.key];
      if (m) {
        ev.preventDefault();
        handle(m[0], m[1]);
      }
    });

    render();
  });
}
