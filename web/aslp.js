/**
 * Supporting code to enable ASLp-in-JS. Handles input/output.
 */

const get = query => {
  const result = document.querySelector(query);
  console.assert(result, "query returned null: " + query);
  return result;
};

// XXX: cannot use worker due to worker stack size being too small
// const worker = new Worker('worker.js');

const input = get('#op');
const output = get('#output');
const loading = get('#loading');


const OPCODE = 'opcode';
const BYTES = 'bytes';
let mode = OPCODE;

const flipEndian = s => {
  const chunked = s.match(/.{1,2}/g); // chunks of 2
  chunked.reverse();
  return chunked.join('');
};

// always returns BIG-endian string
const getOpcode = () => {
  const val = input.value.trim().replace(/^0x/, '').replace(/\s/g, '').padStart(8, '0');

  if (val.length > 8) throw Error('opcode too long: ' + val.length);
  if (mode == OPCODE) return val;
  return flipEndian(val);
}

const setOpcodeMode = newMode => {
  if (mode != newMode) {
    const op = getOpcode();
    mode = newMode;

    if (mode == OPCODE) {
      input.value = '0x' + op.toLowerCase();
    } else {
      input.value = flipEndian(op).toUpperCase().match(/.{1,2}/g).join(' ');
    }
  }
};

const onChangeDebug = el => {
  libASL_web.setDebugLevel(parseInt(el.value, 10));
};

const clearOutput = () => {
  output.innerHTML = '';
};

const submit = () => {
  clearOutput();
  loading.classList.remove('invisible');
  // defer computation so loading indicator shows.
  setTimeout(() => {
    try {
      libASL_web.dis('0x' + getOpcode());
    } finally {
      loading.classList.add('invisible');
    }
  }, 30);
};


const write = (isError) => s => {
  const span = document.createElement('span');
  const data = new Uint8Array(s.length);
  for (let i = 0; i < s.length; i++) {
    data[i] = s.charCodeAt(i);
  }
  span.textContent = new TextDecoder('utf-8').decode(data);
  if (isError)
    span.classList.add('stderr');
  output.appendChild(span);
  return 0;
};

libASL_web.init(write(false), write(true));
libASL_web.setDebugLevel(0);
