import { useState } from 'react';

var log = console.log.bind(console);

//
// When state changes, component re-renders, and all its children,
// even if their props DO NOT change.
//
// It is not only prop changes that cause re-renders.
//

var cntChild = 0;

var cntSubChild = 0;

//
// If any ancestor parent re-renders, the deeply nested child
// components re-render too!
//
function SubChild() {
  log('<SubChild />', ++cntSubChild);
  return <div />;
}

//
// Not receiving any props, but when the parent <App />
// state changes, <Child /> will re-render too! And also
//
function Child() {
  const [cnt, setCnt] = useState(0);
  log('<Child /> render', ++cntChild);

  return (
    <div>
      <p>Child Component!</p>
      <button onClick={() => setCnt(n => 1 + n)}>
        Child click!
      </button>

      <SubChild />
    </div>
  );
}

var cntApp = 0;

export function App() {
  const [count, setCount] = useState(0);
  log('<App /> render', ++cntApp);

  return (
    <div className='app'>
      <h1>ex01</h1>

      <Child />

      <button onClick={() => setCount(n => n + 1)}>
        Click me once!
      </button>
    </div>
  );
}
