import { useState, useEffect } from 'react';

const log: Console["log"] = console.log.bind(console);

function Parent() {
  var [state, setState] = useState(0);

  function handleClick() {
    return setState(state + 1);;
  }

  //
  // Move this component outside and does not
  // re-renders and re-mounts.
  //
  function Inner() {
    log('<Inner /> re-renders');

    useEffect(() => {
      log('<Inner /> re-mounts');
    }, []);

    return <h2>Inner Anti-Pattern</h2>;
  }

  return (
    <section className="parent">
      <button onClick={handleClick}>Click me!</button>
      <Inner />
    </section>
  );
}

function App() {
  return (
    <Parent />
  );
}

export { App };
