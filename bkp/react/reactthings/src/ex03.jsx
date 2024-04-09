import {
  useContext,
  createContext,
  useState,
  useMemo,
} from 'react';

var log = console.log.bind(console);

var Context = createContext({ value: 0 });
var useValue = () => useContext(Context);

function useSth() {
  useValue();
}

function Provider({ children }) {
  var [state, setState] = useState(1);

  var value = useMemo(() => {
    return { value: state };
  }, [state]);

  return (
    <Context.Provider value={value}>
      <button
        onClick={() => setState(n => n + 1)}
      >
        Click!
      </button>

      {children}
    </Context.Provider>
  );
}

function Child() {
  log('<Child /> render');
  return <p>Child Component!</p>
}

function Foo() {
  useSth();
  log('<Foo /> render');
  return <Inner />
}

function Inner() {
  log('<Inner /> render');
  return (
    <p>Inner</p>
  );
}

export function App() {
  log('<App /> render');

  return (
    <Provider>
      <div className='app'>
        <h1>ex03</h1>
        <Child />
        <Foo />
      </div>
    </Provider>
  );
}
