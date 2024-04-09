import {
  useContext,
  createContext,
  useState,
  useMemo,
} from 'react';

var log = console.log.bind(console);

var Context = createContext({ value: 0 });
var useValue = () => useContext(Context);

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
  var { value } = useValue();
  log('<Child /> render', value);

  return <p>Child Component!</p>
}

function Foo() {
  var { value } = useValue();
  log('<Foo />', value);
  return <Inner />;
}

function Inner() {
  log('<Inner />');
  return null;
}

export function App() {
  log('<App /> render');

  return (
    <Provider>
      <div className='app'>
        <h1>ex02</h1>
        <Child />
        <Foo />
      </div>
    </Provider>
  );
}
