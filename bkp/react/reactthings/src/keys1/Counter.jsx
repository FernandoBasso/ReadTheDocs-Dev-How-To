import React, { useState } from 'react';

export function Counter({ name }) {
  const [count, setCount] = useState(0);

  return (
    <section className='counter'>
      <h2>{name}</h2>

      <button
        onClick={() => setCount(c => c - 1)}
      >
        -
      </button>

      <tt>{count}</tt>

      <button
        onClick={() => setCount(c => c + 1)}
      >
        +
      </button>
    </section>
  )
};

export function App() {
  const [isAayla, setIsAayla] = useState(true);

  return (
    <section>
      {
        isAayla
          ? <Counter key='aayla' name='Aayla' />
          : <Counter key='ahsoka' name='Ahsoka' />
      }

      <p>
        <button
          onClick={() => setIsAayla(val => !val)}
        >
          {`Switch to ${isAayla ? 'Ahsoka' : 'Aayla'}`}
        </button>
      </p>
    </section>
  );
};
