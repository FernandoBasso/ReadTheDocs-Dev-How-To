import React, { useState, useEffect } from 'react';

import appStyles from './App.module.scss';
import classes from './Form-dark.module.scss';

// const useStyles = async (name) => {
//   let styl;
//   import(name).then(css => styl = css);
//
//   return styl;
// }

function Form({ title, css }) {
  const [render, setRender] = useState(false);

  useEffect(() => {
    if (render) return;

    const timeoutId = setTimeout(function handleTimeout() {
      setRender(true);
    }, 78);

    return function cleanUp() {
      return clearTimeout(timeoutId);
    };
  }, [render]);

  if (!render) return null;

  return (
    <div className={classes.container}>
      <h2 className={css.title} id="promos-section">{ title }</h2>
      <label className={classes.label} htmlFor="promos">Promo Input:</label>
      <input id="promos"type="text" placeholder="Promo code" />
    </div>
  );
}

function App() {
  const ps = new Array(1).fill(
      'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'
  );

  return (
    <div className="app">
      {ps.map((t, i) => <p key={i}>{t}</p>)}

      <Form css={appStyles} title='The Promo!' />

      {ps.map((t, i) => <p key={i}>{t}</p>)}
    </div>
  );
}

export { App };
