(function qanda () {
  const TEXT_SHOW = '(show answer)';
  const TEXT_HIDE = '(hide answer)';
  const questions = document.querySelectorAll('.qanda');

  /**
   * Adds a toggle (show/hide) answer button on first text element.
   *
   * @param {HTMLElement} parentElem The parent element of the
   *   entire QandA block.
   *
   * @param {HTMLElement} parentElem
   */
  function addToggler(parentElem) {
    const toggler = document.createElement('button');
    toggler.textContent = TEXT_SHOW;

    parentElem
      .querySelector('.question')
      .lastElementChild
      .insertAdjacentElement('beforeend', toggler);
  }

  /**
   * Toggles the text according to the shown/hidden state.
   *
   * @param {HTMLElement} parentElem The parent element of the
   *   entire QandA block.
   */
  function toggleText(parentElem) {
    const button = parentElem.querySelector('button');
    button.textContent = button.textContent === TEXT_SHOW
      ? TEXT_HIDE
      : TEXT_SHOW;
  }

  [...questions].forEach((q) => {
    console.log('===', q);
    addToggler(q);

    q.querySelector('button').addEventListener('click', function questionClicked () {
      q.classList.toggle('shown');
      toggleText(q);
    }, false);
  });
}());
