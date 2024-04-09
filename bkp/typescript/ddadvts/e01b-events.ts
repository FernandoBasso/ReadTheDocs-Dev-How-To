export const MODNAME = 'e01b-events';

type Events = {
  add: string;
  remove: string;
  move: string;

  //
  // Uncomment `hide`, and `userActions` below will complain
  // that the types are no longer matching because `hide` is
  // not present.
  //
  // hide: string;
};

type EventKeys = keyof Events;

type OnEvent = {
  // Mapped types!
  [Key in EventKeys]: () => unknown;
}

// 
// Note we don't have the “onAdd”, "onRemove" with the
// uppercase action any longer.
// 
// But at least, if we add another event name or action, we'll
// immediately get compiler messages informing us of the
// mismatches.
// 
const userActions: OnEvent = {
  add: () => null,
  remove: () => null,
  move: () => null,
  // otherEvent: () => null,
};
