export const MODNAME = 'e01c-events';

type Events = {
  add: string;
  remove: string;
  move: string;
};

type EventKeys = keyof Events;

//
// Using template strings for constructing the types makes it
// possible that we get the “onAdd”, “onRemove” naming
// convention as in the initial example.
//
type OnEvent = {
  [Key in EventKeys as `on${Capitalize<Key>}`]: () => unknown;
}

const userActions: OnEvent = {
  onAdd: () => null,
  onRemove: () => null,
  onMove: () => null,
};
