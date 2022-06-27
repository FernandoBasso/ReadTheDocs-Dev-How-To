export const NAME = "l22a Modeling Data";

const log: Console["log"] = console.log.bind(console);

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type Conference = {
  title: string;
  description: string;
  date: Date,
  capacity: number;
  rsvp: number;
  kind: string;
  location: string;
  price: number;
  talks: Talk[];
};

//
// Pretty much the same as ‘Conference’, except that ‘price’ is
// a string ("free") instead of a number.
//
type Meetup = {
  title: string;
  description: string;
  date: Date,
  capacity: number;
  rsvp: number;
  kind: string;
  location: string;
  price: string;
  talks: Talk[];
};

//
// ‘Webinar’ has only one talk, and a URL instead of a
// physical location. ‘price’ is optional.
//
type Webinar = {
  title: string;
  description: string;
  date: Date,
  capacity: number;
  rsvp: number;
  kind: string;
  url: string;
  price?: string;
  talks: Talk; // <1>
};

//
// Note how many properties are repeated.
//

//
// Why plural in the book? Why ‘talks’ if the type is
// not ‘Talk[]’, but simply ‘Talk’?
//
