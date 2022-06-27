export const NAME = "l32b Modeling Data - Intersection Types";

const log: Console["log"] = console.log.bind(console);

//
// The ‘&’ means we are making an intersection between
// ‘TechEventBase’ and the literal type on the right.
//

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type TechEventBase = {
  title: string;
  description: string;
  date: Date,
  capacity: number;
  rsvp: number;
  kind: string;
}

//
// Let's use intersection types to combine the base
// type with the few other properties that change
// from type to type of event.
//

type Conference = TechEventBase & {
  location: string;
  price: number;
  talks: Talk[];
};

//
// Pretty much the same as ‘Conference’, except that ‘price’ is
// a string ("free") instead of a number.
//
type Meetup = TechEventBase & {
  location: string;
  price: string;
  talks: Talk[];
};

//
// ‘Webinar’ has only one talk, and a URL instead of a
// physical location. ‘price’ is optional.
//
type Webinar = TechEventBase & {
  url: string;
  price?: string;
  talks: Talk;
};

//
// ‘T1 & T2’ creates a type containing keys from both
// ‘T1’ and ‘T2’.
//
