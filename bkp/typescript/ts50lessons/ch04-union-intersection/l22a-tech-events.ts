export const NAME = "l22a Modeling Data"

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type Conference = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
  kind: string;
  location: string;
  price: number;
  talks: Talk[];
};

type Meetup = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
  kind: string;
  location: string;
  price: string; // "free", instead of a number.
  talks: Talk[];
};

//
// Note how ‘price’ is optional.
//
type Webinar = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
  kind: string;
  url: string;
  price?: number;
  //
  // Plural, even though it is a single talk in this case.
  //
  talks: Talk;
};

//
// There is a lot of duplicate stuff among all those types.
//
