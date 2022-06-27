export const NAME = "l22b Modeling Data - Intersection Types";

//
// These are the properties of Tech Event that are common
// to ‘Talk’, ‘Conference’ and ‘Meetup’.
//
type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
  kind: string;
};

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

//
// Let's use Intersection Types (the ‘&’ operator) to
// create the tech event types and reuse ‘TechEventBase’.
//

type Conference = TechEventBase & {
  location: string;
  price: number;
  talks: Talk[];
};

type Meetup = TechEventBase & {
  location: string;
  price: string;
  talks: Talk[];
};

type Webinar = TechEventBase & {
  url: string;
  price?: number;
  //
  // Plural, even though it is a single talk in this case.
  //
  talks: Talk;
};

//
// We call this concept intersection types. We read the ‘&’
// operator as “and”. We combine the properties from one type
// ‘A’ with those of another type ‘B’, much like extending
// classes.  The result is a new type with the properties of
// type ‘A’ and type ‘B’.
//
