export const NAME = "l23c Working with Value Types";

//
// Fixating Value Types
//

//
// Let's NOT use this...
//
// type EventKind = "webinar" | "conference" | "meetup";
//

type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
};

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type Conference = TechEventBase & {
  location: string;
  price: number;
  talks: Talk[];
  kind: "conference";
};

type Meetup = TechEventBase & {
  location: string;
  price: string;
  talks: Talk[];
  kind: "meetup";
};

type Webinar = TechEventBase & {
  url: string;
  price?: number;
  talks: Talk;
  kind: "webinar";
};

type TechEvent = Webinar | Conference | Meetup;

function getEventTeaser(event: TechEvent) {
  switch (event.kind) {
    case "conference":
      //
      // We now know that I'm in type Conference.
      //
      return (
        //
        // Suddenly I don't have to check for price as
        // TypeScript knows it will be there
        //
        `${event.title} (Conference), priced at ${event.price} USD`
      );
    case "meetup":
      //
      // We now know that we're in type Meetup.
      //
      return (
        //
        // Suddenly we can say for sure that this event will
        // have a location, because the type tells us.
        //
        `${event.title} (Meetup), hosted at ${event.location}`
      );
    case "webinar":
      //
      // We now know that we're in type Webinar.
      //
      return (
        //
        // Suddenly we can say for sure that there will be a URL.
        //
        `${event.title} (Webinar), available online at ${event.url}`
      );
    default:
      throw new Error("Not sure what to do with that!");
  }
}

//
// Where before TypeScript just knew that some properties of
// the big TechEvent union type existed or didn’t exist, with
// a specific value type for a property we can directly point
// to the SURROUNDING object type.
//
// Using value types for properties works like a hook for
// TypeScript to find the exact shape inside a union. Types
// like this are called discriminated union types, and they’re
// a safe way to move around in TypeScript’s type space.
//

//
// We are not type-annotating ‘o1’.
//
const o1 = {
  title: "ScriptConf",
  date: new Date("2019-10-25"),
  capacity: 300,
  rsvp: 289,
  description: "The feel-good JS conference",
  kind: "conference", // Type string.
  price: 129,
  location: "Central Linz",
  talks: [
    {
      speaker: "Vitaly Friedman",
      title: "Designing with Privacy in mind",
      abstract: "...",
    },
  ],
};

//
// Hover over o1. A generic object type!
//
getEventTeaser(o1);

//
// Most properties in ‘o1’ will be very generic strings
// and numbers, which are too generic and do not match
// ‘TechEvent’. For example, ‘kind’ is of type ‘string’, not
// ‘conference’ .
//

//
// Here we explicity type-annotate it!
//
const o2: TechEvent = {
  title: "ScriptConf",
  date: new Date("2019-10-25"),
  capacity: 300,
  rsvp: 289,
  description: "The feel-good JS conference",
  kind: "conference", // Type "conference"!
  price: 129,
  location: "Central Linz",
  talks: [
    {
      speaker: "Vitaly Friedman",
      title: "Designing with Privacy in mind",
      abstract: "...",
    },
  ],
};

//
// Hover over o2. Type is ‘Conference’.
//
getEventTeaser(o2);
//
// Here again the context of the ‘kind’ property allows
// TypeScript to infer the surrounding properties. Since only
// of the events has the ‘kind’ property value to
// "conference", TypeScript knows that ‘o2’ is of the type
// ‘Conference’. (Other types of events have other values for
// the ‘kind’ properties).
//

//
// Here the author talks about type inference, and how it
// would make it easier to just rely on structural typing
// without having to annotate values with types.
//
// I disagree in part with this mindset. Writing explicity
// type annotations help to convey intent, and although more
// verbose code is produced, it also prevents more mistakes
// and makes things more understandable, because type names
// are documentation too.
//
// I could say much more about the topic, but this is the gist
// of it.
//
