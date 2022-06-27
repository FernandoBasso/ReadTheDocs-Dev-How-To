export const NAME = "l23d Working with Value Types";

//
// Fixating Value Types using “const assertions”.
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
  //
  // We intentionally don't do this.
  //
  // kind: EventKind;
  //
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
  //
  // Do this instead:
  //
  kind: "conference";
};

type Meetup = TechEventBase & {
  location: string;
  price: string;
  talks: Talk[];
  //
  // Do this instead:
  //
  kind: "meetup";
};

type Webinar = TechEventBase & {
  url: string;
  price?: number;
  talks: Talk;
  //
  // Do this instead:
  //
  kind: "webinar";
};

function getEventTeaser(event: TechEvent) {
  //
  // Hover over this!
  //
  switch (event.kind) {
    case "conference":
      return `${event.title} (Conference)`;
    case "meetup":
      return `${event.title} (Meetup)`;
    case "webinar":
      return `${event.title} (Webinar)`;
    //
    // Again: cool, but not possible. "concert" is not one of
    // the possible event kinds.
    //
    case "concert":
  }
}

//
// Because all three tech events are combined into one union
// type, TypeScript creates a proper union type for the ‘kind’
// property. Awesome!
//
// Hover over ‘kind’ in the ‘switch’ statement.
//
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
const o2 = {
  title: "ScriptConf",
  date: new Date("2019-10-25"),
  capacity: 300,
  rsvp: 289,
  description: "The feel-good JS conference",
  //
  // ‘as conference’ is possible, but one could do:
  //
  //   kind: "conference" as "meetup";
  //
  // which would be a type lie.
  //
  // kind: "conference" as "conference",
  //
  // Better to do this instead:
  //
  kind: "conference" as const,
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
// Hover over o2. Type is a generic object, but at least
// no red squiggles.
//
getEventTeaser(o2);

//
// Not of type ‘Conference’. My point that insisting so much
// on using type inference as much as possible is not always
// the best approach is not that crazy, huh‽
//
