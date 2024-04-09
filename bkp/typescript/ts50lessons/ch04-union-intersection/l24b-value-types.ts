export const NAME = "l23b Working with Value Types";

//
// Discriminated Union Types
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

type TechEvent = Webinar | Conference | Meetup;


//
// Because all three tech events are combined into one union
// type, TypeScript creates a proper union type for the ‘kind’
// property. Awesome!
//
// Hover over ‘kind’ in the ‘switch’ statement.
//
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
