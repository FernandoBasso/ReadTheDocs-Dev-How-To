export const NAME = "e07 tech events";

const log: Console["log"] = console.log.bind(console);

//
// The ‘&’ means we are making an intersection between
// ‘TechEventBase’ and the literal type on the right.
//
// The ‘|’ means we are creating a union type.
//

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type EventKind = "webinar" | "conference" | "meetup";

type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
};

//
// Let's use intersection types to combine the base
// type with the few other properties that change
// from type to type of event.
//

type Conference = TechEventBase & {
  kind: "conference";
  location: string;
  price: number;
  talks: Talk[];
};

//
// Pretty much the same as ‘Conference’, except that ‘price’ is
// a string ("free") instead of a number.
//
type Meetup = TechEventBase & {
  kind: "meetup";
  location: string;
  price: string;
  talks: Talk[];
};

//
// ‘Webinar’ has only one talk, and a URL instead of a
// physical location. ‘price’ is optional.
//
type Webinar = TechEventBase & {
  kind: "webinar";
  url: string;
  price?: string;
  talks: Talk; // Why plural in the book?
};

type TechEvent = Conference | Meetup | Webinar;

//
// The switch statement is making use of the DISCRIMINATED
// UNION TYPES to know precisely what shape to expect for each
// member of the union.
//

function getEventTeaser(event: TechEvent): string | never {
  switch (event.kind) {
    case "conference":
      //
      // TypeScript now knows that conferences have prices.
      //
      return `${event.title} (Conference), price ${event.price}`;
    case "meetup":
      //
      // And that meetups will certainly have a location.
      //
      return `${event.title} (Meetup), location ${event.location}`;
    case "webinar":
      //
      // And that webinars have a url.
      //
      return `${event.title} (Webinar), url: ${event.url}`;
    default:
      throw new Error("Did you lie to the type checker‽");
  }
}

const script19 = {
  title: "ScriptConf",
  date: new Date("2019-10-25"),
  capacity: 300,
  rsvp: 289,
  description: "The feel-good JS conference",

  //
  // Since ‘script19’ is not type-annotated, typescript
  // infers ‘kind’ to be ‘string’. It does not know it
  // could be ‘"conference" | "meetup" | "webinar"’.
  //
  // We can do ‘script19.kind = "gotcha"’. No good.
  //
  // Alternatives is to type-annotate ‘script19’:
  //
  //   const script19: TechEvent = { ... };
  //
  // Or (but mind you that type assertions can be
  // VERY dangerous in several situations because we
  // may be lying to the type checker) we can use
  // a type assertion ‘as "conference"’.
  //
  //   {
  //     ...
  //     kind: "conference" as conference,
  //     ...
  //   }
  //
  // BEWARE: Using proper type annotations are
  // recommended, though.
  //
  // Here, will go with a third approach, which is
  // using ‘as const’.
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

getEventTeaser(script19);

function printEvent(event: TechEvent) {
  if (event.price) {
    //
    // Price exists!
    //
    if (typeof event.price === "number") {
      //
      // We know that price is a number
      //
      console.log("Price in EUR: ", event.price);
    } else {
      //
      // We know that price is a string, so the
      // event is free!
      //
      console.log("It is free!");
    }
  }
  if (Array.isArray(event.talks)) {
    //
    // talks is an array
    //
    event.talks.forEach((talk) => {
      console.log(talk.title);
    });
  } else {
    //
    // It's just a single talk
    //
    console.log(event.talks.title);
  }
}
