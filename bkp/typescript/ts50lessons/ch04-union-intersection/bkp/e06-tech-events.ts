export const NAME = "e06 tech events";

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

//
// Nope, ‘EventKind’ allows for three literal strings,
// but ‘concert’ is not one of them.
//
let s: EventKind = "concert";

type TechEventBase = {
  title: string;
  description: string;
  date: Date,
  capacity: number;
  rsvp: number;
}

//
// Let's use intersection types to combine the base
// type with the few other properties that change
// from type to type of event.
//

type Conference = TechEventBase & {
  kind: "conference",
  location: string;
  price: number;
  talks: Talk[];
};

//
// Pretty much the same as ‘Conference’, except that ‘price’ is
// a string ("free") instead of a number.
//
type Meetup = TechEventBase & {
  kind: "meetup",
  location: string;
  price: string;
  talks: Talk[];
};

//
// ‘Webinar’ has only one talk, and a URL instead of a
// physical location. ‘price’ is optional.
//
type Webinar = TechEventBase & {
  kind: "webinar",
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
  switch(event.kind) {
    case 'conference':
      //
      // TypeScript now knows that conferences have prices.
      //
      return `${event.title} (Conference), price ${event.price}`;
    case 'meetup':
      //
      // And that meetups will certainly have a location.
      //
      return `${event.title} (Meetup), location ${event.location}`;
    case 'webinar':
      //
      // And that webinars have a url.
      //
      return `${event.title} (Webinar), url: ${event.url}`;
    default:
      throw new Error("Did you lie to the type checker‽");
  }
}

function printEvent(event: TechEvent) {
  if (event.price) {
    //
    // Price exists!
    //
    if (typeof event.price === 'number') {
      //
      // We know that price is a number
      //
      console.log('Price in EUR: ', event.price)
    } else {
      //
      // We know that price is a string, so the
      // event is free!
      //
      console.log('It is free!')
    }
  }
  if (Array.isArray(event.talks)) {
    //
    // talks is an array
    //
    event.talks.forEach(talk => {
      console.log(talk.title)
    })
  } else {
    //
    // It's just a single talk
    //
    console.log(event.talks.title)
  }
}
